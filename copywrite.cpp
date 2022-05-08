/***************************************************************************/
/*                                                                         */
/*  copywrite.cpp                                                          */
/*                                                                         */
/*    Driver program to create stamp					                   */
/*                                                                         */
/*  Copyright 2022 by Adesina Meekness                                     */
/*                                                                         */
/*                                                                         */
/*       ##    ## ##                                                       */
/*       ##    ##  #                                                       */
/*       ###  ###  #  ##                                                   */
/*       # # # ##  # #                                                     */
/* ####  # ### ##  ###                                                     */
/*       #  #  ##  # ##                                                    */
/*       #  #  ##  #  ##                                                   */
/*                                                                         */
/*                                                                         */
/*  This file is part of the Copywrite project, and may only be used,      */
/*  modified, and distributed under the terms of the FreeType project      */
/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
/*  this file you indicate that you have read the license and              */
/*  understand and accept it fully.                                        */
/*                                                                         */
/***************************************************************************/

#include "copywrite.hpp"
#include <fontconfig/fontconfig.h>
#include <png.h>
#include <cmath>

#define MAX(x, y) ((x) ^ (((x) ^ (y)) & -((x) < (y))))
#define ALLOWANCE 6
#define FPRINTFD( fmt, argument, include) ({ \
    auto arglength = strlen( argument);\
    auto count = maxlength - arglength + ALLOWANCE + !include * arglength;\
    char spacing[ count + 1];\
    memset( spacing, ' ', count);\
    spacing[ count] = 0;\
    if( include) \
        fprintf( stderr, fmt, argument, spacing);\
    else \
        fprintf( stderr, fmt, "", spacing);\
})

#define FPRINTF( fmt, argument) FPRINTFD( fmt, argument, true)

/*
 * Converts bitmap into binary format.
 * NB!
 * 	- Pitch is the number of bytes used in representing
 * 	  a row.
 * 	- Pixmap is the final monochrome output
 */

unsigned char *to_monochrome( FT_Bitmap bitmap)
{
	FT_Int rows = bitmap.rows,
	       cols = bitmap.width;
	auto *pixmap = ( unsigned char *)calloc( rows * cols, 1);
	for( FT_Int y = 0; y < rows; ++y)
	{
		for( FT_Int ibyte = 0; ibyte < bitmap.pitch; ++ibyte)
		{
			FT_Int ibit = ibyte * 8,
			       base = y * cols + ibit,
			       cbit = bitmap.buffer[ y * bitmap.pitch + ibyte],
			       rbits = (int)(cols - ibit) < 8 ? cols - ibit : 8;
			for( FT_Int i = 0; i < rbits; ++i)
			   pixmap[ base + i] = ( uint8_t)cbit & (1u << ( 7u - i));
		}
	}

	return pixmap;
}

Glyph extract( FT_GlyphSlot slot)
{
	Glyph glyph;
	glyph.width = slot->bitmap.width;
	glyph.height = slot->bitmap.rows;
	glyph.xstep = slot->advance.x >> 6;
	glyph.pixmap = to_monochrome( slot->bitmap);
	glyph.origin.x = slot->bitmap_left;
	glyph.origin.y = glyph.height - slot->bitmap_top;

	return glyph;
}

void insert( Glyph *&index, Glyph glyph)
{
	if( index == nullptr)
	{
		index = (Glyph *)malloc( sizeof( Glyph));
		memcpy( index, &glyph, sizeof( Glyph));

		return;
	}

	insert( index->next, glyph);
}

FT_Int kerning( FT_UInt c, FT_UInt prev, FT_Face face)
{
	FT_Vector kern;

	FT_Get_Kerning( face, c, prev, FT_KERNING_DEFAULT, &kern);

	return ( uint8_t)kern.x >> 6u;
}

void draw(Glyph glyph, FT_Vector *pen, uint64_t *out, FT_Int mdescent, FT_Int width, FT_Int height, size_t index, const std::vector<ColorRule>& rules)
{
	FT_Int base = height - glyph.height - mdescent;
	ColorRule best;
    for( const auto& each: rules)
    {
        if( ( each.end == INT32_MIN && index == each.start) || ( index >= each.start && ( ( index <= each.end && each.end != INT32_MIN) || each.end == -1)))
            best = each;
    }

	for( FT_Int y = glyph.origin.y, j = 0; j < glyph.height; ++y, ++j)
    {
        for( FT_Int x = glyph.origin.x, i = 0; i < glyph.width; ++x, ++i)
        {
            uint64_t pixel = glyph.pixmap[ j * glyph.width + i];
            out[ ( base + pen->y + y) * width + x + pen->x] |= pixel << 32u | ( pixel ? best.color : 0);
        }
    }

	pen->x += glyph.xstep; // Move the pen forward for positioning of the next character

}

void write( const uint64_t *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination)
{
  uint8_t raster_bytes = destination != stdout ? MAX( byteCount( *raster_glyph) - 1, 1) : 1;
  std::string sp( raster_bytes, ' ');

  for ( FT_Int j = 0; j < height; ++j)
  {
    for ( FT_Int i = 0; i < width; ++i)
		fprintf( destination,"%s", ( out[ j * width + i] >> 32u) ? raster_glyph : sp.c_str());
    fputc( '\n', destination );
  }
}

static size_t byteCount( uint8_t c )
{
    for( uint8_t n = 2u; n <= 6u; ++n )
    {
        bool val = ( ( uint8_t)(c >> n) ^ (0xFFU >> n));
        if( !val )
            return 8 - n;
    }
    return 1;
}

static uint32_t collate( uint8_t *str, size_t idx, uint8_t count )
{
    if( count == 1 )
        return str[idx];

    uint32_t copy = count;

    uint8_t buf[ count], *pbuf = buf;
    memcpy( buf, str + idx, count);

    while( copy > 1 )
        buf[ --copy] &= 0x3FU;

    *pbuf &= 0xFFU >> (count + 1u);
    count -= 1;
    size_t i = ( count << 2u ) + ( count << 1u);

    uint32_t value = 0;
    while( (int8_t)count >= 0 )
    {
        value += *( pbuf++) << i;
        --count;
        i -= 6;
    }

    return value;
}

uint32_t getNumber( const char *&ctx, uint8_t base = 10)
{
    uint32_t weight = 0;
    while( isxdigit( *ctx))
    {
        uint8_t character = tolower( *( ctx++));
        int value = character >= 'a' && character <= 'f' ? character - 'a' + 10 : isdigit( character) ? character - '0' : 0;
        weight = weight * base + value;
    }

    return weight;
}

// Format example: [1]{#244839};[2]{#456676};[3..5]{#594930};[4..]{#567898}
auto parseColorRule( const char *rule)
{
    const char *prev = nullptr;
    std::vector<ColorRule> rules;
    while( *rule)
    {
        ColorRule ccolor;
        if( !rules.empty())
        {
            if(*rule != ';')
            {
                fprintf( stderr, "Incomplete color specification missing semicolon -> %s", rule);
                exit( EXIT_FAILURE);
            }
            else
                ++rule;
        }
        while( *rule)
        {
            if( *rule == '[')
            {
                prev = rule;
                ++rule;
                ccolor.start = getNumber( rule);
                if( *rule != '\0')
                {
                    if( *rule == '.' && *( rule + 1) == '.')
                    {
                        rule += 2;
                        if( isdigit( *rule))
                            ccolor.end = getNumber( rule);

                        if( ccolor.end != -1 && ccolor.end < ccolor.start)
                        {
                            fprintf( stderr, "Color start and end should not overlap -> %s", prev);
                            exit( EXIT_FAILURE);
                        }
                    }
                    else if( *rule == '.')
                    {
                        fprintf( stderr, "Expected `.` before end range -> %s", rule + 1);
                        exit( EXIT_FAILURE);
                    }
                    else
                        ccolor.end = INT32_MIN;
                }
                else
                {
                    fprintf( stderr, "Incomplete color specification");
                    exit( EXIT_FAILURE);
                }
                if( *rule == ']')
                    ++rule;
                else
                {
                    fprintf( stderr, "Incomplete color specification");
                    exit( EXIT_FAILURE);
                }
            }
            else if( *rule == '{')
            {
                ++rule;
                if( *rule != '\0')
                {
                    prev = rule;
                    if( *rule == '#')
                        ++rule;
                    else if( *rule == 'x')
                        ++rule;
                    else if( strncasecmp( rule, "0x", 2) == 0)
                        rule += 2;
                    else
                    {
                        fprintf( stderr, "Expected hex indicator near -> %s (allowed: `#`, `x`, `0x`)", prev);
                        exit( EXIT_FAILURE);
                    }

                    prev = rule;
                    if( isxdigit( *rule))
                    {
                        ccolor.color = getNumber( rule, 16);
                        uint8_t ccount = rule - prev;
                        if( ccolor.color == 0 || ( ccount != 6 && ccount != 8))
                        {
                            fprintf( stderr, ccolor.color == 0 ? "Invalid color specification %s"
                                             : "Color has to be 6 or 8 hex digits -> %s", prev);
                            exit( EXIT_FAILURE);
                        }
                        else if( ccount == 6)
                            ccolor.color = ccolor.color << 8u | 0xFFu;
                    }
                    else
                    {
                        fprintf( stderr, "Expected hex digits -> %s", prev);
                        exit( EXIT_FAILURE);
                    }

                    if( *rule == '}')
                        ++rule;
                    else
                    {
                        fprintf( stderr, "Missing end of expression `}`");
                        exit( EXIT_FAILURE);
                    }

                    ccolor.offset = 4 - strlen((const char *)( &ccolor.color));

                    rules.push_back( ccolor);
                    break;
                }
                else
                {
                    fprintf( stderr, "Incomplete color specification");
                    exit( EXIT_FAILURE);
                }
            }
        }
    }

    return rules;
}

void render(const char *word, FT_Face face, const char *raster_glyph, FILE *destination, bool as_image, const  char *color_rule)
{
	Glyph *head = nullptr;
	FT_Int width 	= 0, // Total width of the buffer
		   mdescent = 0, // Holds the baseline for the character with most descent
		   xheight 	= 0, // Holds the height taken by character 'x' (xheight)
		   hexcess 	= 0; // Holds the ascent height based on the
						 // presence of descented characters like 'g'
	FT_UInt prev 	= 0; // Holds previous character read

	FT_Error error = FT_Load_Char( face, 'x', FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
	if( !error)
		xheight = face->glyph->bitmap.rows;

	for( const char *pw = word; *pw; )
	{
	    FT_Int shift = byteCount( *pw);
	    FT_UInt character = collate( (uint8_t *)word, pw - word, shift);

	    error = FT_Load_Char( face, character, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
    	if ( error )
      		continue;

		Glyph current = extract( face->glyph);
		hexcess  = MAX( hexcess,( current.height - xheight));
		mdescent = MAX( mdescent, current.origin.y);
		current.xstep += kerning( *pw, prev, face);
		width += current.xstep;
		insert( head, current);

		prev = character;
		pw += shift;
	}

	// Final container height. Dependent on the presence of
	// characters with stem like 'b' and characters with descent
	// like 'g'

	FT_Int height = hexcess + xheight + mdescent;

	auto *out = ( uint64_t *)calloc( width * height, sizeof( uint64_t));
	FT_Vector pen;
	memset( &pen, 0, sizeof( pen));
	size_t index = 0;
	auto rules = color_rule == nullptr ? std::vector<ColorRule>{} : parseColorRule( color_rule);
	for(Glyph *current = head, *prev_link; current != nullptr;)
	{
        draw( *current, &pen, out, mdescent, width, height, ++index, rules);

        prev_link = current;
		current = current->next;
		free(prev_link->pixmap);
		free(prev_link);
	}

	if( as_image && destination != stdout)
        writePNG( destination, out, width, height);
	else
	    write( out, width, height, raster_glyph, destination);

	free( out);
}

void requestFontList()
{
    if( !FcInit())
        return;

    FcConfig *config = FcConfigGetCurrent();
    FcConfigSetRescanInterval( config, 0);
    FcPattern *pattern = FcPatternCreate();
    FcObjectSet *fontObjectSet = FcObjectSetBuild( FC_FILE, nullptr);
    FcFontSet *fontSet = FcFontList( config, pattern, fontObjectSet);

    if( fontSet && fontSet->nfont > 0)
    {
        int i = 0;
        do
        {
            const char *font = ( const char *)FcNameUnparse( fontSet->fonts[ i]),
                *breakp = strchr( font, '/');

            printf( "\t%s\n", breakp);

            free( ( FcChar8 *)font);
        }
        while( ++i < fontSet->nfont);

    }

    if( fontSet)
        FcFontSetDestroy( fontSet);
}

void writePNG(FILE *cfp, const uint64_t *buffer, png_int_32 width, png_int_32 height)
{
    if(cfp == nullptr)
        return;

    png_structp png_ptr;
    png_infop info_ptr;
    png_uint_32 bit_depth = 8, bytes_per_pixel = 4;

    png_ptr = png_create_write_struct( PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    if( png_ptr == nullptr)
    {
        fclose(cfp);
        return;
    }

    info_ptr = png_create_info_struct( png_ptr);
    if( info_ptr == nullptr)
    {
        fclose(cfp);
        png_destroy_write_struct( &png_ptr, &info_ptr);
        return;
    }

    if( setjmp( png_jmpbuf( png_ptr)))
    {
        fclose(cfp);
        png_destroy_write_struct( &png_ptr, &info_ptr);
        return;
    }

    png_init_io(png_ptr, cfp);

    png_set_IHDR( png_ptr, info_ptr, width, height, bit_depth, PNG_COLOR_TYPE_RGB_ALPHA,
                  PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    png_text text_ptr[3];

    char key0[] = "Title";
    char text0[] = "Copywrite";
    text_ptr[0].key = key0;
    text_ptr[0].text = text0;
    text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
    text_ptr[0].itxt_length = 0;
    text_ptr[0].lang = nullptr;
    text_ptr[0].lang_key = nullptr;

    char key1[] = "Author";
    char text1[] = "Adesina Meekness";
    text_ptr[1].key = key1;
    text_ptr[1].text = text1;
    text_ptr[1].compression = PNG_TEXT_COMPRESSION_NONE;
    text_ptr[1].itxt_length = 0;
    text_ptr[1].lang = nullptr;
    text_ptr[1].lang_key = nullptr;

    char key2[] = "Description";
    char text2[] = "An image generated by copywrite program";
    text_ptr[2].key = key2;
    text_ptr[2].text = text2;
    text_ptr[2].compression = PNG_TEXT_COMPRESSION_zTXt;
    text_ptr[2].itxt_length = 0;
    text_ptr[2].lang = nullptr;
    text_ptr[2].lang_key = nullptr;

    png_set_text( png_ptr, info_ptr, text_ptr, 3);

    png_write_info(png_ptr, info_ptr);

    if( height > PNG_SIZE_MAX / ( width * bytes_per_pixel))
        png_error( png_ptr, "Image data buffer too large!");

    auto image = ( png_bytep)png_calloc( png_ptr, height * width * bytes_per_pixel);
    png_bytep row_pointers[ height];
    unsigned char color_buffer[ bytes_per_pixel];
    memset( color_buffer, 0, bytes_per_pixel);

    for( png_uint_32 j = 0; j < height; ++j)
    {
        for( png_uint_32 i = 0; i < width; ++i)
        {
            uint64_t pixel = *(buffer + j * width + i);
            png_uint_32 color = pixel & 0xFFFFFFFF;
            png_uint_32 mbyte = ( pixel >> 32u) ? color : 0;
            png_uint_32 index = j * width * bytes_per_pixel + i * bytes_per_pixel;
            image[     index] = mbyte >> 24u;
            image[ index + 1] = ( mbyte >> 16u) & 0xFFu;
            image[ index + 2] = ( mbyte >>  8u) & 0xFFu;
            image[ index + 3] = mbyte & 0xFFu;
        }
    }

    if( height > PNG_UINT_32_MAX / ( sizeof(png_bytep)))
        png_error( png_ptr, "Image too small to process!");

    for( png_uint_32 i = 0; i < height; ++i)
        row_pointers[ i] = image + i * width * bytes_per_pixel;

    png_write_image( png_ptr, row_pointers);

    png_write_end( png_ptr, info_ptr);

    png_destroy_write_struct( &png_ptr, &info_ptr);

    png_free( png_ptr, image);

    fclose(cfp);
}

int main( int ac, char *av[])
{

//    parseColorRule( "[1]{#244839};[2]{#456676};[4..4]{#p59930};[4..]{#567898};");

    FT_Library    library;
    FT_Face       face;
    FT_Error      error;

    /*
     * Schema:
     *  av[ 0] [--list-fonts|--font-file=FILE [--font-size=NUM] [--drawing-character=CHAR] [--output FILE]] text
     *
     */

    const char *fontfile = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
               *raster_glyph = "\u2589",
               *color_rule = nullptr,
               *font_size = "10",
               *word = "Hello World",
               *program = *av;
    bool write_as_image = false;
    FILE *screen = stdout;

    while( --ac > 0 && ( *++av)[ 0] == '-')
    {
        const char *directive = *av + 2,
                   *index = nullptr,
                   **selection = nullptr;

        if( strcmp( directive, "list-fonts") == 0)
        {
            puts( "Available fonts:\n");
            requestFontList();

            exit( EXIT_SUCCESS);
        }
        else if( strcmp( directive, "as-image") == 0)
            write_as_image = true;
        else if( strcmp( directive, "output") == 0 && ac > 0)
        {
            ac -= 1;
            screen = fopen( *++av, "wb");
            screen = screen == nullptr ? stdout : screen;
        }
        else if( strstr( directive, "font-file") != nullptr)
            selection = &fontfile;
        else if( strstr( directive, "color-rule") != nullptr)
            selection = &color_rule;
        else if( strstr( directive, "font-size") != nullptr)
            selection = &font_size;
        else if( strstr( directive, "drawing-character") != nullptr)
            selection = &raster_glyph;

        if( selection != nullptr && ( index = strchr( directive, '=')) != nullptr)
            *selection = index + 1;
        else if( selection != nullptr)
        {
            ac = -1;
            break;
        }
    }

    if( ac == 1)
        word = *av;
    else
    {
        const char *start = strrchr( program, '/'),
                   *name = start != nullptr ? start + 1 : program,
                   *arguments[] = {
                        "--list-fonts",
                        "--font-file=FILE",
                        "--color-rule=RULE",
                        "--font-size=NUM",
                        "--drawing-character=CHAR",
                        "--as-image",
                        "--output FILE",
                    };

        size_t maxlength = 0;
        {
            size_t idx = 0, length = sizeof( arguments) / sizeof( arguments[ 0]);
            while( idx < length)
            {
                size_t clen = strlen( arguments[ idx++]);
                maxlength = MAX(clen, maxlength);
            }
        }

        fprintf( stderr, "Usage: %s [%s|%s [%s] [%s] [%s] [%s] [%s]] text\n", name,
                *arguments, *( arguments + 1), *( arguments + 2), *( arguments + 3), *( arguments + 4), *( arguments + 5), *( arguments + 6));

        fprintf( stderr, "Displays block form of character sequence\n\n");
        fprintf( stderr, "Arguments:\n");
        FPRINTF("\t%s%sList location of all installed fonts.\n", *arguments);
        FPRINTF("\t%s%sSet the font file to be used for display.\n", *(arguments + 1));
        FPRINTF("\t%s%sPaint image based on the RULE given by: ^(\\[(\\d+)(\\.\\.(\\d+)?)?\\]"
                       "\\{(#|0?x)\\d{6,8}\\})(;\\[(\\d+)(\\.\\.(\\d+)?)?\\]\\{(\\#|0?x)\\d{6,8}\\})*;?$.\n", *(arguments + 2));
        FPRINTFD( "\t%s%sExample: [1]{#244839};[2]{#456676};[3..4]{#559930};[5..]{#567898};\n", *( arguments + 2), false);
        FPRINTFD( "\t%s%s  Word: `Hello` ==>  H -> #244839\n", *( arguments + 2), false);
        FPRINTFD( "\t%s%s  Word: `Hello` ==>  e -> #456676\n", *( arguments + 2), false);
        FPRINTFD( "\t%s%s  Word: `Hello` ==> ll -> #559930\n", *( arguments + 2), false);
        FPRINTFD( "\t%s%s  Word: `Hello` ==>  0 -> #567898\n", *( arguments + 2), false);
        FPRINTFD( "\t%s%sNB! If two rules match a character, the last one takes precedence.\n", *( arguments + 2), false);
        FPRINTF("\t%s%sSet the font size for display to NUM pixels.\n", *(arguments + 3));
        FPRINTF("\t%s%sSet the character to output in for each block.\n", *(arguments + 4));
        FPRINTF("\t%s%sWrite to file as an image. Used with --output flag\n", *(arguments + 5));
        FPRINTF("\t%s%sWrite the block of characters into the file FILE.\n", *(arguments + 6));
        exit( EXIT_FAILURE);
    }

    error = FT_Init_FreeType( &library );

    if( error != 0)
    {
      fprintf( stderr, "Unable to startup!");
      exit( EXIT_FAILURE);
    }

    error = FT_New_Face( library, fontfile, 0, &face);

    if( error != 0)
    {
        fprintf( stderr, "Font file is invalid!");
        exit( EXIT_FAILURE);
    }

    error = FT_Set_Pixel_Sizes( face, strtol( font_size, nullptr, 10), 0);

    if( error != 0)
    {
        fprintf( stderr, "Setup error!");
        exit( EXIT_FAILURE);
    }

    render( word, face, raster_glyph, screen, write_as_image, color_rule);

    FT_Done_Face( face);
    FT_Done_FreeType( library);

    return 0;
}