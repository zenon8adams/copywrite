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
#include <iostream>

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

#define ABS( expr) ({\
    auto value = expr;\
    auto mask = value >> ( ( sizeof( value) << 3u) - 1u);\
    (value ^ mask) - mask;\
})

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

void insert( KDNode *&node, Color color, size_t index = 0, uint8_t depth = 0)
{
    if( node == nullptr)
    {
        node = new KDNode{ color, index};
        return;
    }

    uint8_t nchannel = color.rgb[ depth],
            cchannel = node->color.rgb[ depth],
            ndepth   = ( depth + 1) % 3;
    if( nchannel < cchannel)
        insert(node->left, color, index, ndepth);
    else
        insert(node->right, color, index, ndepth);
}

void free( KDNode *&node)
{
    if( node == nullptr)
        return;

    free( node->left);
    free( node->right);

    delete node; node = nullptr;
}

float qsqrt( float number)
{
    long i;
    float xhalf, y;
    const float threehalfs = 1.5f;

    xhalf = number * .5f;
    y  = number;
    i  = *( long *)&y;
    i = 0x5f3759df - ( i >> 1);
    y = *( float *)&i;
    y = y * ( threehalfs - ( xhalf * y * y));

    return 1 / y;
}

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best = nullptr, uint8_t depth = 0)
{
    if( node == nullptr)
        return best;

    uint8_t nchannel = search.rgb[ depth],
            cchannel = node->color.rgb[ depth],
            ndepth   = ( depth + 1) % 3;

    int r = search.rgb[ 0] - node->color.rgb[ 0],
            g = search.rgb[ 1] - node->color.rgb[ 1],
            b = search.rgb[ 2] - node->color.rgb[ 2];

    double y = .299    * r + .587   * g + .114   * b,
           u = -.14713 * r - .28886 * g + .436   * b,
           v = .615    * r - .51499 * g - .10001 * b;

    float ndist = qsqrt( ( float)(y * y + u * u + v * v));

    if( ndist < ldist)
    {
        ldist = ndist;
        best = node;
    }

    bool left = true;
    if( nchannel < cchannel)
    {
        if( node->left != nullptr)
            best = approximate(node->left, search, ldist, best, ndepth);
        else
            return best;
    }
    else
    {
        if( node->right != nullptr)
            best = approximate(node->right, search, ldist, best, ndepth);
        else
            return best;
        left = false;
    }

    if( ABS( search.rgb[ depth] - node->color.rgb[ depth]) < ldist)
        best = approximate(!left ? node->left : node->right, search, ldist, best, ndepth);

    return best;
}


void write(const uint64_t *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination, KDNode *root)
{
    bool is_stdout = false;
    uint8_t raster_bytes = destination != stdout ? MAX( byteCount( *raster_glyph) - 1, 1) : is_stdout = true;
    std::string sp( raster_bytes, ' ');

    uint8_t fmt[] = { '\x1B', '[', '3', '8', ';', '5', ';', '0', '0', '0', 'm', '\0'};
    uint8_t offset = 7;

    for ( FT_Int j = 0; j < height; ++j)
    {
        for ( FT_Int i = 0; i < width; ++i)
        {
            double initial = INFINITY;
            uint64_t byte =  out[ j * width + i];
            uint32_t color = byte & 0xFFFFFFFFu;
            bool is_transparent = ( color & 0xFFu) == 0u;
            auto nmatch = approximate( root, {( uint8_t)( color >> 24u),
                                             ( uint8_t)( ( color >> 16u) & 0xFFu), ( uint8_t)( ( color >> 8u) & 0xFFu)}, initial);
            fmt[     offset] = nmatch->index / 100 + '0';
            fmt[ offset + 1] = ( nmatch->index - ( fmt[ offset] - '0') * 100) / 10 + '0';
            fmt[ offset + 2] = nmatch->index % 10  + '0';

            if( is_stdout)
                fprintf( destination, "%s", ( const char *)fmt);
            fprintf( destination,"%s", byte >> 32u && !is_transparent ? raster_glyph : sp.c_str());
            if( is_stdout)
                fprintf( destination, "\x1B[0m");
        }
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

void render(const char *word, FT_Face face, const char *raster_glyph, FILE *destination, bool as_image,
            const char *color_rule, KDNode *root)
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
        write(out, width, height, raster_glyph, destination, root);

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
    KDNode *root = nullptr;

    insert( root, { 0, 0, 0}, 0);
    insert( root, { 128, 0, 0}, 1);
    insert( root, { 0, 128, 0}, 2);
    insert( root, { 128, 128, 0}, 3);
    insert( root, { 0, 0, 128}, 4);
    insert( root, { 128, 0, 128}, 5);
    insert( root, { 0, 128, 128}, 6);
    insert( root, { 192, 192, 192}, 7);
    insert( root, { 128, 128, 128}, 8);
    insert( root, { 255, 0, 0}, 9);
    insert( root, { 0, 255, 0}, 10);
    insert( root, { 255, 255, 0}, 11);
    insert( root, { 0, 0, 255}, 12);
    insert( root, { 255, 0, 255}, 13);
    insert( root, { 0, 255, 255}, 14);
    insert( root, { 255, 255, 255}, 15);
    insert( root, { 0, 0, 0}, 16);
    insert( root, { 0, 0, 95}, 17);
    insert( root, { 0, 0, 135}, 18);
    insert( root, { 0, 0, 175}, 19);
    insert( root, { 0, 0, 215}, 20);
    insert( root, { 0, 0, 255}, 21);
    insert( root, { 0, 95, 0}, 22);
    insert( root, { 0, 95, 95}, 23);
    insert( root, { 0, 95, 135}, 24);
    insert( root, { 0, 95, 175}, 25);
    insert( root, { 0, 95, 215}, 26);
    insert( root, { 0, 95, 255}, 27);
    insert( root, { 0, 135, 0}, 28);
    insert( root, { 0, 135, 95}, 29);
    insert( root, { 0, 135, 135}, 30);
    insert( root, { 0, 135, 175}, 31);
    insert( root, { 0, 135, 215}, 32);
    insert( root, { 0, 135, 255}, 33);
    insert( root, { 0, 175, 0}, 34);
    insert( root, { 0, 175, 95}, 35);
    insert( root, { 0, 175, 135}, 36);
    insert( root, { 0, 175, 175}, 37);
    insert( root, { 0, 175, 215}, 38);
    insert( root, { 0, 175, 255}, 39);
    insert( root, { 0, 215, 0}, 40);
    insert( root, { 0, 215, 95}, 41);
    insert( root, { 0, 215, 135}, 42);
    insert( root, { 0, 215, 175}, 43);
    insert( root, { 0, 215, 215}, 44);
    insert( root, { 0, 215, 255}, 45);
    insert( root, { 0, 255, 0}, 46);
    insert( root, { 0, 255, 95}, 47);
    insert( root, { 0, 255, 135}, 48);
    insert( root, { 0, 255, 175}, 49);
    insert( root, { 0, 255, 215}, 50);
    insert( root, { 0, 255, 255}, 51);
    insert( root, { 95, 0, 0}, 52);
    insert( root, { 95, 0, 95}, 53);
    insert( root, { 95, 0, 135}, 54);
    insert( root, { 95, 0, 175}, 55);
    insert( root, { 95, 0, 215}, 56);
    insert( root, { 95, 0, 255}, 57);
    insert( root, { 95, 95, 0}, 58);
    insert( root, { 95, 95, 95}, 59);
    insert( root, { 95, 95, 135}, 60);
    insert( root, { 95, 95, 175}, 61);
    insert( root, { 95, 95, 215}, 62);
    insert( root, { 95, 95, 255}, 63);
    insert( root, { 95, 135, 0}, 64);
    insert( root, { 95, 135, 95}, 65);
    insert( root, { 95, 135, 135}, 66);
    insert( root, { 95, 135, 175}, 67);
    insert( root, { 95, 135, 215}, 68);
    insert( root, { 95, 135, 255}, 69);
    insert( root, { 95, 175, 0}, 70);
    insert( root, { 95, 175, 95}, 71);
    insert( root, { 95, 175, 135}, 72);
    insert( root, { 95, 175, 175}, 73);
    insert( root, { 95, 175, 215}, 74);
    insert( root, { 95, 175, 255}, 75);
    insert( root, { 95, 215, 0}, 76);
    insert( root, { 95, 215, 95}, 77);
    insert( root, { 95, 215, 135}, 78);
    insert( root, { 95, 215, 175}, 79);
    insert( root, { 95, 215, 215}, 80);
    insert( root, { 95, 215, 255}, 81);
    insert( root, { 95, 255, 0}, 82);
    insert( root, { 95, 255, 95}, 83);
    insert( root, { 95, 255, 135}, 84);
    insert( root, { 95, 255, 175}, 85);
    insert( root, { 95, 255, 215}, 86);
    insert( root, { 95, 255, 255}, 87);
    insert( root, { 135, 0, 0}, 88);
    insert( root, { 135, 0, 95}, 89);
    insert( root, { 135, 0, 135}, 90);
    insert( root, { 135, 0, 175}, 91);
    insert( root, { 135, 0, 215}, 92);
    insert( root, { 135, 0, 255}, 93);
    insert( root, { 135, 95, 0}, 94);
    insert( root, { 135, 95, 95}, 95);
    insert( root, { 135, 95, 135}, 96);
    insert( root, { 135, 95, 175}, 97);
    insert( root, { 135, 95, 215}, 98);
    insert( root, { 135, 95, 255}, 99);
    insert( root, { 135, 135, 0}, 100);
    insert( root, { 135, 135, 95}, 101);
    insert( root, { 135, 135, 135}, 102);
    insert( root, { 135, 135, 175}, 103);
    insert( root, { 135, 135, 215}, 104);
    insert( root, { 135, 135, 255}, 105);
    insert( root, { 135, 175, 0}, 106);
    insert( root, { 135, 175, 95}, 107);
    insert( root, { 135, 175, 135}, 108);
    insert( root, { 135, 175, 175}, 109);
    insert( root, { 135, 175, 215}, 110);
    insert( root, { 135, 175, 255}, 111);
    insert( root, { 135, 215, 0}, 112);
    insert( root, { 135, 215, 95}, 113);
    insert( root, { 135, 215, 135}, 114);
    insert( root, { 135, 215, 175}, 115);
    insert( root, { 135, 215, 215}, 116);
    insert( root, { 135, 215, 255}, 117);
    insert( root, { 135, 255, 0}, 118);
    insert( root, { 135, 255, 95}, 119);
    insert( root, { 135, 255, 135}, 120);
    insert( root, { 135, 255, 175}, 121);
    insert( root, { 135, 255, 215}, 122);
    insert( root, { 135, 255, 255}, 123);
    insert( root, { 175, 0, 0}, 124);
    insert( root, { 175, 0, 95}, 125);
    insert( root, { 175, 0, 135}, 126);
    insert( root, { 175, 0, 175}, 127);
    insert( root, { 175, 0, 215}, 128);
    insert( root, { 175, 0, 255}, 129);
    insert( root, { 175, 95, 0}, 130);
    insert( root, { 175, 95, 95}, 131);
    insert( root, { 175, 95, 135}, 132);
    insert( root, { 175, 95, 175}, 133);
    insert( root, { 175, 95, 215}, 134);
    insert( root, { 175, 95, 255}, 135);
    insert( root, { 175, 135, 0}, 136);
    insert( root, { 175, 135, 95}, 137);
    insert( root, { 175, 135, 135}, 138);
    insert( root, { 175, 135, 175}, 139);
    insert( root, { 175, 135, 215}, 140);
    insert( root, { 175, 135, 255}, 141);
    insert( root, { 175, 175, 0}, 142);
    insert( root, { 175, 175, 95}, 143);
    insert( root, { 175, 175, 135}, 144);
    insert( root, { 175, 175, 175}, 145);
    insert( root, { 175, 175, 215}, 146);
    insert( root, { 175, 175, 255}, 147);
    insert( root, { 175, 215, 0}, 148);
    insert( root, { 175, 215, 95}, 149);
    insert( root, { 175, 215, 135}, 150);
    insert( root, { 175, 215, 175}, 151);
    insert( root, { 175, 215, 215}, 152);
    insert( root, { 175, 215, 255}, 153);
    insert( root, { 175, 255, 0}, 154);
    insert( root, { 175, 255, 95}, 155);
    insert( root, { 175, 255, 135}, 156);
    insert( root, { 175, 255, 175}, 157);
    insert( root, { 175, 255, 215}, 158);
    insert( root, { 175, 255, 255}, 159);
    insert( root, { 215, 0, 0}, 160);
    insert( root, { 215, 0, 95}, 161);
    insert( root, { 215, 0, 135}, 162);
    insert( root, { 215, 0, 175}, 163);
    insert( root, { 215, 0, 215}, 164);
    insert( root, { 215, 0, 255}, 165);
    insert( root, { 215, 95, 0}, 166);
    insert( root, { 215, 95, 95}, 167);
    insert( root, { 215, 95, 135}, 168);
    insert( root, { 215, 95, 175}, 169);
    insert( root, { 215, 95, 215}, 170);
    insert( root, { 215, 95, 255}, 171);
    insert( root, { 215, 135, 0}, 172);
    insert( root, { 215, 135, 95}, 173);
    insert( root, { 215, 135, 135}, 174);
    insert( root, { 215, 135, 175}, 175);
    insert( root, { 215, 135, 215}, 176);
    insert( root, { 215, 135, 255}, 177);
    insert( root, { 215, 175, 0}, 178);
    insert( root, { 215, 175, 95}, 179);
    insert( root, { 215, 175, 135}, 180);
    insert( root, { 215, 175, 175}, 181);
    insert( root, { 215, 175, 215}, 182);
    insert( root, { 215, 175, 255}, 183);
    insert( root, { 215, 215, 0}, 184);
    insert( root, { 215, 215, 95}, 185);
    insert( root, { 215, 215, 135}, 186);
    insert( root, { 215, 215, 175}, 187);
    insert( root, { 215, 215, 215}, 188);
    insert( root, { 215, 215, 255}, 189);
    insert( root, { 215, 255, 0}, 190);
    insert( root, { 215, 255, 95}, 191);
    insert( root, { 215, 255, 135}, 192);
    insert( root, { 215, 255, 175}, 193);
    insert( root, { 215, 255, 215}, 194);
    insert( root, { 215, 255, 255}, 195);
    insert( root, { 255, 0, 0}, 196);
    insert( root, { 255, 0, 95}, 197);
    insert( root, { 255, 0, 135}, 198);
    insert( root, { 255, 0, 175}, 199);
    insert( root, { 255, 0, 215}, 200);
    insert( root, { 255, 0, 255}, 201);
    insert( root, { 255, 95, 0}, 202);
    insert( root, { 255, 95, 95}, 203);
    insert( root, { 255, 95, 135}, 204);
    insert( root, { 255, 95, 175}, 205);
    insert( root, { 255, 95, 215}, 206);
    insert( root, { 255, 95, 255}, 207);
    insert( root, { 255, 135, 0}, 208);
    insert( root, { 255, 135, 95}, 209);
    insert( root, { 255, 135, 135}, 210);
    insert( root, { 255, 135, 175}, 211);
    insert( root, { 255, 135, 215}, 212);
    insert( root, { 255, 135, 255}, 213);
    insert( root, { 255, 175, 0}, 214);
    insert( root, { 255, 175, 95}, 215);
    insert( root, { 255, 175, 135}, 216);
    insert( root, { 255, 175, 175}, 217);
    insert( root, { 255, 175, 215}, 218);
    insert( root, { 255, 175, 255}, 219);
    insert( root, { 255, 215, 0}, 220);
    insert( root, { 255, 215, 95}, 221);
    insert( root, { 255, 215, 135}, 222);
    insert( root, { 255, 215, 175}, 223);
    insert( root, { 255, 215, 215}, 224);
    insert( root, { 255, 215, 255}, 225);
    insert( root, { 255, 255, 0}, 226);
    insert( root, { 255, 255, 95}, 227);
    insert( root, { 255, 255, 135}, 228);
    insert( root, { 255, 255, 175}, 229);
    insert( root, { 255, 255, 215}, 230);
    insert( root, { 255, 255, 255}, 231);
    insert( root, { 8, 8, 8}, 232);
    insert( root, { 18, 18, 18}, 233);
    insert( root, { 28, 28, 28}, 234);
    insert( root, { 38, 38, 38}, 235);
    insert( root, { 48, 48, 48}, 236);
    insert( root, { 58, 58, 58}, 237);
    insert( root, { 68, 68, 68}, 238);
    insert( root, { 78, 78, 78}, 239);
    insert( root, { 88, 88, 88}, 240);
    insert( root, { 98, 98, 98}, 241);
    insert( root, { 108, 108, 108}, 242);
    insert( root, { 118, 118, 118}, 243);
    insert( root, { 128, 128, 128}, 244);
    insert( root, { 138, 138, 138}, 245);
    insert( root, { 148, 148, 148}, 246);
    insert( root, { 158, 158, 158}, 247);
    insert( root, { 168, 168, 168}, 248);
    insert( root, { 178, 178, 178}, 249);
    insert( root, { 188, 188, 188}, 250);
    insert( root, { 198, 198, 198}, 251);
    insert( root, { 208, 208, 208}, 252);
    insert( root, { 218, 218, 218}, 253);
    insert( root, { 228, 228, 228}, 254);
    insert( root, { 238, 238, 238}, 255);

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

    render(word, face, raster_glyph, screen, write_as_image, color_rule, root);

    FT_Done_Face( face);
    FT_Done_FreeType( library);
    free( root);

    return 0;
}