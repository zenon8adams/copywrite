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

#define MAX(x, y) ((x) ^ (((x) ^ (y)) & -((x) < (y))))

#define ALLOWANCE 6
#define FPRINTF( fmt, argument) ({ \
    auto count = maxlength - strlen( argument) + ALLOWANCE;\
    char spacing[ count + 1];\
    memset( spacing, ' ', count);\
    spacing[ count] = 0;\
    fprintf( stderr, fmt, argument, spacing);\
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

void draw( Glyph glyph, FT_Vector *pen, unsigned char *out, FT_Int mdescent, FT_Int width, FT_Int height)
{
	FT_Int base = height - glyph.height - mdescent;
	for( FT_Int y = glyph.origin.y, j = 0; j < glyph.height; ++y, ++j)
		for( FT_Int x = glyph.origin.x, i = 0; i < glyph.width; ++x, ++i)
			out[ ( base + pen->y + y) * width + x + pen->x] |= glyph.pixmap[ j * glyph.width + i];
		
	pen->x += glyph.xstep; // Move the pen forward for positioning of the next character
}

void write( const unsigned char *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination)
{
  uint8_t raster_bytes = destination != stdout ? MAX( byteCount( *raster_glyph) - 1, 1) : 1;
  std::string sp( raster_bytes, ' ');

  for ( FT_Int j = 0; j < height; ++j)
  {
    for ( FT_Int i = 0; i < width; ++i)
		fprintf( destination,"%s", out[ j * width + i] ? raster_glyph : sp.c_str());
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

void render(const char *word, FT_Face face, const char *raster_glyph, FILE *destination, bool as_image)
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

	auto *out = ( unsigned char *)calloc( width * height, 1);
	FT_Vector pen;
	memset( &pen, 0, sizeof( pen));
	for(Glyph *current = head, *prev_link; current != nullptr;)
	{
		draw( *current, &pen, out, mdescent, width, height);
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

void writePNG(FILE *cfp, const  png_bytep buffer, png_int_32 width, png_int_32 height)
{
//    FILE *fp = fdopen( fileno( cfp), "wb");

    FILE *fp = fopen( "image.png", "wb");

    fclose( cfp);
    if( fp == nullptr)
        return;

    png_structp png_ptr;
    png_infop info_ptr;
    png_uint_32 bit_depth = 8, bytes_per_pixel = 4;

    png_ptr = png_create_write_struct( PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    if( png_ptr == nullptr)
    {
        fclose( fp);
        return;
    }

    info_ptr = png_create_info_struct( png_ptr);
    if( info_ptr == nullptr)
    {
        fclose( fp);
        png_destroy_write_struct( &png_ptr, &info_ptr);
        return;
    }

    if( setjmp( png_jmpbuf( png_ptr)))
    {
        fclose( fp);
        png_destroy_write_struct( &png_ptr, &info_ptr);
        return;
    }

    png_init_io( png_ptr, fp);

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

    for( png_uint_32 j = 0; j < height; ++j)
    {
        for( png_uint_32 i = 0; i < width; ++i)
        {
            png_uint_32 mbyte = *(buffer + j * width + i) ? 0xFF0000FF : 0;
            memcpy( image + j * width * bytes_per_pixel + i * bytes_per_pixel, ( const char *)&mbyte, bytes_per_pixel);
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

    fclose( fp);
}

int main( int ac, char *av[])
{
    FT_Library    library;
    FT_Face       face;
    FT_Error      error;

    /*
     * Schema:
     *  av[ 0] [--list-fonts|--font-file=FILE] [--font-size=NUM] [--drawing-character=CHAR] [--output FILE] text
     *
     */

    const char *fontfile = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
               *raster_glyph = "\u2589",
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
            screen = fopen( *++av, "w");
            screen = screen == nullptr ? stdout : screen;
        }
        else if( strstr( directive, "font-file") != nullptr)
            selection = &fontfile;
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

        fprintf( stderr, "Usage: %s [%s|%s] [%s] [%s] [%s] [%s] text\n", name,
                *arguments, *( arguments + 1), *( arguments + 2), *( arguments + 3), *( arguments + 4), *( arguments + 5));

        fprintf( stderr, "Displays block form of character sequence\n\n");
        fprintf( stderr, "Arguments:\n");
        FPRINTF("\t%s%sList location of all installed fonts.\n", *arguments);
        FPRINTF("\t%s%sSet the font file to be used for display.\n", *(arguments + 1));
        FPRINTF("\t%s%sSet the font size for display to NUM pixels.\n", *(arguments + 2));
        FPRINTF("\t%s%sSet the character to output in for each block.\n", *(arguments + 3));
        FPRINTF("\t%s%sWrite to file as an image.\n", *(arguments + 4));
        FPRINTF("\t%s%sWrite the block of characters into the file FILE.\n", *(arguments + 5));
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

    render( word, face, raster_glyph, screen, write_as_image);

    FT_Done_Face( face);
    FT_Done_FreeType( library);

    return 0;
}
