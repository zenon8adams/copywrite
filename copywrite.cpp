/***************************************************************************/
/*                                                                         */
/*  copywrite.cpp                                                          */
/*                                                                         */
/*    Driver program to create stamp									   */
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

#define MAX(x, y) ((x) ^ (((x) ^ (y)) & -((x) < (y))))

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
  for ( FT_Int j = 0; j < height; ++j)
  {
    for ( FT_Int i = 0; i < width; ++i)
		fprintf( destination,"%s", out[ j * width + i] ? raster_glyph : " ");
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

void render( const char *word, FT_Face face, const char *raster_glyph, FILE *destination)
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
		hexcess  = MAX( hexcess, ( current.origin.y == 0) * ( current.height - xheight));
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
    }

    if( ac == 1)
        word = *av;
    else
    {
        fprintf( stderr, "Usage: %s [--list-fonts|--font-file=FILE] [--font-size=NUM] [--drawing-character=CHAR] [--output FILE] text\n", program);
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

    render( word, face, raster_glyph, screen);

    FT_Done_Face( face);
    FT_Done_FreeType( library);

    return 0;
}
