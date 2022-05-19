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
/*  modified, and distributed under the terms of the GNU project           */
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
#include <unordered_map>
#include <functional>
#include <random>
#include "colors_defs.hpp"
#include "easing_defs.hpp"

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

#define ALLOWANCE                      6
#define MAX(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) < (y))))
#define MIN(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) > (y))))

#define FPRINTF( fmt, argument)        FPRINTFD( fmt, argument, true)

#define EPSILON                        ( 1e-5)
#define ZERO( fl)                      ( std::abs( fl) <= EPSILON)
#define EQUAL( al, bl)                 ZERO( ( al) - ( bl))

#define RED( color)                    ( ( color) >> 24u)
#define GREEN( color)                  ( ( ( color) >> 16u) & 0xFFu)
#define BLUE( color)                   ( ( ( color) >> 8u) & 0xFFu)
#define ALPHA( color)                  ( ( color) & 0xFFu)
#define RGBA( red, green, blue, alpha) ( ( ( uint32_t)( red)) << 24u |\
                                          ( ( uint32_t)( green)) << 16u | ( ( uint32_t)( blue)) << 8u | alpha)
#define RGB( red, green, blue)         RGBA( red, green, blue, 0u)
#define SCALE_RGB( color, scale)       RGB( RED( color) * ( scale), GREEN( color) * ( scale), BLUE( color) * ( scale))

#define XYZ_SCALE                      775
#define RGB_SCALE                      255
#define HALF_RGB_SCALE                 128

/*
 * Converts bitmap into binary format.
 * NB!
 * 	- Pitch is the number of bytes used in representing
 * 	  a row.
 * 	- Pixmap is the final monochrome output
 */

unsigned char *toMonochrome( FT_Bitmap bitmap)
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
	glyph.xstep = slot->advance.x / 64;
	glyph.pixmap = toMonochrome(slot->bitmap);
	glyph.origin.x = slot->bitmap_left;
	glyph.origin.y = glyph.height - slot->bitmap_top;

	return glyph;
}

void insert( Glyph *&index, Glyph glyph)
{
	if( index == nullptr)
	{
		index = (Glyph *)malloc( sizeof( Glyph));
		memcpy( ( char *)index, (const char *)&glyph, sizeof( Glyph));

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

void draw( const Glyph& glyph, FT_Vector *pen, uint64_t *out, FT_Int mdescent, FT_Int width, FT_Int height, size_t total)
{
    FT_Int base = height - glyph.height - mdescent;

    uint32_t color = glyph.match.scolor;
    if( glyph.match.color_easing_fn)
    {
        size_t start = glyph.index - glyph.match.start,
                end = glyph.match.end == -1 ? MAX( total, 1) : glyph.match.end - glyph.match.start;

        auto fraction = glyph.match.color_easing_fn(( float)start / end);
        color = interpolateColor( glyph.match.scolor, glyph.match.ecolor, fraction);
    }

    for( FT_Int y = glyph.origin.y, j = 0; j < glyph.height; ++y, ++j)
    {
        for( FT_Int x = glyph.origin.x, i = 0; i < glyph.width; ++x, ++i)
        {
            uint64_t pixel = glyph.pixmap[ j * glyph.width + i];
            out[ ( base + pen->y + y) * width + x + pen->x] |= pixel << 32u | ( pixel ? color : 0);
        }
    }

    pen->x += glyph.xstep; // Move the pen forward for positioning of the next character
}

size_t countCharacters( const char *pw)
{
    size_t value = 0;
    while( *pw)
    {
        size_t ccount = byteCount( *pw);
        value += 1;
        pw += ccount;
    }

    return value;
}

void render( const char *word, FT_Face face, size_t default_font_size, const char *raster_glyph, FILE *destination, bool as_image,
             const char *color_rule, KDNode *root, BKNode *bkroot)
{
    Glyph *head = nullptr;
    FT_Int width 	= 0, // Total width of the buffer
            mdescent = 0, // Holds the baseline for the character with most descent
            mxheight = 0, // Maximum heigh of character 'x' according to various font sizes
            hexcess 	= 0; // Holds the ascent height based on the
    // presence of descented characters like 'g'
    FT_UInt prev 	= 0; // Holds previous character read

    FT_Error error;

    auto rules = color_rule == nullptr ? std::vector<ColorRule>{} : parseColorRule( color_rule, bkroot);

    size_t index = 0, nchars = countCharacters( word);
    for( const char *pw = word; *pw;)
    {
        FT_Int shift = byteCount( *pw);
        FT_UInt character = collate( (uint8_t *)word, pw - word, shift);

        ColorRule best{};
        index += 1;
        for( const auto& each: rules)
        {
            if( ( each.end == INT32_MIN && index == each.start) || ( index >= each.start && ( ( index <= each.end && each.end != INT32_MIN) || each.end == -1)))
                best = each;
        }

        FT_Int font_size = best.font_size_b == UINT32_MAX ? best.font_size_b = default_font_size : best.font_size_b;
        if( best.font_easing_fn)
        {
            size_t start = index - best.start,
                    end = best.end == -1 ? MAX( nchars - 1, 1) : best.end - best.start;

            auto fraction = best.font_easing_fn(( float)start / end);
            if( best.font_size_m == UINT32_MAX && best.font_size_e != UINT32_MAX)
                font_size = round( best.font_size_b * ( 1.0 - fraction) + fraction * best.font_size_e);
            else if( best.font_size_m != UINT32_MAX && best.font_size_e != UINT32_MAX)
                font_size = round( +2.0 * best.font_size_b * ( fraction - .5) * ( fraction - 1.)
                                   -4.0 * best.font_size_m * fraction * ( fraction - 1.)
                                   +2.0 * best.font_size_e * fraction * ( fraction - .5));
        }

        error = FT_Set_Pixel_Sizes( face, font_size, 0);

        if( error != 0)
        {
            fprintf( stderr, "Setup error!");
            exit( EXIT_FAILURE);
        }

        FT_Int xheight = 0;
        error = FT_Load_Char( face, 'x', FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
        if( !error)
            xheight = face->glyph->bitmap.rows;

        error = FT_Load_Char( face, character, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
        if ( error )
            continue;

        Glyph current = extract( face->glyph);
        current.index = index;
        current.match = best;
        mxheight = MAX( mxheight, xheight);
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

    FT_Int height = hexcess + mxheight + mdescent;

    auto *out = ( uint64_t *)calloc( width * height, sizeof( uint64_t));
    FT_Vector pen;
    memset( &pen, 0, sizeof( pen));
    for( Glyph *current = head, *prev_link; current != nullptr;)
    {
        draw( *current, &pen, out, mdescent, width, height, nchars - 1);

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

uint32_t hsvaToRgba( uint32_t hsv)
{
    uint32_t region, p, q, t, remainder,
             h = RED( hsv),
             s = GREEN( hsv),
             v = BLUE( hsv),
             a = ALPHA( hsv);

    if ( s == 0)
        return RGBA( v, v, v, a);

    region = h / 43;
    remainder = ( h - ( region * 43)) * 6;

    p = ( v * ( 0xFFu - s)) >> 8u;
    q = ( v * ( 0xFFu - ((s * remainder) >> 8u))) >> 8u;
    t = ( v * ( 0xFFu - ((s * (0xFFu - remainder)) >> 8u))) >> 8u;

    switch ( region)
    {
        case 0:
            return RGBA( v, t, p, a);
        case 1:
            return RGBA( q, v, p, a);
        case 2:
            return RGBA( p, v, t, a);
        case 3:
            return RGBA( p, q, v, a);
        case 4:
            return RGBA( t, p, v, a);
        default:
            return RGBA( v, p, q, a);
    }
}

uint32_t rgbaToHsva( uint32_t rgb)
{
    uint32_t rgbMin, rgbMax, hsv{},
             r = RED( rgb),
             g = GREEN( rgb),
             b = BLUE( rgb),
             a = ALPHA( rgb);

    rgbMax = MAX( r, MAX( g, b));
    rgbMin = MIN( r, MIN( g, b));

    hsv = rgbMax << 8u;
    if ( hsv == 0)
        return hsv | a;

    hsv |= ( 0xFFu * ( ( int32_t)( rgbMax - rgbMin)) / ( hsv >> 8u)) << 16u;
    if ( ( hsv >> 16u & 0xFFu) == 0)
        return hsv | a;

    if ( rgbMax == r)
        hsv |= ( uint32_t)( ( uint8_t)( 0 + 43 * ( int32_t)( g - b) / ( int32_t)( rgbMax - rgbMin))) << 24u;
    else if ( rgbMax == g)
        hsv |= ( uint32_t)( ( uint8_t)(  85 + 43 * ( int32_t)( b - r) / ( int32_t)( rgbMax - rgbMin))) << 24u;
    else
        hsv |= ( uint32_t)( ( uint8_t)( 171 + 43 * ( int32_t)( r - g) / ( int32_t)( rgbMax - rgbMin))) << 24u;

    return hsv | a;
}

uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, double progress)
{
    uint32_t r = RED( lcolor)   * ( 1.0 - progress) + RED( rcolor)   * progress,
             g = GREEN( lcolor) * ( 1.0 - progress) + GREEN( rcolor) * progress,
             b = BLUE( lcolor)  * ( 1.0 - progress) + BLUE( rcolor)  * progress,
             a = ALPHA( lcolor) * ( 1.0 - progress) + ALPHA( rcolor)  * progress;

    return RGBA( r, g, b, a);
}

uint32_t interpolateColor( uint32_t scolor, uint32_t ecolor, double progress)
{
    auto shsv = rgbaToHsva( scolor);
    auto ehsv = rgbaToHsva( ecolor);

    return hsvaToRgba( colorLerp( shsv, ehsv, progress));
}

KDNode *approximate( KDNode *node, Color search, double &ldist, KDNode *best, uint8_t depth)
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
           v = .615    * r - .51499 * g - .10001 * b,

           ndist = std::sqrt( y * y + u * u + v * v);

    if( ndist < ldist)
    {
        ldist = ndist;
        best = node;
    }

    bool left = true;
    if( nchannel < cchannel)
    {
        if( node->left != nullptr)
            best = approximate( node->left, search, ldist, best, ndepth);
        else
            return best;
    }
    else
    {
        if( node->right != nullptr)
            best = approximate( node->right, search, ldist, best, ndepth);
        else
            return best;
        left = false;
    }

    if( std::abs( search.rgb[ depth] - node->color.rgb[ depth]) < ldist)
        best = approximate(!left ? node->left : node->right, search, ldist, best, ndepth);

    return best;
}

void insert( KDNode *&node, Color color, size_t index, uint8_t depth)
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

void write( const uint64_t *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination, KDNode *root)
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
            auto nmatch = approximate( root, {( uint8_t)RED( color),
                                              ( uint8_t)GREEN( color),
                                              ( uint8_t)BLUE( color)}, initial);
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

void writePNG( FILE *cfp, const uint64_t *buffer, png_int_32 width, png_int_32 height)
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
    uint8_t color_buffer[ bytes_per_pixel];
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

uint32_t getNumber( const char *&ctx, uint8_t base)
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

uint32_t decodeColorName( const char *&ctx, BKNode *bkroot)
{
    static const std::unordered_map<std::string, uint32_t> nameLookup =
    {
            { COLOR_ALICEBLUE,            COLORHEX_ALICEBLUE},
            { COLOR_ANTIQUEWHITE,         COLORHEX_ANTIQUEWHITE},
            { COLOR_AQUA,                 COLORHEX_AQUA},
            { COLOR_AQUAMARINE,           COLORHEX_AQUAMARINE},
            { COLOR_AZURE,                COLORHEX_AZURE},
            { COLOR_BEIGE,                COLORHEX_BEIGE},
            { COLOR_BISQUE,               COLORHEX_BISQUE},
            { COLOR_BLACK,                COLORHEX_BLACK},
            { COLOR_BLANCHEDALMOND,       COLORHEX_BLANCHEDALMOND},
            { COLOR_BLUE,                 COLORHEX_BLUE},
            { COLOR_BLUEVIOLET,           COLORHEX_BLUEVIOLET},
            { COLOR_BROWN,                COLORHEX_BROWN},
            { COLOR_BURLYWOOD,            COLORHEX_BURLYWOOD},
            { COLOR_CADETBLUE,            COLORHEX_CADETBLUE},
            { COLOR_CHARTREUSE,           COLORHEX_CHARTREUSE},
            { COLOR_CHOCOLATE,            COLORHEX_CHOCOLATE},
            { COLOR_CORAL,                COLORHEX_CORAL},
            { COLOR_CORNFLOWERBLUE,       COLORHEX_CORNFLOWERBLUE},
            { COLOR_CORNSILK,             COLORHEX_CORNSILK},
            { COLOR_CRIMSON,              COLORHEX_CRIMSON},
            { COLOR_CYAN,                 COLORHEX_CYAN},
            { COLOR_DARKBLUE,             COLORHEX_DARKBLUE},
            { COLOR_DARKCYAN,             COLORHEX_DARKCYAN},
            { COLOR_DARKGOLDENROD,        COLORHEX_DARKGOLDENROD},
            { COLOR_DARKGRAY,             COLORHEX_DARKGRAY},
            { COLOR_DARKGREY,             COLORHEX_DARKGREY},
            { COLOR_DARKGREEN,            COLORHEX_DARKGREEN},
            { COLOR_DARKKHAKI,            COLORHEX_DARKKHAKI},
            { COLOR_DARKMAGENTA,          COLORHEX_DARKMAGENTA},
            { COLOR_DARKOLIVEGREEN,       COLORHEX_DARKOLIVEGREEN},
            { COLOR_DARKORANGE,           COLORHEX_DARKORANGE},
            { COLOR_DARKORCHID,           COLORHEX_DARKORCHID},
            { COLOR_DARKRED,              COLORHEX_DARKRED},
            { COLOR_DARKSALMON,           COLORHEX_DARKSALMON},
            { COLOR_DARKSEAGREEN,         COLORHEX_DARKSEAGREEN},
            { COLOR_DARKSLATEBLUE,        COLORHEX_DARKSLATEBLUE},
            { COLOR_DARKSLATEGRAY,        COLORHEX_DARKSLATEGRAY},
            { COLOR_DARKSLATEGREY,        COLORHEX_DARKSLATEGREY},
            { COLOR_DARKTURQUOISE,        COLORHEX_DARKTURQUOISE},
            { COLOR_DARKVIOLET,           COLORHEX_DARKVIOLET},
            { COLOR_DEEPPINK,             COLORHEX_DEEPPINK},
            { COLOR_DEEPSKYBLUE,          COLORHEX_DEEPSKYBLUE},
            { COLOR_DIMGRAY,              COLORHEX_DIMGRAY},
            { COLOR_DIMGREY,              COLORHEX_DIMGREY},
            { COLOR_DODGERBLUE,           COLORHEX_DODGERBLUE},
            { COLOR_FIREBRICK,            COLORHEX_FIREBRICK},
            { COLOR_FLORALWHITE,          COLORHEX_FLORALWHITE},
            { COLOR_FORESTGREEN,          COLORHEX_FORESTGREEN},
            { COLOR_FUCHSIA,              COLORHEX_FUCHSIA},
            { COLOR_GAINSBORO,            COLORHEX_GAINSBORO},
            { COLOR_GHOSTWHITE,           COLORHEX_GHOSTWHITE},
            { COLOR_GOLD,                 COLORHEX_GOLD},
            { COLOR_GOLDENROD,            COLORHEX_GOLDENROD},
            { COLOR_GRAY,                 COLORHEX_GRAY},
            { COLOR_GREY,                 COLORHEX_GREY},
            { COLOR_GREEN,                COLORHEX_GREEN},
            { COLOR_GREENYELLOW,          COLORHEX_GREENYELLOW},
            { COLOR_HONEYDEW,             COLORHEX_HONEYDEW},
            { COLOR_HOTPINK,              COLORHEX_HOTPINK},
            { COLOR_INDIANRED,            COLORHEX_INDIANRED},
            { COLOR_INDIGO,               COLORHEX_INDIGO},
            { COLOR_IVORY,                COLORHEX_IVORY},
            { COLOR_KHAKI,                COLORHEX_KHAKI},
            { COLOR_LAVENDER,             COLORHEX_LAVENDER},
            { COLOR_LAVENDERBLUSH,        COLORHEX_LAVENDERBLUSH},
            { COLOR_LAWNGREEN,            COLORHEX_LAWNGREEN},
            { COLOR_LEMONCHIFFON,         COLORHEX_LEMONCHIFFON},
            { COLOR_LIGHTBLUE,            COLORHEX_LIGHTBLUE},
            { COLOR_LIGHTCORAL,           COLORHEX_LIGHTCORAL},
            { COLOR_LIGHTCYAN,            COLORHEX_LIGHTCYAN},
            { COLOR_LIGHTGOLDENRODYELLOW, COLORHEX_LIGHTGOLDENRODYELLOW},
            { COLOR_LIGHTGRAY,            COLORHEX_LIGHTGRAY},
            { COLOR_LIGHTGREY,            COLORHEX_LIGHTGREY},
            { COLOR_LIGHTGREEN,           COLORHEX_LIGHTGREEN},
            { COLOR_LIGHTPINK,            COLORHEX_LIGHTPINK},
            { COLOR_LIGHTSALMON,          COLORHEX_LIGHTSALMON},
            { COLOR_LIGHTSEAGREEN,        COLORHEX_LIGHTSEAGREEN},
            { COLOR_LIGHTSKYBLUE,         COLORHEX_LIGHTSKYBLUE},
            { COLOR_LIGHTSLATEGRAY,       COLORHEX_LIGHTSLATEGRAY},
            { COLOR_LIGHTSLATEGREY,       COLORHEX_LIGHTSLATEGREY},
            { COLOR_LIGHTSTEELBLUE,       COLORHEX_LIGHTSTEELBLUE},
            { COLOR_LIGHTYELLOW,          COLORHEX_LIGHTYELLOW},
            { COLOR_LIME,                 COLORHEX_LIME},
            { COLOR_LIMEGREEN,            COLORHEX_LIMEGREEN},
            { COLOR_LINEN,                COLORHEX_LINEN},
            { COLOR_MAGENTA,              COLORHEX_MAGENTA},
            { COLOR_MAROON,               COLORHEX_MAROON},
            { COLOR_MEDIUMAQUAMARINE,     COLORHEX_MEDIUMAQUAMARINE},
            { COLOR_MEDIUMBLUE,           COLORHEX_MEDIUMBLUE},
            { COLOR_MEDIUMORCHID,         COLORHEX_MEDIUMORCHID},
            { COLOR_MEDIUMPURPLE,         COLORHEX_MEDIUMPURPLE},
            { COLOR_MEDIUMSEAGREEN,       COLORHEX_MEDIUMSEAGREEN},
            { COLOR_MEDIUMSLATEBLUE,      COLORHEX_MEDIUMSLATEBLUE},
            { COLOR_MEDIUMSPRINGGREEN,    COLORHEX_MEDIUMSPRINGGREEN},
            { COLOR_MEDIUMTURQUOISE,      COLORHEX_MEDIUMTURQUOISE},
            { COLOR_MEDIUMVIOLETRED,      COLORHEX_MEDIUMVIOLETRED},
            { COLOR_MIDNIGHTBLUE,         COLORHEX_MIDNIGHTBLUE},
            { COLOR_MINTCREAM,            COLORHEX_MINTCREAM},
            { COLOR_MISTYROSE,            COLORHEX_MISTYROSE},
            { COLOR_MOCCASIN,             COLORHEX_MOCCASIN},
            { COLOR_NAVAJOWHITE,          COLORHEX_NAVAJOWHITE},
            { COLOR_NAVY,                 COLORHEX_NAVY},
            { COLOR_OLDLACE,              COLORHEX_OLDLACE},
            { COLOR_OLIVE,                COLORHEX_OLIVE},
            { COLOR_OLIVEDRAB,            COLORHEX_OLIVEDRAB},
            { COLOR_ORANGE,               COLORHEX_ORANGE},
            { COLOR_ORANGERED,            COLORHEX_ORANGERED},
            { COLOR_ORCHID,               COLORHEX_ORCHID},
            { COLOR_PALEGOLDENROD,        COLORHEX_PALEGOLDENROD},
            { COLOR_PALEGREEN,            COLORHEX_PALEGREEN},
            { COLOR_PALETURQUOISE,        COLORHEX_PALETURQUOISE},
            { COLOR_PALEVIOLETRED,        COLORHEX_PALEVIOLETRED},
            { COLOR_PAPAYAWHIP,           COLORHEX_PAPAYAWHIP},
            { COLOR_PEACHPUFF,            COLORHEX_PEACHPUFF},
            { COLOR_PERU,                 COLORHEX_PERU},
            { COLOR_PINK,                 COLORHEX_PINK},
            { COLOR_PLUM,                 COLORHEX_PLUM},
            { COLOR_POWDERBLUE,           COLORHEX_POWDERBLUE},
            { COLOR_PURPLE,               COLORHEX_PURPLE},
            { COLOR_REBECCAPURPLE,        COLORHEX_REBECCAPURPLE},
            { COLOR_RED,                  COLORHEX_RED},
            { COLOR_ROSYBROWN,            COLORHEX_ROSYBROWN},
            { COLOR_ROYALBLUE,            COLORHEX_ROYALBLUE},
            { COLOR_SADDLEBROWN,          COLORHEX_SADDLEBROWN},
            { COLOR_SALMON,               COLORHEX_SALMON},
            { COLOR_SANDYBROWN,           COLORHEX_SANDYBROWN},
            { COLOR_SEAGREEN,             COLORHEX_SEAGREEN},
            { COLOR_SEASHELL,             COLORHEX_SEASHELL},
            { COLOR_SIENNA,               COLORHEX_SIENNA},
            { COLOR_SILVER,               COLORHEX_SILVER},
            { COLOR_SKYBLUE,              COLORHEX_SKYBLUE},
            { COLOR_SLATEBLUE,            COLORHEX_SLATEBLUE},
            { COLOR_SLATEGRAY,            COLORHEX_SLATEGRAY},
            { COLOR_SLATEGREY,            COLORHEX_SLATEGREY},
            { COLOR_SNOW,                 COLORHEX_SNOW},
            { COLOR_SPRINGGREEN,          COLORHEX_SPRINGGREEN},
            { COLOR_STEELBLUE,            COLORHEX_STEELBLUE},
            { COLOR_TAN,                  COLORHEX_TAN},
            { COLOR_TEAL,                 COLORHEX_TEAL},
            { COLOR_THISTLE,              COLORHEX_THISTLE},
            { COLOR_TOMATO,               COLORHEX_TOMATO},
            { COLOR_TURQUOISE,            COLORHEX_TURQUOISE},
            { COLOR_VIOLET,               COLORHEX_VIOLET},
            { COLOR_WHEAT,                COLORHEX_WHEAT},
            { COLOR_WHITE,                COLORHEX_WHITE},
            { COLOR_WHITESMOKE,           COLORHEX_WHITESMOKE},
            { COLOR_YELLOW,               COLORHEX_YELLOW},
            { COLOR_YELLOWGREEN,          COLORHEX_YELLOWGREEN},
    };

    std::string name;
    do
    {
        name += std::tolower( *ctx);
    }
    while( isalpha( *++ctx));

    auto pos = nameLookup.find( name);
    if( pos != nameLookup.cend())
        return pos->second;
    else
    {
        fprintf( stderr, "Unable to find match for `%s`\n", name.c_str());
        auto matches = findWordMatch( bkroot, name.c_str(), 1);
        if( !matches.empty())
        {
            fprintf( stderr, "The following colors match your search:\n");
            for( size_t i = 0; i < matches.size(); ++i)
               fprintf( stderr, "%zu]. %s\n", i + 1, matches[ i].c_str());
        }
    }

    return 0xFFFFFFu;
}

bool ltrim( const char*& p)
{
    while( isspace( *p))
        ++p;
    return true;
}

uint32_t extractColor( const char *&rule, BKNode *bkroot)
{
    ltrim( rule);
    int8_t cratio = -1;
    const char *prev = rule;
    if( *rule == '#')
        ++rule;
    else if( *rule == 'x')
        ++rule;
    else if( strncasecmp( rule, "0x", 2) == 0)
        rule += 2;
    else if( isalpha( *rule))
    {
        auto color_name = decodeColorName(rule, bkroot);
        if( *rule == ':')
        {
            if( *++rule == ':')
            {


        cratio = getNumber( ++rule);
                cratio = cratio < 0 ? 0 : cratio;
                color_name |= 0xFFu;
            }
            else
            {
                color_name |= getNumber( rule, 16);
                if( *rule == ':')
                    cratio = getNumber( ++rule);
            }

            if( cratio != -1)
            {
                cratio = MIN( cratio, 100);
                double cscale = cratio / 100.0;
                color_name = SCALE_RGB( color_name, cscale) | ALPHA( color_name);
            }

            return color_name;
        }


        return color_name | 0xFFu;
    }
    else
    {
        fprintf( stderr, "Expected hex indicator near -> %s (allowed: `#`, `x`, `0x`)", prev);
        exit( EXIT_FAILURE);
    }

    uint32_t ccolor{};
    prev = rule;
    if( isxdigit( *rule))
    {
        ccolor = getNumber(rule, 16);
        uint8_t ccount = rule - prev;
        if( ccolor == 0 || ( ccount != 6 && ccount != 8))
        {
            fprintf( stderr, ccolor == 0 ? "Invalid color specification %s"
                                                : "Color has to be 6 or 8 hex digits -> %s", prev);
            exit( EXIT_FAILURE);
        }
        else if( ccount == 6)
            ccolor = ccolor << 8u | 0xFFu;

        if( *rule == ':')
        {
            cratio = getNumber( ++rule);
            cratio = cratio < 0 ? 0 : cratio;
            cratio = MIN( cratio, 100);
            double cscale = cratio / 100.0;
            ccolor = SCALE_RGB( ccolor, cscale);
        }
    }
    else
    {
        fprintf( stderr, "Expected hex digits -> %s", prev);
        exit( EXIT_FAILURE);
    }

    return ccolor;
}

XyZColor xyzFromRgb( uint32_t color)
{
    auto r = RED( color),
            g = GREEN( color),
            b = BLUE( color);

    return {
            .x = r * 0.4124564 + g * 0.3575761 + b * 0.1804375,
            .y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750,
            .z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041
    };
}

uint32_t xyzToRgb( XyZColor color)
{
    auto r = color.x * 3.2404542  + color.y * -1.5371385 + color.z * -0.4985314,
         g = color.x * -0.9692660 + color.y * 1.8760108  + color.z * 0.0415560,
         b = color.x * 0.0556434  + color.y * -0.2040259 + color.z * 1.0572252;

    r = ZERO( r) ? 0.0 : MIN( (int)std::round( r * XYZ_SCALE), RGB_SCALE);
    g = ZERO( g) ? 0.0 : MIN( (int)std::round( g * XYZ_SCALE), RGB_SCALE);
    b = ZERO( b) ? 0.0 : MIN( (int)std::round( b * XYZ_SCALE), RGB_SCALE);

    return RGB( r, g, b);
}

uint32_t mixRgb( uint32_t lcolor, uint32_t rcolor)
{
    /*
     * References:
     *  + https://en.wikipedia.org/wiki/CIE_1931_color_space
     *  + http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
     *
     */
    auto lxyz = xyzFromRgb(lcolor),
            rxyz = xyzFromRgb(rcolor);

    double lsum = lxyz.x + lxyz.y + lxyz.z,
            rsum = rxyz.x + rxyz.y + rxyz.z,
            lx = lxyz.x / lsum,
            ly = lxyz.y / lsum,
            rx = rxyz.x / rsum,
            ry = rxyz.y / rsum,
            xmix = ( lx * lxyz.y / ly + rx * rxyz.y / ry) / ( lxyz.y / ly + rxyz.y / ry),
            ymix = ( lxyz.y + rxyz.y) / ( lxyz.y / ly + rxyz.y / ry),
            zmix = 1 - xmix - ymix;

    return xyzToRgb({xmix, ymix, zmix});
}

uint32_t mixColor( const char *&ctx, BKNode *bkroot)
{
    uint32_t lcolor{}, rcolor{};
    uint8_t alpha = 0xFFu;
    char op = '\0';
    while( ltrim( ctx) && *ctx != ')')
    {
        if( *ctx == '+' || *ctx == '-')
            op = *ctx;
        else
        {
            op ? rcolor = extractColor(ctx, bkroot) : lcolor = extractColor(ctx, bkroot);
            ctx -= 1;
        }

        ++ctx;
    }

    ctx += 1;

    if( ltrim( ctx) && *ctx == ':')
        alpha = getNumber( ++ctx, 16);

    if( op == '+') // Additive color mixing
        return mixRgb( lcolor, rcolor) | alpha;
    else if( op == '-')
    {
        auto lred   = RGB_SCALE - RED( lcolor),
             lgreen = RGB_SCALE - GREEN( lcolor),
             lblue  = RGB_SCALE - BLUE( lcolor),
             rred   = RGB_SCALE - RED( rcolor),
             rgreen = RGB_SCALE - GREEN( rcolor),
             rblue  = RGB_SCALE - BLUE( rcolor);

        auto r = RGB_SCALE - std::sqrt( .5 * ( lred * lred + rred * rred)),
             g = RGB_SCALE - std::sqrt( .5 * ( lgreen * lgreen + rgreen * rgreen)),
             b = RGB_SCALE - std::sqrt( .5 * ( lblue * lblue + rblue * rblue));

        return RGBA( r, g, b, alpha);
    }

    return lcolor;
}

// Format example: [1..2:10-20-10 -ease-in-sine]{(Black:ff + Green::50) -> (Brown:ff:30 + Red:4f:50) -ease-in-out-sine}
std::vector<ColorRule> parseColorRule( const char *rule, BKNode *bkroot)
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

                        if( ccolor.end != -1 && ccolor.end <= ccolor.start)
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

                if( *rule == ':')
                {
                    ccolor.font_size_b = getNumber(++rule);
                    bool mid = false;
                    if( *rule == '-')
                    {
                        ccolor.font_size_m = getNumber(++rule);
                        mid = true;
                    }
                    if( *rule == '-')
                        ccolor.font_size_e = getNumber(++rule);
                    else if( mid)
                    {
                        ccolor.font_size_e = ccolor.font_size_m;
                        ccolor.font_size_m = UINT32_MAX;
                    }

                    if( ltrim( rule) && *rule == '-')
                        fillEasingMode(ccolor.font_easing_fn, ++rule, bkroot, ']');
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
                    if( ltrim( rule) && *rule == '(')
                    {
                        ccolor.scolor = mixColor(++rule, nullptr);
                    }
                    else
                    {
                        prev = rule;
                        ccolor.scolor = extractColor(rule, bkroot);
                    }

                    if( ltrim( rule) && *rule == '-')
                    {
                        if( ccolor.end == INT32_MIN)
                        {
                            fprintf( stderr, "Easing only available for range based colors -> %s", prev);
                            exit( EXIT_FAILURE);
                        }

                        ++rule;
                        if( *rule != '>')
                        {
                            fprintf( stderr, "Expected '>' near -> %s", rule);
                            exit( EXIT_FAILURE);
                        }

                        if( ltrim( ++rule) && *rule == '(')
                            ccolor.ecolor = mixColor(++rule, nullptr);
                        else if( *rule != '\0')
                            ccolor.ecolor = extractColor(rule, bkroot);
                        else
                        {
                            fprintf( stderr, "Incomplete color specification");
                            exit( EXIT_FAILURE);
                        }

                        fillEasingMode(ccolor.color_easing_fn, rule, bkroot, '}');
                    }

                    if( *rule == '}')
                        ++rule;
                    else
                    {
                        fprintf( stderr, "Missing end of expression `}`");
                        exit( EXIT_FAILURE);
                    }

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

uint32_t editDistance( const std::string& main, const std::string& ref)
{
    auto mlength = main.size() + 1, rlength = ref.size() + 1;
    uint8_t lookup[ mlength][ rlength];

    for( size_t j = 0; j < rlength; ++j)
        lookup[ 0][ j] = j;
    for( size_t i = 0; i < mlength; ++i)
        lookup[ i][ 0] = i;

    for ( size_t j = 1; j < mlength; ++j)
    {
        for ( size_t i = 1; i < rlength; ++i)
        {
            lookup[ j][ i] = std::min( lookup[ j - 1][ i] + 1,
                                       std::min( lookup[ j][ i - 1] + 1,
                                                 lookup[ j - 1][ i - 1] + ( tolower( main[ j - 1]) != tolower( ref[ i - 1]))));
        }
    }

    return lookup[ mlength - 1][ rlength - 1];
}

void insert( BKNode *&node, const char *word)
{
    if( node == nullptr)
    {
        node = new BKNode{word};
        return;
    }

    auto dist = editDistance( node->word, word);

    if( dist >= MAX_DIFF_TOLERANCE)
        return;

    if( node->next[ dist] == nullptr)
        insert( node->next[ dist], word);
    else
    {
        auto ndist = editDistance( node->next[ dist]->word, word);
        insert( node->next[ dist]->next[ ndist], word);
    }
}

void free( BKNode *&node)
{
    if( node == nullptr)
        return;

    for ( auto &next : node->next)
    {
        free( next);
        delete next; next = nullptr;
    }

    delete node; node = nullptr;
}

void findWordMatch( BKNode *node, const char *word, int threshold, std::vector<std::string>& matches)
{
    if( node == nullptr)
        return;

    int dist = editDistance( node->word, word),
            mindist = MAX( dist - threshold, 1),
            maxdist = MIN( dist + threshold, MAX_DIFF_TOLERANCE - 1);

    if( dist <= threshold)
        matches.emplace_back(node->word);

    for( int i = mindist; i <= maxdist; ++i)
        findWordMatch( node->next[ i], word, threshold, matches);
}

std::vector<std::string> findWordMatch( BKNode *node, const char *word, int threshold)
{
    std::vector<std::string> matches;
    findWordMatch( node, word, threshold, matches);

    return matches;
}

void fillEasingMode( std::function<float(float)> &function, const char *&rule, BKNode *bkroot, char eoc)
{
    // Reference: https://easings.net/
    const auto easeOutBounce = []( float progress)
    {
        const float n1 = 7.5625f;
        const float d1 = 2.75f;

        if (progress < 1 / d1) {
            return n1 * progress * progress;
        } else if (progress < 2 / d1) {
            progress -= 1.5f / d1;
            return n1 * progress * progress + 0.75f;
        } else if (progress < 2.5 / d1) {
            progress -= 2.25f / d1;
            return n1 * progress * progress + 0.9375f;
        } else {
            progress -= 2.625f / d1;
            return n1 * progress * progress + 0.984375f;
        }
    };

    static const std::unordered_map<std::string, std::function<float( float)>> easingLookup =
    {
            {
                FN_EASEINSINE, []( float progress)
               {
                   return 1 - cos( ( progress * M_PI) / 2.0);
               }
            },
            {
                FN_EASEOUTSINE, []( float progress)
               {
                   return sin( progress * M_PI / 2);
               }
            },
            {
               FN_EASEINOUTSINE, []( float progress)
               {
                   return -( cos( progress * M_PI) - 1) / 2;
               }
            },
            {
                FN_EASEINCUBIC, []( float progress)
               {
                   return progress * progress * progress;
               }
            },
            {
                FN_EASEOUTCUBIC, []( float progress)
               {
                   return 1 - pow( 1 - progress, 3);
               }
            },
            {
                FN_EASEINOUTCUBIC, []( float progress)
               {
                   return progress < 0.5 ? 4 * progress * progress * progress : 1 - pow(-2 * progress + 2, 3) / 2;
               }
            },
            {
                FN_EASEINQUINT, []( float progress)
               {
                   return progress * progress * progress * progress * progress;
               }
            },
            {
                FN_EASEOUTQUINT, []( float progress)
               {
                   return  1 - pow( 1 - progress, 5);
               }
            },
            {
                FN_EASEINOUTQUINT, []( float progress)
               {
                   return  progress < 0.5 ? 16 * progress * progress * progress * progress * progress
                                          : 1 - pow( -2 * progress + 2, 5) / 2;
               }
            },
            {
                FN_EASEINCIRC, []( float progress)
               {
                   return  1 - std::sqrt( (float)( 1 - pow( progress, 2)));
               }
            },
            {
                FN_EASEOUTCIRC, []( float progress)
               {
                   return  std::sqrt( ( float)( 1 - pow( progress - 1, 2)));
               }
            },
            {
                FN_EASEINOUTCIRC, []( float progress)
               {
                   return  progress < 0.5
                           ? ( 1 - std::sqrt(( float)( 1 - pow( 2 * progress, 2)))) / 2
                           : ( std::sqrt( ( float)(1 - pow( -2 * progress + 2, 2))) + 1) / 2;
               }
            },
            {
                FN_EASEINELASTIC, []( float progress)
               {
                   const float c4 = ( 2 * M_PI) / 3.0f;

                   return ZERO( progress)
                          ? 0
                          : EQUAL( progress, 1) ? 1
                                                : -pow(2, 10 * progress - 10) * sin(( progress * 10 - 10.75) * c4);
               }
            },
            {
                FN_EASEOUTELASTIC, []( float progress)
               {
                   const float c4 = ( 2 * M_PI) / 3.0f;

                   return ZERO( progress)
                          ? 0
                          : EQUAL( progress, 1) ? 1
                                                : pow( 2, -10 * progress) * sin( ( progress * 10 - 0.75) * c4) + 1;
               }
            },
            {
                FN_EASEINOUTELASTIC, []( float progress)
               {
                   const float c5 = ( 2 * M_PI) / 4.5f;

                   return ZERO( progress) ? 0 : EQUAL( progress, 1) ? 1 : progress < 0.5
                                                                          ? -( pow(2, 20 * progress - 10) * sin( ( 20 * progress - 11.125) * c5)) / 2
                                                                          : ( pow(2, -20 * progress + 10) * sin( ( 20 * progress - 11.125) * c5)) / 2 + 1;
               }
            },
            {
                FN_EASEINQUAD, []( float progress)
               {
                   return progress * progress;
               }
            },
            {
                FN_EASEOUTQUAD, []( float progress)
               {
                   return 1 - ( 1 - progress) * ( 1 - progress);
               }
            },
            {
                FN_EASEINOUTQUAD, []( float progress)
               {
                   return progress < 0.5 ? 2 * progress * progress : 1 - pow( -2 * progress + 2, 2) / 2;
               }
            },
            {
                FN_EASEINQUART, []( float progress)
               {
                   return progress  * progress * progress * progress;
               }
            },
            {
                FN_EASEOUTQUART, []( float progress)
               {
                   return 1 - pow( 1 - progress, 4);
               }
            },
            {
                FN_EASEINOUTQUART, []( float progress)
               {
                   return progress < 0.5 ? 8 * progress * progress * progress * progress
                                         : 1 - pow( -2 * progress + 2, 4) / 2;
               }
            },
            {
                FN_EASEINEXPO, []( float progress)
               {
                   return ZERO( progress) ? 0 : pow( 2, 10 * progress - 10);
               }
            },
            {
                FN_EASEOUTEXPO, []( float progress)
               {
                   return EQUAL( progress, 1) ? 1 : 1 - pow(2, -10 * progress);
               }
            },
            {
                FN_EASEINOUTEXPO, []( float progress)
               {
                   return ZERO( progress) ? 0
                                          : EQUAL( progress, 1) ? 1 : progress < 0.5 ? pow( 2, 20 * progress - 10) / 2
                                                                                     : (2 - pow( 2, -20 * progress + 10)) / 2;
               }
            },
            {
                FN_EASEINBACK, []( float progress)
               {
                   const float c1 = 1.70158f;
                   const float c3 = c1 + 1.0f;

                   return c3 * progress * progress * progress - c1 * progress * progress;
               }
            },
            {
                FN_EASEOUTBACK, []( float progress)
               {
                   const float c1 = 1.70158f;
                   const float c3 = c1 + 1.0f;

                   return 1 + c3 * pow( progress - 1, 3) + c1 * pow( progress - 1, 2);
               }
            },
            {
                FN_EASEINOUTBACK, []( float progress)
               {
                   const float c1 = 1.70158f;
                   const float c2 = c1 * 1.525f;

                   return progress < 0.5
                          ? ( pow( 2 * progress, 2) * ( ( c2 + 1) * 2 * progress - c2)) / 2
                          : ( pow( 2 * progress - 2, 2) * ( ( c2 + 1) * ( progress * 2 - 2) + c2) + 2) / 2;
               }
            },
            {
                FN_EASEINBOUNCE, [=]( float progress)
               {
                   return 1 - easeOutBounce( progress);
               }
            },
            {
                FN_EASEOUTBOUNCE, easeOutBounce
            },
            {
                FN_EASEINOUTBOUNCE, [=]( float progress)
               {
                   return progress < 0.5f
                          ? ( 1.0f - easeOutBounce( 1.0f - 2.0f * progress)) / 2.0f
                          : ( 1.0f + easeOutBounce( 2.0f * progress - 1.0f)) / 2.0f;
               }
            }
    };

    std::string easing;
    while( *rule && *rule != eoc)
    {
        if( *rule != '-' && *rule != ' ')
            easing += std::tolower( *rule);
        ++rule;
    }

    auto fn = easingLookup.find( easing);
    if( fn != easingLookup.cend())
        function = fn->second;
    else
    {
        fprintf( stderr, "Unknown easing function specified! Default easing function will be used\n");
        auto matches = findWordMatch( bkroot, easing.c_str());
        if( !matches.empty())
        {
            fprintf( stderr, "Easing function suggestions based on your search: \n");
            for ( size_t i = 0; i < matches.size(); ++i)
                fprintf( stderr, "%zu]. %s\n", i + 1, matches[ i].c_str());
        }
        function = []( float progress){ return progress; };
    }
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
    KDNode *kdroot = nullptr;

    insert( kdroot, { 0, 0, 0}, 0);
    insert( kdroot, { HALF_RGB_SCALE, 0, 0}, 1);
    insert( kdroot, { 0, HALF_RGB_SCALE, 0}, 2);
    insert( kdroot, { HALF_RGB_SCALE, HALF_RGB_SCALE, 0}, 3);
    insert( kdroot, { 0, 0, HALF_RGB_SCALE}, 4);
    insert( kdroot, { HALF_RGB_SCALE, 0, HALF_RGB_SCALE}, 5);
    insert( kdroot, { 0, HALF_RGB_SCALE, HALF_RGB_SCALE}, 6);
    insert( kdroot, { 192, 192, 192}, 7);
    insert( kdroot, { HALF_RGB_SCALE, HALF_RGB_SCALE, HALF_RGB_SCALE}, 8);
    insert( kdroot, { RGB_SCALE, 0, 0}, 9);
    insert( kdroot, { 0, RGB_SCALE, 0}, 10);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 0}, 11);
    insert( kdroot, { 0, 0, RGB_SCALE}, 12);
    insert( kdroot, { RGB_SCALE, 0, RGB_SCALE}, 13);
    insert( kdroot, { 0, RGB_SCALE, RGB_SCALE}, 14);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, RGB_SCALE}, 15);
    insert( kdroot, { 0, 0, 0}, 16);
    insert( kdroot, { 0, 0, 95}, 17);
    insert( kdroot, { 0, 0, 135}, 18);
    insert( kdroot, { 0, 0, 175}, 19);
    insert( kdroot, { 0, 0, 215}, 20);
    insert( kdroot, { 0, 0, RGB_SCALE}, 21);
    insert( kdroot, { 0, 95, 0}, 22);
    insert( kdroot, { 0, 95, 95}, 23);
    insert( kdroot, { 0, 95, 135}, 24);
    insert( kdroot, { 0, 95, 175}, 25);
    insert( kdroot, { 0, 95, 215}, 26);
    insert( kdroot, { 0, 95, RGB_SCALE}, 27);
    insert( kdroot, { 0, 135, 0}, 28);
    insert( kdroot, { 0, 135, 95}, 29);
    insert( kdroot, { 0, 135, 135}, 30);
    insert( kdroot, { 0, 135, 175}, 31);
    insert( kdroot, { 0, 135, 215}, 32);
    insert( kdroot, { 0, 135, RGB_SCALE}, 33);
    insert( kdroot, { 0, 175, 0}, 34);
    insert( kdroot, { 0, 175, 95}, 35);
    insert( kdroot, { 0, 175, 135}, 36);
    insert( kdroot, { 0, 175, 175}, 37);
    insert( kdroot, { 0, 175, 215}, 38);
    insert( kdroot, { 0, 175, RGB_SCALE}, 39);
    insert( kdroot, { 0, 215, 0}, 40);
    insert( kdroot, { 0, 215, 95}, 41);
    insert( kdroot, { 0, 215, 135}, 42);
    insert( kdroot, { 0, 215, 175}, 43);
    insert( kdroot, { 0, 215, 215}, 44);
    insert( kdroot, { 0, 215, RGB_SCALE}, 45);
    insert( kdroot, { 0, RGB_SCALE, 0}, 46);
    insert( kdroot, { 0, RGB_SCALE, 95}, 47);
    insert( kdroot, { 0, RGB_SCALE, 135}, 48);
    insert( kdroot, { 0, RGB_SCALE, 175}, 49);
    insert( kdroot, { 0, RGB_SCALE, 215}, 50);
    insert( kdroot, { 0, RGB_SCALE, RGB_SCALE}, 51);
    insert( kdroot, { 95, 0, 0}, 52);
    insert( kdroot, { 95, 0, 95}, 53);
    insert( kdroot, { 95, 0, 135}, 54);
    insert( kdroot, { 95, 0, 175}, 55);
    insert( kdroot, { 95, 0, 215}, 56);
    insert( kdroot, { 95, 0, RGB_SCALE}, 57);
    insert( kdroot, { 95, 95, 0}, 58);
    insert( kdroot, { 95, 95, 95}, 59);
    insert( kdroot, { 95, 95, 135}, 60);
    insert( kdroot, { 95, 95, 175}, 61);
    insert( kdroot, { 95, 95, 215}, 62);
    insert( kdroot, { 95, 95, RGB_SCALE}, 63);
    insert( kdroot, { 95, 135, 0}, 64);
    insert( kdroot, { 95, 135, 95}, 65);
    insert( kdroot, { 95, 135, 135}, 66);
    insert( kdroot, { 95, 135, 175}, 67);
    insert( kdroot, { 95, 135, 215}, 68);
    insert( kdroot, { 95, 135, RGB_SCALE}, 69);
    insert( kdroot, { 95, 175, 0}, 70);
    insert( kdroot, { 95, 175, 95}, 71);
    insert( kdroot, { 95, 175, 135}, 72);
    insert( kdroot, { 95, 175, 175}, 73);
    insert( kdroot, { 95, 175, 215}, 74);
    insert( kdroot, { 95, 175, RGB_SCALE}, 75);
    insert( kdroot, { 95, 215, 0}, 76);
    insert( kdroot, { 95, 215, 95}, 77);
    insert( kdroot, { 95, 215, 135}, 78);
    insert( kdroot, { 95, 215, 175}, 79);
    insert( kdroot, { 95, 215, 215}, 80);
    insert( kdroot, { 95, 215, RGB_SCALE}, 81);
    insert( kdroot, { 95, RGB_SCALE, 0}, 82);
    insert( kdroot, { 95, RGB_SCALE, 95}, 83);
    insert( kdroot, { 95, RGB_SCALE, 135}, 84);
    insert( kdroot, { 95, RGB_SCALE, 175}, 85);
    insert( kdroot, { 95, RGB_SCALE, 215}, 86);
    insert( kdroot, { 95, RGB_SCALE, RGB_SCALE}, 87);
    insert( kdroot, { 135, 0, 0}, 88);
    insert( kdroot, { 135, 0, 95}, 89);
    insert( kdroot, { 135, 0, 135}, 90);
    insert( kdroot, { 135, 0, 175}, 91);
    insert( kdroot, { 135, 0, 215}, 92);
    insert( kdroot, { 135, 0, RGB_SCALE}, 93);
    insert( kdroot, { 135, 95, 0}, 94);
    insert( kdroot, { 135, 95, 95}, 95);
    insert( kdroot, { 135, 95, 135}, 96);
    insert( kdroot, { 135, 95, 175}, 97);
    insert( kdroot, { 135, 95, 215}, 98);
    insert( kdroot, { 135, 95, RGB_SCALE}, 99);
    insert( kdroot, { 135, 135, 0}, 100);
    insert( kdroot, { 135, 135, 95}, 101);
    insert( kdroot, { 135, 135, 135}, 102);
    insert( kdroot, { 135, 135, 175}, 103);
    insert( kdroot, { 135, 135, 215}, 104);
    insert( kdroot, { 135, 135, RGB_SCALE}, 105);
    insert( kdroot, { 135, 175, 0}, 106);
    insert( kdroot, { 135, 175, 95}, 107);
    insert( kdroot, { 135, 175, 135}, 108);
    insert( kdroot, { 135, 175, 175}, 109);
    insert( kdroot, { 135, 175, 215}, 110);
    insert( kdroot, { 135, 175, RGB_SCALE}, 111);
    insert( kdroot, { 135, 215, 0}, 112);
    insert( kdroot, { 135, 215, 95}, 113);
    insert( kdroot, { 135, 215, 135}, 114);
    insert( kdroot, { 135, 215, 175}, 115);
    insert( kdroot, { 135, 215, 215}, 116);
    insert( kdroot, { 135, 215, RGB_SCALE}, 117);
    insert( kdroot, { 135, RGB_SCALE, 0}, 118);
    insert( kdroot, { 135, RGB_SCALE, 95}, 119);
    insert( kdroot, { 135, RGB_SCALE, 135}, 120);
    insert( kdroot, { 135, RGB_SCALE, 175}, 121);
    insert( kdroot, { 135, RGB_SCALE, 215}, 122);
    insert( kdroot, { 135, RGB_SCALE, RGB_SCALE}, 123);
    insert( kdroot, { 175, 0, 0}, 124);
    insert( kdroot, { 175, 0, 95}, 125);
    insert( kdroot, { 175, 0, 135}, 126);
    insert( kdroot, { 175, 0, 175}, 127);
    insert( kdroot, { 175, 0, 215}, HALF_RGB_SCALE);
    insert( kdroot, { 175, 0, RGB_SCALE}, 129);
    insert( kdroot, { 175, 95, 0}, 130);
    insert( kdroot, { 175, 95, 95}, 131);
    insert( kdroot, { 175, 95, 135}, 132);
    insert( kdroot, { 175, 95, 175}, 133);
    insert( kdroot, { 175, 95, 215}, 134);
    insert( kdroot, { 175, 95, RGB_SCALE}, 135);
    insert( kdroot, { 175, 135, 0}, 136);
    insert( kdroot, { 175, 135, 95}, 137);
    insert( kdroot, { 175, 135, 135}, 138);
    insert( kdroot, { 175, 135, 175}, 139);
    insert( kdroot, { 175, 135, 215}, 140);
    insert( kdroot, { 175, 135, RGB_SCALE}, 141);
    insert( kdroot, { 175, 175, 0}, 142);
    insert( kdroot, { 175, 175, 95}, 143);
    insert( kdroot, { 175, 175, 135}, 144);
    insert( kdroot, { 175, 175, 175}, 145);
    insert( kdroot, { 175, 175, 215}, 146);
    insert( kdroot, { 175, 175, RGB_SCALE}, 147);
    insert( kdroot, { 175, 215, 0}, 148);
    insert( kdroot, { 175, 215, 95}, 149);
    insert( kdroot, { 175, 215, 135}, 150);
    insert( kdroot, { 175, 215, 175}, 151);
    insert( kdroot, { 175, 215, 215}, 152);
    insert( kdroot, { 175, 215, RGB_SCALE}, 153);
    insert( kdroot, { 175, RGB_SCALE, 0}, 154);
    insert( kdroot, { 175, RGB_SCALE, 95}, 155);
    insert( kdroot, { 175, RGB_SCALE, 135}, 156);
    insert( kdroot, { 175, RGB_SCALE, 175}, 157);
    insert( kdroot, { 175, RGB_SCALE, 215}, 158);
    insert( kdroot, { 175, RGB_SCALE, RGB_SCALE}, 159);
    insert( kdroot, { 215, 0, 0}, 160);
    insert( kdroot, { 215, 0, 95}, 161);
    insert( kdroot, { 215, 0, 135}, 162);
    insert( kdroot, { 215, 0, 175}, 163);
    insert( kdroot, { 215, 0, 215}, 164);
    insert( kdroot, { 215, 0, RGB_SCALE}, 165);
    insert( kdroot, { 215, 95, 0}, 166);
    insert( kdroot, { 215, 95, 95}, 167);
    insert( kdroot, { 215, 95, 135}, 168);
    insert( kdroot, { 215, 95, 175}, 169);
    insert( kdroot, { 215, 95, 215}, 170);
    insert( kdroot, { 215, 95, RGB_SCALE}, 171);
    insert( kdroot, { 215, 135, 0}, 172);
    insert( kdroot, { 215, 135, 95}, 173);
    insert( kdroot, { 215, 135, 135}, 174);
    insert( kdroot, { 215, 135, 175}, 175);
    insert( kdroot, { 215, 135, 215}, 176);
    insert( kdroot, { 215, 135, RGB_SCALE}, 177);
    insert( kdroot, { 215, 175, 0}, 178);
    insert( kdroot, { 215, 175, 95}, 179);
    insert( kdroot, { 215, 175, 135}, 180);
    insert( kdroot, { 215, 175, 175}, 181);
    insert( kdroot, { 215, 175, 215}, 182);
    insert( kdroot, { 215, 175, RGB_SCALE}, 183);
    insert( kdroot, { 215, 215, 0}, 184);
    insert( kdroot, { 215, 215, 95}, 185);
    insert( kdroot, { 215, 215, 135}, 186);
    insert( kdroot, { 215, 215, 175}, 187);
    insert( kdroot, { 215, 215, 215}, 188);
    insert( kdroot, { 215, 215, RGB_SCALE}, 189);
    insert( kdroot, { 215, RGB_SCALE, 0}, 190);
    insert( kdroot, { 215, RGB_SCALE, 95}, 191);
    insert( kdroot, { 215, RGB_SCALE, 135}, 192);
    insert( kdroot, { 215, RGB_SCALE, 175}, 193);
    insert( kdroot, { 215, RGB_SCALE, 215}, 194);
    insert( kdroot, { 215, RGB_SCALE, RGB_SCALE}, 195);
    insert( kdroot, { RGB_SCALE, 0, 0}, 196);
    insert( kdroot, { RGB_SCALE, 0, 95}, 197);
    insert( kdroot, { RGB_SCALE, 0, 135}, 198);
    insert( kdroot, { RGB_SCALE, 0, 175}, 199);
    insert( kdroot, { RGB_SCALE, 0, 215}, 200);
    insert( kdroot, { RGB_SCALE, 0, RGB_SCALE}, 201);
    insert( kdroot, { RGB_SCALE, 95, 0}, 202);
    insert( kdroot, { RGB_SCALE, 95, 95}, 203);
    insert( kdroot, { RGB_SCALE, 95, 135}, 204);
    insert( kdroot, { RGB_SCALE, 95, 175}, 205);
    insert( kdroot, { RGB_SCALE, 95, 215}, 206);
    insert( kdroot, { RGB_SCALE, 95, RGB_SCALE}, 207);
    insert( kdroot, { RGB_SCALE, 135, 0}, 208);
    insert( kdroot, { RGB_SCALE, 135, 95}, 209);
    insert( kdroot, { RGB_SCALE, 135, 135}, 210);
    insert( kdroot, { RGB_SCALE, 135, 175}, 211);
    insert( kdroot, { RGB_SCALE, 135, 215}, 212);
    insert( kdroot, { RGB_SCALE, 135, RGB_SCALE}, 213);
    insert( kdroot, { RGB_SCALE, 175, 0}, 214);
    insert( kdroot, { RGB_SCALE, 175, 95}, 215);
    insert( kdroot, { RGB_SCALE, 175, 135}, 216);
    insert( kdroot, { RGB_SCALE, 175, 175}, 217);
    insert( kdroot, { RGB_SCALE, 175, 215}, 218);
    insert( kdroot, { RGB_SCALE, 175, RGB_SCALE}, 219);
    insert( kdroot, { RGB_SCALE, 215, 0}, 220);
    insert( kdroot, { RGB_SCALE, 215, 95}, 221);
    insert( kdroot, { RGB_SCALE, 215, 135}, 222);
    insert( kdroot, { RGB_SCALE, 215, 175}, 223);
    insert( kdroot, { RGB_SCALE, 215, 215}, 224);
    insert( kdroot, { RGB_SCALE, 215, RGB_SCALE}, 225);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 0}, 226);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 95}, 227);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 135}, 228);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 175}, 229);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, 215}, 230);
    insert( kdroot, { RGB_SCALE, RGB_SCALE, RGB_SCALE}, 231);
    insert( kdroot, { 8, 8, 8}, 232);
    insert( kdroot, { 18, 18, 18}, 233);
    insert( kdroot, { 28, 28, 28}, 234);
    insert( kdroot, { 38, 38, 38}, 235);
    insert( kdroot, { 48, 48, 48}, 236);
    insert( kdroot, { 58, 58, 58}, 237);
    insert( kdroot, { 68, 68, 68}, 238);
    insert( kdroot, { 78, 78, 78}, 239);
    insert( kdroot, { 88, 88, 88}, 240);
    insert( kdroot, { 98, 98, 98}, 241);
    insert( kdroot, { 108, 108, 108}, 242);
    insert( kdroot, { 118, 118, 118}, 243);
    insert( kdroot, { HALF_RGB_SCALE, HALF_RGB_SCALE, HALF_RGB_SCALE}, 244);
    insert( kdroot, { 138, 138, 138}, 245);
    insert( kdroot, { 148, 148, 148}, 246);
    insert( kdroot, { 158, 158, 158}, 247);
    insert( kdroot, { 168, 168, 168}, 248);
    insert( kdroot, { 178, 178, 178}, 249);
    insert( kdroot, { 188, 188, 188}, 250);
    insert( kdroot, { 198, 198, 198}, 251);
    insert( kdroot, { 208, 208, 208}, 252);
    insert( kdroot, { 218, 218, 218}, 253);
    insert( kdroot, { 228, 228, 228}, 254);
    insert( kdroot, { 238, 238, 238}, RGB_SCALE);


    BKNode *bkroot = nullptr;

    /*
     * Easing function listing
     */

    insert( bkroot, FN_IEASEINSINE);
    insert( bkroot, FN_IEASEOUTSINE);
    insert( bkroot, FN_IEASEINOUTSINE);
    insert( bkroot, FN_IEASEINQUAD);
    insert( bkroot, FN_IEASEOUTQUAD);
    insert( bkroot, FN_IEASEINOUTQUAD);
    insert( bkroot, FN_IEASEINCUBIC);
    insert( bkroot, FN_IEASEOUTCUBIC);
    insert( bkroot, FN_IEASEINOUTCUBIC);
    insert( bkroot, FN_IEASEINQUART);
    insert( bkroot, FN_IEASEOUTQUART);
    insert( bkroot, FN_IEASEINOUTQUART);
    insert( bkroot, FN_IEASEINQUINT);
    insert( bkroot, FN_IEASEOUTQUINT);
    insert( bkroot, FN_IEASEINOUTQUINT);
    insert( bkroot, FN_IEASEINEXPO);
    insert( bkroot, FN_IEASEOUTEXPO);
    insert( bkroot, FN_IEASEINOUTEXPO);
    insert( bkroot, FN_IEASEINCIRC);
    insert( bkroot, FN_IEASEOUTCIRC);
    insert( bkroot, FN_IEASEINOUTCIRC);
    insert( bkroot, FN_IEASEINBACK);
    insert( bkroot, FN_IEASEOUTBACK);
    insert( bkroot, FN_IEASEINOUTBACK);
    insert( bkroot, FN_IEASEINELASTIC);
    insert( bkroot, FN_IEASEOUTELASTIC);
    insert( bkroot, FN_IEASEINOUTELASTIC);
    insert( bkroot, FN_IEASEINBOUNCE);
    insert( bkroot, FN_IEASEOUTBOUNCE);
    insert( bkroot, FN_IEASEINOUTBOUNCE);

    /*
     * Color name listing
     */

    insert( bkroot, ICOLOR_ALICEBLUE);
    insert( bkroot, ICOLOR_ANTIQUEWHITE);
    insert( bkroot, ICOLOR_AQUA);
    insert( bkroot, ICOLOR_AQUAMARINE);
    insert( bkroot, ICOLOR_AZURE);
    insert( bkroot, ICOLOR_BEIGE);
    insert( bkroot, ICOLOR_BISQUE);
    insert( bkroot, ICOLOR_BLACK);
    insert( bkroot, ICOLOR_BLANCHEDALMOND);
    insert( bkroot, ICOLOR_BLUE);
    insert( bkroot, ICOLOR_BLUEVIOLET);
    insert( bkroot, ICOLOR_BROWN);
    insert( bkroot, ICOLOR_BURLYWOOD);
    insert( bkroot, ICOLOR_CADETBLUE);
    insert( bkroot, ICOLOR_CHARTREUSE);
    insert( bkroot, ICOLOR_CHOCOLATE);
    insert( bkroot, ICOLOR_CORAL);
    insert( bkroot, ICOLOR_CORNFLOWERBLUE);
    insert( bkroot, ICOLOR_CORNSILK);
    insert( bkroot, ICOLOR_CRIMSON);
    insert( bkroot, ICOLOR_CYAN);
    insert( bkroot, ICOLOR_DARKBLUE);
    insert( bkroot, ICOLOR_DARKCYAN);
    insert( bkroot, ICOLOR_DARKGOLDENROD);
    insert( bkroot, ICOLOR_DARKGRAY);
    insert( bkroot, ICOLOR_DARKGREY);
    insert( bkroot, ICOLOR_DARKGREEN);
    insert( bkroot, ICOLOR_DARKKHAKI);
    insert( bkroot, ICOLOR_DARKMAGENTA);
    insert( bkroot, ICOLOR_DARKOLIVEGREEN);
    insert( bkroot, ICOLOR_DARKORANGE);
    insert( bkroot, ICOLOR_DARKORCHID);
    insert( bkroot, ICOLOR_DARKRED);
    insert( bkroot, ICOLOR_DARKSALMON);
    insert( bkroot, ICOLOR_DARKSEAGREEN);
    insert( bkroot, ICOLOR_DARKSLATEBLUE);
    insert( bkroot, ICOLOR_DARKSLATEGRAY);
    insert( bkroot, ICOLOR_DARKSLATEGREY);
    insert( bkroot, ICOLOR_DARKTURQUOISE);
    insert( bkroot, ICOLOR_DARKVIOLET);
    insert( bkroot, ICOLOR_DEEPPINK);
    insert( bkroot, ICOLOR_DEEPSKYBLUE);
    insert( bkroot, ICOLOR_DIMGRAY);
    insert( bkroot, ICOLOR_DIMGREY);
    insert( bkroot, ICOLOR_DODGERBLUE);
    insert( bkroot, ICOLOR_FIREBRICK);
    insert( bkroot, ICOLOR_FLORALWHITE);
    insert( bkroot, ICOLOR_FORESTGREEN);
    insert( bkroot, ICOLOR_FUCHSIA);
    insert( bkroot, ICOLOR_GAINSBORO);
    insert( bkroot, ICOLOR_GHOSTWHITE);
    insert( bkroot, ICOLOR_GOLD);
    insert( bkroot, ICOLOR_GOLDENROD);
    insert( bkroot, ICOLOR_GRAY);
    insert( bkroot, ICOLOR_GREY);
    insert( bkroot, ICOLOR_GREEN);
    insert( bkroot, ICOLOR_GREENYELLOW);
    insert( bkroot, ICOLOR_HONEYDEW);
    insert( bkroot, ICOLOR_HOTPINK);
    insert( bkroot, ICOLOR_INDIANRED);
    insert( bkroot, ICOLOR_INDIGO);
    insert( bkroot, ICOLOR_IVORY);
    insert( bkroot, ICOLOR_KHAKI);
    insert( bkroot, ICOLOR_LAVENDER);
    insert( bkroot, ICOLOR_LAVENDERBLUSH);
    insert( bkroot, ICOLOR_LAWNGREEN);
    insert( bkroot, ICOLOR_LEMONCHIFFON);
    insert( bkroot, ICOLOR_LIGHTBLUE);
    insert( bkroot, ICOLOR_LIGHTCORAL);
    insert( bkroot, ICOLOR_LIGHTCYAN);
    insert( bkroot, ICOLOR_LIGHTGOLDENRODYELLOW);
    insert( bkroot, ICOLOR_LIGHTGRAY);
    insert( bkroot, ICOLOR_LIGHTGREY);
    insert( bkroot, ICOLOR_LIGHTGREEN);
    insert( bkroot, ICOLOR_LIGHTPINK);
    insert( bkroot, ICOLOR_LIGHTSALMON);
    insert( bkroot, ICOLOR_LIGHTSEAGREEN);
    insert( bkroot, ICOLOR_LIGHTSKYBLUE);
    insert( bkroot, ICOLOR_LIGHTSLATEGRAY);
    insert( bkroot, ICOLOR_LIGHTSLATEGREY);
    insert( bkroot, ICOLOR_LIGHTSTEELBLUE);
    insert( bkroot, ICOLOR_LIGHTYELLOW);
    insert( bkroot, ICOLOR_LIME);
    insert( bkroot, ICOLOR_LIMEGREEN);
    insert( bkroot, ICOLOR_LINEN);
    insert( bkroot, ICOLOR_MAGENTA);
    insert( bkroot, ICOLOR_MAROON);
    insert( bkroot, ICOLOR_MEDIUMAQUAMARINE);
    insert( bkroot, ICOLOR_MEDIUMBLUE);
    insert( bkroot, ICOLOR_MEDIUMORCHID);
    insert( bkroot, ICOLOR_MEDIUMPURPLE);
    insert( bkroot, ICOLOR_MEDIUMSEAGREEN);
    insert( bkroot, ICOLOR_MEDIUMSLATEBLUE);
    insert( bkroot, ICOLOR_MEDIUMSPRINGGREEN);
    insert( bkroot, ICOLOR_MEDIUMTURQUOISE);
    insert( bkroot, ICOLOR_MEDIUMVIOLETRED);
    insert( bkroot, ICOLOR_MIDNIGHTBLUE);
    insert( bkroot, ICOLOR_MINTCREAM);
    insert( bkroot, ICOLOR_MISTYROSE);
    insert( bkroot, ICOLOR_MOCCASIN);
    insert( bkroot, ICOLOR_NAVAJOWHITE);
    insert( bkroot, ICOLOR_NAVY);
    insert( bkroot, ICOLOR_OLDLACE);
    insert( bkroot, ICOLOR_OLIVE);
    insert( bkroot, ICOLOR_OLIVEDRAB);
    insert( bkroot, ICOLOR_ORANGE);
    insert( bkroot, ICOLOR_ORANGERED);
    insert( bkroot, ICOLOR_ORCHID);
    insert( bkroot, ICOLOR_PALEGOLDENROD);
    insert( bkroot, ICOLOR_PALEGREEN);
    insert( bkroot, ICOLOR_PALETURQUOISE);
    insert( bkroot, ICOLOR_PALEVIOLETRED);
    insert( bkroot, ICOLOR_PAPAYAWHIP);
    insert( bkroot, ICOLOR_PEACHPUFF);
    insert( bkroot, ICOLOR_PERU);
    insert( bkroot, ICOLOR_PINK);
    insert( bkroot, ICOLOR_PLUM);
    insert( bkroot, ICOLOR_POWDERBLUE);
    insert( bkroot, ICOLOR_PURPLE);
    insert( bkroot, ICOLOR_REBECCAPURPLE);
    insert( bkroot, ICOLOR_RED);
    insert( bkroot, ICOLOR_ROSYBROWN);
    insert( bkroot, ICOLOR_ROYALBLUE);
    insert( bkroot, ICOLOR_SADDLEBROWN);
    insert( bkroot, ICOLOR_SALMON);
    insert( bkroot, ICOLOR_SANDYBROWN);
    insert( bkroot, ICOLOR_SEAGREEN);
    insert( bkroot, ICOLOR_SEASHELL);
    insert( bkroot, ICOLOR_SIENNA);
    insert( bkroot, ICOLOR_SILVER);
    insert( bkroot, ICOLOR_SKYBLUE);
    insert( bkroot, ICOLOR_SLATEBLUE);
    insert( bkroot, ICOLOR_SLATEGRAY);
    insert( bkroot, ICOLOR_SLATEGREY);
    insert( bkroot, ICOLOR_SNOW);
    insert( bkroot, ICOLOR_SPRINGGREEN);
    insert( bkroot, ICOLOR_STEELBLUE);
    insert( bkroot, ICOLOR_TAN);
    insert( bkroot, ICOLOR_TEAL);
    insert( bkroot, ICOLOR_THISTLE);
    insert( bkroot, ICOLOR_TOMATO);
    insert( bkroot, ICOLOR_TURQUOISE);
    insert( bkroot, ICOLOR_VIOLET);
    insert( bkroot, ICOLOR_WHEAT);
    insert( bkroot, ICOLOR_WHITE);
    insert( bkroot, ICOLOR_WHITESMOKE);
    insert( bkroot, ICOLOR_YELLOW);
    insert( bkroot, ICOLOR_YELLOWGREEN);


    FT_Library    library;
    FT_Face       face;
    FT_Error      error;

    /*
     * Schema:
     *  av[ 0] [1..2:10-20-10 -ease-in-sine]{(Black:ff + Green::50):40 -> (Brown:ff:30 + Red:4f:50):50 -ease-in-out-sine} text
     */

    const char *fontfile = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
               *raster_glyph = "\u2589",
               *color_rule = nullptr,
               *font_size = "10",
               *word,
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
                maxlength = MAX( clen, maxlength);
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

    render( word, face, std::strtol( font_size, nullptr, 10), raster_glyph, screen, write_as_image, color_rule, kdroot, bkroot);

    FT_Done_Face( face);
    FT_Done_FreeType( library);
    free( kdroot);
    free(bkroot);

    return 0;
}