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
#include <regex>
#include <cfloat>
#include "colors_defs.hpp"
#include "easing_defs.hpp"
#include "geo_vector.hpp"

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

#define RAD_SCALE					   M_PI / 180.f
#define DEG_SCALE					   180.f / M_PI

/*
 * Converts bitmap into binary format.
 * NB!
 * 	- Pitch is the number of bytes used in representing
 * 	  a row.
 * 	- Pixmap is the final monochrome output
 */

 std::unique_ptr<unsigned char, void( *)( unsigned char *)> toMonochrome( FT_Bitmap bitmap)
{
	FT_Int rows = bitmap.rows,
	       cols = bitmap.width;
	auto pixmap = std::unique_ptr<unsigned char, void( *)( unsigned char *)>(
	    ( unsigned char *)calloc( rows * cols, 1), []( auto p){ free( p); });
	for( FT_Int y = 0; y < rows; ++y)
	{
		for( FT_Int ibyte = 0; ibyte < bitmap.pitch; ++ibyte)
		{
			FT_Int ibit = ibyte * 8,
			       base = y * cols + ibit,
			       cbit = bitmap.buffer[ y * bitmap.pitch + ibyte],
			       rbits = (int)(cols - ibit) < 8 ? cols - ibit : 8;
			for( FT_Int i = 0; i < rbits; ++i)
			   pixmap.get()[ base + i] = ( uint8_t)cbit & (1u << ( 7u - i));
		}
	}

	return pixmap;
}

Glyph extract( FT_GlyphSlot slot)
{
	Glyph glyph( toMonochrome(( slot->bitmap)));
	glyph.width = slot->bitmap.width;
	glyph.height = slot->bitmap.rows;
	glyph.xstep = slot->advance.x / 64;
	glyph.origin.x = slot->bitmap_left;
	glyph.origin.y = glyph.height - slot->bitmap_top;

	return glyph;
}

void insert( std::unique_ptr<Glyph>& index, Glyph glyph)
{
	if( index == nullptr)
	{
		index.reset( new Glyph( std::move( glyph)));
		return;
	}

	insert( index->next, std::move( glyph));
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
    auto& match = glyph.match;
    uint32_t color = match->scolor;
    std::unique_ptr<uint32_t[]> row_colors;
  	size_t start = glyph.index - match->start,
	  	   end = match->end == -1 ? MAX( total, 1) : match->end - match->start;
  	auto is_conic_gradient = match->gradient->gradient_type == GradientType::Conic;
	if( match->color_easing_fn && !is_conic_gradient)
	{
	  if( !match->soak)
	  {
		auto fraction = match->color_easing_fn(( float)start / end);
		color = interpolateColor( match->scolor, match->ecolor, fraction);
	  }
	  else
	  {
		row_colors.reset( new uint32_t[ glyph.width]);
		for( FT_Int i = 0; i < glyph.width; ++i)
		{
		  auto fraction = match->color_easing_fn( ( float)match->gradient->startx++ / match->gradient->width);
		  color = interpolateColor( match->scolor, match->ecolor, fraction);
		  row_colors.get()[ i] = color;
		}
	  }
	}

    auto cwidth = match->soak ? match->gradient->width : ( int32_t)end, cheight = match->gradient->height;
    for( FT_Int y = glyph.origin.y, j = 0; j < glyph.height; ++y, ++j)
    {
        for( FT_Int x = glyph.origin.x, i = 0; i < glyph.width; ++x, ++i)
        {
          if( match->color_easing_fn)
		  {
			if( match->gradient->gradient_type == GradientType::Radial)
			{
			  auto& props = static_cast<RadialGradient *>( match->gradient.get())->props;
			  /*
			   * Computes the radial-gradient of color starting at `match->gradient->startx`
			   */
			  auto xy = Vec2D<float>( !match->soak ?
							   start : match->gradient->startx - glyph.width + i, y + base)
							   	/ Vec2D<float>( cwidth, cheight);
			  xy -= Vec2D<float>( props.x, props.y); // Adjust all pixels so as to push the users origin at (0,0)
			  auto scale = cwidth / cheight;
			  if( cwidth > cheight)         // Converts the resulting elliptical shape into a circle.
				xy.x *= scale;
			  else
				xy.y *= scale;
			  auto d = xy.length();
			  auto r = props.z;               // Defines the spread distance.
			  auto c = smoothstep( r + d + 1., 3. * r - 1., d); // Spread the circle around to form a smooth gradient.
			  auto startcolor = Vec3D( ( float)RED( match->scolor) / RGB_SCALE,
									   ( float)GREEN( match->scolor) / RGB_SCALE,
									   ( float)BLUE( match->scolor) / RGB_SCALE),
				  endcolor    = Vec3D( ( float)RED( match->ecolor) / RGB_SCALE,
									  ( float)GREEN( match->ecolor) / RGB_SCALE,
									  ( float)BLUE( match->ecolor) / RGB_SCALE),
				  finalcolor  = startcolor.lerp( endcolor, match->color_easing_fn( c));
			  color = RGBA( finalcolor.x * RGB_SCALE, finalcolor.y * RGB_SCALE,
							finalcolor.z * RGB_SCALE, ALPHA( color)); //TODO: Correct alpha value
			}
			else if( is_conic_gradient)
			{
			  Vec2D<float> center( cwidth / 2.f, cheight / 2.f);
			  auto diff = ( Vec2D<float>( x + pen->x, base + pen->y + y) - center);
			  float angle;
			  if( diff.x < 0)
				angle = 270.f - ( std::atan2( diff.y, -diff.x) * DEG_SCALE);
			  else
				angle = 90.f + ( std::atan2( diff.y, diff.x) * DEG_SCALE);
			  
			  auto stops = static_cast<ConicGradient *>( match->gradient.get())->color_variations;
			  if( !stops.empty())
			  {
				auto prev_stop = *stops.begin();
				auto stop_match = std::find_if( stops.begin(), stops.end(), [ angle, &prev_stop]( auto& stop)
				{
				  auto condition = angle >= prev_stop.second
					  && angle <= stop.second && prev_stop.second != stop.second;
				  if( !condition)
					prev_stop = stop;
				  return condition;
				});
				auto cur_stop = stop_match == stops.cend() ? *stops.cbegin() : *stop_match;
				if( memcmp( prev_stop.first.rgb, cur_stop.first.rgb, 3) == 0)
				  color = RGBA( cur_stop.first.rgb[ 0], cur_stop.first.rgb[ 1],
				  	           cur_stop.first.rgb[ 2], 255);
				else
				{
				  auto fraction = ( angle - prev_stop.second) / ( cur_stop.second - prev_stop.second);
				  color = colorLerp( RGBA( prev_stop.first.rgb[ 0], prev_stop.first.rgb[ 1],
										   prev_stop.first.rgb[ 2], 255),
									 RGBA( cur_stop.first.rgb[ 0], cur_stop.first.rgb[ 1],
										   cur_stop.first.rgb[ 2], 255), fraction);
				}
			  }
			}
			else if( match->soak)
			  color = row_colors.get()[ i];
		  }
          uint64_t pixel = glyph.pixmap.get()[ j * glyph.width + i];
          out[ ( base + pen->y + y) * width + x + pen->x] |= pixel << 32u | ( pixel ? color : 0);
        }
    }
    
    pen->x += glyph.xstep; // Move the pen forward for positioning of the next character
}

float smoothstep( float left, float right, float x)
{
  // Scale, and clamp x to 0..1 range
  x = clamp( ( x - left) / ( right - left), 0.f, 1.f);
  // Evaluate Perlin polynomial( Smoother than Hermite's interpolation)
  return x * x * x * ( x * ( x * 6.f - 15.f) + 10.f);
}

float clamp( float x, float lowerlimit, float upperlimit)
{
  return x < lowerlimit ? lowerlimit : x > upperlimit ? upperlimit : x;
}

size_t countCharacters( std::string_view word)
{
    size_t value = 0;
    auto *pw = word.data();
    while( *pw)
    {
        size_t ccount = byteCount( *pw);
        value += 1;
        pw += ccount;
    }

    return value;
}

void render( std::string_view word, FT_Face face, size_t default_font_size, const char *raster_glyph,
			 FILE *destination, bool as_image, const char *color_rule,
			 const std::shared_ptr<KDNode>& root, const std::shared_ptr<BKNode>& bkroot)
{
    std::unique_ptr<Glyph> head;
    FT_Int width 	= 0, // Total width of the buffer
            mdescent = 0, // Holds the baseline for the character with most descent
            mxheight = 0, // Maximum heigh of character 'x' according to various font sizes
            hexcess 	= 0; // Holds the ascent height based on the
    // presence of descented characters like 'g'
    FT_UInt prev 	= 0; // Holds previous character read

    FT_Error error;

    auto map = []( auto color_rule, auto bkroot)
    {
      auto rules = parseColorRule( color_rule, bkroot);
      std::vector<std::shared_ptr<ColorRule>> new_rules( rules.size());
      std::transform( rules.cbegin(), rules.cend(), new_rules.begin(),
          []( auto rule) { return std::make_shared<ColorRule>( rule);});
      return new_rules;
    };

    auto rules = color_rule == nullptr ? std::vector<std::shared_ptr<ColorRule>>{} :  map( color_rule, bkroot.get());
    auto dummy = ColorRule{};
    size_t index = 0, nchars = countCharacters( word);
    for( const char *pw = word.data(); *pw;)
    {
        FT_Int shift = byteCount( *pw);
        FT_UInt character = collate( (uint8_t *)word.data(), pw - word.data(), shift);

        std::shared_ptr<ColorRule> best( &dummy, []( auto dummy){});
        index += 1;
        for( auto& each: rules)
        {
            if( ( each->end == INT32_MIN && index == each->start) ||
                ( index >= each->start && ( ( index <= each->end && each->end != INT32_MIN) || each->end == -1)))
                best = each;
        }

        FT_Int font_size = best->font_size_b == UINT32_MAX ? best->font_size_b = default_font_size : best->font_size_b;
        if( best->font_easing_fn)
        {
            size_t start = index - best->start,
                    end = best->end == -1 ? MAX( nchars - 1, 1) : best->end - best->start;
            auto fraction = best->font_easing_fn(( float)start / end);
            if( best->font_size_m == UINT32_MAX && best->font_size_e != UINT32_MAX)
                font_size = round( best->font_size_b * ( 1.0 - fraction) + fraction * best->font_size_e);
            else if( best->font_size_m != UINT32_MAX && best->font_size_e != UINT32_MAX)
                font_size = round( +2.0 * best->font_size_b * ( fraction - .5) * ( fraction - 1.)
                                   -4.0 * best->font_size_m * fraction * ( fraction - 1.)
                                   +2.0 * best->font_size_e * fraction * ( fraction - .5));
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
        if( best->soak && best->color_easing_fn)
          best->gradient->width += current.xstep;
		best->gradient->height = hexcess + mxheight + mdescent;
        insert( head, std::move( current));

        prev = character;
        pw += shift;
    }

    // Final container height. Dependent on the presence of
    // characters with stem like 'b' and characters with descent
    // like 'g'

    FT_Int height = hexcess + mxheight + mdescent;

    std::unique_ptr<uint64_t, void( *)( uint64_t *)> out( ( uint64_t *)calloc( width * height, sizeof( uint64_t)),
        []( uint64_t *p) { free( p);});

    FT_Vector pen;
    memset( &pen, 0, sizeof( pen));
    for( std::shared_ptr<Glyph> current = std::move( head), prev_link; current != nullptr;)
    {
        draw( *current, &pen, out.get(), mdescent, width, height, nchars - 1);
        prev_link = current;
        current = std::move( current->next);
    }
    
    if( as_image && destination != stdout)
        writePNG( destination, out.get(), width, height);
    else
        write( out.get(), width, height, raster_glyph, destination, root.get());
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
            best = approximate( node->left.get(), search, ldist, best, ndepth);
        else
            return best;
    }
    else
    {
        if( node->right != nullptr)
            best = approximate( node->right.get(), search, ldist, best, ndepth);
        else
            return best;
        left = false;
    }

    if( std::abs( search.rgb[ depth] - node->color.rgb[ depth]) < ldist)
        best = approximate( !left ? node->left.get() : node->right.get(), search, ldist, best, ndepth);

    return best;
}

void insert( std::shared_ptr<KDNode>& node, Color color, size_t index, uint8_t depth)
{
    if( node == nullptr)
    {
        node = std::make_shared<KDNode>( color, index);
        return;
    }

    uint8_t nchannel = color.rgb[ depth],
            cchannel = node->color.rgb[ depth],
            ndepth   = ( depth + 1) % 3;
    if( nchannel < cchannel)
        insert( node->left, color, index, ndepth);
    else
        insert( node->right, color, index, ndepth);
}

void write( const uint64_t *out, FT_Int width, FT_Int height, const char *raster_glyph, FILE *destination, KDNode *root)
{
    bool is_stdout = false;
    uint8_t raster_bytes = destination != stdout ? MAX( byteCount( *raster_glyph) - 1, 1) : is_stdout = true;
    std::string sp( raster_bytes, ' ');

    uint8_t fmt[] = { '\x1B', '[', '3', '8', ';', '5', ';', '0', '0', '0', 'm', '\0'};
    constexpr uint8_t offset = 7;

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

    auto *mem = ( png_structpp)png_calloc( png_ptr, height * width * bytes_per_pixel);
    auto deleter = [ &mem]( png_structpp ptr) { png_free( *ptr, ( png_voidp)mem); free( mem); };
    std::unique_ptr<png_structp, decltype( deleter)> image( mem, deleter);
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
            auto local_image_ref = ( uint8_t *)image.get();
            local_image_ref[     index] = mbyte >> 24u;
            local_image_ref[ index + 1] = ( mbyte >> 16u) & 0xFFu;
            local_image_ref[ index + 2] = ( mbyte >>  8u) & 0xFFu;
            local_image_ref[ index + 3] = mbyte & 0xFFu;
        }
    }

    if( height > PNG_UINT_32_MAX / ( sizeof(png_bytep)))
        png_error( png_ptr, "Image too small to process!");

    for( png_uint_32 i = 0; i < height; ++i)
        row_pointers[ i] = ( ( uint8_t *)image.get()) + i * width * bytes_per_pixel;

    png_write_image( png_ptr, row_pointers);

    png_write_end( png_ptr, info_ptr);

    png_destroy_write_struct( &png_ptr, &info_ptr);

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
    static const std::unordered_map<std::string_view, uint32_t> nameLookup =
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
        auto matches = findWordMatch( bkroot, name.c_str(), BKNode::Group::Color, 3);
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
	if( compareOr<std::equal_to<char>>( *rule, '#', 'x'))
	  ++rule;
	else if( strncasecmp( rule, "0x", 2) == 0)
	  rule += 2;
	else if( isalpha( *rule))
    {
        auto color_name = decodeColorName( rule, bkroot);
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
      return {};
//        fprintf( stderr, "Expected hex indicator near -> %s (allowed: `#`, `x`, `0x`)", prev);
//        exit( EXIT_FAILURE);
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

    return xyzToRgb( { xmix, ymix, zmix});
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
            op ? rcolor = extractColor( ctx, bkroot) : lcolor = extractColor( ctx, bkroot);
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

template <size_t count>
std::array<float, count> parseFloats( const char *&rule)
{
   std::array<float, count> response;
   for( auto& result : response)
   {
	   result = 0;
	   float post_decimal_point_value = 0, shift = 1;
	   if( ltrim( rule) && ( *rule == '(' || *rule == ','))
		 ++rule;
	   bool seen_dot = false;
	   do
	   {
		 if( ltrim( rule) && *rule == '.')
		 {
		   seen_dot = true;
		   ++rule;
		   continue;
		 }
		 seen_dot ? post_decimal_point_value += ( shift *= .1f) * ( *rule - '0') :
					result = result * 10.f + ( *rule - '0');
		 ++rule;
	   }
	   while( *rule != ')' && *rule != ',');
	   result += post_decimal_point_value;
   }
   if( *rule == ')')
     ++rule;
   
   return response;
}

std::vector<std::string> partition( std::string_view sgradient)
{
   std::cmatch search_result;
   std::regex  key( R"(\s*,\s*)");
  std::cregex_iterator begin( sgradient.cbegin(), sgradient.cend(), key),
  					   end, prev;
  
  std::vector<std::string> results;
  for( ;begin != end; prev = begin, ++begin)
    results.emplace_back( begin->prefix().str());
  
  if( prev != end && begin == end)
    results.emplace_back( prev->suffix().str());
  
  return results;
}
ConicGradient generateConicGradient( const char *&rule, const ColorRule& color_rule, BKNode *bkroot)
{
   ConicGradient actual_gradient;
   std::string gradient_part;
   size_t start_angle = 0;
   while( *rule && *rule != ')')
     gradient_part += *rule++;
   if( *rule == ')')
     ++rule;
   else
   {
     // TODO: Report error of missing parenthesis
   }
   
   auto stops = partition( gradient_part);
   std::smatch match_results;
   std::regex  matcher( R"(^(.+?)(?:\s+(\d+)deg)?(?:\s+(\d+)deg)?\s*$)");
   for( size_t i = 0; i < stops.size(); ++i)
   {
	 size_t end_angle{ SIZE_MAX};
	 std::smatch previous_outcome;
	 auto active = stops[ i];
	 do
	 {
	   if( std::regex_match( active.cbegin(), active.cend(), match_results, matcher))
	   {
		 if( match_results[ 2].matched)
		 {
		   if( end_angle == SIZE_MAX)
			 end_angle = std::stoul( match_results[ 2 + match_results[ 3].matched].str());
		   active = match_results[ 1].str();
		 }
		 else
		 {
		   auto initial_run = previous_outcome.empty();
		   auto maybe_color = initial_run ? match_results[ 1].str() :  previous_outcome[ 1].str();
		   if( maybe_color == "from")
		     start_angle = i == 0 ? end_angle : start_angle;
		   else
		   {
		     const char *underlying_data = maybe_color.data();
		     auto color_components = isalpha( *underlying_data) ? decodeColorName( underlying_data, bkroot)
		     	                                                : extractColor( underlying_data, bkroot);
		     actual_gradient.color_variations.emplace_back( Color{ ( uint8_t)RED( color_components),
															       ( uint8_t)GREEN( color_components),
															       ( uint8_t)BLUE( color_components)},
		     	                                            initial_run ? SIZE_MAX : end_angle);
		   }
		   break;
		 }
		 previous_outcome = match_results;
	   }
	   else
		 break;
	 }
	 while( true);
   }
   
   auto& cstops = actual_gradient.color_variations;
   if( color_rule.scolor.changed())
     cstops.insert( cstops.begin(), { { ( uint8_t)RED( color_rule.scolor),
										( uint8_t)GREEN( color_rule.scolor),
										( uint8_t)BLUE( color_rule.scolor)
									  }, SIZE_MAX});
   if( color_rule.ecolor.changed())
     cstops.emplace_back( Color{ ( uint8_t)RED( color_rule.scolor), ( uint8_t)GREEN( color_rule.scolor),
						         ( uint8_t)BLUE( color_rule.scolor)}, SIZE_MAX);
   size_t cstops_length = cstops.size();
   if( cstops_length > 0)
   {
     // Adjust cstops angle values to start at 0deg and end at 360deg
     if( cstops[ 0].second == SIZE_MAX)
       cstops[ 0].second = 0;
	 else if( cstops[ 0].second > 0)
	   cstops.insert( cstops.begin(), { cstops[ 0].first, 0});
     if( cstops.back().second == SIZE_MAX)
       cstops.back().second = 360;
     else if( cstops.back().second < 360)
	   cstops.emplace_back( cstops.back().first, 360);
     
     // Adjust cstop colors so that they appear in an increasing sequence.
     for( size_t i = 1; i < cstops_length; ++i)
	 {
       if( cstops[ i].second == SIZE_MAX)
	   {
         auto start = cstops.cbegin() + i;
         auto next_valid = std::find_if( start, cstops.cend(), []( auto& el){ return el.second != SIZE_MAX;});
         auto dist = std::distance( start, next_valid);
         cstops[ i].second = cstops[ i - 1].second + ( next_valid->second - cstops[ i - 1].second) / ( dist + 1);
	   }
       else
	   	cstops[ i].second = std::max( cstops[ i - 1].second, cstops[ i].second);
	 }
   }
   
   if( size_t i = 0; true)
   {
	 for( ; i < cstops_length; ++i)
	 {
	   // Offset all entries to start at start_angle
	   if( ( cstops[ i].second += start_angle) > 360)
	   {
		 cstops[ i].second = 360;
		 break;
	   }
	 }
	 // Delete remaining entries, they will not be visible to the user.
	 if( i < cstops_length)
	   cstops.erase( cstops.begin() + i + 1, cstops.end());
   }
   
   return actual_gradient;
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
                        fillEasingMode( ccolor.font_easing_fn, ++rule, bkroot, ']');
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
                        ccolor.scolor = mixColor( ++rule, nullptr);
                    }
                    else if( *rule != '-')
                    {
                        prev = rule;
                        ccolor.scolor = extractColor( rule,  bkroot);
                    }

                    if( ltrim( rule) && *rule == '-')
                    {
                      	auto gradient_is_next = compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r');
                        if( ccolor.end == INT32_MIN && !gradient_is_next)
                        {
                            fprintf( stderr, "Easing only available for range based colors -> %s", prev);
                            exit( EXIT_FAILURE);
                        }

                        if( !gradient_is_next)
						{
							++rule;
							if( *rule != '>')
							{
							  fprintf( stderr, "Expected '>' near -> %s", rule);
							  exit( EXIT_FAILURE);
							}
							
							if( ltrim( ++rule) && *rule == '(')
							  ccolor.ecolor = mixColor( ++rule, nullptr);
							else if( *rule != '\0')
							  ccolor.ecolor = extractColor( rule, bkroot);
							else
							{
							  fprintf( stderr, "Incomplete color specification");
							  exit( EXIT_FAILURE);
							}
						}
                        
                        if( ltrim( rule) && *rule == '-'
                        	 && compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r'))
						{
                          ltrim( ++rule);
                          char lc = std::tolower( *rule);
                          if( lc == 'r')
						  {
							auto [ x, y, z] = parseFloats<3>( rule += 2);
							ccolor.gradient.reset( new RadialGradient( x, y, z));
						  }
                          else if( lc == 'c')
                            ccolor.gradient.reset( new ConicGradient{
                              generateConicGradient( rule += 2, ccolor, bkroot)});
						}

                        if( ltrim( rule) && *rule == '+')
                        {
                            ccolor.soak = true;
                            ++rule;
                        }

                        fillEasingMode( ccolor.color_easing_fn, rule, bkroot, '}');
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

uint32_t editDistance( std::string_view main, std::string_view ref)
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
                                                 lookup[ j - 1][ i - 1] +
                                                 ( tolower( main[ j - 1]) != tolower( ref[ i - 1]))));
        }
    }

    return lookup[ mlength - 1][ rlength - 1];
}

void insert( std::shared_ptr<BKNode> &node, std::string_view word, BKNode::Group word_group)
{
    if( node == nullptr)
    {
        node = std::make_shared<BKNode>( word, word_group);
        return;
    }

    auto dist = editDistance( node->word, word);

    if( dist >= MAX_DIFF_TOLERANCE)
        return;

    if( node->next[ dist] == nullptr)
        insert( node->next[ dist], word, word_group);
    else
    {
        auto ndist = editDistance( node->next[ dist]->word, word);
        insert( node->next[ dist]->next[ ndist], word, word_group);
    }
}

void findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group,
				   int threshold, std::vector<std::string> &matches)
{
    if( node == nullptr)
        return;

    int dist = editDistance( node->word, word),
            mindist = MAX( dist - threshold, 1),
            maxdist = MIN( dist + threshold, MAX_DIFF_TOLERANCE - 1);

    if( dist <= threshold && node->group == word_group)
        matches.emplace_back( node->word);

    for( int i = mindist; i <= maxdist; ++i)
	  findWordMatch( node->next[i].get(), word, word_group, threshold, matches);
}

std::vector<std::string> findWordMatch( BKNode *node, std::string_view word, BKNode::Group word_group, int threshold)
{
    std::vector<std::string> matches;
  	findWordMatch( node, word, word_group, threshold, matches);
    return matches;
}

void fillEasingMode( std::function<float(float)> &function, const char *&rule, BKNode *bkroot, char eoc)
{
    // Reference: https://easings.net/
    const auto easeOutBounce = []( float progress)
    {
        const float n1 = 7.5625f;
        const float d1 = 2.75f;

        if (progress < 1 / d1)
            return n1 * progress * progress;
        else if (progress < 2 / d1)
        {
            progress -= 1.5f / d1;
            return n1 * progress * progress + 0.75f;
        }
        else if (progress < 2.5 / d1)
        {
            progress -= 2.25f / d1;
            return n1 * progress * progress + 0.9375f;
        }
        else
        {
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
                   return progress < 0.5 ? 4 * progress * progress * progress : 1 - pow( -2 * progress + 2, 3) / 2;
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

                   return ZERO( progress) ? 0 : EQUAL( progress, 1) ? 1
                                              : progress < 0.5 ?
                                                -( pow(2, 20 * progress - 10) * sin( ( 20 * progress - 11.125) * c5)) / 2
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

    std::string easing{};
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
        fprintf( stderr, "Unknown easing function `%s` specified!"
                         " Default easing function will be used\n", easing.c_str());
        auto matches = findWordMatch( bkroot, easing.c_str(), BKNode::Group::Easing, 3);
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

    PropertyManager<FcConfig *> config( FcConfigGetCurrent(), FcConfigDestroy);
    FcConfigSetRescanInterval( config.get(), 0);
    PropertyManager<FcPattern *> pattern( FcPatternCreate(), FcPatternDestroy);
    PropertyManager<FcObjectSet *> font_object_set( FcObjectSetBuild( FC_FILE, nullptr), FcObjectSetDestroy);
    PropertyManager<FcFontSet *> font_set( FcFontList(config.get(), pattern.get(), font_object_set.get()),
	[]( auto font_set_local)
    {
      if( font_set_local)
        FcFontSetDestroy( font_set_local);
    });

    if( font_set && font_set->nfont > 0)
    {
        int i = 0;
        do
        {
          PropertyManager<const char *> font( ( const char *)FcNameUnparse( font_set->fonts[ i]),
              []( auto font) { free( ( FcChar8 *)font); });
            auto *breakp = strchr( font.get(), '/');
            printf( "\t%s\n", breakp);
        }
        while( ++i < font_set->nfont);
    }
    FcFini();
}

int main( int ac, char *av[])
{
  std::shared_ptr<KDNode> kdroot;
  {
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
  }

  std::shared_ptr<BKNode> bkroot;

    /*
     * Easing function listing
     */
{
    insert( bkroot, FN_IEASEINSINE,       BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTSINE,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTSINE,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINQUAD,       BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTQUAD,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTQUAD,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINCUBIC,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTCUBIC,     BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTCUBIC,   BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINQUART,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTQUART,     BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTQUART,   BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINQUINT,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTQUINT,     BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTQUINT,   BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINEXPO,       BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTEXPO,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTEXPO,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINCIRC,       BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTCIRC,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTCIRC,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINBACK,       BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTBACK,      BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTBACK,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINELASTIC,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTELASTIC,   BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTELASTIC, BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINBOUNCE,     BKNode::Group::Easing);
    insert( bkroot, FN_IEASEOUTBOUNCE,    BKNode::Group::Easing);
    insert( bkroot, FN_IEASEINOUTBOUNCE,  BKNode::Group::Easing);

    /*
     * Color name listing
     */

    insert( bkroot, ICOLOR_ALICEBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_ANTIQUEWHITE,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_AQUA,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_AQUAMARINE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_AZURE,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_BEIGE,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_BISQUE,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_BLACK,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_BLANCHEDALMOND,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_BLUE,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_BLUEVIOLET,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_BROWN,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_BURLYWOOD,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_CADETBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_CHARTREUSE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_CHOCOLATE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_CORAL,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_CORNFLOWERBLUE,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_CORNSILK,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_CRIMSON,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_CYAN,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKBLUE,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKCYAN,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKGOLDENROD,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKGRAY,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKGREY,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKGREEN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKKHAKI,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKMAGENTA,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKOLIVEGREEN,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKORANGE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKORCHID,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKRED,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKSALMON,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKSEAGREEN,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKSLATEBLUE,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKSLATEGRAY,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKSLATEGREY,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKTURQUOISE,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_DARKVIOLET,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_DEEPPINK,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_DEEPSKYBLUE,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_DIMGRAY,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_DIMGREY,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_DODGERBLUE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_FIREBRICK,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_FLORALWHITE,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_FORESTGREEN,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_FUCHSIA,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_GAINSBORO,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_GHOSTWHITE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_GOLD,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_GOLDENROD,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_GRAY,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_GREY,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_GREEN,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_GREENYELLOW,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_HONEYDEW,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_HOTPINK,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_INDIANRED,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_INDIGO,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_IVORY,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_KHAKI,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_LAVENDER,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_LAVENDERBLUSH,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_LAWNGREEN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LEMONCHIFFON,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTCORAL,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTCYAN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTGOLDENRODYELLOW, BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTGRAY,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTGREY,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTGREEN,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTPINK,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSALMON,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSEAGREEN,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSKYBLUE,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSLATEGRAY,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSLATEGREY,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTSTEELBLUE,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIGHTYELLOW,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIME,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_LIMEGREEN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_LINEN,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_MAGENTA,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_MAROON,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMAQUAMARINE,     BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMBLUE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMORCHID,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMPURPLE,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMSEAGREEN,       BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMSLATEBLUE,      BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMSPRINGGREEN,    BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMTURQUOISE,      BKNode::Group::Color);
    insert( bkroot, ICOLOR_MEDIUMVIOLETRED,      BKNode::Group::Color);
    insert( bkroot, ICOLOR_MIDNIGHTBLUE,         BKNode::Group::Color);
    insert( bkroot, ICOLOR_MINTCREAM,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_MISTYROSE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_MOCCASIN,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_NAVAJOWHITE,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_NAVY,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_OLDLACE,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_OLIVE,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_OLIVEDRAB,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_ORANGE,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_ORANGERED,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_ORCHID,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_PALEGOLDENROD,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_PALEGREEN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_PALETURQUOISE,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_PALEVIOLETRED,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_PAPAYAWHIP,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_PEACHPUFF,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_PERU,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_PINK,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_PLUM,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_POWDERBLUE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_PURPLE,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_REBECCAPURPLE,        BKNode::Group::Color);
    insert( bkroot, ICOLOR_RED,                  BKNode::Group::Color);
    insert( bkroot, ICOLOR_ROSYBROWN,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_ROYALBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_SADDLEBROWN,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_SALMON,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_SANDYBROWN,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_SEAGREEN,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_SEASHELL,             BKNode::Group::Color);
    insert( bkroot, ICOLOR_SIENNA,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_SILVER,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_SKYBLUE,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_SLATEBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_SLATEGRAY,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_SLATEGREY,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_SNOW,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_SPRINGGREEN,          BKNode::Group::Color);
    insert( bkroot, ICOLOR_STEELBLUE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_TAN,                  BKNode::Group::Color);
    insert( bkroot, ICOLOR_TEAL,                 BKNode::Group::Color);
    insert( bkroot, ICOLOR_THISTLE,              BKNode::Group::Color);
    insert( bkroot, ICOLOR_TOMATO,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_TURQUOISE,            BKNode::Group::Color);
    insert( bkroot, ICOLOR_VIOLET,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_WHEAT,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_WHITE,                BKNode::Group::Color);
    insert( bkroot, ICOLOR_WHITESMOKE,           BKNode::Group::Color);
    insert( bkroot, ICOLOR_YELLOW,               BKNode::Group::Color);
    insert( bkroot, ICOLOR_YELLOWGREEN,          BKNode::Group::Color);
}

  PropertyManager<FT_Library> library( FT_Done_FreeType);
  PropertyManager<FT_Face>    face( FT_Done_Face);
  FT_Error          error;

    /*
     * Schema: av[ 0] [--list-fonts|--font-file=FILE [--font-size=NUM] [--color-rule=RULE] [--drawing-character=CHAR] [--output FILE]] text
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

    error = FT_Init_FreeType( &library.get());

    if( error != 0)
    {
      fprintf( stderr, "Unable to startup!");
      exit( EXIT_FAILURE);
    }

    error = FT_New_Face( library.get(), fontfile, 0, &face.get());

    if( error != 0)
    {
        fprintf( stderr, "Font file is invalid!");
        exit( EXIT_FAILURE);
    }

    render( word, face.get(), std::strtol( font_size, nullptr, 10),
            raster_glyph, screen, write_as_image, color_rule, kdroot, bkroot);

    return 0;
}