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
#include <cmath>
#include <iostream>
#include <unordered_map>
#include <functional>
#include <regex>
#include <variant>
#include "colors_defs.hpp"
#include "easing_defs.hpp"
#include "geo_vector.hpp"
#include "composition_defs.hpp"

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
#define ZERO( fl)                      ( std::abs( fl) <= EPSILON)
#define EQUAL( al, bl)                 ZERO( ( al) - ( bl))
#define UNSET( x)                      (( x) == -1)
#define RED( color)                    ( ( color) >> 24u)
#define GREEN( color)                  ( ( ( color) >> 16u) & 0xFFu)
#define BLUE( color)                   ( ( ( color) >> 8u) & 0xFFu)
#define ALPHA( color)                  ( ( color) & 0xFFu)
#define RGBA( red, green, blue, alpha) ( ( ( uint32_t)( red)) << 24u |\
                                         ( ( uint32_t)( green)) << 16u |\
                                         ( ( uint32_t)( blue)) << 8u | alpha)
#define RGB( red, green, blue)         RGBA( red, green, blue, 0u)
#define SCALE_RGB( color, scale)       RGB( RED( color) * ( scale), GREEN( color) * ( scale), BLUE( color) * ( scale))
#define XYZ_SCALE                      775
#define RGB_SCALE                      255
#define HALF_RGB_SCALE                 128
#define DEG_SCALE					   180.f / M_PI
#define ENUM_CAST( idx)					( static_cast<uint8_t>( idx))
#define MODEL_ENUM( mode)			  CompositionRule::CompositionModel::mode
/*
 * The composition table is made up of 2 bit field
 * per flag for CompositionModel.
 * E.g Copy has an index of 0, and a value of 00.
 * The lower zero from the right means that the size of the canvas should be maximum or minimum( 1 or 0).
 * The rightmost zero indicates that if the first bit is maximum, then the size should be that
 * of the source or destination( 1 or 0).
 * The fields are:
 * Copy 		   = 00,
 * DestinationAtop = 01,
 * DestinationIn   = 02,
 * DestinationOver = 03,
 * DestinationOut  = 04,
 * Lighter         = 05,
 * NotApplicable   = 06,
 * SourceAtop      = 07,
 * SourceIn        = 08,
 * SourceOver      = 09,
 * SourceOut       = 10,
 * Xor             = 11
 * 01 10 01 00 00 00 01 00 01 10 10 10
 * 11 10 09 08 07 06 05 04 03 02 01 00
 */
#define COMPOSITION_TABLE		       0x0064046AU
#define COMPOSITON_SIZE( idx)          ( ( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U)) & 1U)
#define COMPOSITION_SIDE( idx)		   ( ( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U + 1U)) & 1U)

void spansCallback( int y, int count, const FT_Span *spans, void *user)
{
    auto *sptr = (Spans *)user;
    for (int i = 0; i < count; ++i)
        sptr->emplace_back( spans[i].x, y, spans[i].len, spans[i].coverage);
}

void renderSpans( FT_Library library, FT_Outline *outline, Spans *spans)
{
    FT_Raster_Params params;
    memset( &params, 0, sizeof( params));
    params.flags = FT_RASTER_FLAG_AA | FT_RASTER_FLAG_DIRECT;
    params.gray_spans = spansCallback;
    params.user = spans;

    FT_Outline_Render( library, outline, &params);
}

void render( FT_Library library, FT_Face face, std::wstring_view text, ApplicationHyperparameters& guide)
{
    auto& color_rule = guide.color_rule;
    auto map = []( auto color_rule, auto bkroot)
    {
        auto rules = parseColorRule( color_rule, bkroot);
        std::vector<std::shared_ptr<ColorRule>> new_rules( rules.size());
        std::transform( rules.cbegin(), rules.cend(), new_rules.begin(),
                        []( auto rule) { return std::make_shared<ColorRule>( rule);});
        return new_rules;
    };
    auto rules = color_rule == nullptr ? std::vector<std::shared_ptr<ColorRule>>{}
                                       : map( color_rule, guide.bkroot.get());
    auto general = std::make_shared<ColorRule>();
    auto [ text_rows, max_length]= expand( text, guide.j_mode);
    MonoGlyphs rasters( text_rows.size() * max_length);
    RowDetails row_details( text_rows.size());
    FT_Glyph o_glyph;
    FT_Stroker stroker;
    FT_Stroker_New( library, &stroker);
    PropertyManager<FT_Stroker> stroker_deleter( stroker, FT_Stroker_Done);
    FT_Stroker_Set( stroker, guide.thickness << 6,
                    FT_STROKER_LINECAP_BUTT, FT_STROKER_LINEJOIN_MITER, 0);
    FT_Vector box_size{ 0, 0};
    for( size_t j = 0, t_row = text_rows.size(); j < t_row; ++j)
    {
        FT_Int max_ascent{};
        FT_Vector max_row_box{ 0, 0}, adjustment;
        auto &baseline = row_details[ j].baseline,
             &max_descent = row_details[ j].max_descent;
        auto line_height = 0;
        auto best = general;
        for( size_t i = 0, t_len = text_rows[ i].length(); i < t_len; ++i)
        {
            auto c_char = text_rows[ j][ i];
            /*
             * Select color to apply on glyph
             */
            for( auto& each: rules)
            {
                if( UNSET( each->end.x) && UNSET( each->end.y))
                {
                    best = each;
                    continue;
                }

                if( UNSET( each->end.x) || UNSET( each->end.y))
                {
                    if( UNSET( each->end.x) && j + 1 >= each->start.y && j <= each->end.y)
                        best = each;
                    else if( i + 1 >= each->start.x && i + 1 <= each->end.x)
                        best = each;
                    continue;
                }

                if( i + 1 >= each->start.x && i + 1 <= each->end.x && j + 1 >= each->start.y && j + 1 <= each->end.y)
                    best = each;
            }

            /*
             * Calculate font size for each glyph
             */
            FT_Int font_size = UNSET( best->font_size_b) ? best->font_size_b = guide.font_size
                                                               : best->font_size_b;
            line_height = font_size * guide.line_height;
            bool col = true;
            if( best->font_easing_fn)
            {
                auto start = Vec2D<int>( i + 1 - best->start.x, j + 1 - best->start.y),
                     end   = Vec2D<int>( UNSET( best->end.x)
                                         ? std::max( max_length - 1, 1) : best->end.x - best->start.x,
                                         UNSET( best->end.y)
                                         ? std::max<int>( t_row - 1, 1) : best->end.y - best->start.y);

                float fraction;
                if( guide.ease_col)
                    fraction = best->font_easing_fn(( float)start.y / ( float)end.y);
                else
                    fraction = best->font_easing_fn(( float)start.x / (float)end.x);
                if( UNSET( best->font_size_m) && !UNSET( best->font_size_e))
                    font_size = round( best->font_size_b * ( 1.0 - fraction) + fraction * best->font_size_e);
                else if( UNSET( best->font_size_m) && UNSET( best->font_size_e))
                    font_size = round( +2.0 * best->font_size_b * ( fraction - .5) * ( fraction - 1.)
                                       -4.0 * best->font_size_m * fraction * ( fraction - 1.)
                                       +2.0 * best->font_size_e * fraction * ( fraction - .5));
            }

            if ( FT_Set_Char_Size( face, font_size << 6, 0, guide.dpi, 0) != 0)
                continue;

            if( FT_Load_Char( face, c_char, FT_LOAD_DEFAULT) != 0
                || FT_Get_Glyph( face->glyph, &o_glyph) != 0
                || o_glyph->format != FT_GLYPH_FORMAT_OUTLINE)
                continue;

            /*
             * Draw the outline for the glyph
             */
            FT_Glyph_StrokeBorder( &o_glyph, stroker, false, true);

            auto& raster = rasters[ j * max_length + i];
            renderSpans( library, &reinterpret_cast<FT_OutlineGlyph>( o_glyph)->outline,
                         &raster.spans.second);
            raster.match = best;
            raster.is_valid = true;
            raster.level = j;
            raster.pos = Vec2D<int>( i + 1, j + 1);
            /*
             * Render bitmap for the glyph
             */
            FT_Render_Glyph( face->glyph, FT_RENDER_MODE_NORMAL);
            auto slot = face->glyph;
            BBox rect;
            int height{}, bearing_y{};
            if( !std::isspace( c_char))
            {
                std::unique_ptr<uint8_t[]> buffer( new uint8_t[ slot->bitmap.width * slot->bitmap.rows]);
                memmove( buffer.get(), slot->bitmap.buffer, slot->bitmap.width * slot->bitmap.rows);
                auto &[ main, outline] = raster.spans;
                /*
                 * main: Defines the filling of the glyph
                 * outline: defines the stroking of the glyph
                 */
                main = { .buffer = std::move( buffer),
                        .left   = slot->bitmap_left,
                        .top    = slot->bitmap_top,
                        .width  = (int) slot->bitmap.width,
                        .height = (int) slot->bitmap.rows};
                if( !outline.empty())
                {
                    rect = { outline.front().x,
                             outline.front().y,
                             outline.front().x,
                             outline.front().y};
                }
                else
                {
                    rect = {
                            main.left,
                            main.top - main.height,
                            main.left + main.width - 1,
                            main.top - main.height + main.height - 1
                    };
                }
                /*
                 * Calculate the exact bounding box for the outline
                 * given a list of spans.
                 */
                for ( auto &s: outline)
                {
                    rect.expandTo( s.x, s.y);
                    rect.expandTo( s.x + s.width - 1, s.y);
                }
                raster.advance = { static_cast<FT_Pos>(( o_glyph->advance.x >> 16) + guide.thickness * 2),
                                  ( o_glyph->advance.y >> 16)};
                height = rect.height();
                if( i > 0 && FT_HAS_KERNING( face))
                {
                    FT_Get_Kerning( face, c_char, text_rows[ j][ i - 1],
                                    FT_KERNING_DEFAULT, &adjustment);
                    max_row_box.x += adjustment.x >> 6;
                    max_row_box.y += adjustment.y >> 6;
                }
                /*
                 * Set the spacing of the glyphs to ensure no extra space occurs
                 * at the far end.
                 */
                max_row_box.x += ( i + 1 == t_len ? rect.width() : raster.advance.x);
                max_row_box.y = std::max<long>( max_row_box.y, height);
                raster.bbox = rect;
                bearing_y = ( slot->metrics.horiBearingY >> 6) + guide.thickness * 2;
                max_ascent  = std::max( max_ascent, bearing_y);
                max_descent = std::max( max_descent, -rect.ymin);
                baseline = std::min( rect.ymin, baseline); // This is the underline position for glyph groups
            }
            else
            {
                /*
                 * If the character is space, use the default advance as the width of the glyph
                 * since the bitmap.width and bitmap.rows will be zero.
                 */
                int default_width  = slot->metrics.horiAdvance >> 6;
                max_row_box.x += default_width;
                raster.advance.x = default_width;
            }

            FT_Done_Glyph( o_glyph);
        }
        /*
         * For every empty row( row containing only spaces),
         * update height to the default vertical advance.
         * Also, since the maximum height is zero, set it
         * to the default height.
         */
        if( max_row_box.y == 0)
        {
            auto default_height = ( face->glyph->metrics.vertAdvance >> 6) + guide.thickness * 2;
            for( size_t i = 0; i < text_rows[ j].size(); ++i)
            {
                auto c_char = text_rows[ j][ i];
                if( !std::isspace( c_char))
                    continue;
                auto& raster = rasters[ j * max_length + i];
                raster.advance.y = face->glyph->metrics.vertAdvance >> 6;
            }
            max_ascent = (int)default_height;
        }
        max_row_box.y = max_ascent + max_descent;
        row_details[ j].width = (int)max_row_box.x;
        if( max_row_box.y <= 0)
            continue;
        box_size.x = std::max( max_row_box.x, box_size.x); // The width of the largest row
        auto row_line_height = line_height * ( j + 1 != t_row); // The spacing between glyphs
        box_size.y += max_row_box.y + row_line_height;
        row_details[ j].v_disp += box_size.y - row_line_height; // Offset at which each row should start
        if( best->soak && best->color_easing_fn)
            best->gradient->width = box_size.x;
        best->gradient->height = box_size.y;
    }

    std::shared_ptr<uint32_t> pixel(( uint32_t *)malloc( sizeof( uint32_t) * box_size.x * box_size.y),
                                     []( auto *p){ free( p);});
    std::fill_n( pixel.get(), box_size.x * box_size.y, guide.background_color.cast());
    auto buffer = FrameBuffer<uint32_t>{ pixel, static_cast<int32_t>( box_size.x), static_cast<int32_t>( box_size.y)};

    draw( rasters, row_details, buffer, guide);

    PropertyManager<FILE *> destination( stdout, []( FILE *maybe_destructible)
    {
        if( maybe_destructible != stdout)
            fclose( maybe_destructible);
        maybe_destructible = nullptr;
    });

    if( guide.src_filename != nullptr)
    {
        auto *handle = fopen( guide.src_filename, "wb");
        if( handle)
            destination.get() = handle;
    }

    if( guide.composition_rule)
        composite( guide, buffer);
    else
    {
        if( guide.as_image && guide.src_filename)
            (guide.out_format == OutputFormat::JPEG ? writeJPG : writePNG)
            ( guide.src_filename, buffer, guide.image_quality);
        else
            write( buffer, guide.raster_glyph,
                   destination.get(), guide.kdroot.get());
    }
}

void draw( const MonoGlyphs &rasters, RowDetails &row_details,
           FrameBuffer<uint32_t> &frame, ApplicationHyperparameters &guide)
{
    auto n_glyphs = rasters.size();
    auto n_levels = row_details.size();
    int acc_dim;
    for( auto& raster : rasters)
     {
        auto& [ main, outline] = raster.spans;
        auto& rect = raster.bbox;
        int width = raster.bbox.width(),
            height = raster.bbox.height();
        if( !raster.is_valid)
            continue;

        auto& row_detail = row_details[ raster.level];
        auto& pen = row_detail.pen;
        auto& match = raster.match;
        uint32_t color = match->scolor;
        std::unique_ptr<uint32_t[]> row_colors;
        if( match->color_easing_fn && match->gradient->gradient_type == GradientType::Linear)
        {
         if( !match->soak)
         {
             auto start = Vec2D<int>( raster.pos.x - match->start.x, raster.pos.y - match->start.y),
                  end   = Vec2D<int>( UNSET( match->end.x)
                                     ? std::max<int>( n_glyphs / n_levels - 1, 1) : match->end.x - match->start.x,
                                      UNSET( match->end.y)
                                     ? std::max<int>( n_levels - 1, 1) : match->end.y - match->start.y);
             auto fraction = match->color_easing_fn( guide.ease_col ? ( float)start.y
                             / end.y : ( float)start.x / end.x);
             color = interpolateColor( match->scolor, match->ecolor, fraction);
             printf( "start: (%d, %d), end: (%d, %d)\n", start.x, start.y, end.x, end.y);
         }
         else
         {
             auto length = guide.ease_col ? height : width,
                  extent = guide.ease_col ? match->gradient->height : match->gradient->width;
             acc_dim = guide.ease_col ? row_detail.v_disp - height : ( raster.pos.x != 1) * ( acc_dim + length);
             row_colors.reset( new uint32_t[ length]);
             for( FT_Int i = 0; i < length; ++i)
             {
                 auto fraction = match->color_easing_fn(
                         ( float) ( i + acc_dim) / extent);
                 color = interpolateColor( match->scolor, match->ecolor, fraction);
                 row_colors.get()[ i] = color;
             }
         }
        }
         /* v_offset: Aligns every row glyph to their respective baselines
         *  h_offset: The adjustment necessary to comply with `Justification::Right` and `Justification::Center`
         */
        int v_offset = row_detail.v_disp - height - row_detail.max_descent - rect.ymin,
            h_offset = (int)( ENUM_CAST( guide.j_mode) > 0)
                           * (( frame.width - row_detail.width) / ENUM_CAST( guide.j_mode));
         for ( auto& s: outline)
        {
            for ( int w = 0; w < s.width; ++w)
            {
                auto& dest = frame.buffer.get()[(( v_offset + height - 1 - ( s.y - rect.ymin) + pen.y)
                                                 * frame.width + s.x - rect.xmin + w + h_offset + pen.x)];
                int i = s.x - rect.xmin + w,
                    j = s.y - rect.ymin,
                    color_index = guide.ease_col ? height - 1 - j : i;
                if( match->color_easing_fn)
                {
                    if( match->gradient->gradient_type == GradientType::Linear)
                        dest = match->soak ? row_colors.get()[ color_index] : color;
                    else
                        dest = easeColor( raster, row_detail, Vec2D<int>( n_glyphs / n_levels, n_levels),
                                          { i, height - 1 - j}, pen);
                }
                else
                    dest = color;
                if( i >= guide.thickness && i < ( width - guide.thickness))
                {
                    int m_index = ( height - 1 - j - guide.thickness) * main.width + ( i - guide.thickness);
                    if( m_index >= 0 && m_index < main.width * main.height)
                    {
                        if ( main.buffer.get()[ m_index] > 0)
                        {
                            if ( match->color_easing_fn)
                            {
                                if ( match->gradient->gradient_type == GradientType::Linear)
                                    dest = match->soak ? row_colors.get()[color_index] : color;
                                else
                                    dest = easeColor( raster, row_detail,
                                                      Vec2D<int>(n_glyphs / n_levels, n_levels),
                                                      {(int) (i - guide.thickness),
                                                      (int) (height - 1 - j - guide.thickness)}, pen);
                            }
                            else
                                dest = color;
                        }
                    }
                }
            }
        }

        pen.x += raster.advance.x;
        pen.y += raster.advance.y;
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

static uint32_t collate( uint8_t *str, size_t idx, uint8_t count)
{
    if( count == 1 )
        return str[ idx];

    uint32_t copy = count;
    while( copy > 1)
        str[ idx + --copy] &= 0x3FU;

    str[ idx] &= 0xFFU >> count;
    count -= 1;
    size_t i = ( count << 2u ) + ( count << 1u);

    uint32_t value = 0;
    while( (int8_t)count >= 0 )
    {
        value += str[ idx++ ] << i;
        --count;
        i -= 6;
    }

    return value;
}

std::wstring toWString( std::string str)
{
    std::wstring wsRep;
    for( size_t i = 0uL; i < str.size(); )
    {
        size_t byte_count = byteCount( str[i]);
        wsRep += collate( ( uint8_t *)&str[ 0], i, byte_count);
        i += byte_count;
    }

    return wsRep;
}

std::pair<std::vector<std::wstring>, int> expand( std::wstring_view provision, Justification mode)
{
    std::vector<std::wstring> parts;
    int j = 0, max_length = 0, prev = L'\0';
    for( size_t i = 0; i < provision.length(); ++i)
    {
        if( provision[ i] == L'\n')
        {
            auto length = i - j - ( prev == L'\r');
            auto substring = provision.substr( j, length);
            parts.emplace_back( substring);
            j = i + 1;
            max_length = std::max<int>( length, max_length);
        }
        prev = provision[ i];
    }
    parts.emplace_back( provision.substr( j));
    max_length = std::max<int>( parts.back().length(), max_length);
    for( auto& line : parts)
    {
        int rem  = max_length - line.length(),
                left  = rem / ENUM_CAST( mode),
                right = rem - ( left = ( left > 0) * left);
        line.insert( 0, std::wstring( left, ' '));
        line.append( std::wstring( right, ' '));
    }
    return { parts, max_length};
}

uint32_t easeColor( const MonoGlyph &raster, const RowDetail &row_detail,
                    Vec2D<int> size, Vec2D<int> pos, FT_Vector pen)
{
    auto& match = raster.match;
    auto glyph_width = raster.bbox.width();
    auto glyph_height = raster.bbox.height();
    auto color = match->scolor;
    auto start = Vec2D<int>( raster.pos.x - match->start.x, raster.pos.y - match->start.y),
         end   = Vec2D<int>( UNSET( match->end.x) ? std::max<int>( size.x - 1, 1) : match->end.x - match->start.x,
                             UNSET( match->end.y) ? std::max<int>( size.y - 1, 1) : match->end.y - match->start.y);
    auto cwidth  = match->soak ? match->gradient->width : ( int32_t)end.x,
         cheight = match->soak ? match->gradient->height : ( int32_t)end.y;
    if( match->gradient->gradient_type == GradientType::Radial)
    {
        auto& props = static_cast<RadialGradient *>( match->gradient.get())->props;
        /*
         * Computes the radial-gradient of color starting at `match->gradient->startx`
         */
        auto xy = Vec2D<float>( match->soak ? pos.x + pen.x : start.x,
                                match->soak ? pos.y + row_detail.v_disp - glyph_height : start.y)
                  / Vec2D<float>( cwidth, cheight);
        xy -= Vec2D<float>( props.x, props.y); // Adjust all pixels so as to push the users origin at (0,0)
        auto scale = ( float)cwidth / ( float)cheight;
        if( cwidth > cheight)         // Converts the resulting elliptical shape into a circle.
            xy.x *= scale;
        else
            xy.y *= scale;
        auto d = xy.length();
        /*
         *  Adjust the spread to smoothen out edges
         */
        auto left  = props.z - .3 < 0 ? props.z : props.z - .3;
        auto right = props.z - .3 < 0 ? props.z : props.z + .3;
        auto c = smoothstep( left, right, d); // Spread the circle around to form a smooth gradient.
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
    else if( match->gradient->gradient_type == GradientType::Conic)
    {
        auto gradient = static_cast<ConicGradient *>( match->gradient.get());
        auto origin = gradient->origin; // Object construction does not call assignment operator
        Vec2D<float> center( cwidth / 2.f, cheight / 2.f);
        if( gradient->origin.changed()) // Explicit access is needed is `origin` object is not copy assigned
            center = Vec2D<float>( origin.x * ( cwidth - 1), origin.y * ( cheight - 1));
        auto diff = ( Vec2D<float>( match->soak ? pos.x + pen.x : start.x,
                                    match->soak ? pos.y + pen.y + row_detail.v_disp - glyph_height : start.y)
                      - center);
        float angle = diff.angle();
        auto stops = gradient->color_variations;
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
            if( ( prev_stop.first & ~0xFFu) == ( cur_stop.first & ~0xFFu))
                color = cur_stop.first;
            else
            {
                auto fraction = ( angle - prev_stop.second) / ( cur_stop.second - prev_stop.second);
                if( match->color_easing_fn)
                    fraction = match->color_easing_fn( fraction);
                color = colorLerp( prev_stop.first, cur_stop.first, fraction);
            }
        }
    }

    return color;
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

uint32_t colorLerp( uint32_t lcolor, uint32_t rcolor, float progress)
{
    uint32_t r = RED( lcolor)   * ( 1.0 - progress) + RED( rcolor)   * progress,
             g = GREEN( lcolor) * ( 1.0 - progress) + GREEN( rcolor) * progress,
             b = BLUE( lcolor)  * ( 1.0 - progress) + BLUE( rcolor)  * progress,
             a = ALPHA( lcolor) * ( 1.0 - progress) + ALPHA( rcolor)  * progress;

    return RGBA( r, g, b, a);
}

uint32_t interpolateColor(uint32_t scolor, uint32_t ecolor, float progress)
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

void write( FrameBuffer<uint32_t> &frame, const char *raster_glyph, FILE *destination, KDNode *root)
{
    bool is_stdout = false;
    uint8_t raster_bytes = destination != stdout ? MAX( byteCount( *raster_glyph) - 1, 1) : is_stdout = true;
    std::string sp( raster_bytes, ' ');
    auto width = frame.width,
    	 height = frame.height;
    uint8_t fmt[] = { '\x1B', '[', '3', '8', ';', '5', ';', '0', '0', '0', 'm', '\0'};
    constexpr uint8_t offset = 7;

    for ( FT_Int j = 0; j < height; ++j)
    {
        for ( FT_Int i = 0; i < width; ++i)
        {
            double initial = INFINITY;
            uint32_t color =  frame.buffer.get()[ j * width + i];
            bool is_transparent = ( color & 0xFFu) == 0u;
            auto nmatch = approximate( root, { ( uint8_t)RED( color),
                                               ( uint8_t)GREEN( color),
                                               ( uint8_t)BLUE( color)}, initial);
            fmt[     offset] = nmatch->index / 100 + '0';
            fmt[ offset + 1] = ( nmatch->index - ( fmt[ offset] - '0') * 100) / 10 + '0';
            fmt[ offset + 2] = nmatch->index % 10  + '0';

            if( is_stdout)
                fprintf( destination, "%s", ( const char *)fmt);
            fprintf( destination,"%s", color && !is_transparent ? raster_glyph : sp.c_str());
            if( is_stdout)
                fprintf( destination, "\x1B[0m");
        }
        fputc( '\n', destination );
    }
}

bool intersects( std::array<Vec2D<float>, 4> corners, Vec2D<float> test)
{
   bool is_in = false;
   for( size_t i = 0, j = 3; i < 4u; j = i++)
  {
     // First check asserts that the point is located between the y coordinates and not above or below
     if( ( corners[ i].y > test.y) != ( corners[ j].y > test.y) &&
		 ( test.x < ( corners[ j].x - corners[ i].x) * ( test.y - corners[ i].y) // x = (x2 - x1)*(y-y1)/(y2-y1)+x1
		 / ( corners[ j].y - corners[ i].y) + corners[ i].x)) // x is a point on the line (x1,y1),(x2,y2) check if the
       is_in = !is_in; // given x coordinate is within the bounding box of the polygon
  }
   
   return is_in;
}

void applyFilter( FrameBuffer<uint32_t> &frame, uint8_t filter)
{
   const size_t bk_size =  9,
	  			ek_size = 25;
   float basic_kernels[ FilterMode::B_FILTER_SENTINEL][ bk_size] =
   {
   		[ FilterMode::SHARPEN]       = {  0, -1,  0,
									     -1,  5, -1,
									      0, -1,  0},
		[ FilterMode::BOX_BLUR]		 = { 1/9., 1/9., 1/9.,
								         1/9., 1/9., 1/9.,
										 1/9., 1/9., 1/9.},
   		[ FilterMode::GAUSSIAN_BLUR] = { 1/16., 2/16., 1/16.,
									     2/16., 4/16., 2/16.,
									     1/16., 2/16., 1/16.},
   };
 
   float extended_kernels[ FilterMode::E_FILTER_SENTINEL][ ek_size] =
   {
   		[ FilterMode::GAUSSIAN_BLUR_x5 - B_FILTER_SENTINEL - 1] = { 1/256., 4/256., 6/256., 4/256., 1/256.,
																    4/256., 16/256., 24/256., 16/256., 4/256.,
																    6/256., 24/256., 36/256., 24/256., 6/256.,
																    4/256., 16/256., 24/256., 16/256., 4/256.,
																    1/256., 4/256., 6/256., 4/256., 1/256.}
   };
 
  if( filter >= FilterMode::B_FILTER_SENTINEL && filter >= FilterMode::E_FILTER_SENTINEL)
	return;
  
  const auto k_width = filter < FilterMode::B_FILTER_SENTINEL ? 3 : 5;
  const auto k_size = k_width > 3 ? ek_size : bk_size;
  auto kernel = filter < FilterMode::B_FILTER_SENTINEL ? basic_kernels[ filter]
													   : extended_kernels[ filter - FilterMode::B_FILTER_SENTINEL - 1];
  auto width   = frame.width,
	  height  = frame.height;
  
  std::shared_ptr<uint32_t> dest( ( uint32_t *)malloc( width * height * sizeof( uint32_t)), []( auto *p){ free( p);});
  auto buffer   = frame.buffer.get(),
       d_buffer = dest.get();
  for( size_t j = 0; j < height; ++j)
  {
	for( size_t i = 0; i < width; ++i)
	{
	  int red{}, green{}, blue{}, alpha{};
	  for(size_t k = 0; k < k_size; ++k)
	  {
		size_t row = k % k_width,
			   col = k / k_width;
		if( row + j < height && col + i < width)
		{
		  size_t index = ( row + j) * width + i + col;
		  auto pixel = buffer[ index];
		  red   += kernel[ k] * RED( pixel);
		  green += kernel[ k] * GREEN( pixel);
		  blue  += kernel[ k] * BLUE( pixel);
		  alpha += kernel[ k] * ALPHA( pixel);
		}
	  }
	  d_buffer[ j * width + i] = RGBA( red, green, blue, alpha);
	}
  }
  frame.buffer = dest;
}

void composite( ApplicationHyperparameters &guide, FrameBuffer<uint32_t> &s_frame)
{
   auto c_rule = parseCompositionRule( guide.composition_rule);
   if( c_rule.model == MODEL_ENUM( NotApplicable))
     return;
   
   auto d_frame = ( isJPEG( guide.dest_filename) ? readJPEG : readPNG)( guide.dest_filename);
   if( d_frame.buffer == nullptr)
     return;
   
   int32_t dwidth  = d_frame.width,
   		   dheight = d_frame.height;
   auto swidth     = s_frame.width,
        sheight    = s_frame.height;

   auto pos = Vec2D<float>( c_rule.position.x * ( dwidth - 1)  + ( 1 - c_rule.position.x) * - swidth  + 1,
   	                        c_rule.position.y * ( dheight - 1) + ( 1 - c_rule.position.y) * - sheight + 1),
		center = Vec2D<float>( pos.x + swidth / 2.f, pos.y + sheight / 2.f);

   // Defines the rotated corners of the given image.
	std::array<Vec2D<float>, 4> corners = {
		( pos                                               - center).rotate( c_rule.angle) + center,
		( pos + Vec2D<float>( swidth - 1.f, 0)              - center).rotate( c_rule.angle) + center,
		( pos + Vec2D<float>( swidth - 1.f, sheight - 1.f)  - center).rotate( c_rule.angle) + center,
		( pos + Vec2D<float>( 0, sheight - 1.f)             - center).rotate( c_rule.angle) + center
	};

	// Defines the corners of the destination image
	std::array<Vec2D<float>, 4> big_corners = {
		Vec2D<float>( 0, 0),
		Vec2D<float>( dwidth - 1, 0),
		Vec2D<float>( dwidth - 1, dheight - 1),
		Vec2D<float>( 0, dheight - 1)
	};

	// Selects the boundaries of the image
	auto cherryPick = []( auto corners, bool is_min = true)
	{
	  using iter_type = decltype( std::begin( corners));
	  auto comp       = []( auto l, auto r){ return l.x < r.x;};
	  auto model      = ( is_min ? std::min_element<iter_type, decltype( comp)>
	                             : std::max_element<iter_type, decltype( comp)>);
	  auto x_ptr      = model( std::begin( corners), std::end( corners), comp);

	  Vec2D<float> *y_ptr{ nullptr};
	  for( auto& corner : corners)
	  {
		if( &corner == x_ptr)
		  continue;
		if( y_ptr == nullptr)
		  y_ptr = &corner;
		else if( is_min ? corner.y < y_ptr->y : corner.y > y_ptr->y)
		  y_ptr = &corner;
	  }
	  
	  return std::pair{ *x_ptr, *y_ptr};
	};
   Vec2D<float> x_min, y_min, x_max, y_max;
  
  std::tie( x_min, y_min) 	 = cherryPick( corners);
  std::tie( x_max, y_max)	 = cherryPick( corners, false);
   
   auto angle                = ( y_min - x_min).angle();
   auto smallbox_top_edge    = Vec2D<float>( ( ( y_min - x_min).rotate( -angle) + x_min).x, y_min.y);
   auto origin      		 = smallbox_top_edge;
   float width      		 = x_max.x - x_min.x + 1,
   		 height     		 = y_max.y - y_min.y + 1;
   int   final_width,
   		 final_height;
   
   if( COMPOSITON_SIZE( c_rule.model))
   { // The final canvas size is the maximum of the width and height of both canvas
     auto bottom_edge = Vec2D<float>( std::max( origin.x + width  - 1.f,  dwidth  - 1.f),
     	                              std::max( origin.y + height - 1.f, dheight  - 1.f));
     origin.x         = std::min( origin.x, 0.f);
     origin.y         = std::min( origin.y, 0.f);
     final_width      = bottom_edge.x - origin.x + 1;
     final_height     = bottom_edge.y - origin.y + 1;
   }
   else if( COMPOSITION_SIDE( c_rule.model))
   { // The final canvas size is the size of the bounding box of the source canvas
     final_width  = width;
     final_height = height;
   }
   else
   { // The final canvas size is the size of the destination canvas
     origin       = big_corners[ 0];
     final_width  = dwidth;
     final_height = dheight;
   }
   
   auto image = FrameBuffer<uint32_t>( std::shared_ptr<uint32_t>(
   	                                   ( uint32_t *)calloc( final_width * final_height, sizeof( uint32_t)),
   	                                   []( auto *p) { free( p);}), final_width, final_height);
  struct Default{};
  struct Top{};
  struct Bottom{};
  
  auto s_frame_dimension = s_frame.width * s_frame.height;
  bool source_over = false;
  std::array<std::function<void( std::variant<Default, Top, Bottom>)>,
             ENUM_CAST( MODEL_ENUM( Xor)) + 1> models = {
	  [&]( auto part) // Copy
	  {
		for( int y = smallbox_top_edge.y, j = 0; j < final_height; ++y, ++j)
		{
		  for ( int x = smallbox_top_edge.x, i = 0; i < final_width; ++x, ++i)
		  {
			auto point = Vec2D<float>( x, y);
			if( intersects( corners, point))
			{
			  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
			  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
			  if( index > 0 && index < s_frame_dimension)
				image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
			}
		  }
		}
	  },
	  [&]( auto part) // DestinationAtop
	  {
		std::visit( [ &]( auto&& current)
		{
		  using T = std::remove_cv_t<std::remove_reference_t<decltype(current)>>;
		  const size_t d_max_index = d_frame.width * d_frame.height * d_frame.n_channel;
		  auto *d_buffer = d_frame.buffer.get();
		  for( int y = smallbox_top_edge.y, j = 0; j < final_height; ++y, ++j)
		  {
			for ( int x = smallbox_top_edge.x, i = 0; i < final_width; ++x, ++i)
			{
			  auto point = Vec2D<float>( x, y);
			  if constexpr ( std::is_same_v<T, Default>) // DestinationAtop
			  {
				auto d_index = j * d_frame.width * d_frame.n_channel +  i * d_frame.n_channel;
				if( d_index < d_max_index && intersects( corners, point) && intersects( big_corners, point))
				{
					image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
						                                             d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
				}
				else if( intersects( corners, point))
				{
				  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				  if( index >= 0 && index < s_frame_dimension)
					image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
				}
			  }
			  else if constexpr ( std::is_same_v<T, Top>) // DestinationIn
			  {
				auto d_index = j * d_frame.width * d_frame.n_channel + i * d_frame.n_channel;
				if( d_index < d_max_index && intersects( corners, point) && intersects( big_corners, point))
				{
				  image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
																   d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
				}
			  }
			  else if constexpr( std::is_same_v<T, Bottom>) // Source-Out
			  {
				if( intersects( corners, point) && !intersects( big_corners, point))
				{
				  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				  if( index > 0 && index < s_frame_dimension)
					image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
				}
			  }
			}
		  }
		}, part);
	  },
	  [&]( auto part) // DestinationIn
	  {
		models[ ENUM_CAST( MODEL_ENUM( DestinationAtop))]( Top());
	  },
	  [&]( auto part) // DestinationOver
	  {
	    std::visit( [&]( auto&& current)
	    {
	      using T = std::remove_cv_t<std::remove_reference_t<decltype( current)>>;
		  for( int y = origin.y, j = 0; j < final_height; ++y, ++j)
		  {
			for ( int x = origin.x, i = 0; i < final_width; ++x, ++i)
			{
			  auto point = Vec2D<float>( x, y);
			  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
			  if constexpr ( !std::is_same_v<T, Default>)
			  {
			    if( auto b_intersects = intersects( big_corners, point), c_intersects = intersects( corners, point);
			        b_intersects && c_intersects)
				{
			      if constexpr( std::is_same_v<T, Bottom>) // Selection for Lighter
				  {
					auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
					int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
					auto *buffer = d_frame.buffer.get();
					auto b_color = RGBA( buffer[ d_index], buffer[ d_index + 1],
										 buffer[ d_index + 2], buffer[ d_index + 3]);
					uint32_t c_color = b_color;
					if( index > 0 && index < s_frame_dimension)
					 	c_color = s_frame.buffer.get()[ index];
					auto final_color = mixRgb( b_color, c_color);
					auto alpha = ( uint16_t)( c_color & 0xFFu) + ( b_color & 0xFFu);
					final_color = final_color | std::min( 0xFFu, alpha);
					image.buffer.get()[ j * final_width + i] = final_color;
				  }
				}
			    else if( b_intersects) // Bypass the intersection of source and destination
				{
				  auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
				  auto *buffer = d_frame.buffer.get();
				  image.buffer.get()[ j * final_width + i] = RGBA( buffer[ d_index], buffer[ d_index + 1],
																   buffer[ d_index + 2], buffer[ d_index + 3]);
				}
			    else if( c_intersects)
				{
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				  if( index > 0 && index < s_frame_dimension)
					image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
				}
			  }
			  else if( intersects( big_corners, point)) // Selection for DestinationOver
			  {
				auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
				auto *buffer = d_frame.buffer.get();
				image.buffer.get()[ j * final_width + i] = RGBA( buffer[ d_index], buffer[ d_index + 1],
																 buffer[ d_index + 2], buffer[ d_index + 3]);
			  }
			  else if( intersects( corners, point)) // Selection for DestinationOver
			  {
				int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				if( index > 0 && index < s_frame_dimension)
				  image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
			  }
			}
		  }
		}, part);
	  },
	  [&]( auto part) // DestinationOut
	  {
		models[ ENUM_CAST( MODEL_ENUM( SourceAtop))]( Top());
	  },
	  [&]( auto part) // Lighter
	  {
		models[ ENUM_CAST( MODEL_ENUM( DestinationOver))]( Bottom());
	  },
	  []( auto part) // Not Applicable
	  {
	  },
	  [&]( auto part) // SourceAtop
	  {
	    std::visit( [ &]( auto&& current)
	    {
	      using T = std::remove_cv_t<std::remove_reference_t<decltype( current)>>;
		  for( int y = origin.y, j = 0; j < d_frame.height; ++y, ++j)
		  {
			for( int x = origin.x, i = 0; i < d_frame.width; ++x, ++i)
			{
			  auto s_index    = j * d_frame.width * d_frame.n_channel + i * d_frame.n_channel,
				  d_index     = j * d_frame.width + i;
			  auto *d_ptr   = d_frame.buffer.get();
			  uint32_t &pixel = image.buffer.get()[ d_index];
			  auto point      = Vec2D<float>( x, y);
		 
			  if constexpr ( std::is_same_v<T, Default>) // SourceAtop
			  {
				if( intersects( corners, point) && intersects( big_corners, point))
				{
				  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				  auto rgba    = s_frame.buffer.get()[ index];
				  if( ALPHA( rgba) < 180)
				    pixel = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
								  d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
				  else
				  	pixel = s_frame.buffer.get()[ index];
				}
				else
				{
				  pixel = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
								d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
				}
			  }
			  else if constexpr ( std::is_same_v<T, Top>) // DestinationOut
			  {
				if( !intersects( corners, point) || !intersects( big_corners, point))
				  pixel = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
								d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
			  }
			  else if constexpr ( std::is_same_v<T, Bottom>) // SourceIn
			  {
				if( intersects( corners, point) && intersects( big_corners, point))
				{
				  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
				  pixel        = s_frame.buffer.get()[ index];
				}
			  }
			}
		  }
	    }, part);
	  },
	  [&]( auto part) // SourceIn
	  {
		models[ ENUM_CAST( MODEL_ENUM( SourceAtop))]( Bottom());
	  },
	  [&]( auto part) // SourceOver
	  {
	    auto *s_buffer = s_frame.buffer.get();
	    auto *d_buffer = d_frame.buffer.get();
		for( int y = origin.y, j = 0; j < final_height; ++y, ++j)
		{
		  for (int x = origin.x, i = 0; i < final_width; ++x, ++i)
		  {
			auto point = Vec2D<float>( x, y);
			auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
			if( intersects( corners, point))
			{
			  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
			  if( index > 0 && index < s_frame_dimension)
              {
			      if( ALPHA( s_buffer[ index]) < 180 && intersects( big_corners, point))
                  {
                      auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                      image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                                                       d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                  }
                  else
                      image.buffer.get()[ j * final_width + i] = s_buffer[ index];
              }
			}
			else if( intersects( big_corners, point))
			{
			  auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
			  image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
															   d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
			}
		  }
		}
	  },
	  [&]( auto part) // SourceOut
	  {
		models[ ENUM_CAST( MODEL_ENUM( DestinationAtop))]( Bottom());
	  },
	  [&]( auto part) // Xor
	  {
		models[ ENUM_CAST( MODEL_ENUM( DestinationOver))]( Top());
	  }
   };
  
  models[ ENUM_CAST( c_rule.model)]( Default());

  ( guide.out_format == OutputFormat::JPEG ? writeJPG : writePNG)( guide.src_filename, image, guide.image_quality);
}

bool isJPEG( std::string_view filename)
{
    auto *extension = strrchr( filename.data(), '.');
    if( extension && ++extension)
    {
        std::regex rule( R"(^jpe?g$)", std::regex_constants::icase);
        return std::regex_match( extension, extension + strlen( extension), rule);
    }

    return false;
}

FrameBuffer<png_byte> readPNG( std::string_view filename)
{
   constexpr const auto BYTES_READ = 8;
   char magic[ BYTES_READ];
   PropertyManager<FILE *> handle( fopen( filename.data(), "rb"), []( auto *p) { if( p) fclose( p); p = nullptr;});
   if( handle.get() == nullptr)
     return {};
   
   if( fread( magic, 1, BYTES_READ, handle.get()) != BYTES_READ)
     return {};
   
   if( png_sig_cmp( png_const_bytep ( magic), 0, BYTES_READ)) //TODO report invalid png
     return {};
   
   png_structp png_ptr = png_create_read_struct( PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
   if( png_ptr == nullptr)
     return {};
   
   png_infop info_ptr = png_create_info_struct( png_ptr);
   if( info_ptr == nullptr)
     png_destroy_read_struct( &png_ptr, nullptr, nullptr);
   
   if( setjmp( png_jmpbuf( png_ptr)))
   {
     png_destroy_read_struct( &png_ptr, &info_ptr, nullptr);
     return {};
   }

   png_init_io( png_ptr, handle.get());
   png_set_sig_bytes( png_ptr, BYTES_READ);
   png_read_info( png_ptr, info_ptr);
 
   FrameBuffer<png_byte> frame;
   png_int_32 bit_depth, color_type;
   png_get_IHDR( png_ptr, info_ptr,
				 reinterpret_cast<png_uint_32 *>( &frame.width),
				 reinterpret_cast<png_uint_32 *>( &frame.height),
				 &bit_depth, &color_type, nullptr, nullptr, nullptr);
   Color background_color;
   bool has_background = false;
   if( png_get_valid( png_ptr, info_ptr, PNG_INFO_bKGD))
   {
	 png_color_16p background;
     png_get_bKGD( png_ptr, info_ptr, &background);
     auto& [ red, green, blue] = background_color.rgb;
     if( bit_depth == 16)
	 {
       red   = background->red   >> 8u;
       green = background->green >> 8u;
       blue  = background->blue  >> 8u;
 	 }
     else if( color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
	 {
       if( bit_depth == 1)
         red = green = blue = background->gray ? 255 : 0;
       else if( bit_depth == 2)
         red = green = blue = ( 255 / 3)  * background->gray;
       else
         red = green = blue = ( 255 / 15) * background->gray;
	 }
     else
	 {
       red   = background->red;
       green = background->green;
       blue  = background->blue;
	 }
     
     has_background = true;
   }
   
   if( color_type == PNG_COLOR_TYPE_PALETTE)
     png_set_palette_to_rgb( png_ptr);
   else if( color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
     png_set_expand_gray_1_2_4_to_8( png_ptr);
   else if( png_get_valid( png_ptr, info_ptr, PNG_INFO_tRNS))
     png_set_tRNS_to_alpha( png_ptr);
   
   if( bit_depth == 16)
   	png_set_strip_16( png_ptr);
   if( color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
     png_set_gray_to_rgb( png_ptr);
   
   png_read_update_info( png_ptr, info_ptr);
   png_uint_32 row_bytes = png_get_rowbytes( png_ptr, info_ptr);
   std::unique_ptr<png_byte, DeleterType> image_data( ( png_bytep)malloc( frame.height * row_bytes),
   	                                                  []( auto *p){ free( p);});
  if( image_data)
   {
	 png_bytep row_pointers[ frame.height];
	 for( size_t i = 0; i < frame.height; ++i)
	   row_pointers[ i] = image_data.get() + i * row_bytes;
	 png_read_image( png_ptr, row_pointers);
	 png_read_end( png_ptr, info_ptr);
	 frame.buffer = std::move( image_data);
	 frame.n_channel = png_get_channels( png_ptr, info_ptr);
   }

  png_destroy_read_struct( &png_ptr, &info_ptr, nullptr);

   return std::move( frame);
}

void writePNG( std::string_view filename, FrameBuffer<uint32_t> &frame, int quality)
{
    png::image<png::rgba_pixel> image( frame.width, frame.height);
    auto buffer = frame.buffer.get();
    for( png_uint_32 j = 0; j < frame.height; ++j)
    {
        for ( png_uint_32 i = 0; i < frame.width; ++i)
        {
            auto pixel = PNG_ENDIAN( buffer[ j * frame.width + i]);
            image.set_pixel( i, j, *(( png::rgba_pixel *)&pixel));
        }
    }
    image.write( filename.data());
}

FrameBuffer<uint8_t> readJPEG( std::string_view filename)
{
    auto *handle = fopen( filename.data(), "rb");
    if( handle == nullptr)
        return {};
    PropertyManager<FILE *> file_manager( handle, []( auto *p){ fclose( p);});

    jpeg_decompress_struct decompressor;
    memset( &decompressor, 0, sizeof(decompressor));
    PropertyManager<jpeg_decompress_struct *> manager( &decompressor, jpeg_destroy_decompress);

    jpeg_error_mgr error_mgr;
    decompressor.err = jpeg_std_error( &error_mgr);

    jpeg_create_decompress( &decompressor);
    jpeg_stdio_src( &decompressor, handle);

    if( jpeg_read_header( &decompressor, TRUE) != JPEG_HEADER_OK)
        return {};

    assert( decompressor.out_color_space == J_COLOR_SPACE::JCS_RGB ||
            decompressor.out_color_space == J_COLOR_SPACE::JCS_GRAYSCALE);

    std::shared_ptr<uint8_t> mem( ( uint8_t *)malloc( decompressor.image_width * decompressor.image_height * 4),
                                  []( auto *p){ free( p);});
    FrameBuffer<uint8_t> frame( mem, decompressor.image_width, decompressor.image_height, 4);
    PropertyManager<JSAMPROW> row_manager( new JSAMPLE[ frame.width * decompressor.num_components],
                                           []( auto *p) { delete[] p;});

    auto *s_row_pointer = row_manager.get();
    auto *d_row_pointer = frame.buffer.get();
    auto is_rgb = decompressor.out_color_space == JCS_RGB;

    jpeg_start_decompress( &decompressor);
    while( decompressor.output_scanline < frame.height)
    {
        jpeg_read_scanlines( &decompressor, &s_row_pointer, 1);
        for( size_t i = 0; i < frame.width; ++i)
        {
            auto index = ( decompressor.output_scanline - 1) * frame.width * frame.n_channel + i * frame.n_channel;
            d_row_pointer[ index + 0] = s_row_pointer[ i * decompressor.num_components + 0];
            d_row_pointer[ index + 1] = s_row_pointer[ i * decompressor.num_components + is_rgb];
            d_row_pointer[ index + 2] = s_row_pointer[ i * decompressor.num_components + is_rgb + is_rgb];
            d_row_pointer[ index + 3] = 0xFFu;
        }
    }
    jpeg_finish_decompress( &decompressor);

    return frame;
}

void writeJPG( std::string_view filename, FrameBuffer<uint32_t> &frame, int quality)
{
   auto *handle = fopen( filename.data(), "wb");
   if( handle == nullptr)
       return;
   PropertyManager<FILE *> f_manager( handle, []( auto *p){ fclose( p);});
   jpeg_compress_struct compressor;
   PropertyManager<jpeg_compress_struct *> manager( &compressor, jpeg_destroy_compress);
   
   jpeg_error_mgr error_mgr;
   compressor.err = jpeg_std_error( &error_mgr);
   
   jpeg_create_compress( &compressor);
   jpeg_stdio_dest(&compressor, handle);
   
   compressor.image_width = frame.width;
   compressor.image_height = frame.height;
   compressor.input_components = 3;
   compressor.in_color_space = JCS_RGB;
   compressor.write_JFIF_header = TRUE;
   compressor.JFIF_major_version = JPEG_LIB_VERSION_MAJOR;
   compressor.JFIF_minor_version = JPEG_LIB_VERSION_MINOR;
   
   jpeg_set_defaults( &compressor);
   compressor.dct_method  = JDCT_FLOAT;
   jpeg_set_quality( &compressor, quality, TRUE);
   compressor.raw_data_in = FALSE;
   compressor.smoothing_factor = 100;
//   compressor.optimize_coding = TRUE;

	const auto row_stride = compressor.image_width * 3;
	jpeg_start_compress( &compressor, TRUE);
	auto buffer = frame.buffer.get();
	PropertyManager<uint8_t *> row_manager( new uint8_t[ row_stride], []( auto *p) { delete[] p;});
	auto row_buffer = row_manager.get();
	for( int j = 0; j < compressor.image_height; ++j)
	{
	  for( int i = 0; i < compressor.image_width; ++i)
	  {
		auto index = j * compressor.image_width + i;
		auto pixel = buffer[ index];
		row_buffer[ i * 3 + 0] = RED( pixel);
		row_buffer[ i * 3 + 1] = GREEN( pixel);
		row_buffer[ i * 3 + 2] = BLUE( pixel);
	  }
	  jpeg_write_scanlines( &compressor, &row_buffer, 1);
	}

	jpeg_finish_compress( &compressor);
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
    auto lxyz = xyzFromRgb( lcolor),
         rxyz = xyzFromRgb( rcolor);

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
	   while( *rule && *rule != ')' && *rule != ',');
	   result += post_decimal_point_value;
   }
   if( *rule == ')')
     ++rule;
   
   return response;
}

std::vector<std::string> partition( std::string_view provision, std::string_view regexpr)
{
   std::cmatch search_result;
   std::regex  key( regexpr.data()); // Find a `,` and assert that the comma is followed by an alphabet.
  std::cregex_iterator begin( provision.cbegin(), provision.cend(), key),
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
   if( *rule == '(')
   {
     ++rule;
	 while( *rule && *rule != ')')
	   gradient_part += *rule++;
	 if( *rule == ')')
	   ++rule;
	 else
	 {
	   // TODO: Report error of missing parenthesis
	 }
  
	 auto stops = partition( gradient_part, R"(\s*,\s*(?=[a-zA-Z#]|0[xX]))");
	 size_t pos;
	 if( !stops.empty() && ( pos = stops[ 0].find( "at")) != std::string::npos)
	 {
	   std::string srange = stops[ 0].substr( pos + 3);
	   const char *srange_ptr = &srange[ 0];
	   stops[ 0] = stops[ 0].substr( 0, pos - 1);
	   auto floats = parseFloats<2>( srange_ptr);
	   actual_gradient.origin = Vec2D( floats[ 0], floats[ 1]);
	 }
	 std::smatch match_results;
     if( !stops.empty())
     {
         std::regex matcher( R"(^(.+?)(?:\s+([-+]?\d+)deg)?(?:\s+([-+]?\d+)deg)?\s*$)", std::regex_constants::icase);
         for( size_t i = 0, n_stops = stops.size(); i < n_stops; ++i)
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
                             auto color_components = isalpha( *underlying_data)
                                                     ? decodeColorName( underlying_data, bkroot)
                                                     : extractColor( underlying_data, bkroot);
                             actual_gradient.color_variations.emplace_back(
                                     color_components, initial_run ? SIZE_MAX : std::abs( (int)end_angle));
                             if( end_angle != SIZE_MAX && (int)end_angle < 0)
                                 fprintf( stderr, "Negative position not allowed! |%d| will be used\n", (int)end_angle);
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
     }
     else
     {
         std::regex matcher( R"(\s*from\s+([+-]?\d+)deg\s*)", std::regex_constants::icase);
         if( std::regex_match( gradient_part.cbegin(), gradient_part.cend(), match_results, matcher))
             start_angle = std::stoi( match_results[ 1].str());
     }
   }
   else if( !( color_rule.scolor.changed() && color_rule.ecolor.changed()))
   {
     if( color_rule.scolor.changed())
       fprintf( stderr, "One color is not enough to define a conic gradient\n");
     else
       fprintf( stderr, "Conic gradient specified without colors\n");
     exit( EXIT_FAILURE);
   }
   
   auto& cstops = actual_gradient.color_variations;
   if( color_rule.scolor.changed())
     cstops.insert( cstops.begin(), { color_rule.scolor, SIZE_MAX});
   if( color_rule.ecolor.changed())
     cstops.emplace_back( color_rule.ecolor, SIZE_MAX);
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
        ColorRule ccolor{};
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
        auto easing_enabled = false;
        while( *rule)
        {
            if( *rule == '[')
            {
                prev = rule;
                if( *++rule == '(')
                {
                    ccolor.start.x = getNumber( ++rule);
                    if( ltrim( rule) && *rule++ != ',')
                    {
                        fprintf( stderr, "Expected a `,` near -> %s", rule);
                        exit( EXIT_FAILURE);
                    }
                    ccolor.start.y = getNumber( rule);
                    if( ltrim( rule) && *rule++ != ')')
                    {
                        fprintf( stderr, "Expected a `)` near -> %s", rule);
                        exit( EXIT_FAILURE);
                    }
                }
                else
                    ccolor.start.x = getNumber( rule);
                if( *rule != '\0')
                {
                    if( *rule == '.' && *( rule + 1) == '.')
                    {
                        rule += 2;
                        if( isdigit( *rule))
                            ccolor.end.x = getNumber( rule);
                        else if( *rule == '(')
                        {
                            ++rule;
                            ccolor.end.x = getNumber( rule);
                            if( ltrim( rule) && *rule++ != ',')
                            {
                                fprintf( stderr, "Expected a `,` near -> %s", rule);
                                exit( EXIT_FAILURE);
                            }
                            ccolor.end.y = getNumber( rule);
                            if( ltrim( rule) && *rule++ != ')')
                            {
                                fprintf( stderr, "Expected a `)` near -> %s", rule);
                                exit( EXIT_FAILURE);
                            }
                        }

                       if( ( ccolor.end.x != -1 && ccolor.end.x <= ccolor.start.x)
                           || ( ccolor.end.y != -1 && ccolor.end.y <= ccolor.start.y))
                        {
                            fprintf( stderr, "Color start and end should not overlap -> %s", prev);
                            exit( EXIT_FAILURE);
                        }
                        easing_enabled = true;
                    }
                    else if( *rule == '.')
                    {
                        fprintf( stderr, "Expected `.` before end range -> %s", rule + 1);
                        exit( EXIT_FAILURE);
                    }
                }
                else
                {
                    fprintf( stderr, "Incomplete color specification");
                    exit( EXIT_FAILURE);
                }

                if( *rule == ':')
                {
                    ccolor.font_size_b = getNumber( ++rule);
                    bool mid = false;
                    if( *rule == '-')
                    {
                        ccolor.font_size_m = getNumber( ++rule);
                        mid = true;
                    }
                    if( *rule == '-')
                        ccolor.font_size_e = getNumber( ++rule);
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
                        ccolor.scolor = mixColor( ++rule, bkroot);
                    }
                    else if( *rule != '-')
                    {
                        prev = rule;
                        ccolor.scolor = extractColor( rule,  bkroot);
                    }

                    if( ltrim( rule) && *rule == '-')
                    {
                      	auto gradient_is_next = compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r');
                        if( !easing_enabled && gradient_is_next)
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
							  ccolor.ecolor = mixColor( ++rule, bkroot);
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
                              generateConicGradient( ++rule, ccolor, bkroot)});
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

CompositionRule::CompositionModel selectCompositionModel( std::string_view given)
{
   size_t view_size = given.size();
   if( !view_size)
     return CompositionRule::CompositionModel::NotApplicable;
   
   static const std::unordered_map<std::string_view, CompositionRule::CompositionModel> possibilities
   {
	   { COPY			 , CompositionRule::CompositionModel::Copy},
	   { DESTINATION_ATOP, CompositionRule::CompositionModel::DestinationAtop},
	   { DESTINATION_IN  , CompositionRule::CompositionModel::DestinationIn},
	   { DESTINATION_OVER, CompositionRule::CompositionModel::DestinationOver},
	   { DESTINATION_OUT , CompositionRule::CompositionModel::DestinationOut},
	   { LIGHTER 		 , CompositionRule::CompositionModel::Lighter},
	   { SOURCE_ATOP 	 , CompositionRule::CompositionModel::SourceAtop},
	   { SOURCE_IN 		 , CompositionRule::CompositionModel::SourceIn},
	   { SOURCE_OVER 	 , CompositionRule::CompositionModel::SourceOver},
	   { SOURCE_OUT 	 , CompositionRule::CompositionModel::SourceOut},
	   { XOR 			 , CompositionRule::CompositionModel::Xor},
   };
   
   std::string clone( view_size, 0);
   std::transform( given.cbegin(), given.cend(), clone.begin(), tolower);
   auto index = possibilities.find( clone);
   if( index == possibilities.cend())
     return CompositionRule::CompositionModel::NotApplicable;
   
   return index->second;
}

CompositionRule parseCompositionRule( std::string_view rule)
{
  std::cmatch match_results;
  //Match any of the following:
  //1. from 30deg, mode=source-over
  //2. from 30deg, at .5, 0.45, mode=source-over
  //3. mode=source-over or mode=lighter
  std::regex matcher( R"(^(?:from\s+([+-]?\d{1,3})deg(?:\s+at\s+)"
					  R"(([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?),\s*([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?))?,\s*)?)"
                      R"(mode=([a-zA-Z]+(?:-[a-zA-Z]+)?)$)");
  if( std::regex_match( rule.cbegin(), rule.cend(), match_results, matcher))
  {
    auto x_origin = match_results[ 2].str(),
         y_origin = match_results[ 3].str(),
         angle    = match_results[ 1].str();
	return {
		.model = selectCompositionModel( match_results[ 4].str()),
		.position = Vec2D(x_origin.size() ? std::stof(x_origin, nullptr) : 0.f,
						 y_origin.size() ? std::stof( y_origin, nullptr) : 0.f),
		.angle = angle.size() ? std::stoi( angle, nullptr, 10) : 0
	};
  }
  return {};
}

void requestFontList()
{
    if( !FcInit())
        return;

    auto locale_name = std::locale("").name();
    if( size_t idx = locale_name.find( '_'); idx != std::string::npos)
        locale_name = locale_name.substr( 0, idx).insert( 0, ":lang=");
    PropertyManager<FcConfig *> config( FcConfigGetCurrent(), FcConfigDestroy);
    FcConfigSetRescanInterval( config.get(), 0);
    PropertyManager<FcPattern *> pattern( FcNameParse((FcChar8 *)locale_name.c_str()), FcPatternDestroy);
    PropertyManager<FcObjectSet *> font_object_set( FcObjectSetBuild ( FC_OUTLINE, FC_STYLE, FC_FAMILY, nullptr),
                                                    FcObjectSetDestroy);
    PropertyManager<FcFontSet *> font_set( FcFontList( config.get(), pattern.get(), font_object_set.get()),
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
          PropertyManager<const char *> font_manager( ( const char *)FcNameUnparse( font_set->fonts[ i]),
              []( auto *p) { free( ( FcChar8 *)p); });
            std::string_view font( font_manager.get());
            if( font.empty())
                continue;
            auto parts = partition( font, ":");
            if( parts.size() != 3)
                continue;
            std::smatch sm;
            std::regex key( ".*(?:(normal)|(bold)|(italic)|(regular)).*", std::regex_constants::icase);
            if( std::regex_search( parts[ 1].cbegin(), parts[ 1].cend(), sm, key))
            {
                std::string style;
                if( sm[ 1].matched || sm[ 4].matched) style.append( "Normal");
                if( sm[ 2].matched) style.append( style.empty() ? "Bold" : ", Bold");
                if( sm[ 3].matched) style.append( style.empty() ? "Italic" : ", Italic");
                std::cout << "Family: " << parts[ 0] << ", style(s): " << style <<'\n';
            }
        }
        while( ++i < font_set->nfont);
    }
    FcFini();
}

std::string getFontFile( std::string_view font)
{
    if( !FcInit())
        return {};

    const char *styles[] = { nullptr, "normal", "bold", "italic", "regular"},
               *style    = styles[ 1];
    std::cmatch cm;
    std::regex key( ".*(?:(normal)|(bold)|(italic)|(regular)).*", std::regex_constants::icase);
    if( std::regex_search( font.cbegin(), font.cend(), cm, key))
    {
        auto trim_pos = std::min( { cm.position( 1), cm.position( 2), cm.position( 3), cm.position( 4)});
        style = styles[ cm[ 1].matched * 1];
        style = styles[ cm[ 2].matched * 2];
        style = styles[ cm[ 3].matched * 3];
        style = styles[ cm[ 4].matched * 4];
        if( style == nullptr)
            style = styles[ 1];
        while( trim_pos > 0 && font[ --trim_pos] == ' ')
            ;
        font.remove_suffix( font.size() - trim_pos - 1);
    }

    PropertyManager<FcConfig *> config( FcConfigGetCurrent(), FcConfigDestroy);
    FcConfigSetRescanInterval( config.get(), 0);
    auto parts = partition( font, R"(\s*,\s*)");
    if( parts.empty())
        parts.emplace_back( font);
    for( auto& part : parts)
    {
        PropertyManager<FcPattern *> pattern( FcPatternCreate(), FcPatternDestroy);
        FcPatternAddString( pattern.get(), FC_FAMILY, ( const FcChar8 *)part.data());
        PropertyManager<FcObjectSet *> font_object_set( FcObjectSetBuild ( FC_FILE, nullptr),
                                                        FcObjectSetDestroy);
        PropertyManager<FcFontSet *> font_set( FcFontList( config.get(), pattern.get(), font_object_set.get()),
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
                PropertyManager<const char *> font_manager( ( const char *)FcNameUnparse( font_set->fonts[ 0]),
                                                            []( auto *p) { free( ( FcChar8 *)p); });
                if( font_manager.get() == nullptr)
                    continue;
                auto *filename = strchr( font_manager.get(), '/');
                if( strcasestr( filename, style) == 0)
                    return filename;
            }
            while( ++i < font_set->nfont);
        }
    }

    return {};
}

int main( int ac, char *av[])
{
   ApplicationHyperparameters business_rules;
   
  auto& kdroot = business_rules.kdroot;
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

  auto& bkroot = business_rules.bkroot;

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
     * Schema: av[ 0] [--list-fonts|--font-file=FILE [--font-size=NUM] [--color-rule=RULE]
     * [--drawing-character=CHAR] [--output FILE]] text
     */
  
  const char *font_profile{ "../common-fonts/Times New Roman/times new roman.ttf"},
             *justification{ nullptr},
             *image_quality{ nullptr},
             *line_height{ nullptr},
             *font_size{ "10"},
             *background_color{ nullptr},
             *word,
             *program{ *av};

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
            business_rules.as_image = true;
        else if( strcmp( directive, "output") == 0 && ac > 0)
        {
            ac -= 1;
            business_rules.src_filename = *++av;
            business_rules.out_format = isJPEG( business_rules.src_filename) ?
                                        OutputFormat::JPEG : business_rules.out_format;
        }
        else if( strstr( directive, "font-profile") != nullptr)
            selection = &font_profile;
        else if( strstr( directive, "color-rule") != nullptr)
            selection = &business_rules.color_rule;
		else if( strstr( directive, "composition-rule") != nullptr)
		  selection = &business_rules.composition_rule;
        else if( strstr( directive, "font-size") != nullptr)
            selection = &font_size;
        else if( strstr( directive, "background-color") != nullptr)
          selection = &background_color;
        else if( strstr( directive, "drawing-character") != nullptr)
            selection = &business_rules.raster_glyph;
        else if( strstr( directive, "composition-image") != nullptr)
          	selection = &business_rules.dest_filename;
        else if( strstr( directive, "line-height") != nullptr)
            selection = &line_height;
        else if( strstr( directive, "justify") != nullptr)
            selection = &justification;
        else if( strstr( directive, "quality-index") != nullptr)
            selection = &image_quality;

        if( selection != nullptr && ( index = strchr( directive, '=')) != nullptr)
            *selection = index + 1;
        else if( selection != nullptr)
        {
            ac = -1;
            break;
        }
    }

    if( line_height != nullptr)
        business_rules.line_height = strtof( line_height, nullptr);
    if( justification != nullptr)
    {
        if( std::cmatch cm; std::regex_match( justification, justification + strlen( justification), cm,
            std::regex( R"(^(left)|(right)|(center)$)")), std::regex_constants::icase)
        {
            business_rules.j_mode = cm[ 1].matched ? Justification::Left
                                                   : cm[ 2].matched ? Justification::Right : Justification::Center;
        }
    }

    if( image_quality != nullptr)
        business_rules.image_quality = strtol( image_quality, nullptr, 10);
    
    if( background_color)
      business_rules.background_color = extractColor( background_color, business_rules.bkroot.get());

    business_rules.font_size = strtol( font_size, nullptr, 10);

    if( ac == 1)
        word = *av;
    else
    {
        const char *start = strrchr( program, '/'),
                   *name = start != nullptr ? start + 1 : program,
                   *arguments[] = {
                        "--list-fonts",
                        "--font-profile=FILE|Family [Normal|Regular|Bold|Italic]",
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
                *arguments, *( arguments + 1), *( arguments + 2), *( arguments + 3),
                *( arguments + 4), *( arguments + 5), *( arguments + 6));

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

    if( strrchr( font_profile, '.') == nullptr)
    {
        auto file = getFontFile( font_profile);
        if( file.empty())
        {
            fprintf( stderr, "Unable to parse font");
            exit( EXIT_FAILURE);
        }
        error = FT_New_Face( library.get(), file.c_str(), 0, &face.get());
    }
    else
        error = FT_New_Face(library.get(), font_profile, 0, &face.get());

    if( error != 0)
    {
        fprintf( stderr, "Font file is invalid!");
        exit( EXIT_FAILURE);
    }

    auto ext_pos = strrchr( word, '.');
    if( ext_pos != nullptr && strcasecmp( ext_pos + 1, "txt") == 0)
    {
        std::wifstream handle( word, std::ios::in | std::ios::ate);
        if( handle.good())
        {
            std::wstring content( handle.tellg(), L' ');
            handle.seekg( 0);
            handle.read( &content[ 0], content.size());
            render( library.get(), face.get(), content, business_rules);
            handle.close();
        }
    }
    else
        render( library.get(), face.get(), toWString( word), business_rules);
  
  return 0;
}