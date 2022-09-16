/***************************************************************************/
/*                                                                         */
/*  copywrite.cpp                                                          */
/*                                                                         */
/*    A terminal based text customization utility					       */
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
#include <cassert>
#include <cstdint>
#include <iomanip>
#include <numeric>
#include "colors_defs.hpp"
#include "easing_defs.hpp"
#include "blend_defs.hpp"
#include "geo_vector.hpp"
#include "composition_defs.hpp"
#include "timer.hpp"

#define ALLOWANCE                      2
#define MAX(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) < (y))))
#define MIN(x, y)                      ((x) ^ (((x) ^ (y)) & -((x) > (y))))
#define ZERO( fl)                      ( std::abs( fl) <= EPSILON)
#define EQUAL( al, bl)                 ZERO( ( al) - ( bl))
#define UNSET( x)                      (( x) == -1)
#define ACCESSIBLE( ptr)               (( ptr) != nullptr)
#define RED( color)                    (( uint8_t)(( color) >> 24u))
#define GREEN( color)                  (( uint8_t)((( color) >> 16u) & 0xFFu))
#define BLUE( color)                   (( uint8_t)((( color) >> 8u) & 0xFFu))
#define ALPHA( color)                  (( uint8_t)(( color) & 0xFFu))
#define RGBA( red, green, blue, alpha) (((( uint32_t)(( uint8_t)red))  << 24u) |\
                                       ((( uint32_t)(( uint8_t)green)) << 16u) |\
                                       ((( uint32_t)(( uint8_t)blue))  << 8u) | (( uint8_t)alpha))
#define RGB( red, green, blue)         RGBA( red, green, blue, 0u)
#define LUMIN( color)                  (( uint8_t)(( color) >> 8u)  & 0x7Fu)
#define SAT( color)                    (( uint8_t)(( color) >> 15u) & 0x7Fu)
#define HUE( color)                    (( uint16_t)(( color) >> 22u))
#define HSLA( hue, sat, lum, alpha)    ((( uint32_t)( hue)) << 22u |\
                                       (( uint32_t)( sat))  << 15u |\
                                       (( uint32_t)( lum))  << 8u | alpha)
#define SCALE_RGB( color, scale)       RGB( RED( color) * ( scale), GREEN( color) * ( scale), BLUE( color) * ( scale))
#define XYZ_SCALE                      775
#define RGB_SCALE                      255
#define DEG_MAX                        360
#define HALF_RGB_SCALE                 128
#define DEG_SCALE					   180.f / M_PI
#define ENUM_CAST( idx)					( static_cast<uint8_t>( idx))
#define MODEL_ENUM( mode)			   CompositionRule::CompositionModel::mode
#define BLEND_ENUM( mode)              CompositionRule::BlendModel::mode
/*
 * The composition table is made up of 2 bit field
 * per flag for CompositionModel.
 * E.g Copy has an index of 1, and a value of 10.
 * The lower zero from the right means that the size of the canvas should be maximum or minimum( 1 or 0).
 * The rightmost zero indicates that if the first bit is maximum, then the size should be that
 * of the source or destination( 1 or 0).
 * The fields are:
 * Clip 		   = 00,
 * Copy            = 01
 * DestinationAtop = 02,
 * DestinationIn   = 03,
 * DestinationOver = 04,
 * DestinationOut  = 05,
 * Lighter         = 06,
 * NotApplicable   = 07,
 * SourceAtop      = 08,
 * SourceIn        = 09,
 * SourceOver      = 10,
 * SourceOut       = 11,
 * Xor             = 12
 * COMPOSITION_TABLE LAYOUT:
 * 01 10 01 00 00 00 01 00 01 10 10 10 10
 * 12 11 10 09 08 07 06 05 04 03 02 01 00
 * 01 1001 0000 0001 0001 1010 1010
 */
#define COMPOSITION_TABLE		       0x19011AAU
#define COMPOSITON_SIZE( idx)          (( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U)) & 1U)
#define COMPOSITION_SIDE( idx)		   (( COMPOSITION_TABLE >> ( ENUM_CAST( idx) * 2U + 1U)) & 1U)
#define LOW_BYTE( value)               (( uint8_t)(( value) & 0xFFu))
#define HIGH_BYTE( value)              (( uint8_t)(( value) >> 8u))
#define MAKE_WORD( high, low)          (( uint16_t)( high) << 8u | ( low))
#define LOW_DWORD( value)              (( uint32_t)(( value) & 0xFFFFFFFFu))
#define HIGH_DWORD( value)             (( uint32_t)(( value) >> 32u))
#define MAKE_QWORD( high, low)         (( uint64_t)( high) << 32u | ( low))

#define FOUND_STRING( xpr)             (( xpr) == 0)

#define STR_LIGHTER                    "lighter"
#define STR_DARKER                     "darker"

#define GLOBAL_TIME_ID                 0

void spansCallback(int y, int count, const FT_Span *spans, void *user)
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
        auto line_height = 0, max_shadow_y = 0, max_row_bounce = 0;
        for( size_t i = 0, t_len = text_rows[ i].length(); i < t_len; ++i)
        {
            auto c_char = text_rows[ j][ i];
            auto best = general;
            /*
             * Select color to apply on glyph
             */
            for( auto& each: rules)
            {
                if( UNSET( each->end.x) && UNSET( each->end.y))
                {
                    if( i + 1 >= each->start.x && j + 1 >= each->start.y)
                        best = each;
                    continue;
                }

               if(( UNSET( each->end.x) && j + 1 >= each->start.y && j + 1 <= each->end.y) ||
                  ( UNSET( each->end.y) && i + 1 >= each->start.x && i + 1 <= each->end.x))
               {
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
                else if( !UNSET( best->font_size_m) && !UNSET( best->font_size_e))
                    font_size = round( +2.0 * best->font_size_b * ( fraction - .5) * ( fraction - 1.)
                                       -4.0 * best->font_size_m * fraction * ( fraction - 1.)
                                       +2.0 * best->font_size_e * fraction * ( fraction - .5));
            }

            line_height = font_size * guide.line_height;

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
                raster.is_graph = true;
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
                        .width  = ( int)slot->bitmap.width,
                        .height = ( int)slot->bitmap.rows};
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
                max_row_box.x += ( i + 1 == t_len ? rect.width() : raster.advance.x) + std::abs( best->shadow.x);
                max_row_box.y = std::max<long>( max_row_box.y, height);
                max_shadow_y  = std::max<long>( max_shadow_y, std::abs( best->shadow.y));
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
                int default_width  = ( slot->metrics.horiAdvance >> 6) + guide.thickness * 2;
                max_row_box.x += default_width;
                raster.advance.x = default_width;
            }
            max_row_bounce = std::max<int>( max_row_bounce, best->max_bounce);

            if( best->color_easing_fn && best != general)
            {
                best->gradient->width = std::max( max_row_box.x, box_size.x);
                best->gradient->height = box_size.y + ( max_row_box.y == 0 ? ( face->glyph->metrics.vertAdvance >> 6)
                                         + guide.thickness * 2
                                         : max_ascent + max_descent + max_shadow_y) + line_height * ( j + 1 != t_row);
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
                raster.advance.y = default_height;
            }
            max_ascent = (int)default_height;
        }
        max_row_box.y = max_ascent + max_descent + max_shadow_y + max_row_bounce;
        row_details[ j].width = (int)max_row_box.x + guide.pad.right + guide.pad.left - max_shadow_y;
        row_details[ j].max_shadow_y = max_shadow_y;
        if( max_row_box.y <= 0)
            continue;
        box_size.x = std::max( max_row_box.x, box_size.x); // The width of the largest row
        auto row_line_height = line_height * ( j + 1 != t_row); // The spacing between glyphs
        box_size.y += max_row_box.y + row_line_height;
        row_details[ j].v_disp += box_size.y - row_line_height - max_shadow_y; // Offset at which each row should start
    }

    std::clog << "Generated glyphs after: " << Timer::yield( GLOBAL_TIME_ID) << "s\n";

    box_size.x += guide.pad.left + guide.pad.right;
    box_size.y += guide.pad.top  + guide.pad.bottom;
    std::shared_ptr<uint32_t> pixel(( uint32_t *)malloc( sizeof( uint32_t) * box_size.x * box_size.y),
                                     []( auto *p){ free( p);});
    std::fill_n( pixel.get(), box_size.x * box_size.y, guide.background_color.cast());
    auto buffer = FrameBuffer<uint32_t>{ pixel, static_cast<int32_t>( box_size.x), static_cast<int32_t>( box_size.y)};

    printf( "Started Shadow %lus\n", Timer::yield( GLOBAL_TIME_ID));
    drawShadow( rasters, row_details, buffer, guide);
    printf( "Finished Shadow %lus\n", Timer::yield( GLOBAL_TIME_ID));
    printf( "Started Text Placement %lus\n", Timer::yield( GLOBAL_TIME_ID));
    draw( rasters, row_details, buffer, guide);
    printf( "Ended Text Placement %lus\n", Timer::yield( GLOBAL_TIME_ID));

    std::clog << "Writing to file after: " << Timer::yield( GLOBAL_TIME_ID) << "s\n";
    PropertyManager<FILE *> destination( stdout, []( FILE *maybe_destructible)
    {
        if( maybe_destructible != stdout)
            fclose( maybe_destructible);
        maybe_destructible = nullptr;
    });

#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)

    if( guide.composition_rule && guide.dest_filename)
        composite( guide, buffer);
    else
    {
        if( guide.interpolation[ 0] && guide.interpolation[ 1])
            resize( buffer, guide.interpolation);
        if( guide.as_image && guide.src_filename)
    #if defined( JPG_SUPPORTED) && defined( PNG_SUPPORTED)
            ( guide.out_format == OutputFormat::JPEG ? writeJPEG : writePNG)
    #elif defined( JPG_SUPPORTED)
            writeJPEG
    #else
            writePNG
    #endif
            ( guide.src_filename, buffer, guide.image_quality);
        else
#endif
        {
            if( guide.src_filename != nullptr)
            {
                auto *handle = fopen( guide.src_filename, "wb");
                if( handle)
                    destination.get() = handle;
            }
            write( buffer, guide.raster_glyph, destination.get(), guide.kdroot.get());
        }
#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)
    }
#endif
}

std::vector<float> makeEmboss( int width)
{
    auto size = width * width;
    auto mid = size / 2;
    std::vector<float> emboss( size);
    emboss[ 0] = 1.8;
//    emboss[ 1] = 1.5;
//    emboss[ 1 * width] = 1.5;
//    emboss[ 1 * width + 1] = 1.5;
//    emboss[ ( width - 2) * width + width - 2] = -1.1;
//    emboss[ ( width - 2) * width + width - 1] = -1.1;
//    emboss[ size - 2] = -1.1;
    emboss[ size - 1] = -1.1;

    for ( int j = 0; j < width; ++j)
    {
        for ( int i = 0; i < width; ++i)
            printf( "%f ", emboss[ j * width + i]);
        putchar( '\n');
    }

    return emboss;
}

std::vector<float> makeGaussian( float radius)
{
    constexpr auto width = 13;
    constexpr auto size = width * width;
    constexpr auto mid = size / 2;
    std::vector<float> result( size);
    auto scale = static_cast<float>( 1.f / ( radius * std::sqrt( 2.f * M_PI)));
    float sum = 0;
    for( short i = 0; i < size; ++i)
    {
        result[ i] = scale * ( 1.f / std::exp((( i - mid) * ( i - mid)) / ( 2.f * radius * radius)));
        sum += result[ i];
    }
    return result;
}

template <typename Tp, typename = std::enable_if_t<std::is_integral_v<Tp>>>
void applyEffect( FrameBuffer<Tp> &frame, SpecialEffect effect, void *extras = nullptr)
{
    if( ENUM_CAST( effect) < ENUM_CAST( SpecialEffect::RequiresKernelSentinel))
    {
        auto kernel = *( const std::vector<float> *)extras;
        int n_channel = std::max( frame.n_channel, 1);
        auto width   = frame.width,
                height  = frame.height;
        int k_size  = kernel.size(),
            k_width = std::sqrt( k_size),
            mid     = k_width / 2;
        std::shared_ptr<Tp> dest(( Tp *)calloc( width * height  * n_channel, sizeof( Tp)), []( auto *p){ free( p);});
        auto buffer   = frame.buffer.get(),
             d_buffer = dest.get();
        for( int j = 0; j < height; ++j)
        {
            for( int i = 0; i < width; ++i)
            {
                double red{}, green{}, blue{}, alpha{};
                for( int k = 0; k < k_size; ++k)
                {
                    int row = ( k_width - k / k_width + 1) - mid + j,
                        col = ( k_width - k % k_width - 1) - mid + i;
                    if( row >= 0 && row < height && col >= 0 && col < width)
                    {
                        size_t index = row * width * n_channel + col * n_channel;
                        if constexpr ( sizeof( Tp) == 4)
                        {
                            auto pixel = buffer[ index];
                            red   += kernel[ k] * RED( pixel);
                            green += kernel[ k] * GREEN( pixel);
                            blue  += kernel[ k] * BLUE( pixel);
                            alpha += kernel[ k] * ALPHA( pixel);
                        }
                        else
                        {
                            red   += kernel[ k] * buffer[ index];
                            green += kernel[ k] * buffer[ index + 1];
                            blue  += kernel[ k] * buffer[ index + 2];
                            alpha += kernel[ k] * buffer[ index + 3];
                        }
                    }
                }
                red = clamp( red, 0, RGB_SCALE);
                green = clamp( green, 0, RGB_SCALE);
                blue = clamp( blue, 0, RGB_SCALE);
                alpha = clamp( alpha, 0, RGB_SCALE);
                if constexpr ( sizeof( Tp) == 4)
                    d_buffer[ j * width + i] =  RGBA( red, green, blue, ALPHA( buffer[ j * width + i]));
                else
                {
                    auto index = j * width * n_channel + i * n_channel;
                    d_buffer[ index]    = red;
                    d_buffer[ index + 1] = green;
                    d_buffer[ index + 2] = blue;
                    d_buffer[ index + 3] = ALPHA( buffer[ index + 3]);
                }
            }
        }
        frame.buffer = dest;
    }
    else if( effect == SpecialEffect::GrayScale || effect == SpecialEffect::Grainy)
    {
        auto buffer = frame.buffer.get();
        auto size = frame.width * frame.height;
        constexpr auto color_change_freq = 8;
        for( size_t i = 0; i < size; ++i)
        {
            if constexpr( sizeof( Tp) == 4)
            {
                if( effect == SpecialEffect::Grainy && rand() % 4 == 0)
                {
                    buffer[ i] = rand() % color_change_freq == 0 ? RGBA( 0x9a, 0x9a, 0x90, ALPHA( buffer[ i]))
                                                                 : RGBA( 0xa9, 0xa9, 0x90, ALPHA( buffer[ i]));
                }
                else if( effect == SpecialEffect::GrayScale)
                {
                    auto avg = .2126 * RED( buffer[ i]) + .7152 * GREEN( buffer[ i]) + .0722 * BLUE( buffer[ i]);
                    buffer[ i] = RGBA( avg, avg, avg, ALPHA( buffer[ i]));
                }
            }
            else
            {
                auto index = i * frame.n_channel;
                if( effect == SpecialEffect::Grainy && rand() % 4 == 0)
                {
                    uint32_t color = rand() % color_change_freq == 0 ? RGBA( 0x9a, 0x9a, 0x90, ALPHA( buffer[ index]))
                                                                     : RGBA( 0xa9, 0xa9, 0x90, ALPHA( buffer[ index]));
                    buffer[ index + 0] = RED( color);
                    buffer[ index + 1] = GREEN( color);
                    buffer[ index + 2] = BLUE( color);
                    buffer[ index + 3] = ALPHA( color);
                }
                else if( effect == SpecialEffect::GrayScale)
                {
                    auto avg = .2126 * buffer[ index] + .7152 * buffer[ index + 1]
                               + .0722 * buffer[ index + 2];
                    buffer[ index + 0] = avg;
                    buffer[ index + 1] = avg;
                    buffer[ index + 2] = avg;
                    buffer[ index + 3] = buffer[ i + 3];
                }
            }
        }
    }
}

void drawShadow( const MonoGlyphs &rasters, RowDetails &row_details,
                 FrameBuffer<uint32_t> &frame, ApplicationHyperparameters &guide)
{
    auto n_glyphs = rasters.size();
    auto n_levels = row_details.size();
    for( auto& raster : rasters)
    {
        auto& [ main, outline] = raster.spans;
        std::clog << "RasterIDX: " << raster.level << ", n_points: " << outline.size() <<'\n';
        auto& rect  = raster.bbox;
        int width   = raster.is_graph ? raster.bbox.width()  : raster.advance.x,
            height  = raster.is_graph ? raster.bbox.height() : raster.advance.y;
        auto& match = raster.match;
        if( ZERO( match->shadow.x) && ZERO( match->shadow.y) && ZERO( match->shadow.z))
            continue;
        auto& row_detail = row_details[ raster.level];
        auto& pen = row_detail.pen;
        if( !raster.is_graph)
        {
            pen.x += width;
            pen.y += height;
            continue;
        }

        int bounce_disp = 0;
        if( match->max_bounce > 0 && match->font_easing_fn)
        {
            auto start = raster.pos.x - match->start.x,
                 end   = UNSET( match->end.x) ? std::max<int>( n_glyphs / n_levels - 1, 1)
                                              : match->end.x - match->start.x;
            auto fraction = match->font_easing_fn(( float)start / end);
            fraction = clamp( fraction, 0.f, 1.f);
            bounce_disp = match->max_bounce * fraction;
        }
        int v_offset = row_detail.v_disp - height - row_detail.max_descent - rect.ymin
                       + guide.pad.top + ( match->shadow.y >= 0) * match->shadow.y - bounce_disp,
            h_offset = (int)( ENUM_CAST( guide.j_mode) > 0)
                           * (( frame.width - row_detail.width + row_detail.max_shadow_y) / ENUM_CAST( guide.j_mode))
                           + guide.pad.left + ( match->shadow.x >= 0) * match->shadow.x;
        for ( auto& s: outline)
        {
            for ( int w = 0; w < s.width; ++w)
            {
                frame.buffer.get()[ (( v_offset + height - 1 - ( s.y - rect.ymin) + pen.y)
                                     * frame.width + s.x - rect.xmin + w + h_offset + pen.x)] = match->shadow_color;
            }
        }

        pen.x += raster.advance.x;
        pen.y += raster.advance.y;
    }
    // Reset head of all pens to zero.
    for( auto& raster : rasters)
        memset( &row_details[ raster.level].pen, 0, sizeof( FT_Vector));

//    auto kernel = makeGaussian( 8);
//    applyEffect( frame, SpecialEffect::GaussianBlur, &kernel);
}

void draw( const MonoGlyphs &rasters, RowDetails &row_details,
           FrameBuffer<uint32_t> &frame, ApplicationHyperparameters &guide)
{
    auto n_glyphs = rasters.size();
    auto n_levels = row_details.size();
    int acc_dim{};
    for( auto& raster : rasters)
     {
        auto& [ main, outline] = raster.spans;
        auto& rect  = raster.bbox;
        int width   = raster.is_graph ? raster.bbox.width()  : raster.advance.x,
            height  = raster.is_graph ? raster.bbox.height() : raster.advance.y;
        auto length = guide.ease_col ? height : width;
        auto& match = raster.match;
        auto& row_detail = row_details[ raster.level];
        auto& pen = row_detail.pen;
        if( !raster.is_graph)
        {
            /*
             * If this glyph is not a graph, make sure to keep the linear gradient
             * consistent.
             */
            if( match->soak && match->gradient->gradient_type == GradientType::Linear)
                acc_dim = guide.ease_col ? row_detail.v_disp - height : ( raster.pos.x != 1) * ( acc_dim + length);
            pen.x += width;
            pen.y += height;
            continue;
        }

        uint32_t inner_color   = LOW_DWORD( match->scolor),
                 outline_color = HIGH_DWORD( match->scolor);
        std::unique_ptr<uint64_t[]> row_colors;
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
             inner_color   = interpolateColor( LOW_DWORD( match->scolor), LOW_DWORD( match->ecolor), fraction);
             outline_color = interpolateColor( HIGH_DWORD( match->scolor), HIGH_DWORD( match->ecolor), fraction);
         }
         else
         {
             auto extent = guide.ease_col ? match->gradient->height : match->gradient->width;
             acc_dim = guide.ease_col ? row_detail.v_disp - height : ( raster.pos.x != 1) * ( acc_dim + length);
             row_colors.reset( new uint64_t[ length]);
             for( FT_Int i = 0; i < length; ++i)
             {
                 auto fraction = match->color_easing_fn(( float)( i + acc_dim) / extent);
                 inner_color   = interpolateColor( LOW_DWORD( match->scolor), LOW_DWORD( match->ecolor), fraction);
                 outline_color = interpolateColor( HIGH_DWORD( match->scolor), HIGH_DWORD( match->ecolor), fraction);
                 row_colors.get()[ i] = MAKE_QWORD( outline_color, inner_color);
             }
             /*
              * Add text advance gap to character to arrive at appropriate color easing value.
              */
             acc_dim += guide.ease_col ? raster.advance.y - height : raster.advance.x - width;
         }
        }
         /* v_offset: Aligns every row glyph to their respective baselines
         *  h_offset: The adjustment necessary to comply with `Justification::Right` and `Justification::Center`
         */
        int bounce_disp = 0;
        if( match->max_bounce > 0 && match->font_easing_fn)
        {
            auto start = raster.pos.x - match->start.x,
                 end   = UNSET( match->end.x) ? std::max<int>( n_glyphs / n_levels - 1, 1)
                                              : match->end.x - match->start.x;
            auto fraction = match->font_easing_fn(( float)start / end);
            fraction = clamp( fraction, 0.f, 1.f);
            bounce_disp = match->max_bounce * fraction;
        }
        int v_offset = row_detail.v_disp - height - row_detail.max_descent - rect.ymin + guide.pad.top - bounce_disp,
            h_offset = (int)( ENUM_CAST( guide.j_mode) > 0)
                           * (( frame.width - row_detail.width) / ENUM_CAST( guide.j_mode))
                           + guide.pad.left;
         for ( auto& s: outline)
        {
            for ( int w = 0; w < s.width; ++w)
            {
                auto& dest = frame.buffer.get()[(( v_offset + height - 1 - ( s.y - rect.ymin) + pen.y)
                                                 * frame.width + s.x - rect.xmin + w + h_offset + pen.x)];
                int i = s.x - rect.xmin + w,
                    j = s.y - rect.ymin,
                    color_index = guide.ease_col ? height - 1 - j : i;
                if( guide.thickness > 0)
                {
                    if( match->color_easing_fn)
                    {
                        // Paint outline
                        if( match->gradient->gradient_type == GradientType::Linear)
                            dest = match->soak ? HIGH_DWORD( row_colors.get()[ color_index]) : outline_color;
                        else
                            dest = easeColor( raster, row_detail, Vec2D<int>( n_glyphs / n_levels, n_levels),
                                              { i, height - 1 - j}, pen,
                                              { HIGH_DWORD( match->scolor), HIGH_DWORD( match->ecolor)}, true);
                    }
                    else
                        dest = outline_color;
                }
                if( i >= guide.thickness && i < ( width - guide.thickness)) // Paint fill
                {
                    int m_index = ( height - 1 - j - guide.thickness) * main.width + ( i - guide.thickness);
                    if( m_index >= 0 && m_index < main.width * main.height)
                    {
                        if ( main.buffer.get()[ m_index] > 0)
                        {
                            if ( match->color_easing_fn)
                            {
                                if ( match->gradient->gradient_type == GradientType::Linear)
                                    dest = match->soak ? LOW_DWORD( row_colors.get()[ color_index]) : inner_color;
                                else
                                    dest = easeColor( raster, row_detail, Vec2D<int>( n_glyphs / n_levels, n_levels),
                                                      { i, height - 1 - j}, pen,
                                                      { LOW_DWORD( match->scolor), LOW_DWORD( match->ecolor)}, false);
                            }
                            else
                                dest = inner_color;
                        }
                    }
                }
            }
        }

        pen.x += raster.advance.x;
        pen.y += raster.advance.y;
    }
}

static size_t byteCount( uint8_t c)
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

template <typename T>
std::pair<std::vector<std::basic_string<T>>, int> expand( std::basic_string_view<T> provision, Justification mode)
{
    std::vector<std::basic_string<T>> parts;
    int j = 0, max_length = 0, prev = '\0';
    for( size_t i = 0; i < provision.length(); ++i)
    {
        if( provision[ i] == '\n')
        {
            auto length = i - j - ( prev == '\r');
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
        line.insert( 0, std::basic_string<T>( left, ' '));
        line.append( std::basic_string<T>( right, ' '));
    }
    return { parts, max_length};
}

uint32_t easeColor( const MonoGlyph &raster, const RowDetail &row_detail, Vec2D<int> size,
                    Vec2D<int> pos, FT_Vector pen, Vec2D<uint32_t> color_shift, bool is_outline)
{
    auto& match = raster.match;
    auto glyph_width = raster.bbox.width();
    auto glyph_height = raster.bbox.height();
    auto color = is_outline ? HIGH_DWORD( match->scolor) : LOW_DWORD( match->scolor);
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
        auto spread = props.z * std::sqrt( .25 * scale * scale + .25);
        auto left   = spread - .2 < 0 ? spread : spread - .2;
        auto right  = spread - .2 < 0 ? spread : spread + .2;
        auto c = smoothstep( left, right, d); // Spread the circle around to form a smooth gradient.
        color = colorLerp( color_shift.y, color_shift.x, match->color_easing_fn( c));
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
        auto red   = std::pair{ 0xff0000ff, 0};
        auto dummy = std::pair{ 0x000000ff, 0};
        if( !stops.empty())
        {
            auto cur_stop = *stops.cbegin(), next_stop = cur_stop;
            for( size_t i = 0, n_stops = stops.size() - 1; i < n_stops; ++i)
            {
                auto &stop  = stops[ i],
                     &other = stops[ i + 1];
                if( angle >= stop.second && angle < other.second)
                {
                    cur_stop = stop;
                    next_stop = other;
                    break;
                }
            }

            if( ( cur_stop.first & ~0xFFuLL) == ( next_stop.first & ~0xFFuLL))
                color = is_outline ? HIGH_DWORD( cur_stop.first) : LOW_DWORD( cur_stop.first);
            else
            {
                if( cur_stop.second == next_stop.second)
                    return is_outline ? HIGH_DWORD( cur_stop.first) : LOW_DWORD( cur_stop.first);
                auto fraction = ( angle - cur_stop.second)
                        / std::abs(( float)( next_stop.second - cur_stop.second));
                if( match->color_easing_fn)
                    fraction = match->color_easing_fn( fraction);
                return is_outline ? colorLerp( HIGH_DWORD( cur_stop.first), HIGH_DWORD( next_stop.first), fraction)
                                  : colorLerp( LOW_DWORD( cur_stop.first), LOW_DWORD( next_stop.first), fraction);
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
    uint32_t rgb_min, rgb_max, hsv{},
             r = RED( rgb),
             g = GREEN( rgb),
             b = BLUE( rgb),
             a = ALPHA( rgb);

    rgb_max = MAX( r, MAX( g, b));
    rgb_min = MIN( r, MIN( g, b));

    hsv = rgb_max << 8u;
    if ( hsv == 0)
        return hsv | a;

    hsv |= ( 0xFFu * (( int32_t)( rgb_max - rgb_min)) / ( hsv >> 8u)) << 16u;
    if (( hsv >> 16u & 0xFFu) == 0)
        return hsv | a;

    if( rgb_max == r)
        hsv |= ( uint32_t)(( uint8_t)( 0 + 43 * ( int32_t)( g - b) / ( int32_t)( rgb_max - rgb_min))) << 24u;
    else if( rgb_max == g)
        hsv |= ( uint32_t)(( uint8_t)(  85 + 43 * ( int32_t)( b - r) / ( int32_t)( rgb_max - rgb_min))) << 24u;
    else
        hsv |= ( uint32_t)(( uint8_t)( 171 + 43 * ( int32_t)( r - g) / ( int32_t)( rgb_max - rgb_min))) << 24u;

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

#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)

uint32_t rgbaToHsla( uint32_t rgba)
{
    auto red   = RED( rgba),
         green = GREEN( rgba),
         blue  = BLUE( rgba);
    auto c_max = std::max( std::max( red, green), blue),
         c_min = std::min( std::min( red, green), blue),
         delta = ( uint8_t)( c_max - c_min);

    float hue {}, lum = ( float)( c_max + c_min) / ( 2 * RGB_SCALE),
                  sat = delta == 0 ? 0 : delta / (( 1 - std::abs( 2 * lum - 1)) * RGB_SCALE);
    if( c_max == red)
    {
        auto segment = ( float)( green - blue) / delta,
             shift   = 0.0f;
             if( segment < 0)
                 shift = 360.f / 60.f;
             hue = segment + shift;
    }
    else if( c_max == green)
    {
        auto segment = ( float)( blue - red) / delta,
             shift = 120.f / 60.f;
        hue = segment + shift;
    }
    else if( c_max == blue)
    {
        auto segment = ( float)( red - green) / delta,
             shift = 240.f / 60.f;
        hue = segment + shift;
    }

    return HSLA( hue * 60, sat * 100, lum * 100, ALPHA( rgba));
}

float hueToSpace( float p, float q, float t)
{
    if( t < 0)
        t += 1;
    if( t > 1)
        t -= 1;
    if( t < 1. / 6)
        return p + ( q - p) * 6 * t;
    if( t < 1. / 2)
        return q;
    if( t < 2. / 3)
        return p + ( q - p) * ( 2. / 3 - t) * 6;

    return p;
}

uint32_t hslaToRgba( uint32_t hsla)
{
    float h = HUE( hsla) / 360.f,
          s = SAT( hsla) / 100.f,
          l = LUMIN( hsla) / 100.f;
    if( ZERO( s))
        return RGBA( l * RGB_SCALE, l * RGB_SCALE, l * RGB_SCALE, ALPHA( hsla));

    float q = l < .5f ? l * ( 1 + s) : l + s - l * s;
    float p =  2 * l - q;
    return RGBA( hueToSpace( p, q, h + 1./3) * RGB_SCALE, hueToSpace( p, q, h) * RGB_SCALE,
                 hueToSpace( p, q, h - 1./3) * RGB_SCALE, ALPHA( hsla));
}

std::function<uint32_t( uint32_t, uint32_t)> selectBlendFn( CompositionRule::BlendModel model)
{
    static std::function<uint32_t( uint32_t, uint32_t)> selector[ NUMBER_OF_BLEND_MODES] =
    {
        [ ENUM_CAST( BLEND_ENUM( Dissolve))]     = []( auto top, auto base)
        {
            auto value = ( rand() % ( 0x100 - ALPHA( top)));
            return value >= 0 && value <= 10 ? top | 0xff : base;
        },
        [ ENUM_CAST( BLEND_ENUM( Darken))]       = []( auto top, auto base)
        {
            int t_rgb_min = std::min( std::min( RED( top), GREEN( top)), BLUE( top)),
                b_rgb_min = std::min( std::min( RED( base), GREEN( base)), BLUE( base)),
                t_rgb_max = std::max( std::max( RED( top), GREEN( top)), BLUE( top)),
                b_rgb_max = std::max( std::max( RED( base), GREEN( base)), BLUE( base));
            return ( t_rgb_max + t_rgb_min) < ( b_rgb_max + b_rgb_min) ? top : base;
        },
        [ ENUM_CAST( BLEND_ENUM( Multiply))]     = []( auto top, auto base)
        {
            uint8_t red   = (( float)RED( top) / RGB_SCALE   * ( float)RED( base) / RGB_SCALE) * RGB_SCALE,
                    green = (( float)GREEN( top) / RGB_SCALE * ( float)GREEN( base) / RGB_SCALE) * RGB_SCALE,
                    blue  = (( float)BLUE( top) / RGB_SCALE  * ( float)BLUE( base) / RGB_SCALE) * RGB_SCALE,
                    alpha = (( float)ALPHA( top) / RGB_SCALE * ( float)ALPHA( base) / RGB_SCALE) * RGB_SCALE;

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( ColorBurn))]    = []( auto top, auto base)
        {
            uint8_t red   = RED( top) == 0 ? 0 :
                            clamp( ( 1.f - (( RGB_SCALE - RED( base)) / RED( top))) * RGB_SCALE, 0, RGB_SCALE),
                    green = GREEN( top) == 0 ? 0 :
                            clamp( ( 1.f - (( RGB_SCALE - GREEN( base)) / GREEN( top))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = BLUE( top) == 0 ? 0 :
                            clamp( ( 1.f - (( RGB_SCALE - BLUE( base)) / BLUE( top))) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = ALPHA( top) == 0 ? 0 :
                            clamp( ( 1.f - (( RGB_SCALE - ALPHA( base)) / ALPHA( top))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( LinearBurn))]   = []( auto top, auto base)
        {
            uint8_t red_sum   = clamp(( int16_t)RED( base) + RED( top) - RGB_SCALE, 0, RGB_SCALE),
                    green_sum = clamp(( int16_t)GREEN( base) + GREEN( top) - RGB_SCALE, 0, RGB_SCALE),
                    blue_sum  = clamp(( int16_t)BLUE( base) + BLUE( top) - RGB_SCALE, 0, RGB_SCALE),
                    alpha_sum = clamp(( int16_t)ALPHA( base) + ALPHA( top) - RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red_sum, green_sum, blue_sum, alpha_sum);
        },
        [ ENUM_CAST( BLEND_ENUM( DarkerColor))]  = []( auto top, auto base)
        {
            constexpr auto factor = .5f;
            return ALPHA( top) > 180 ?
                   RGBA( clamp( RED( top) * factor, 0, RGB_SCALE), clamp( GREEN( top) * factor, 0, RGB_SCALE),
                         clamp( BLUE( top) * factor, 0, RGB_SCALE), ALPHA( top))
                   : RGBA( clamp( RED( base) * factor, 0, RGB_SCALE), clamp( GREEN( base) * factor, 0, RGB_SCALE),
                           clamp( BLUE( base) * factor, 0, RGB_SCALE), ALPHA( base));
        },
        [ ENUM_CAST( BLEND_ENUM( Lighten))]      = []( auto top, auto base)
        {
            return selector[ ENUM_CAST( CompositionRule::BlendModel::Darken)]( top, base) == top ? base : top;
        },
        [ ENUM_CAST( BLEND_ENUM( Screen))]       = []( auto top, auto base)
        {
            constexpr auto factor = 0.00001538f;
            uint8_t red   = ( 1 - factor * (( RGB_SCALE - RED( base)) * ( RGB_SCALE - RED( top)))) * RGB_SCALE,
                    green = ( 1 - factor * (( RGB_SCALE - GREEN( base)) * ( RGB_SCALE - GREEN( top)))) * RGB_SCALE,
                    blue  = ( 1 - factor * (( RGB_SCALE - BLUE( base)) * ( RGB_SCALE - BLUE( top)))) * RGB_SCALE,
                    alpha = ( 1 - factor * (( RGB_SCALE - ALPHA( base)) * ( RGB_SCALE - ALPHA( top)))) * RGB_SCALE;

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( ColorDodge))]   = []( auto top, auto base)
        {
            uint8_t red   = ( float)RED( base) * RGB_SCALE / ( RGB_SCALE - RED( top)),
                    green = ( float)GREEN( base) * RGB_SCALE / ( RGB_SCALE - GREEN( top)),
                    blue  = ( float)BLUE( base) * RGB_SCALE / ( RGB_SCALE - BLUE( top)),
                    alpha = ( float)ALPHA( base) * RGB_SCALE / ( RGB_SCALE - ALPHA( top));

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( LinearDodge))]  = []( auto top, auto base)
        {
            uint8_t red_sum   = clamp(( int16_t)RED( base) + RED( top), 0, RGB_SCALE),
                    green_sum = clamp(( int16_t)GREEN( base) + GREEN( top), 0, RGB_SCALE),
                    blue_sum  = clamp(( int16_t)BLUE( base) + BLUE( top), 0, RGB_SCALE),
                    alpha_sum = clamp(( int16_t)ALPHA( base) + ALPHA( top), 0, RGB_SCALE);

            return RGBA( red_sum, green_sum, blue_sum, alpha_sum);
        },
        [ ENUM_CAST( BLEND_ENUM( LighterColor))] = []( auto top, auto base)
        {
            constexpr auto factor = 2.f;
            return ALPHA( top) > 180 ?
                   RGBA( clamp( RED( top) * factor, 0, RGB_SCALE), clamp( GREEN( top) * factor, 0, RGB_SCALE),
                         clamp( BLUE( top) * factor, 0, RGB_SCALE), ALPHA( top))
                   : RGBA( clamp( RED( base) * factor, 0, RGB_SCALE), clamp( GREEN( base) * factor, 0, RGB_SCALE),
                           clamp( BLUE( base) * factor, 0, RGB_SCALE), ALPHA( base));
        },
        [ ENUM_CAST( BLEND_ENUM( Overlay))]      = []( auto top, auto base)
        {
            uint8_t red   = clamp((( RED( base) > 127.5) * ( 1 - ( 1 - 2 * ( ( float)RED( base) / RGB_SCALE - .5f))
                            * ( 1 - ( float)RED( top) / RGB_SCALE)) + ( RED( base) <= 127.5f)
                            * (( 2 * ( float)RED( base) / RGB_SCALE) * RED( top))) * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp((( GREEN( base) > 127.5) * ( 1 - ( 1 - 2 * (( float)GREEN( base) / RGB_SCALE - .5))
                            * ( 1 - ( float)GREEN( top) / RGB_SCALE)) + ( GREEN( base) <= 127.5f)
                            * (( 2 * ( float)GREEN( base) / RGB_SCALE) * GREEN( top))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp((( BLUE( base) > 127.5) * ( 1 - ( 1 - 2 * (( float)BLUE( base) / RGB_SCALE - .5))
                            * ( 1 - ( float)BLUE( top) / RGB_SCALE)) + ( BLUE( base) <= 127.5f)
                            * (( 2 * ( float)BLUE( base) / RGB_SCALE) * BLUE( top))) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp((( ALPHA( base) > 127.5) * ( 1 - ( 1 - 2 * (( float)ALPHA( base) / RGB_SCALE - .5))
                            * ( 1 - ( float)ALPHA( top) / RGB_SCALE)) + ( ALPHA( base) <= 127.5f)
                            * (( 2 * ( float)ALPHA( base) / RGB_SCALE) * ALPHA( top))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, ALPHA( top) < 180 ? ALPHA( base) : ALPHA( top));
        },
        [ ENUM_CAST( BLEND_ENUM( SoftLight))]    = []( auto top, auto base)
        {
            uint8_t red   = clamp((( RED( top) > 127.5) * ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE)
                            * ( 1 - (( float)RED( top) / RGB_SCALE - .5f))) + ( RED( top) <= 127.5)
                            * (( float)RED( base) / RGB_SCALE * (( float)RED( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp((( GREEN( top) > 127.5) * ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE)
                            * ( 1 - (( float)GREEN( top) / RGB_SCALE - .5f))) + ( GREEN( top) <= 127.5)
                            * ( ( float)GREEN( base) / RGB_SCALE * (( float)GREEN( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp((( BLUE( top) > 127.5) * ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE)
                            * ( 1 - (( float)BLUE( top) / RGB_SCALE - .5f))) + ( BLUE( top) <= 127.5)
                            * (( float)BLUE( base) / RGB_SCALE * (( float)BLUE( top) / RGB_SCALE + .5)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp((( ALPHA( top) > 127.5) * ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE)
                            * ( 1 - (( float)ALPHA( top) / RGB_SCALE - .5f))) + ( ALPHA( top) <= 127.5)
                            * (( float)ALPHA( base) / RGB_SCALE * (( float)ALPHA( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( HardLight))]    = []( auto top, auto base)
        {
            uint8_t red   = clamp((( RED( top) > 127.5) * ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)RED( top) / RGB_SCALE - .5f)))
                            + ( RED( top) <= 127.5) * (( float)RED( base) * ( 2 * ( float)RED( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp((( GREEN( top) > 127.5) * ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)GREEN( top) / RGB_SCALE - .5f)))
                            + ( GREEN( top) <= 127.5) * (( float)GREEN( base)
                            * ( 2 * ( float)GREEN( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp((( BLUE( top) > 127.5) * ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)BLUE( top) / RGB_SCALE - .5f)))
                            + ( BLUE( top) <= 127.5) * (( float)BLUE( base) * ( 2 * ( float)BLUE( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp((( ALPHA( top) > 127.5) * ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)ALPHA( top) / RGB_SCALE - .5f)))
                            + ( ALPHA( top) <= 127.5) * (( float)ALPHA( base)
                            * ( 2 * ( float)ALPHA( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( VividLight))]   = []( auto top, auto base)
        {
            auto red_f_base = ( 1 - 2 * (( float)RED( top) / RGB_SCALE - .5f)),
                 red_e_base = ( 2 * ( float)RED( top) / RGB_SCALE),
                 green_f_base = ( 1 - 2 * (( float)GREEN( top) / RGB_SCALE - .5f)),
                 green_e_base = ( 2 * ( float)GREEN( top) / RGB_SCALE),
                 blue_f_base = ( 1 - 2 * (( float)BLUE( top) / RGB_SCALE - .5f)),
                 blue_e_base = ( 2 * ( float)BLUE( top) / RGB_SCALE),
                 alpha_f_base = ( 1 - 2 * (( float)ALPHA( top) / RGB_SCALE - .5f)),
                 alpha_e_base = ( 2 * ( float)ALPHA( top) / RGB_SCALE);
            uint8_t red   = clamp(((( RED( top) > 127.5) * ( ZERO( red_f_base) ? 1 :
                            ((( float)RED( base) / RGB_SCALE) / red_f_base))) + (( RED( top) <= 127.5)
                            * ( ZERO( red_e_base) ? 0 : ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE) / red_e_base))))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp(((( GREEN( top) > 127.5) * ( ZERO( green_f_base) ? 1 :
                            ((( float)GREEN( base) / RGB_SCALE) / green_f_base))) + (( RED( top) <= 127.5)
                            * ( ZERO( green_e_base) ? 0 :
                            ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE) / green_e_base)))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp(((( BLUE( top) > 127.5) * ( ZERO( blue_f_base) ? 1 :
                            ((( float)BLUE( base) / RGB_SCALE) / blue_f_base))) + (( BLUE( top) <= 127.5)
                            * ( ZERO( blue_e_base) ? 0 : ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE) / blue_e_base))))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp(((( ALPHA( top) > 127.5) * ( ZERO( alpha_f_base) ? 1 :
                            ((( float)ALPHA( base) / RGB_SCALE) / alpha_f_base))) + (( ALPHA( top) <= 127.5)
                            * ( ZERO( alpha_e_base) ? 0 :
                            ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE) / alpha_e_base)))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( LinearLight))]  = []( auto top, auto base)
        {
            uint8_t red  = clamp((( RED( top) > 127.5) * (( float)RED( base) / RGB_SCALE + 2
                           * (( float)RED( top) / RGB_SCALE - .5f)) + ( RED( top) <= 127.5)
                           * (( float)RED( base) / RGB_SCALE + 2 * (( float)RED( top) / RGB_SCALE) - 1))
                           * RGB_SCALE, 0, RGB_SCALE),
                   green = clamp((( GREEN( top) > 127.5) * (( float)GREEN( base) / RGB_SCALE + 2
                           * (( float)GREEN( top) / RGB_SCALE - .5f)) + ( GREEN( top) <= 127.5)
                           * (( float)GREEN( base) / RGB_SCALE + 2 * (( float)GREEN( top) / RGB_SCALE) - 1))
                           * RGB_SCALE, 0, RGB_SCALE),
                   blue  = clamp((( BLUE( top) > 127.5) * (( float)BLUE( base) / RGB_SCALE + 2
                          * (( float)BLUE( top) / RGB_SCALE - .5f)) + ( BLUE( top) <= 127.5)
                          * (( float)BLUE( base) / RGB_SCALE + 2 * (( float)BLUE( top) / RGB_SCALE) - 1))
                          * RGB_SCALE, 0, RGB_SCALE),
                   alpha = clamp((( ALPHA( top) > 127.5) * (( float)ALPHA( base) / RGB_SCALE + 2
                          * (( float)ALPHA( top) / RGB_SCALE - .5f)) + ( ALPHA( top) <= 127.5)
                          * (( float)ALPHA( base) / RGB_SCALE + 2 * (( float)ALPHA( top) / RGB_SCALE) - 1))
                          * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( PinLight))]     = []( auto top, auto base)
        {
            uint8_t red   = clamp((( RED( top) > 127.5) * ( std::max<float>(( float)RED( base) / RGB_SCALE,
                            2 * (( float)RED( top) / RGB_SCALE - .5f))) + ( RED( top) <= 127.5)
                            * ( std::min<float>(( float)RED( base) / RGB_SCALE, 2 * ( float)RED( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp((( GREEN( top) > 127.5) * ( std::max<float>(( float)GREEN( base) / RGB_SCALE,
                            2 * (( float)GREEN( top) / RGB_SCALE - .5f))) + ( GREEN( top) <= 127.5)
                            * ( std::min<float>(( float)GREEN( base) / RGB_SCALE, 2 * ( float)GREEN( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp((( BLUE( top) > 127.5) * ( std::max<float>(( float)BLUE( base) / RGB_SCALE,
                            2 * (( float)BLUE( top) / RGB_SCALE - .5f))) + ( BLUE( top) <= 127.5)
                            * ( std::min<float>(( float)BLUE( base) / RGB_SCALE, 2 * ( float)BLUE( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp((( ALPHA( top) > 127.5) * ( std::max<float>(( float)ALPHA( base) / RGB_SCALE,
                            2 * (( float)ALPHA( top) / RGB_SCALE - .5f))) + ( ALPHA( top) <= 127.5)
                            * ( std::min<float>(( float)ALPHA( base) / RGB_SCALE, 2 * ( float)ALPHA( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( HardMix))]      = []( auto top, auto base)
        {
            auto mix = selector[ ENUM_CAST( BLEND_ENUM( LinearDodge))]( top, base);

            return RGBA( RED( mix) < RGB_SCALE ? 0 : RGB_SCALE, GREEN( mix) < RGB_SCALE ? 0 : RGB_SCALE,
                         BLUE( mix) < RGB_SCALE ? 0 : RGB_SCALE, ALPHA( mix) < RGB_SCALE ? 0 : RGB_SCALE);
        },
        [ ENUM_CAST( BLEND_ENUM( Difference))]   = []( auto top, auto base)
        {
            uint8_t red   = std::abs(( int16_t)RED( base) - ( int8_t)RED( top)),
                    green = std::abs(( int16_t)GREEN( base) - ( int8_t)GREEN( top)),
                    blue  = std::abs(( int16_t)BLUE( base) - ( int8_t)BLUE( top)),
                    alpha = clamp(( uint16_t)ALPHA( base) + ALPHA( top), 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( Exclusion))]    = []( auto top, auto base)
        {
            uint8_t red   = clamp(( .5f - 2 * (( float)RED( base) / RGB_SCALE - .5f)
                            * (( float)RED( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    green = clamp(( .5f - 2 * (( float)GREEN( base) / RGB_SCALE - .5f)
                            * (( float)GREEN( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = clamp(( .5f - 2 * (( float)BLUE( base) / RGB_SCALE - .5f)
                            * (( float)BLUE( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = clamp(( .5f - 2 * (( float)ALPHA( base) / RGB_SCALE - .5f)
                            * (( float)ALPHA( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( Subtract))]     = []( auto top, auto base)
        {
            uint8_t red   = RED( top) > RED( base) ? RED( top) - RED( base) : RED( top),
                    green = GREEN( top) > GREEN( base) ? GREEN( top) - GREEN( base) : GREEN( top),
                    blue  = BLUE( top) > BLUE( base) ? BLUE( top) - BLUE( base) : BLUE( top),
                    alpha = ALPHA( top) > ALPHA( base) ? ALPHA( top) - ALPHA( base) : ALPHA( top);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( Divide))]       = []( auto top, auto base)
        {
            uint8_t red   = RED( base) == 0 ? RGB_SCALE : clamp(( float)RED( top) * RGB_SCALE / RED( base),
                                                                0, RGB_SCALE),
                    green = GREEN( base) == 0 ? RGB_SCALE : clamp(( float)GREEN( top) * RGB_SCALE / GREEN( base),
                                                                0, RGB_SCALE),
                    blue  = BLUE( base) == 0 ? RGB_SCALE : clamp(( float)BLUE( top) * RGB_SCALE / BLUE( base),
                                                                0, RGB_SCALE),
                    alpha = ALPHA( base) == 0 ? RGB_SCALE : clamp(( float)ALPHA( top) * RGB_SCALE / ALPHA( base),
                                                                0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        [ ENUM_CAST( BLEND_ENUM( Hue))]          = []( auto top, auto base)
        {
            auto top_hsla   = rgbaToHsla( top),
                 base_hsla  = rgbaToHsla( base),
                 lum_change = HSLA( HUE( top_hsla), SAT( base_hsla), LUMIN( base_hsla), ALPHA( base_hsla));

            return hslaToRgba( lum_change);
        },
        [ ENUM_CAST( BLEND_ENUM( Saturation))]   = []( auto top, auto base)
        {
            auto top_hsla   = rgbaToHsla( top),
                 base_hsla  = rgbaToHsla( base),
                 lum_change = HSLA( HUE( base_hsla), SAT( top_hsla), LUMIN( base_hsla), ALPHA( base_hsla));

            return hslaToRgba( lum_change);
        },
        [ ENUM_CAST( BLEND_ENUM( Color))]        = []( auto top, auto base)
        {
            auto top_hsla   = rgbaToHsla( top),
                 lum_change = HSLA( HUE( top_hsla), SAT( top_hsla), LUMIN( top_hsla), ALPHA( base));

            return hslaToRgba( lum_change);
        },
        [ ENUM_CAST( BLEND_ENUM( Luminosity))]   = []( auto top, auto base)
        {
            auto top_hsla   = rgbaToHsla( top),
                 base_hsla  = rgbaToHsla( base),
                 lum_change = HSLA( HUE( base_hsla), SAT( base_hsla), LUMIN( top_hsla), ALPHA( base_hsla));

            return hslaToRgba( lum_change);
        }
    };
    return selector[ ENUM_CAST( model)];
}

template <typename Tp>
void resize( FrameBuffer<Tp> &frame, int *interpolation)
{
    std::array<int, 2> neighbours = { 9, 16};

    auto n_channel = std::max( frame.n_channel, 1);
    auto new_width  = interpolation[ 0],
         new_height = interpolation[ 1],
         o_width    = frame.width,
         o_height   = frame.height,
         o_size     = frame.width * frame.height * n_channel;

    auto adjust = [ &]( auto& side, auto other, bool is_width = true)
    {
        if( side > 0)
            return;

        auto aspect = static_cast<float>( o_width) / static_cast<float>( o_height);
        side = other * ( is_width ? aspect : 1. / aspect);
    };

    adjust( new_width == -1 ? new_width : new_height, new_width == -1 ? new_height : new_width, new_width == -1);

    auto w_scale = static_cast<float>( o_width) / static_cast<float>( new_width),
         h_scale = static_cast<float>( o_height) / static_cast<float>( new_height);

    auto n_neighbour = static_cast<float>( neighbours[ interpolation[ 2]]);
    auto n_size  = static_cast<int>( n_neighbour),
         n_width = static_cast<int>( std::sqrt( n_size)),
         mid      = n_width / 2;
    std::shared_ptr<Tp> dest(( Tp *)calloc( new_width * new_height * n_channel, sizeof( Tp)),
                                   []( auto *p){ free( p);});
    auto s_buffer = frame.buffer.get();
    auto d_buffer = dest.get();
    for( int j = 0; j < new_height; ++j)
    {
        for( int i = 0; i < new_width; ++i)
        {
            int red{}, green{}, blue{}, alpha{};
            for( int k = 0; k < n_size; ++k)
            {
                int row = k / n_width - mid + j,
                    col = k % n_width - mid + i,
                    a_row = static_cast<int>( row * h_scale),
                    a_col = static_cast<int>( col * w_scale);
                auto index = a_row * o_width * n_channel + a_col * n_channel;
                if( a_row >= 0 && a_col >= 0 && index >= 0 && index < o_size)
                {
                    auto color = s_buffer[ index];
                    if constexpr ( sizeof( Tp) == 4)
                    {
                        red += RED( color);
                        green += GREEN( color);
                        blue += BLUE( color);
                        alpha += ALPHA( color);
                    }
                    else
                    {
                        red += s_buffer[ index];
                        green += s_buffer[ index + 1];
                        blue += s_buffer[ index + 2];
                        alpha += s_buffer[ index + 3];
                    }
                }
            }
            if constexpr ( sizeof( Tp) == 4)
            {
                d_buffer[ j * new_width * n_channel + i * n_channel] = RGBA(( red / n_neighbour),
                                                                            ( green / n_neighbour),
                                                                            ( blue / n_neighbour),
                                                                            ( alpha / n_neighbour));
            }
            else
            {
                auto base_index = j * new_width * n_channel + i * n_channel;
                d_buffer[ base_index]     = red / n_neighbour;
                d_buffer[ base_index + 1] = green / n_neighbour;
                d_buffer[ base_index + 2] = blue / n_neighbour;
                d_buffer[ base_index + 3] = alpha / n_neighbour;
            }
        }
    }
    frame.width  = new_width;
    frame.height = new_height;
    frame.buffer = dest;
}

void composite( ApplicationHyperparameters &guide, FrameBuffer<uint32_t> &s_frame)
{
   auto c_rule = parseCompositionRule( guide.composition_rule);
   if( c_rule.c_model == MODEL_ENUM( NotApplicable))
     return;

#if defined( PNG_SUPPORTED) && defined( JPG_SUPPORTED)
   auto d_frame = ( isJPEG( guide.dest_filename) ? readJPEG : readPNG)( guide.dest_filename);
#elif defined( PNG_SUPPORTED)
   auto d_frame = readPNG( guide.dest_filename);
#else
   if( !isJPEG( guide.dest_filename))
       return;
   auto d_frame = readJPEG( guide.dest_filename);
#endif

   if( d_frame.buffer == nullptr)
     return;

   auto emboss = makeEmboss( 9);
   auto gaussian = makeGaussian( 30);

   int interpolation[ 3] = { s_frame.width >= s_frame.height ? s_frame.width : -1,
                             s_frame.height >= s_frame.width ? s_frame.height : -1, 0};
   resize( d_frame, interpolation);
//   applyEffect( s_frame, SpecialEffect::Emboss, &emboss);
//   applyEffect( s_frame, SpecialEffect::GaussianBlur, &gaussian);

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
   
   if( COMPOSITON_SIZE( c_rule.c_model))
   { // The final canvas size is the maximum of the width and height of both canvas
     auto bottom_edge = Vec2D<float>( std::max( origin.x + width  - 1.f,  dwidth  - 1.f),
     	                              std::max( origin.y + height - 1.f, dheight  - 1.f));
     origin.x         = std::min( origin.x, 0.f);
     origin.y         = std::min( origin.y, 0.f);
     final_width      = bottom_edge.x - origin.x + 1;
     final_height     = bottom_edge.y - origin.y + 1;
   }
   else if( COMPOSITION_SIDE( c_rule.c_model))
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
  std::vector<std::function<uint32_t(uint32_t, uint32_t)>> blendFns( c_rule.b_models.size());
  for( size_t i = 0; i < blendFns.size(); ++i)
  {
      if( c_rule.b_models[ i] == BLEND_ENUM( Normal))
          continue;
      blendFns[ i] = selectBlendFn( c_rule.b_models[ i]);
  }
  std::array<std::function<void( std::variant<Default, Top, Bottom>)>,
      ENUM_CAST( MODEL_ENUM( Xor)) + 1> models = {
	  [&]( auto part) // Clip
      {
          models[ ENUM_CAST( MODEL_ENUM( SourceAtop))]( Bottom());
      },
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
				auto d_index = y * d_frame.width * d_frame.n_channel +  x * d_frame.n_channel;
                auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
                int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                if(!c_rule.b_models.empty())
                {
                    bool corners_intersects = false;
                    if(( corners_intersects = ( index >= 0 && index < s_frame_dimension && intersects( corners, point)))
                        && d_index < d_max_index && intersects( big_corners, point))
                    {
                        auto top = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                         d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                        auto base = s_frame.buffer.get()[ index];
                        for( auto& blendFn : blendFns)
                            top = blendFn( top, base);
                        image.buffer.get()[ j * final_width + i] = top;
                    }
                    else if( corners_intersects)
                        image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
                }
                else if( d_index < d_max_index && intersects( corners, point) && intersects( big_corners, point))
				{
					image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
						                                             d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
				}
				else if( intersects( corners, point))
				{
				  if( index >= 0 && index < s_frame_dimension)
					image.buffer.get()[ j * final_width + i] = s_frame.buffer.get()[ index];
				}
			  }
			  else if constexpr ( std::is_same_v<T, Top>) // DestinationIn
			  {
				auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
				if( d_index < d_max_index && intersects( corners, point) && intersects( big_corners, point))
				{
                  auto top = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                   d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                  if( !c_rule.b_models.empty())
                  {
                      auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
                      int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                      auto base = s_frame.buffer.get()[ index];
                      for( auto& blendFn : blendFns)
                          top = blendFn( top, base);
                  }

                  image.buffer.get()[ j * final_width + i] = top;
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
					auto final_color = sumMix(b_color, c_color);
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
              else if( !c_rule.b_models.empty() &&
                      intersects( big_corners, point) && intersects( corners, point))
              {
                  auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                  auto *buffer = d_frame.buffer.get();
                  auto top = RGBA( buffer[ d_index], buffer[ d_index + 1],
                                   buffer[ d_index + 2], buffer[ d_index + 3]),
                       base = s_frame.buffer.get()[ index];
                  for( auto& blendFn : blendFns)
                      top = blendFn( top, base);
                  image.buffer.get()[ j * final_width + i] = top;
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
          auto max_d_frame_dimension = d_frame.width * d_frame.height * d_frame.n_channel;
	      using T = std::remove_cv_t<std::remove_reference_t<decltype( current)>>;
		  for( int y = origin.y, j = 0; j < final_height; ++y, ++j)
		  {
			for( int x = origin.x, i = 0; i < final_width; ++x, ++i)
			{
			  int s_index    = j * d_frame.width * d_frame.n_channel + i * d_frame.n_channel,
				  d_index     = j * final_width + i;
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
                  if( c_rule.b_models.empty())
                  {
                      if( ALPHA( rgba) < 180)
                          pixel = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
                                        d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
                      else
                          pixel = s_frame.buffer.get()[ index];
                  }
                  else
                  {
                      auto top  = s_frame.buffer.get()[ index],
                           base = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
                                        d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
                      for( auto& blendFn : blendFns)
                          top = blendFn( top, base);
                      pixel = top;
                  }
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
			  else if constexpr ( std::is_same_v<T, Bottom>) // SourceIn, Clip
			  {
				if( intersects( corners, point) && intersects( big_corners, point))
				{
				  auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
				  int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                  auto color   = s_frame.buffer.get()[ index];
                  if( s_index >= max_d_frame_dimension)
                      continue;
                  if(!c_rule.b_models.empty())
                  {
                      auto top  = color,
                           base = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
                                        d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
                       for( auto& blendFn : blendFns)
                           top = blendFn( top, base);
                       pixel = top;
                  }
                  else
                  {
                      if( ALPHA( color) < 180)
                          pixel = RGBA( d_ptr[ s_index], d_ptr[ s_index + 1],
                                        d_ptr[ s_index + 2], d_ptr[ s_index + 3]);
                      else
                          pixel = color;
                  }
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
		  for( int x = origin.x, i = 0; i < final_width; ++x, ++i)
		  {
			auto point = Vec2D<float>( x, y);
			auto s_coord = ( point - center).rotate( -c_rule.angle) + center - pos;
			if( intersects( corners, point))
			{
			  int index = (int)s_coord.y * s_frame.width + (int)s_coord.x;
			  if( index > 0 && index < s_frame_dimension)
              {
                  if( c_rule.b_models.empty())
                  {
                      if( ALPHA( s_buffer[ index]) < 180 && intersects( big_corners, point))
                      {
                          auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                          image.buffer.get()[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                                                           d_buffer[ d_index + 2],
                                                                           d_buffer[ d_index + 3]);
                      }
                      else
                          image.buffer.get()[ j * final_width + i] = s_buffer[ index];
                  }
                  else if( intersects( big_corners, point))
                  {
                      auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                      auto top  = s_buffer[ index],
                           base = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                        d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                      for( auto& blendFn : blendFns)
                          top = blendFn( top, base);
                      image.buffer.get()[ j * final_width + i] = top;
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
//  applyEffect( d_frame, SpecialEffect::Grainy);
  models[ ENUM_CAST( c_rule.c_model)]( Default());

  if( guide.interpolation[ 0] && guide.interpolation[ 1])
      resize( image, guide.interpolation);
#if defined( PNG_SUPPORTED) && defined( JPG_SUPPORTED)
    ( guide.out_format == OutputFormat::JPEG ? writeJPEG : writePNG)( guide.src_filename, image, guide.image_quality);
#elif defined( PNG_SUPPORTED)
    writePNG( guide.src_filename, image, guide.image_quality);
#else
    writeJPEG( guide.src_filename, image, guide.image_quality);
#endif

}

#endif

#if defined( JPG_SUPPORTED)

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

void writeJPEG( std::string filename, FrameBuffer<uint32_t> &frame, int quality)
{
    using namespace std::string_literals;
    filename = std::regex_replace( filename, std::regex( R"(\..+)", std::regex_constants::icase), R"(.jpg)"s);
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

#endif

#if defined( PNG_SUPPORTED)

FrameBuffer<uint8_t> readPNG( std::string_view filename)
{
   constexpr const auto BYTES_READ = 8;
   char magic[ BYTES_READ];
   PropertyManager<FILE *> handle( fopen( filename.data(), "rb"), []( auto *p) { if( p) fclose( p); p = nullptr;});
   if( handle.get() == nullptr)
     return {};
   
   if( fread( magic, 1, BYTES_READ, handle.get()) != BYTES_READ)
     return {};
   
   if( png_sig_cmp( png_const_bytep ( magic), 0, BYTES_READ))
   {
       fprintf( stderr, "This is not a png file.\n");
       return {};
   }

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
 
   FrameBuffer<uint8_t> frame;
   int32_t bit_depth, color_type;
   png_get_IHDR( png_ptr, info_ptr,
				 reinterpret_cast<uint32_t *>( &frame.width),
				 reinterpret_cast<uint32_t *>( &frame.height),
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
   uint32_t row_bytes = png_get_rowbytes( png_ptr, info_ptr);
   std::unique_ptr<uint8_t, DeleterType> image_data( ( uint8_t *)malloc( frame.height * row_bytes),
   	                                                  []( auto *p){ free( p);});
  if( image_data)
   {
	 uint8_t * row_pointers[ frame.height];
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

void writePNG( std::string filename, FrameBuffer<uint32_t> &frame, int quality)
{
    using namespace std::string_literals;
    filename = std::regex_replace( filename, std::regex( R"(\..+)", std::regex_constants::icase), R"(.png)"s);
    png::image<png::rgba_pixel> image( frame.width, frame.height);
    auto buffer = frame.buffer.get();
    for( uint32_t j = 0; j < frame.height; ++j)
    {
        for ( uint32_t i = 0; i < frame.width; ++i)
        {
            auto pixel = PNG_ENDIAN( buffer[ j * frame.width + i]);
            image.set_pixel( i, j, *(( png::rgba_pixel *)&pixel));
        }
    }
    image.write( filename.data());
}

#endif

uint64_t getNumber( const char *&ctx, uint8_t base)
{
    uint64_t weight = 0;
    while( isxdigit( *ctx))
    {
        uint8_t character = tolower( *( ctx++));
        int value = character >= 'a' && character <= 'f' ? character - 'a' + 10 : isdigit( character) ? character - '0' : 0;
        weight = weight * base + value;
    }

    return weight;
}

std::unordered_map<std::string_view, uint32_t>& colorCodeLookup()
{
    static std::unordered_map<std::string_view, uint32_t> name_lookup =
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
    return name_lookup;
}

std::string_view getColorNameAt( size_t pos)
{
    assert(( int)pos > 0 && pos < NUMBER_OF_COLORS);
    static std::array<std::string_view, NUMBER_OF_COLORS> color_names =
    {
        ICOLOR_ALICEBLUE,
        ICOLOR_ANTIQUEWHITE,
        ICOLOR_AQUA,
        ICOLOR_AQUAMARINE,
        ICOLOR_AZURE,
        ICOLOR_BEIGE,
        ICOLOR_BISQUE,
        ICOLOR_BLACK,
        ICOLOR_BLANCHEDALMOND,
        ICOLOR_BLUE,
        ICOLOR_BLUEVIOLET,
        ICOLOR_BROWN,
        ICOLOR_BURLYWOOD,
        ICOLOR_CADETBLUE,
        ICOLOR_CHARTREUSE,
        ICOLOR_CHOCOLATE,
        ICOLOR_CORAL,
        ICOLOR_CORNFLOWERBLUE,
        ICOLOR_CORNSILK,
        ICOLOR_CRIMSON,
        ICOLOR_CYAN,
        ICOLOR_DARKBLUE,
        ICOLOR_DARKCYAN,
        ICOLOR_DARKGOLDENROD,
        ICOLOR_DARKGRAY,
        ICOLOR_DARKGREY,
        ICOLOR_DARKGREEN,
        ICOLOR_DARKKHAKI,
        ICOLOR_DARKMAGENTA,
        ICOLOR_DARKOLIVEGREEN,
        ICOLOR_DARKORANGE,
        ICOLOR_DARKORCHID,
        ICOLOR_DARKRED,
        ICOLOR_DARKSALMON,
        ICOLOR_DARKSEAGREEN,
        ICOLOR_DARKSLATEBLUE,
        ICOLOR_DARKSLATEGRAY,
        ICOLOR_DARKSLATEGREY,
        ICOLOR_DARKTURQUOISE,
        ICOLOR_DARKVIOLET,
        ICOLOR_DEEPPINK,
        ICOLOR_DEEPSKYBLUE,
        ICOLOR_DIMGRAY,
        ICOLOR_DIMGREY,
        ICOLOR_DODGERBLUE,
        ICOLOR_FIREBRICK,
        ICOLOR_FLORALWHITE,
        ICOLOR_FORESTGREEN,
        ICOLOR_FUCHSIA,
        ICOLOR_GAINSBORO,
        ICOLOR_GHOSTWHITE,
        ICOLOR_GOLD,
        ICOLOR_GOLDENROD,
        ICOLOR_GRAY,
        ICOLOR_GREY,
        ICOLOR_GREEN,
        ICOLOR_GREENYELLOW,
        ICOLOR_HONEYDEW,
        ICOLOR_HOTPINK,
        ICOLOR_INDIANRED,
        ICOLOR_INDIGO,
        ICOLOR_IVORY,
        ICOLOR_KHAKI,
        ICOLOR_LAVENDER,
        ICOLOR_LAVENDERBLUSH,
        ICOLOR_LAWNGREEN,
        ICOLOR_LEMONCHIFFON,
        ICOLOR_LIGHTBLUE,
        ICOLOR_LIGHTCORAL,
        ICOLOR_LIGHTCYAN,
        ICOLOR_LIGHTGOLDENRODYELLOW,
        ICOLOR_LIGHTGRAY,
        ICOLOR_LIGHTGREY,
        ICOLOR_LIGHTGREEN,
        ICOLOR_LIGHTPINK,
        ICOLOR_LIGHTSALMON,
        ICOLOR_LIGHTSEAGREEN,
        ICOLOR_LIGHTSKYBLUE,
        ICOLOR_LIGHTSLATEGRAY,
        ICOLOR_LIGHTSLATEGREY,
        ICOLOR_LIGHTSTEELBLUE,
        ICOLOR_LIGHTYELLOW,
        ICOLOR_LIME,
        ICOLOR_LIMEGREEN,
        ICOLOR_LINEN,
        ICOLOR_MAGENTA,
        ICOLOR_MAROON,
        ICOLOR_MEDIUMAQUAMARINE,
        ICOLOR_MEDIUMBLUE,
        ICOLOR_MEDIUMORCHID,
        ICOLOR_MEDIUMPURPLE,
        ICOLOR_MEDIUMSEAGREEN,
        ICOLOR_MEDIUMSLATEBLUE,
        ICOLOR_MEDIUMSPRINGGREEN,
        ICOLOR_MEDIUMTURQUOISE,
        ICOLOR_MEDIUMVIOLETRED,
        ICOLOR_MIDNIGHTBLUE,
        ICOLOR_MINTCREAM,
        ICOLOR_MISTYROSE,
        ICOLOR_MOCCASIN,
        ICOLOR_NAVAJOWHITE,
        ICOLOR_NAVY,
        ICOLOR_OLDLACE,
        ICOLOR_OLIVE,
        ICOLOR_OLIVEDRAB,
        ICOLOR_ORANGE,
        ICOLOR_ORANGERED,
        ICOLOR_ORCHID,
        ICOLOR_PALEGOLDENROD,
        ICOLOR_PALEGREEN,
        ICOLOR_PALETURQUOISE,
        ICOLOR_PALEVIOLETRED,
        ICOLOR_PAPAYAWHIP,
        ICOLOR_PEACHPUFF,
        ICOLOR_PERU,
        ICOLOR_PINK,
        ICOLOR_PLUM,
        ICOLOR_POWDERBLUE,
        ICOLOR_PURPLE,
        ICOLOR_REBECCAPURPLE,
        ICOLOR_RED,
        ICOLOR_ROSYBROWN,
        ICOLOR_ROYALBLUE,
        ICOLOR_SADDLEBROWN,
        ICOLOR_SALMON,
        ICOLOR_SANDYBROWN,
        ICOLOR_SEAGREEN,
        ICOLOR_SEASHELL,
        ICOLOR_SIENNA,
        ICOLOR_SILVER,
        ICOLOR_SKYBLUE,
        ICOLOR_SLATEBLUE,
        ICOLOR_SLATEGRAY,
        ICOLOR_SLATEGREY,
        ICOLOR_SNOW,
        ICOLOR_SPRINGGREEN,
        ICOLOR_STEELBLUE,
        ICOLOR_TAN,
        ICOLOR_TEAL,
        ICOLOR_THISTLE,
        ICOLOR_TOMATO,
        ICOLOR_TURQUOISE,
        ICOLOR_VIOLET,
        ICOLOR_WHEAT,
        ICOLOR_WHITE,
        ICOLOR_WHITESMOKE,
        ICOLOR_YELLOW,
        ICOLOR_YELLOWGREEN
    };

    return color_names[ pos];
}

uint32_t decodeColorName( const char *&ctx, BKNode *bkroot)
{
    std::string name;
    do
    {
        name += std::tolower( *ctx);
    }
    while( isalpha( *++ctx));

    auto& code_lookup = colorCodeLookup();
    auto pos = code_lookup.find( name);
    if( pos != code_lookup.cend())
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

uint64_t extractColor( const char *&rule, BKNode *bkroot)
{
    uint64_t ccolor{};
    ltrim( rule);
    int8_t cratio = -1, shift = 0;
    const char *prev = rule;
    if( *rule == '/')
    {
        shift = 32u;
        ++rule;
    }

	if( compareOr<std::equal_to<char>>( *rule, '#', 'x'))
	  ++rule;
	else if( strncasecmp( rule, "0x", 2) == 0)
	  rule += 2;
	else if( isalpha( *rule))
    {
        uint64_t color_name = decodeColorName( rule, bkroot);
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

            if( ltrim( rule) && *rule == '/')
                return MAKE_QWORD( extractColor( ++rule, bkroot), color_name);

            return color_name << shift;
        }

        if( ltrim( rule) && *rule == '/')
            return MAKE_QWORD( extractColor( ++rule, bkroot), color_name | 0xFFu);

        return ( color_name | 0xFFu) << shift;
    }
    else
    {
      return {};
//        fprintf( stderr, "Expected hex indicator near -> %s (allowed: `#`, `x`, `0x`)", prev);
//        exit( EXIT_FAILURE);
    }

    prev = rule;
    if( isxdigit( *rule))
    {
        ccolor = getNumber( rule, 16);
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

        if( ltrim( rule) && *rule == '/')
            return MAKE_QWORD( extractColor( ++rule, bkroot), ccolor);
    }
    else
    {
        fprintf( stderr, "Expected hex digits -> %s", prev);
        exit( EXIT_FAILURE);
    }

    return ccolor << shift;
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

uint32_t sumMix(uint32_t lcolor, uint32_t rcolor)
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

uint32_t subMix( uint32_t lcolor, uint32_t rcolor)
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

    return RGB( r, g, b);
}

uint64_t mixColor( const char *&ctx, BKNode *bkroot)
{
    uint64_t lcolor{}, rcolor{};
    uint8_t alpha = 0xFFu;
    uint16_t ops{};
    while( ltrim( ctx) && *ctx != ')')
    {
        if( *ctx && ctx[ 1] == '/')
        {
            ops = MAKE_WORD( ctx[ 2], *ctx);
            ctx += 2;
        }
        else if( compareOr<std::equal_to<char>>( *ctx, '+', '-'))
            ops = *ctx;
        else
        {
            auto *before = ctx;
            ops ? rcolor = extractColor( ctx, bkroot) : lcolor = extractColor( ctx, bkroot);
            ctx -= 1 - ( before == ctx);
        }
        ++ctx;
    }

    ctx += 1;

    if( ltrim( ctx) && *ctx == ':')
        alpha = getNumber( ++ctx, 16);

    return compareOr<std::equal_to<char>>( ops, '+', '-') ?
           MAKE_QWORD( HIGH_BYTE( ops) == '+' ? sumMix( HIGH_DWORD( lcolor), HIGH_DWORD( rcolor)) | alpha :
                                                subMix( HIGH_DWORD( lcolor), HIGH_DWORD( rcolor)) | alpha,
                       LOW_BYTE( ops) == '+' ?  sumMix( LOW_DWORD( lcolor), LOW_DWORD( rcolor)) | alpha :
                                                subMix( LOW_DWORD( lcolor), LOW_DWORD( rcolor)) | alpha) : lcolor;
}

template <size_t count>
std::array<float, count> parseFloats( const char *&rule)
{
   std::array<float, count> response;
   for( auto& result : response)
   {
	   result = 0;
	   float post_decimal_point_value = 0, shift = 1;
       if( ltrim( rule) && *rule == ')')
           break;
	   if(( *rule == '(' || *rule == ',' || *rule == '+'))
		 ++rule;
       float sign = 1;
       if( *rule == '-')
       {
           sign = -1;
           ++rule;
       }
       rule += ( *rule == '-') || ( *rule == '+');
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
	   result += post_decimal_point_value * sign;
   }

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
   int start_angle = 0;
   if( *rule == '(')
   {
     ++rule;
	 while( *rule && *rule != ')')
	   gradient_part += *rule++;
	 if( *rule == ')')
	   ++rule;
	 else
	 {
         fprintf( stderr, "Expected `)` after expression: %s\n", rule);
         exit( EXIT_FAILURE);
	 }
  
	 auto stops = partition( gradient_part, R"(\s*,\s*(?=[a-zA-Z#]|0[xX]))");
     if( stops.empty() && !gradient_part.empty())
         stops.push_back( gradient_part);
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
         std::regex matcher( R"(^(.+?)(?:\s+(\d+)deg)?(?:\s+(\d+)deg)?\s*$)", std::regex_constants::icase);
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
                             auto color_components = extractColor( underlying_data, bkroot);
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
         std::regex matcher( R"(\s*from\s+(\d+)deg\s*)", std::regex_constants::icase);
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
   if( !cstops.empty())
   {
     // Adjust cstops angle values to start at 0deg and end at 360deg
     if( cstops[ 0].second == SIZE_MAX)
       cstops[ 0].second = 0;
	 else if( cstops[ 0].second > 0)
	   cstops.insert( cstops.begin(), { cstops.back().first, 0});
     else if( cstops.back().second < DEG_MAX)
	   cstops.emplace_back( cstops.back().first, SIZE_MAX);

     size_t cstops_length = cstops.size();
       // Adjust cstop colors so that they appear in an increasing sequence.
     for( size_t i = 1; i < cstops_length; ++i)
	 {
       if( cstops[ i].second == SIZE_MAX)
	   {
         auto start = cstops.cbegin() + i;
         auto next_valid = std::find_if( start, cstops.cend(), []( auto& el){ return el.second != SIZE_MAX;});
         if( next_valid == cstops.cend())
         {
             auto dist = std::distance( start, --cstops.cend()) + 2;
             auto step = ( 360 - cstops[ i - 1].second) / dist;
             cstops[ i].second = cstops[ i - 1].second + step;
             ( --cstops.end())->second = cstops[ i - 1].second + step * --dist;
         }
         else
         {
             auto dist = std::distance( start, next_valid);
             cstops[ i].second = cstops[ i - 1].second + ( next_valid->second - cstops[ i - 1].second) / ( dist + 1);
         }
	   }
       else
	   	cstops[ i].second = std::max( cstops[ i - 1].second, cstops[ i].second);
	 }

/*     auto last_angle = cstops.back().second;
     if( last_angle < DEG_MAX)
         cstops.emplace_back( cstops.back().first, DEG_MAX);
     auto start = cstops.front().second;
     if( start > 0)
         cstops.emplace_back( cstops.back().first, 0);
     cstops_length = cstops.size();*/

     size_t i = 0;
     int rem;
     for( ; i < cstops_length; ++i)
     {
       if(( cstops[ i].second += start_angle) > DEG_MAX)
       {
         rem = i > 0 ? static_cast<int>( cstops[ i].second - cstops[ i - 1].second) : 0;
         cstops[ i++].second = DEG_MAX;
         break;
       }
     }
     for( size_t j = i, shift = 0; j < cstops_length; ++j)
     {
         if( cstops[ j + shift - 1].second == DEG_MAX)
             rem = static_cast<int>( cstops[ j + shift].second + start_angle -  cstops[ j + shift - 1].second - rem);
         else
            rem = static_cast<int>( cstops[ j + shift].second - cstops[ j + shift - 1].second);
         rem = cstops[ 0].second - rem;
         if( rem >= 0 && cstops[ 0].second > 0)
         {
             cstops.insert( cstops.begin(), { cstops[ j + shift].first, rem});
             ++shift;
         }
         else if( rem < 0)
             break;
     }
     if( cstops[ 0].second > 0)
         cstops.insert( cstops.begin(), { cstops.back().first, 0});
     //	  Delete remaining entries, they will not be visible to the user.
     cstops.erase( cstops.end() - cstops_length + i, cstops.end());
     if( cstops.back().second != DEG_MAX)
         cstops.emplace_back( cstops.back().first, DEG_MAX);
   }

   return actual_gradient;
}

void setColor( const char *& rule, BKNode *bkroot, PropertyProxy<uint64_t> &color)
{
    ltrim( rule);
    const char *prev = rule;
    size_t rule_len = strlen( rule),
            lighter_len = strlen( STR_LIGHTER),
            darker_len = strlen( STR_DARKER);
    int effect = 0;
    effect = ( rule_len > lighter_len &&
               FOUND_STRING( strncasecmp( rule, STR_LIGHTER, lighter_len))) * -1;
    effect = -( effect == -1) + ( rule_len > darker_len &&
                                  FOUND_STRING( strncasecmp( rule, STR_DARKER, darker_len)));
    rule += ( effect == -1) * lighter_len + ( effect == 1) * darker_len;
    if( effect != 0)
    {
        ltrim( rule);
        prev = rule;
        if( *rule == '(')
            ++rule;
        else
        {
            fprintf( stderr, "Expected `(` after -> %s\n", effect == -1 ? STR_LIGHTER : STR_DARKER);
            exit( EXIT_FAILURE);
        }
    }
    if( ltrim( rule) && *rule == '(')
        color = mixColor( ++rule, bkroot);
    else if( *rule != '-')
    {
        prev = rule;
        color = extractColor( rule,  bkroot);
    }
    if( ltrim( rule) && effect != 0)
    {
        auto factor = 2.f;
        if( *rule == ',' && ltrim( ++rule))
            factor = static_cast<int>( parseFloats<1>( rule)[ 0]);
        // Make sure annotation of `Lighter` are values below 1 and those annotated `Darker` are values above 1.
        factor = effect == -1 ? factor : 1.f / factor;
        color = MAKE_QWORD( tintColor( HIGH_DWORD( color), std::abs( factor)),
                            tintColor( LOW_DWORD( color), std::abs( factor)));
        if( *rule != ')')
        {
            fprintf( stderr, "Expected `)` to match %s", prev);
            exit( EXIT_FAILURE);
        }
        ++rule;
    }
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
                            // Allow user to specify -1 in case they want to
                            // set `end.y` alone.
                            if( *rule == '-' && rule[ 1] == '1')
                                rule += 2;
                            else
                                ccolor.end.x = getNumber( rule);
                            if( ltrim( rule) && *rule++ != ',')
                            {
                                fprintf( stderr, "Expected a `,` near -> %s", rule);
                                exit( EXIT_FAILURE);
                            }
                            if( *rule == '-' && rule[ 1] == '1') // A sentinel of -1 is also allowed
                                rule += 2;
                            else
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

                    if( ltrim( rule) && *rule == ';')
                        ccolor.max_bounce = getNumber( ++rule);

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
                if( ltrim( rule) && *rule != '\0')
                {
                    setColor( rule, bkroot, ccolor.scolor);
                    if( ltrim( rule) && *rule == '-')
                    {
                      	auto gradient_is_next = compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r');
                        if( !easing_enabled && gradient_is_next)
                        {
                            fprintf( stderr, "Easing only available for range based colors -> %s", prev);
                            exit( EXIT_FAILURE);
                        }

                        if( !gradient_is_next && std::tolower( rule[ 1]) != 's')
						{
							if( rule[ 1] != '>')
							{
							  fprintf( stderr, "Expected '>' near -> %s", rule);
							  exit( EXIT_FAILURE);
							}
                            setColor( rule += 2, bkroot, ccolor.ecolor);
						}
                        
                        if( ltrim( rule) && *rule == '-'
                        	 && compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r'))
						{
                          ltrim( ++rule);
                          auto lc = std::tolower( *rule);
                          if( lc == 'r')
						  {
							auto [ x, y, z] = parseFloats<3>( rule += 2);
							ccolor.gradient.reset( new RadialGradient( x, y, z));
                            if( ltrim( rule) && *rule == ')')
                              ++rule;
						  }
                          else if( lc == 'c')
                            ccolor.gradient.reset( new ConicGradient{
                              generateConicGradient( ++rule, ccolor, bkroot)});
						}
                        if( ltrim( rule) && *rule == '-' && std::tolower( rule[ 1]) == 's')
                        {
                            auto[ x_pos, y_pos, spread] = parseFloats<3>( rule += 2);
                            ccolor.shadow = Vec3D(( int)x_pos, ( int)y_pos, ( int)spread);
                            if( ltrim( rule) && *rule == ')')
                                ++rule;
                            else if( *rule == ',')
                            {
                                ccolor.shadow_color = extractColor( ++rule, bkroot);
                                if( ltrim( rule) && *rule == ')')
                                    ++rule;
                            }
                        }

                        if( ltrim( rule) && *rule == '+')
                        {
                            ccolor.soak = true;
                            ++rule;
                        }

                        fillEasingMode( ccolor.color_easing_fn, rule, bkroot, '}');
                    }
                    if( ltrim( rule) && *rule == '}')
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

void testColor( const char *rule, BKNode *bkroot)
{
    std::shared_ptr<KDNode> kdroot;
    {
        insert( kdroot, { RED( COLORHEX_ALICEBLUE), GREEN( COLORHEX_ALICEBLUE),
                          BLUE( COLORHEX_ALICEBLUE)}, 0);
        insert( kdroot, { RED( COLORHEX_ANTIQUEWHITE), GREEN( COLORHEX_ANTIQUEWHITE),
                          BLUE( COLORHEX_ANTIQUEWHITE)}, 1);
        insert( kdroot, { RED( COLORHEX_AQUA), GREEN( COLORHEX_AQUA),
                          BLUE( COLORHEX_AQUA)}, 2);
        insert( kdroot, { RED( COLORHEX_AQUAMARINE), GREEN( COLORHEX_AQUAMARINE),
                          BLUE( COLORHEX_AQUAMARINE)}, 3);
        insert( kdroot, { RED( COLORHEX_AZURE), GREEN( COLORHEX_AZURE),
                          BLUE( COLORHEX_AZURE)}, 4);
        insert( kdroot, { RED( COLORHEX_BEIGE), GREEN( COLORHEX_BEIGE),
                          BLUE( COLORHEX_BEIGE)}, 5);
        insert( kdroot, { RED( COLORHEX_BISQUE), GREEN( COLORHEX_BISQUE),
                          BLUE( COLORHEX_BISQUE)}, 6);
        insert( kdroot, { RED( COLORHEX_BLACK), GREEN( COLORHEX_BLACK),
                          BLUE( COLORHEX_BLACK)}, 7);
        insert( kdroot, { RED( COLORHEX_BLANCHEDALMOND), GREEN( COLORHEX_BLANCHEDALMOND),
                          BLUE( COLORHEX_BLANCHEDALMOND)}, 8);
        insert( kdroot, { RED( COLORHEX_BLUE), GREEN( COLORHEX_BLUE),
                          BLUE( COLORHEX_BLUE)}, 9);
        insert( kdroot, { RED( COLORHEX_BLUEVIOLET), GREEN( COLORHEX_BLUEVIOLET),
                          BLUE( COLORHEX_BLUEVIOLET)}, 10);
        insert( kdroot, { RED( COLORHEX_BROWN), GREEN( COLORHEX_BROWN),
                          BLUE( COLORHEX_BROWN)}, 11);
        insert( kdroot, { RED( COLORHEX_BURLYWOOD), GREEN( COLORHEX_BURLYWOOD),
                          BLUE( COLORHEX_BURLYWOOD)}, 12);
        insert( kdroot, { RED( COLORHEX_CADETBLUE), GREEN( COLORHEX_CADETBLUE),
                          BLUE( COLORHEX_CADETBLUE)}, 13);
        insert( kdroot, { RED( COLORHEX_CHARTREUSE), GREEN( COLORHEX_CHARTREUSE),
                          BLUE( COLORHEX_CHARTREUSE)}, 14);
        insert( kdroot, { RED( COLORHEX_CHOCOLATE), GREEN( COLORHEX_CHOCOLATE),
                          BLUE( COLORHEX_CHOCOLATE)}, 15);
        insert( kdroot, { RED( COLORHEX_CORAL), GREEN( COLORHEX_CORAL),
                          BLUE( COLORHEX_CORAL)}, 16);
        insert( kdroot, { RED( COLORHEX_CORNFLOWERBLUE), GREEN( COLORHEX_CORNFLOWERBLUE),
                          BLUE( COLORHEX_CORNFLOWERBLUE)}, 17);
        insert( kdroot, { RED( COLORHEX_CORNSILK), GREEN( COLORHEX_CORNSILK),
                          BLUE( COLORHEX_CORNSILK)}, 18);
        insert( kdroot, { RED( COLORHEX_CRIMSON), GREEN( COLORHEX_CRIMSON),
                          BLUE( COLORHEX_CRIMSON)}, 19);
        insert( kdroot, { RED( COLORHEX_CYAN), GREEN( COLORHEX_CYAN),
                          BLUE( COLORHEX_CYAN)}, 20);
        insert( kdroot, { RED( COLORHEX_DARKBLUE), GREEN( COLORHEX_DARKBLUE),
                          BLUE( COLORHEX_DARKBLUE)}, 21);
        insert( kdroot, { RED( COLORHEX_DARKCYAN), GREEN( COLORHEX_DARKCYAN),
                          BLUE( COLORHEX_DARKCYAN)}, 22);
        insert( kdroot, { RED( COLORHEX_DARKGOLDENROD), GREEN( COLORHEX_DARKGOLDENROD),
                          BLUE( COLORHEX_DARKGOLDENROD)}, 23);
        insert( kdroot, { RED( COLORHEX_DARKGRAY), GREEN( COLORHEX_DARKGRAY),
                          BLUE( COLORHEX_DARKGRAY)}, 24);
        insert( kdroot, { RED( COLORHEX_DARKGREY), GREEN( COLORHEX_DARKGREY),
                          BLUE( COLORHEX_DARKGREY)}, 25);
        insert( kdroot, { RED( COLORHEX_DARKGREEN), GREEN( COLORHEX_DARKGREEN),
                          BLUE( COLORHEX_DARKGREEN)}, 26);
        insert( kdroot, { RED( COLORHEX_DARKKHAKI), GREEN( COLORHEX_DARKKHAKI),
                          BLUE( COLORHEX_DARKKHAKI)}, 27);
        insert( kdroot, { RED( COLORHEX_DARKMAGENTA), GREEN( COLORHEX_DARKMAGENTA),
                          BLUE( COLORHEX_DARKMAGENTA)}, 28);
        insert( kdroot, { RED( COLORHEX_DARKOLIVEGREEN), GREEN( COLORHEX_DARKOLIVEGREEN),
                          BLUE( COLORHEX_DARKOLIVEGREEN)}, 29);
        insert( kdroot, { RED( COLORHEX_DARKORANGE), GREEN( COLORHEX_DARKORANGE),
                          BLUE( COLORHEX_DARKORANGE)}, 30);
        insert( kdroot, { RED( COLORHEX_DARKORCHID), GREEN( COLORHEX_DARKORCHID),
                          BLUE( COLORHEX_DARKORCHID)}, 31);
        insert( kdroot, { RED( COLORHEX_DARKRED), GREEN( COLORHEX_DARKRED),
                          BLUE( COLORHEX_DARKRED)}, 32);
        insert( kdroot, { RED( COLORHEX_DARKSALMON), GREEN( COLORHEX_DARKSALMON),
                          BLUE( COLORHEX_DARKSALMON)}, 33);
        insert( kdroot, { RED( COLORHEX_DARKSEAGREEN), GREEN( COLORHEX_DARKSEAGREEN),
                          BLUE( COLORHEX_DARKSEAGREEN)}, 34);
        insert( kdroot, { RED( COLORHEX_DARKSLATEBLUE), GREEN( COLORHEX_DARKSLATEBLUE),
                          BLUE( COLORHEX_DARKSLATEBLUE)}, 35);
        insert( kdroot, { RED( COLORHEX_DARKSLATEGRAY), GREEN( COLORHEX_DARKSLATEGRAY),
                          BLUE( COLORHEX_DARKSLATEGRAY)}, 36);
        insert( kdroot, { RED( COLORHEX_DARKSLATEGREY), GREEN( COLORHEX_DARKSLATEGREY),
                          BLUE( COLORHEX_DARKSLATEGREY)}, 37);
        insert( kdroot, { RED( COLORHEX_DARKTURQUOISE), GREEN( COLORHEX_DARKTURQUOISE),
                          BLUE( COLORHEX_DARKTURQUOISE)}, 38);
        insert( kdroot, { RED( COLORHEX_DARKVIOLET), GREEN( COLORHEX_DARKVIOLET),
                          BLUE( COLORHEX_DARKVIOLET)}, 39);
        insert( kdroot, { RED( COLORHEX_DEEPPINK), GREEN( COLORHEX_DEEPPINK),
                          BLUE( COLORHEX_DEEPPINK)}, 40);
        insert( kdroot, { RED( COLORHEX_DEEPSKYBLUE), GREEN( COLORHEX_DEEPSKYBLUE),
                          BLUE( COLORHEX_DEEPSKYBLUE)}, 41);
        insert( kdroot, { RED( COLORHEX_DIMGRAY), GREEN( COLORHEX_DIMGRAY),
                          BLUE( COLORHEX_DIMGRAY)}, 42);
        insert( kdroot, { RED( COLORHEX_DIMGREY), GREEN( COLORHEX_DIMGREY),
                          BLUE( COLORHEX_DIMGREY)}, 43);
        insert( kdroot, { RED( COLORHEX_DODGERBLUE), GREEN( COLORHEX_DODGERBLUE),
                          BLUE( COLORHEX_DODGERBLUE)}, 44);
        insert( kdroot, { RED( COLORHEX_FIREBRICK), GREEN( COLORHEX_FIREBRICK),
                          BLUE( COLORHEX_FIREBRICK)}, 45);
        insert( kdroot, { RED( COLORHEX_FLORALWHITE), GREEN( COLORHEX_FLORALWHITE),
                          BLUE( COLORHEX_FLORALWHITE)}, 46);
        insert( kdroot, { RED( COLORHEX_FORESTGREEN), GREEN( COLORHEX_FORESTGREEN),
                          BLUE( COLORHEX_FORESTGREEN)}, 47);
        insert( kdroot, { RED( COLORHEX_FUCHSIA), GREEN( COLORHEX_FUCHSIA),
                          BLUE( COLORHEX_FUCHSIA)}, 48);
        insert( kdroot, { RED( COLORHEX_GAINSBORO), GREEN( COLORHEX_GAINSBORO),
                          BLUE( COLORHEX_GAINSBORO)}, 49);
        insert( kdroot, { RED( COLORHEX_GHOSTWHITE), GREEN( COLORHEX_GHOSTWHITE),
                          BLUE( COLORHEX_GHOSTWHITE)}, 50);
        insert( kdroot, { RED( COLORHEX_GOLD), GREEN( COLORHEX_GOLD),
                          BLUE( COLORHEX_GOLD)}, 51);
        insert( kdroot, { RED( COLORHEX_GOLDENROD), GREEN( COLORHEX_GOLDENROD),
                          BLUE( COLORHEX_GOLDENROD)}, 52);
        insert( kdroot, { RED( COLORHEX_GRAY), GREEN( COLORHEX_GRAY),
                          BLUE( COLORHEX_GRAY)}, 53);
        insert( kdroot, { RED( COLORHEX_GREY), GREEN( COLORHEX_GREY),
                          BLUE( COLORHEX_GREY)}, 54);
        insert( kdroot, { RED( COLORHEX_GREEN), GREEN( COLORHEX_GREEN),
                          BLUE( COLORHEX_GREEN)}, 55);
        insert( kdroot, { RED( COLORHEX_GREENYELLOW), GREEN( COLORHEX_GREENYELLOW),
                          BLUE( COLORHEX_GREENYELLOW)}, 56);
        insert( kdroot, { RED( COLORHEX_HONEYDEW), GREEN( COLORHEX_HONEYDEW),
                          BLUE( COLORHEX_HONEYDEW)}, 57);
        insert( kdroot, { RED( COLORHEX_HOTPINK), GREEN( COLORHEX_HOTPINK),
                          BLUE( COLORHEX_HOTPINK)}, 58);
        insert( kdroot, { RED( COLORHEX_INDIANRED), GREEN( COLORHEX_INDIANRED),
                          BLUE( COLORHEX_INDIANRED)}, 59);
        insert( kdroot, { RED( COLORHEX_INDIGO), GREEN( COLORHEX_INDIGO),
                          BLUE( COLORHEX_INDIGO)}, 60);
        insert( kdroot, { RED( COLORHEX_IVORY), GREEN( COLORHEX_IVORY),
                          BLUE( COLORHEX_IVORY)}, 61);
        insert( kdroot, { RED( COLORHEX_KHAKI), GREEN( COLORHEX_KHAKI),
                          BLUE( COLORHEX_KHAKI)}, 62);
        insert( kdroot, { RED( COLORHEX_LAVENDER), GREEN( COLORHEX_LAVENDER),
                          BLUE( COLORHEX_LAVENDER)}, 63);
        insert( kdroot, { RED( COLORHEX_LAVENDERBLUSH), GREEN( COLORHEX_LAVENDERBLUSH),
                          BLUE( COLORHEX_LAVENDERBLUSH)}, 64);
        insert( kdroot, { RED( COLORHEX_LAWNGREEN), GREEN( COLORHEX_LAWNGREEN),
                          BLUE( COLORHEX_LAWNGREEN)}, 65);
        insert( kdroot, { RED( COLORHEX_LEMONCHIFFON), GREEN( COLORHEX_LEMONCHIFFON),
                          BLUE( COLORHEX_LEMONCHIFFON)}, 66);
        insert( kdroot, { RED( COLORHEX_LIGHTBLUE), GREEN( COLORHEX_LIGHTBLUE),
                          BLUE( COLORHEX_LIGHTBLUE)}, 67);
        insert( kdroot, { RED( COLORHEX_LIGHTCORAL), GREEN( COLORHEX_LIGHTCORAL),
                          BLUE( COLORHEX_LIGHTCORAL)}, 68);
        insert( kdroot, { RED( COLORHEX_LIGHTCYAN), GREEN( COLORHEX_LIGHTCYAN),
                          BLUE( COLORHEX_LIGHTCYAN)}, 69);
        insert( kdroot, { RED( COLORHEX_LIGHTGOLDENRODYELLOW), GREEN( COLORHEX_LIGHTGOLDENRODYELLOW),
                          BLUE( COLORHEX_LIGHTGOLDENRODYELLOW)}, 70);
        insert( kdroot, { RED( COLORHEX_LIGHTGRAY), GREEN( COLORHEX_LIGHTGRAY),
                          BLUE( COLORHEX_LIGHTGRAY)}, 71);
        insert( kdroot, { RED( COLORHEX_LIGHTGREY), GREEN( COLORHEX_LIGHTGREY),
                          BLUE( COLORHEX_LIGHTGREY)}, 72);
        insert( kdroot, { RED( COLORHEX_LIGHTGREEN), GREEN( COLORHEX_LIGHTGREEN),
                          BLUE( COLORHEX_LIGHTGREEN)}, 73);
        insert( kdroot, { RED( COLORHEX_LIGHTPINK), GREEN( COLORHEX_LIGHTPINK),
                          BLUE( COLORHEX_LIGHTPINK)}, 74);
        insert( kdroot, { RED( COLORHEX_LIGHTSALMON), GREEN( COLORHEX_LIGHTSALMON),
                          BLUE( COLORHEX_LIGHTSALMON)}, 75);
        insert( kdroot, { RED( COLORHEX_LIGHTSEAGREEN), GREEN( COLORHEX_LIGHTSEAGREEN),
                          BLUE( COLORHEX_LIGHTSEAGREEN)}, 76);
        insert( kdroot, { RED( COLORHEX_LIGHTSKYBLUE), GREEN( COLORHEX_LIGHTSKYBLUE),
                          BLUE( COLORHEX_LIGHTSKYBLUE)}, 77);
        insert( kdroot, { RED( COLORHEX_LIGHTSLATEGRAY), GREEN( COLORHEX_LIGHTSLATEGRAY),
                          BLUE( COLORHEX_LIGHTSLATEGRAY)}, 78);
        insert( kdroot, { RED( COLORHEX_LIGHTSLATEGREY), GREEN( COLORHEX_LIGHTSLATEGREY),
                          BLUE( COLORHEX_LIGHTSLATEGREY)}, 79);
        insert( kdroot, { RED( COLORHEX_LIGHTSTEELBLUE), GREEN( COLORHEX_LIGHTSTEELBLUE),
                          BLUE( COLORHEX_LIGHTSTEELBLUE)}, 80);
        insert( kdroot, { RED( COLORHEX_LIGHTYELLOW), GREEN( COLORHEX_LIGHTYELLOW),
                          BLUE( COLORHEX_LIGHTYELLOW)}, 81);
        insert( kdroot, { RED( COLORHEX_LIME), GREEN( COLORHEX_LIME),
                          BLUE( COLORHEX_LIME)}, 82);
        insert( kdroot, { RED( COLORHEX_LIMEGREEN), GREEN( COLORHEX_LIMEGREEN),
                          BLUE( COLORHEX_LIMEGREEN)}, 83);
        insert( kdroot, { RED( COLORHEX_LINEN), GREEN( COLORHEX_LINEN),
                          BLUE( COLORHEX_LINEN)}, 84);
        insert( kdroot, { RED( COLORHEX_MAGENTA), GREEN( COLORHEX_MAGENTA),
                          BLUE( COLORHEX_MAGENTA)}, 85);
        insert( kdroot, { RED( COLORHEX_MAROON), GREEN( COLORHEX_MAROON),
                          BLUE( COLORHEX_MAROON)}, 86);
        insert( kdroot, { RED( COLORHEX_MEDIUMAQUAMARINE), GREEN( COLORHEX_MEDIUMAQUAMARINE),
                          BLUE( COLORHEX_MEDIUMAQUAMARINE)}, 87);
        insert( kdroot, { RED( COLORHEX_MEDIUMBLUE), GREEN( COLORHEX_MEDIUMBLUE),
                          BLUE( COLORHEX_MEDIUMBLUE)}, 88);
        insert( kdroot, { RED( COLORHEX_MEDIUMORCHID), GREEN( COLORHEX_MEDIUMORCHID),
                          BLUE( COLORHEX_MEDIUMORCHID)}, 89);
        insert( kdroot, { RED( COLORHEX_MEDIUMPURPLE), GREEN( COLORHEX_MEDIUMPURPLE),
                          BLUE( COLORHEX_MEDIUMPURPLE)}, 90);
        insert( kdroot, { RED( COLORHEX_MEDIUMSEAGREEN), GREEN( COLORHEX_MEDIUMSEAGREEN),
                          BLUE( COLORHEX_MEDIUMSEAGREEN)}, 91);
        insert( kdroot, { RED( COLORHEX_MEDIUMSLATEBLUE), GREEN( COLORHEX_MEDIUMSLATEBLUE),
                          BLUE( COLORHEX_MEDIUMSLATEBLUE)}, 92);
        insert( kdroot, { RED( COLORHEX_MEDIUMSPRINGGREEN), GREEN( COLORHEX_MEDIUMSPRINGGREEN),
                          BLUE( COLORHEX_MEDIUMSPRINGGREEN)}, 93);
        insert( kdroot, { RED( COLORHEX_MEDIUMTURQUOISE), GREEN( COLORHEX_MEDIUMTURQUOISE),
                          BLUE( COLORHEX_MEDIUMTURQUOISE)}, 94);
        insert( kdroot, { RED( COLORHEX_MEDIUMVIOLETRED), GREEN( COLORHEX_MEDIUMVIOLETRED),
                          BLUE( COLORHEX_MEDIUMVIOLETRED)}, 95);
        insert( kdroot, { RED( COLORHEX_MIDNIGHTBLUE), GREEN( COLORHEX_MIDNIGHTBLUE),
                          BLUE( COLORHEX_MIDNIGHTBLUE)}, 96);
        insert( kdroot, { RED( COLORHEX_MINTCREAM), GREEN( COLORHEX_MINTCREAM),
                          BLUE( COLORHEX_MINTCREAM)}, 97);
        insert( kdroot, { RED( COLORHEX_MISTYROSE), GREEN( COLORHEX_MISTYROSE),
                          BLUE( COLORHEX_MISTYROSE)}, 98);
        insert( kdroot, { RED( COLORHEX_MOCCASIN), GREEN( COLORHEX_MOCCASIN),
                          BLUE( COLORHEX_MOCCASIN)}, 99);
        insert( kdroot, { RED( COLORHEX_NAVAJOWHITE), GREEN( COLORHEX_NAVAJOWHITE),
                          BLUE( COLORHEX_NAVAJOWHITE)}, 100);
        insert( kdroot, { RED( COLORHEX_NAVY), GREEN( COLORHEX_NAVY),
                          BLUE( COLORHEX_NAVY)}, 101);
        insert( kdroot, { RED( COLORHEX_OLDLACE), GREEN( COLORHEX_OLDLACE),
                          BLUE( COLORHEX_OLDLACE)}, 102);
        insert( kdroot, { RED( COLORHEX_OLIVE), GREEN( COLORHEX_OLIVE),
                          BLUE( COLORHEX_OLIVE)}, 103);
        insert( kdroot, { RED( COLORHEX_OLIVEDRAB), GREEN( COLORHEX_OLIVEDRAB),
                          BLUE( COLORHEX_OLIVEDRAB)}, 104);
        insert( kdroot, { RED( COLORHEX_ORANGE), GREEN( COLORHEX_ORANGE),
                          BLUE( COLORHEX_ORANGE)}, 105);
        insert( kdroot, { RED( COLORHEX_ORANGERED), GREEN( COLORHEX_ORANGERED),
                          BLUE( COLORHEX_ORANGERED)}, 106);
        insert( kdroot, { RED( COLORHEX_ORCHID), GREEN( COLORHEX_ORCHID),
                          BLUE( COLORHEX_ORCHID)}, 107);
        insert( kdroot, { RED( COLORHEX_PALEGOLDENROD), GREEN( COLORHEX_PALEGOLDENROD),
                          BLUE( COLORHEX_PALEGOLDENROD)}, 108);
        insert( kdroot, { RED( COLORHEX_PALEGREEN), GREEN( COLORHEX_PALEGREEN),
                          BLUE( COLORHEX_PALEGREEN)}, 109);
        insert( kdroot, { RED( COLORHEX_PALETURQUOISE), GREEN( COLORHEX_PALETURQUOISE),
                          BLUE( COLORHEX_PALETURQUOISE)}, 110);
        insert( kdroot, { RED( COLORHEX_PALEVIOLETRED), GREEN( COLORHEX_PALEVIOLETRED),
                          BLUE( COLORHEX_PALEVIOLETRED)}, 111);
        insert( kdroot, { RED( COLORHEX_PAPAYAWHIP), GREEN( COLORHEX_PAPAYAWHIP),
                          BLUE( COLORHEX_PAPAYAWHIP)}, 112);
        insert( kdroot, { RED( COLORHEX_PEACHPUFF), GREEN( COLORHEX_PEACHPUFF),
                          BLUE( COLORHEX_PEACHPUFF)}, 113);
        insert( kdroot, { RED( COLORHEX_PERU), GREEN( COLORHEX_PERU),
                          BLUE( COLORHEX_PERU)}, 114);
        insert( kdroot, { RED( COLORHEX_PINK), GREEN( COLORHEX_PINK),
                          BLUE( COLORHEX_PINK)}, 115);
        insert( kdroot, { RED( COLORHEX_PLUM), GREEN( COLORHEX_PLUM),
                          BLUE( COLORHEX_PLUM)}, 116);
        insert( kdroot, { RED( COLORHEX_POWDERBLUE), GREEN( COLORHEX_POWDERBLUE),
                          BLUE( COLORHEX_POWDERBLUE)}, 117);
        insert( kdroot, { RED( COLORHEX_PURPLE), GREEN( COLORHEX_PURPLE),
                          BLUE( COLORHEX_PURPLE)}, 118);
        insert( kdroot, { RED( COLORHEX_REBECCAPURPLE), GREEN( COLORHEX_REBECCAPURPLE),
                          BLUE( COLORHEX_REBECCAPURPLE)}, 119);
        insert( kdroot, { RED( COLORHEX_RED), GREEN( COLORHEX_RED),
                          BLUE( COLORHEX_RED)}, 120);
        insert( kdroot, { RED( COLORHEX_ROSYBROWN), GREEN( COLORHEX_ROSYBROWN),
                          BLUE( COLORHEX_ROSYBROWN)}, 121);
        insert( kdroot, { RED( COLORHEX_ROYALBLUE), GREEN( COLORHEX_ROYALBLUE),
                          BLUE( COLORHEX_ROYALBLUE)}, 122);
        insert( kdroot, { RED( COLORHEX_SADDLEBROWN), GREEN( COLORHEX_SADDLEBROWN),
                          BLUE( COLORHEX_SADDLEBROWN)}, 123);
        insert( kdroot, { RED( COLORHEX_SALMON), GREEN( COLORHEX_SALMON),
                          BLUE( COLORHEX_SALMON)}, 124);
        insert( kdroot, { RED( COLORHEX_SANDYBROWN), GREEN( COLORHEX_SANDYBROWN),
                          BLUE( COLORHEX_SANDYBROWN)}, 125);
        insert( kdroot, { RED( COLORHEX_SEAGREEN), GREEN( COLORHEX_SEAGREEN),
                          BLUE( COLORHEX_SEAGREEN)}, 126);
        insert( kdroot, { RED( COLORHEX_SEASHELL), GREEN( COLORHEX_SEASHELL),
                          BLUE( COLORHEX_SEASHELL)}, 127);
        insert( kdroot, { RED( COLORHEX_SIENNA), GREEN( COLORHEX_SIENNA),
                          BLUE( COLORHEX_SIENNA)}, 128);
        insert( kdroot, { RED( COLORHEX_SILVER), GREEN( COLORHEX_SILVER),
                          BLUE( COLORHEX_SILVER)}, 129);
        insert( kdroot, { RED( COLORHEX_SKYBLUE), GREEN( COLORHEX_SKYBLUE),
                          BLUE( COLORHEX_SKYBLUE)}, 130);
        insert( kdroot, { RED( COLORHEX_SLATEBLUE), GREEN( COLORHEX_SLATEBLUE),
                          BLUE( COLORHEX_SLATEBLUE)}, 131);
        insert( kdroot, { RED( COLORHEX_SLATEGRAY), GREEN( COLORHEX_SLATEGRAY),
                          BLUE( COLORHEX_SLATEGRAY)}, 132);
        insert( kdroot, { RED( COLORHEX_SLATEGREY), GREEN( COLORHEX_SLATEGREY),
                          BLUE( COLORHEX_SLATEGREY)}, 133);
        insert( kdroot, { RED( COLORHEX_SNOW), GREEN( COLORHEX_SNOW),
                          BLUE( COLORHEX_SNOW)}, 134);
        insert( kdroot, { RED( COLORHEX_SPRINGGREEN), GREEN( COLORHEX_SPRINGGREEN),
                          BLUE( COLORHEX_SPRINGGREEN)}, 135);
        insert( kdroot, { RED( COLORHEX_STEELBLUE), GREEN( COLORHEX_STEELBLUE),
                          BLUE( COLORHEX_STEELBLUE)}, 136);
        insert( kdroot, { RED( COLORHEX_TAN), GREEN( COLORHEX_TAN),
                          BLUE( COLORHEX_TAN)}, 137);
        insert( kdroot, { RED( COLORHEX_TEAL), GREEN( COLORHEX_TEAL),
                          BLUE( COLORHEX_TEAL)}, 138);
        insert( kdroot, { RED( COLORHEX_THISTLE), GREEN( COLORHEX_THISTLE),
                          BLUE( COLORHEX_THISTLE)}, 139);
        insert( kdroot, { RED( COLORHEX_TOMATO), GREEN( COLORHEX_TOMATO),
                          BLUE( COLORHEX_TOMATO)}, 140);
        insert( kdroot, { RED( COLORHEX_TURQUOISE), GREEN( COLORHEX_TURQUOISE),
                          BLUE( COLORHEX_TURQUOISE)}, 141);
        insert( kdroot, { RED( COLORHEX_VIOLET), GREEN( COLORHEX_VIOLET),
                          BLUE( COLORHEX_VIOLET)}, 142);
        insert( kdroot, { RED( COLORHEX_WHEAT), GREEN( COLORHEX_WHEAT),
                          BLUE( COLORHEX_WHEAT)}, 143);
        insert( kdroot, { RED( COLORHEX_WHITE), GREEN( COLORHEX_WHITE),
                          BLUE( COLORHEX_WHITE)}, 144);
        insert( kdroot, { RED( COLORHEX_WHITESMOKE), GREEN( COLORHEX_WHITESMOKE),
                          BLUE( COLORHEX_WHITESMOKE)}, 145);
        insert( kdroot, { RED( COLORHEX_YELLOW), GREEN( COLORHEX_YELLOW),
                          BLUE( COLORHEX_YELLOW)}, 146);
        insert( kdroot, { RED( COLORHEX_YELLOWGREEN), GREEN( COLORHEX_YELLOWGREEN),
                          BLUE( COLORHEX_YELLOWGREEN)}, 147);
    }

    auto getName = [ &kdroot]( uint32_t color)
    {
        double initial = INFINITY;
        return getColorNameAt(
                approximate( kdroot.get(), Color{ RED( color), GREEN( color), BLUE( color)}, initial)->index).data();
    };

    auto *given_rule = rule;
    uint64_t first_color{}, second_color{}, color_sum{};
    if( ltrim( rule) && *rule == '(')
        first_color = extractColor( ++rule, bkroot);
    else if( *rule != 0)
        first_color = extractColor( rule, bkroot);
    uint16_t ops{};
    if( ltrim( rule) && *rule && rule[ 1] == '/')
    {
        ops = MAKE_WORD( rule[ 2], *rule);
        rule += 3;
    }
    else if( compareOr<std::equal_to<char>>( *rule, '+', '-'))
        ops = *rule++;
    if( ops != 0)
    {
        second_color = extractColor( rule, bkroot);
        uint8_t alpha{ 0xff};
        if( ltrim( rule) && *rule == ':')
            alpha = getNumber( ++rule, 16);
        color_sum = MAKE_QWORD( HIGH_BYTE( ops) == '+' ?
                                sumMix( HIGH_DWORD( first_color), HIGH_DWORD( second_color)) | alpha :
                                subMix( HIGH_DWORD( first_color), HIGH_DWORD( second_color)) | alpha,
                                LOW_BYTE( ops) == '+' ?
                                sumMix( LOW_DWORD( first_color), LOW_DWORD( second_color)) | alpha :
                                subMix( LOW_DWORD( first_color), LOW_DWORD( second_color)) | alpha);
        if( compareOr<std::equal_to<char>>( LOW_BYTE( ops), '+', '-'))
            printf( "The result of the rule `%s` is thus:\n"
                    "Background colors: #%x(%s) %c #%x(%s) = #%x(%s)\n", given_rule,
                    LOW_DWORD( first_color), getName( LOW_DWORD( first_color)),
                    LOW_BYTE( ops), LOW_DWORD( second_color), getName( LOW_DWORD( second_color)),
                    LOW_DWORD( color_sum), getName( LOW_DWORD( color_sum)));
        if( compareOr<std::equal_to<char>>( HIGH_BYTE( ops), '+', '-'))
            printf( "Foreground colors: #%x(%s) %c #%x(%s) = #%x(%s)\n",
                    HIGH_DWORD( first_color), getName( HIGH_DWORD( first_color)),
                    HIGH_BYTE( ops), HIGH_DWORD( second_color), getName( HIGH_DWORD( second_color)),
                    HIGH_DWORD( color_sum), getName( HIGH_DWORD( color_sum)));
    }
    else
    {
        printf( "The result of the rule `%s` is thus:\n"
                "Background color: #%x(%s)\n", given_rule,
                LOW_DWORD( first_color), getName( LOW_DWORD( first_color)));
        printf( "Foreground color: #%x(%s)\n",
                HIGH_DWORD( first_color), getName( HIGH_DWORD( first_color)));
    }
}

uint32_t tintColor( uint32_t color, float factor)
{
    uint8_t red   = clamp( RED( color) * factor, 0, RGB_SCALE),
            green = clamp( GREEN( color) * factor, 0, RGB_SCALE),
            blue  = clamp( BLUE( color) * factor, 0, RGB_SCALE);

    return RGBA( red, green, blue, ALPHA( color));
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
	   { CLIP			 , CompositionRule::CompositionModel::Clip},
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

std::deque<CompositionRule::BlendModel> selectBlendModels( std::string_view given)
{
    if( given.empty())
        return {};
    auto s_models = partition( given, R"(\s*\|\s*)");
    if( s_models.empty())
        s_models.emplace_back( given.data());

    static const std::unordered_map<std::string_view, CompositionRule::BlendModel> possibilities
    {
        { BM_NORMAL,        CompositionRule::BlendModel::Normal},
        { BM_DISSOLVE,      CompositionRule::BlendModel::Dissolve},
        { BM_DARKEN,        CompositionRule::BlendModel::Darken},
        { BM_MULTIPLY,      CompositionRule::BlendModel::Multiply},
        { BM_COLOR_BURN,    CompositionRule::BlendModel::ColorBurn},
        { BM_LINEAR_BURN,   CompositionRule::BlendModel::LinearBurn},
        { BM_DARKER_COLOR,  CompositionRule::BlendModel::DarkerColor},
        { BM_LIGHTEN,       CompositionRule::BlendModel::Lighten},
        { BM_SCREEN,        CompositionRule::BlendModel::Screen},
        { BM_COLOR_DODGE,   CompositionRule::BlendModel::ColorDodge},
        { BM_LINEAR_DODGE,  CompositionRule::BlendModel::LinearDodge},
        { BM_LIGHTER_COLOR, CompositionRule::BlendModel::LighterColor},
        { BM_OVERLAY,       CompositionRule::BlendModel::Overlay},
        { BM_SOFT_LIGHT,    CompositionRule::BlendModel::SoftLight},
        { BM_HARD_LIGHT,    CompositionRule::BlendModel::HardLight},
        { BM_VIVID_LIGHT,   CompositionRule::BlendModel::VividLight},
        { BM_LINEAR_LIGHT,  CompositionRule::BlendModel::LinearLight},
        { BM_PIN_LIGHT,     CompositionRule::BlendModel::PinLight},
        { BM_HARD_MIX,      CompositionRule::BlendModel::HardMix},
        { BM_DIFFERENCE,    CompositionRule::BlendModel::Difference},
        { BM_EXCLUSION,     CompositionRule::BlendModel::Exclusion},
        { BM_SUBTRACT,      CompositionRule::BlendModel::Subtract},
        { BM_DIVIDE,        CompositionRule::BlendModel::Divide},
        { BM_HUE,           CompositionRule::BlendModel::Hue},
        { BM_SATURATION,    CompositionRule::BlendModel::Saturation},
        { BM_COLOR,         CompositionRule::BlendModel::Color},
        { BM_LUMINOSITY,    CompositionRule::BlendModel::Luminosity},
    };
    std::deque<CompositionRule::BlendModel> models;
    for( auto& s_model : s_models)
    {
        std::transform( s_model.cbegin(), s_model.cend(), s_model.begin(), tolower);
        auto index = possibilities.find( s_model);
        if( index == possibilities.cend())
            continue;
        models.push_back( index->second);
    }

    return models;
}

CompositionRule parseCompositionRule( std::string_view rule)
{
  std::cmatch match_results;
  //Match any of the following:
  //1. from 30deg, mode=source-over
  //2. from 30deg, at .5, 0.45, mode=source-over
  //3. mode=source-over or mode=lighter
  //4. blend=dissolve or blend
  std::regex matcher( R"(^(?:from\s+([+-]?\d{1,3})deg(?:\s+at\s+)"
					  R"(([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?),\s*([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?))?,\s*)?)"
                      R"(mode=([a-z]+(?:-[a-z]+)?)(?:,\s*blend=([a-z]+(?:-[a-z]+)?(?:\s*\|\s*[a-z]+(?:-[a-z]+)?)*))?$)",
                      std::regex_constants::icase);
  if( std::regex_match( rule.cbegin(), rule.cend(), match_results, matcher))
  {
    auto x_origin = match_results[ 2].str(),
         y_origin = match_results[ 3].str(),
         angle    = match_results[ 1].str();
	return {
		.c_model = selectCompositionModel( match_results[ 4].str()),
        .b_models = selectBlendModels( match_results[ 5].str()),
		.position = Vec2D( x_origin.size() ? std::stof( x_origin, nullptr) : 0.f,
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
                PropertyManager<const char *> font_manager( ( const char *)FcNameUnparse( font_set->fonts[ i]),
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

#if defined( CUSTOM_FONT_SUPPORTED)

std::pair<std::string, std::string> requestFontInfo( std::string_view font_file)
{
    if( !FcInit())
        return {};

    PropertyManager<void *> finalizer( ( void *)nullptr, []( auto *){ FcFini();});
    PropertyManager<FcFontSet *> font_set( FcFontSetCreate(), FcFontSetDestroy);
    PropertyManager<FcStrSet *> font_dirs( FcStrSetCreate(), FcStrSetDestroy);
    if( !FcFileScan( font_set.get(), font_dirs.get(), nullptr, nullptr, ( const FcChar8 *)font_file.data(), false))
        return {};

    if( font_set && font_set->nfont > 0)
    {
        PropertyManager<FcChar8*> font_family(
                FcPatternFormat( font_set->fonts[ 0], ( const FcChar8 *)"%{family}"), free);
        PropertyManager<FcChar8*> font_style(
                FcPatternFormat( font_set->fonts[ 0], ( const FcChar8 *)"%{style}"), free);
        return { ( const char *)font_family.get(), ( const char *)font_style.get()};
    }
    return {};
}

void installFont( std::string_view font_file)
{
    int code;
    zip_error_t zip_error;
    zip_error_init( &zip_error);
    PropertyManager<zip_error_t *>( &zip_error, zip_error_fini);
    PropertyManager<zip_t *> zipper( zip_open( FONT_ARCHIVE, ZIP_CREATE | ZIP_CHECKCONS, &code),
                                     []( auto *zipper) { if( ACCESSIBLE(  zipper)) zip_close( zipper);});
    if( code == -1)
        return;
    auto [ font_family, font_style] = requestFontInfo( font_file);
    if( font_family.empty() || font_style.empty())
        return;

    code = zip_dir_add( zipper.get(), font_family.c_str(), ZIP_FL_ENC_GUESS);
    if( code == -1 && zip_error.zip_err != ZIP_ER_EXISTS)
        return;

    PropertyManager<zip_source *> src( zip_source_file_create( font_file.data(), 0, -1, &zip_error),
                                       []( auto *src) { zip_source_close( src);  zip_source_free( src);});
    if( src.get() == nullptr)
        return;

    zip_source_keep( src.get());

    std::string modified_font_filename;
    auto *font_filename = strrchr( font_file.data(), '/');
    if( font_filename == nullptr)
        font_filename = font_file.data();
    else
        ++font_filename;

    std::string file( font_family);
    font_family.erase( std::remove_if( font_family.begin(), font_family.end(), isspace), font_family.end());
    if( strcasestr( font_filename, font_style.c_str()) == nullptr)
    {
        auto *p_ext = strrchr( font_filename, '.');
        if( ACCESSIBLE(  p_ext))
            modified_font_filename.append( font_family);
        modified_font_filename.append( "-");
        modified_font_filename.append( font_style);
        if( ACCESSIBLE(  p_ext))
            modified_font_filename.append( p_ext);
    }
    else
        modified_font_filename = font_filename;

    file.append( "/");
    file.append( modified_font_filename);
    code = zip_file_add( zipper.get(), file.c_str(), src.get(), ZIP_FL_ENC_GUESS);
    if( code == -1 && zip_error.zip_err == ZIP_ER_EXISTS)
    {
        printf( "Font file already exists do you want to overwrite(yes/no/y/n)? ");
        std::string reply;
        std::getline( std::cin, reply);
        std::transform( reply.begin(), reply.end(), reply.begin(), tolower);
        if( compareOr<std::equal_to<std::string>>( reply, "yes", "no", "y", "n"))
        {
            code = zip_file_add( zipper.get(), file.c_str(), src.get(), ZIP_FL_OVERWRITE);
            if( code == -1)
            {
                fprintf( stderr, "Unable to add font file: (An error occurred!\n");
                return;
            }
        }
    }
}

void executeActionOnFont( std::string_view font_name, FontActivity mode, std::function<void( std::vector<void *>)> fn)
{
    int code;
    zip_error_t zip_error;
    zip_error_init( &zip_error);
    PropertyManager<zip_error_t *> finalizer( &zip_error, zip_error_fini);
    PropertyManager<zip_t *> zipper( zip_open( FONT_ARCHIVE, ZIP_CREATE | ZIP_CHECKCONS, &code),
                                     []( auto *zipper) { if( ACCESSIBLE(  zipper)) zip_close( zipper);});
    if( code == -1)
        return;

    auto parts = partition( font_name, R"(\s*-\s*)");
    std::string style( "Regular");
    if( parts.empty())
        return;

    if( parts.size() > 1)
        style = parts[ 1];

    auto n_entries = zip_get_num_entries( zipper.get(), 0);
    if( n_entries == -1)
        return;

    zip_stat_t stat;
    zip_stat_init( &stat);

    std::string font_file( parts[ 0]);
    parts[ 0].erase( std::remove_if( parts[ 0].begin(), parts[ 0].end(), isspace), parts[ 0].end());
    font_file.append( "/");
    font_file.append( parts[ 0]);
    font_file.append( "-");
    font_file.append( style);
    int64_t i = 0;
    for( ; i < n_entries; ++i)
    {
        if( zip_stat_index( zipper.get(), i, 0, &stat) < 0
            || ( stat.valid & ZIP_STAT_NAME) == 0
            ||  *( stat.name + strlen( stat.name) - 1) == '/')
            continue;
        if( strcasestr( stat.name, font_file.c_str()) != nullptr)
            break;
    }

    if( ( stat.valid & ZIP_STAT_INDEX) == 0)
        return;

    std::vector<void *> params;
    if( mode == FontActivity::Delete)
    {
        params.push_back( ( void *)( zipper.get()));
        params.push_back( ( void *)&stat.index);
    }
    else if( mode == FontActivity::Read)
    {
        if( stat.size == 0)
            return;
        void *buffer = malloc( stat.size);
        if( buffer == nullptr)
            return;

        PropertyManager<zip_file_t *> handle( zip_fopen_index( zipper.get(), stat.index, ZIP_FL_UNCHANGED),
                                              []( auto *p){ if( p != nullptr) zip_fclose( p);});
        if( zip_fread( handle.get(), buffer, stat.size) != stat.size)
        {
            fprintf( stderr, "Read invalid number of bytes\n");
            return;
        }
        params.push_back( buffer);
        params.push_back( ( void *)&stat.size);
    }

    fn( params);
}

void uninstallFont( std::string_view font)
{
    executeActionOnFont( font, FontActivity::Delete, []( std::vector<void *> params)
    {
        auto *zipper = ( zip_t *)params[ 0];
        auto index = ( int64_t *)params[ 1];
        zip_delete( zipper, *index);
    });
}

std::pair<int64_t, std::unique_ptr<uint8_t, DeleterType>> useInstalledFont( std::string_view font_name)
{
    std::unique_ptr<uint8_t, DeleterType> information(( uint8_t *)nullptr, []( auto *p){ if( p) free( p);});
    int64_t size{ -1};
    executeActionOnFont( font_name, FontActivity::Read, [ &information, &size]( std::vector<void *> params)
    {
        information.reset(( uint8_t *)params[ 0]);
        size = *( int64_t *)params[ 1];
    });

    return { size, std::move( information)};
}

#endif

int main( int ac, char *av[])
{
   Timer::start();
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
#if defined( CUSTOM_FONT_SUPPORTED)
             *custom_font{ nullptr},
             *custom_font_action{ nullptr},
#endif
             *resolution{ nullptr},
             *padding_left{ nullptr},
             *padding_right{ nullptr},
             *padding_top{ nullptr},
             *padding_bottom{ nullptr},
             *test_rule{nullptr},
             *stroke_width{ nullptr},
             *line_height{ nullptr},
             *easing_direction{ nullptr},
             *font_size{ nullptr},
             *background_color{ nullptr},
             *final_size{ nullptr},
             *word,
             *program{ *av};

    while( --ac > 0 && ( *++av)[ 0] == '-')
    {
        const char *directive = *av + 1 + ( *( *av + 1) == '-'), // Allow for only one hyphen
                   *end = strchr( directive, '='),
                   *index = nullptr,
                   **selection = nullptr;
        size_t count = end == nullptr ? strlen( directive) : end - directive;

        if( FOUND_STRING( strcmp( directive, HELP_PROMPT)) || FOUND_STRING( strcmp( directive, SHORT( HELP_PROMPT))))
            goto help;
        else if( FOUND_STRING( strcmp( directive, LIST_FONTS)) || FOUND_STRING( strcmp( directive, SHORT( LIST_FONTS))))
        {
            puts( "Available fonts:\n");
            requestFontList();

            exit( EXIT_SUCCESS);
        }
        else if( FOUND_STRING( strcmp( directive, LIST_EASINGS))
         || FOUND_STRING( strcmp( directive, SHORT( LIST_EASINGS))))
        {
            printf( "Available easing functions:\n");
            printf( "%s\n", FN_IEASEINSINE);
            printf( "%s\n", FN_IEASEOUTSINE);
            printf( "%s\n", FN_IEASEINOUTSINE);
            printf( "%s\n", FN_IEASEINQUAD);
            printf( "%s\n", FN_IEASEOUTQUAD);
            printf( "%s\n", FN_IEASEINOUTQUAD);
            printf( "%s\n", FN_IEASEINCUBIC);
            printf( "%s\n", FN_IEASEOUTCUBIC);
            printf( "%s\n", FN_IEASEINOUTCUBIC);
            printf( "%s\n", FN_IEASEINQUART);
            printf( "%s\n", FN_IEASEOUTQUART);
            printf( "%s\n", FN_IEASEINOUTQUART);
            printf( "%s\n", FN_IEASEINQUINT);
            printf( "%s\n", FN_IEASEOUTQUINT);
            printf( "%s\n", FN_IEASEINOUTQUINT);
            printf( "%s\n", FN_IEASEINEXPO);
            printf( "%s\n", FN_IEASEOUTEXPO);
            printf( "%s\n", FN_IEASEINOUTEXPO);
            printf( "%s\n", FN_IEASEINCIRC);
            printf( "%s\n", FN_IEASEOUTCIRC);
            printf( "%s\n", FN_IEASEINOUTCIRC);
            printf( "%s\n", FN_IEASEINBACK);
            printf( "%s\n", FN_IEASEOUTBACK);
            printf( "%s\n", FN_IEASEINOUTBACK);
            printf( "%s\n", FN_IEASEINELASTIC);
            printf( "%s\n", FN_IEASEOUTELASTIC);
            printf( "%s\n", FN_IEASEINOUTELASTIC);
            printf( "%s\n", FN_IEASEINBOUNCE);
            printf( "%s\n", FN_IEASEOUTBOUNCE);
            printf( "%s\n", FN_IEASEINOUTBOUNCE);
        }
        else if( ( strlen( FONT_PROFILE) == count && FOUND_STRING( strncmp( directive, FONT_PROFILE, count)))
         || ( strlen( SHORT( FONT_PROFILE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( FONT_PROFILE), count))))
            selection = &font_profile;
        else if( ( strlen( COLOR_RULE) == count && FOUND_STRING( strncmp( directive, COLOR_RULE, count)))
         || ( strlen( SHORT( COLOR_RULE)) == count && FOUND_STRING( strncmp( directive, SHORT( COLOR_RULE), count))))
            selection = &business_rules.color_rule;
        else if( FOUND_STRING( strcmp( directive, OUTPUT))
         || FOUND_STRING( strcmp( directive, SHORT( OUTPUT))) && ac > 0)
        {
            ac -= 1;
            business_rules.src_filename = *++av;
#if defined( PNG_SUPPORTED) && defined( JPG_SUPPORTED)
            business_rules.out_format = isJPEG( business_rules.src_filename) ?
                                        OutputFormat::JPEG : business_rules.out_format;
#elif defined( PNG_SUPPORTED)
            business_rules.out_format = OutputFormat::PNG;
#elif defined( JPG_SUPPORTED)
            business_rules.out_format = OutputFormat::JPEG;
#endif
        }
#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)
        else if( FOUND_STRING( strcmp( directive, LIST_COMPOSITION_MODES))
         || FOUND_STRING( strcmp( directive, SHORT( LIST_COMPOSITION_MODES))))
        {
            printf( "Available composition methods:\n");
            printf( "%s\n", CLIP);
            printf( "%s\n", COPY);
            printf( "%s\n", DESTINATION_ATOP);
            printf( "%s\n", DESTINATION_IN);
            printf( "%s\n", DESTINATION_OVER);
            printf( "%s\n", DESTINATION_OUT);
            printf( "%s\n", LIGHTER);
            printf( "%s\n", SOURCE_ATOP);
            printf( "%s\n", SOURCE_IN);
            printf( "%s\n", SOURCE_OVER);
            printf( "%s\n", SOURCE_OUT);
            printf( "%s\n", XOR);
        }
        else if( FOUND_STRING( strcmp( directive, LIST_BLEND_MODES))
                 || FOUND_STRING( strcmp( directive, SHORT( LIST_BLEND_MODES))))
        {
            printf( "Available blend modes:\n");
            printf( "    Normal Group:\n");
            printf( "%s\n",  BM_NORMAL);
            printf( "%s\n",  BM_DISSOLVE);
            printf( "    Darken Group:\n");
            printf( "%s\n",  BM_DARKEN);
            printf( "%s\n",  BM_MULTIPLY);
            printf( "%s\n",  BM_COLOR_BURN);
            printf( "%s\n",  BM_LINEAR_BURN);
            printf( "%s\n",  BM_DARKER_COLOR);
            printf( "    Lighten Group:\n");
            printf( "%s\n",  BM_LIGHTEN);
            printf( "%s\n",  BM_SCREEN);
            printf( "%s\n",  BM_COLOR_DODGE);
            printf( "%s\n",  BM_LINEAR_DODGE);
            printf( "%s\n",  BM_LIGHTER_COLOR);
            printf( "    Contrast Group:\n");
            printf( "%s\n",  BM_OVERLAY);
            printf( "%s\n",  BM_SOFT_LIGHT);
            printf( "%s\n",  BM_HARD_LIGHT);
            printf( "%s\n",  BM_VIVID_LIGHT);
            printf( "%s\n",  BM_LINEAR_LIGHT);
            printf( "%s\n",  BM_PIN_LIGHT);
            printf( "%s\n",  BM_HARD_MIX);
            printf( "    Inversion Group:\n");
            printf( "%s\n",  BM_DIFFERENCE);
            printf( "%s\n",  BM_EXCLUSION);
            printf( "    Cancelation Group:\n");
            printf( "%s\n",  BM_SUBTRACT);
            printf( "%s\n",  BM_DIVIDE);
            printf( "    Component Group:\n");
            printf( "%s\n",  BM_HUE);
            printf( "%s\n",  BM_SATURATION);
            printf( "%s\n",  BM_COLOR);
            printf( "%s\n\n", BM_LUMINOSITY);
        }
        else if( ( strlen( COMPOSITION_RULE) == count && FOUND_STRING( strncmp( directive, COMPOSITION_RULE, count)))
         || ( strlen( SHORT( COMPOSITION_RULE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( COMPOSITION_RULE), count))))
		  selection = &business_rules.composition_rule;
        else if( ( strlen( COMPOSITION_IMAGE) == count && FOUND_STRING( strncmp( directive, COMPOSITION_IMAGE, count)))
         || ( strlen( SHORT( COMPOSITION_IMAGE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( COMPOSITION_IMAGE), count))))
            selection = &business_rules.dest_filename;
        else if( FOUND_STRING( strcmp( directive, AS_IMAGE)) || FOUND_STRING( strcmp( directive, SHORT( AS_IMAGE))))
            business_rules.as_image = true;
        else if( ( strlen( QUALITY_INDEX) == count && FOUND_STRING( strncmp( directive, QUALITY_INDEX, count)))
         || ( strlen( SHORT( QUALITY_INDEX)) == count
         && FOUND_STRING( strncmp( directive, SHORT( QUALITY_INDEX), count))))
            selection = &image_quality;
        else if( ( strlen( DPI) == count && FOUND_STRING( strncmp( directive, DPI, count)))
         || ( strlen( SHORT( DPI)) == count
         && FOUND_STRING( strncmp( directive, SHORT( DPI), count))))
            selection = &resolution;
        else if(( strlen( EASING_DIRECTION) == count && FOUND_STRING( strncmp( directive, EASING_DIRECTION, count)))
         || ( strlen( SHORT( EASING_DIRECTION)) == count
         && FOUND_STRING( strncmp( directive, SHORT( EASING_DIRECTION), count))))
            selection = &easing_direction;
        else if(( strlen( FINAL_SIZE) == count && FOUND_STRING( strncmp( directive, FINAL_SIZE, count)))
         || ( strlen( SHORT( FINAL_SIZE)) == count && FOUND_STRING( strncmp( directive, SHORT( FINAL_SIZE), count))))
            selection = &final_size;
#endif
        else if( ( strlen( FONT_SIZE) == count && FOUND_STRING( strncmp( directive, FONT_SIZE, count)))
         || ( strlen( SHORT( FONT_SIZE)) == count && FOUND_STRING( strncmp( directive, SHORT( FONT_SIZE), count))))
            selection = &font_size;
        else if( ( strlen( BACKGROUND_COLOR) == count && FOUND_STRING( strncmp( directive, BACKGROUND_COLOR, count)))
         || ( strlen( SHORT( BACKGROUND_COLOR)) == count
         && FOUND_STRING( strncmp( directive, SHORT( BACKGROUND_COLOR), count))))
          selection = &background_color;
        else if( ( strlen( DRAWING_CHARACTER) == count && FOUND_STRING( strncmp( directive, DRAWING_CHARACTER, count)))
         || ( strlen( SHORT( DRAWING_CHARACTER)) == count
         && FOUND_STRING( strncmp( directive, SHORT( DRAWING_CHARACTER), count))))
            selection = &business_rules.raster_glyph;
        else if( ( strlen( LINE_HEIGHT) == count && FOUND_STRING( strncmp( directive, LINE_HEIGHT, count)))
         || ( strlen( SHORT( LINE_HEIGHT)) == count && FOUND_STRING( strncmp( directive, SHORT( LINE_HEIGHT), count))))
            selection = &line_height;
        else if( ( strlen( STROKE_WIDTH) == count && FOUND_STRING( strncmp( directive, STROKE_WIDTH, count)))
         || ( strlen( SHORT( STROKE_WIDTH)) == count
         && FOUND_STRING( strncmp( directive, SHORT( STROKE_WIDTH), count))))
            selection = &stroke_width;
        else if( ( strlen( JUSTIFY) == count && FOUND_STRING( strncmp( directive, JUSTIFY, count)))
         || ( strlen( SHORT( JUSTIFY)) == count && FOUND_STRING( strncmp( directive, SHORT( JUSTIFY), count))))
            selection = &justification;
#if defined( CUSTOM_FONT_SUPPORTED)
        else if( ( strlen( UNINSTALL_FONT) == count && FOUND_STRING( strncmp( directive, UNINSTALL_FONT, count)))
         || ( strlen( SHORT( UNINSTALL_FONT)) == count
         && FOUND_STRING( strncmp( directive, SHORT( UNINSTALL_FONT), count))))
        {
            custom_font_action = "uninstall";
            selection = &custom_font;
        }
        else if( ( strlen( INSTALL_FONT) == count && FOUND_STRING( strncmp( directive, INSTALL_FONT, count)))
         || ( strlen( SHORT( INSTALL_FONT)) == count
         && FOUND_STRING( strncmp( directive, SHORT( INSTALL_FONT), count))))
        {
            custom_font_action = "install";
            selection = &custom_font;
        }
        else if( ( strlen( PADDING_LEFT) == count && FOUND_STRING( strncmp( directive, PADDING_LEFT, count)))
         || ( strlen( SHORT( PADDING_LEFT)) == count
         && FOUND_STRING( strncmp( directive, SHORT( PADDING_LEFT), count))))
            selection = &padding_left;
        else if( ( strlen( PADDING_RIGHT) == count && FOUND_STRING( strncmp( directive, PADDING_RIGHT, count)))
         || ( strlen( SHORT( PADDING_RIGHT)) == count
         && FOUND_STRING( strncmp( directive, SHORT( PADDING_RIGHT), count))))
            selection = &padding_right;
        else if( ( strlen( PADDING_TOP) == count && FOUND_STRING( strncmp( directive, PADDING_TOP, count)))
         || ( strlen( SHORT( PADDING_TOP)) == count && FOUND_STRING( strncmp( directive, SHORT( PADDING_TOP), count))))
            selection = &padding_top;
        else if( ( strlen( PADDING_BOTTOM) == count && FOUND_STRING( strncmp( directive, PADDING_BOTTOM, count)))
         || ( strlen( SHORT( PADDING_BOTTOM)) == count
         && FOUND_STRING( strncmp( directive, SHORT( PADDING_BOTTOM), count))))
            selection = &padding_bottom;
        else if( ( strlen( TEST_COLOR) == count && FOUND_STRING( strncmp( directive, TEST_COLOR, count)))
         || ( strlen( SHORT( TEST_COLOR)) == count && FOUND_STRING( strncmp( directive, SHORT( TEST_COLOR), count))))
            selection = &test_rule;
#endif
        if( selection != nullptr && ( index = strchr( directive, '=')) != nullptr)
            *selection = index + 1;
        else if( ACCESSIBLE(  selection))
        {
            ac = -1;
            break;
        }
    }

    if( ACCESSIBLE(  line_height))
        business_rules.line_height = strtof( line_height, nullptr);
    if( ACCESSIBLE(  stroke_width))
        business_rules.thickness = strtoul( stroke_width, nullptr, 10);
    if( ACCESSIBLE(  padding_left))
        business_rules.pad.left = strtol( padding_left, nullptr, 10);
    if( ACCESSIBLE(  padding_right))
        business_rules.pad.right = strtol( padding_right, nullptr, 10);
    if( ACCESSIBLE(  padding_top))
        business_rules.pad.top = strtol( padding_top, nullptr, 10);
    if( ACCESSIBLE(  padding_bottom))
        business_rules.pad.bottom = strtol( padding_bottom, nullptr, 10);

    if( ACCESSIBLE(  justification))
    {
        if( std::cmatch cm; std::regex_match( justification, justification + strlen( justification), cm,
            std::regex( R"(^(left)|(right)|(center)$)")), std::regex_constants::icase)
        {
            business_rules.j_mode = cm[ 1].matched ? Justification::Left
                                                   : cm[ 2].matched ? Justification::Right : Justification::Center;
        }
    }

#if defined( PNG_SUPPORTED) || defined( JPG_SUPPORTED)
    if( ACCESSIBLE(  resolution))
        business_rules.dpi = strtoul( resolution, nullptr, 10);

    if( ACCESSIBLE( image_quality))
        business_rules.image_quality = strtol( image_quality, nullptr, 10);

    if( ACCESSIBLE( final_size))
    {
        std::cmatch cm;
        std::regex key( R"(^\s*(?:(\d+)(w|h)|(\d+)\s*[x]\s*(\d+))\s*(bi(?:-)?linear|bi(?:-)?cubic)?\s*$)",
                        std::regex_constants::icase);
        if( std::regex_match( final_size, cm, key))
        {
            constexpr auto size  = 1,
                           side  = size + 1,
                           width  = side + 1,
                           height = width + 1,
                           interp = height + 1;
            constexpr const char *cubic = "cubic";
            auto auto_scale = cm[ size].matched;
            if( cm[ width].matched || auto_scale)
            {
                auto selection = std::tolower( cm[ side].str()[ 0]);
                business_rules.interpolation[ size - 1]  = auto_scale ? selection == 'h' ? -1 :
                                                            std::stoi( cm[ size].str()) : std::stoi( cm[ width].str());
                business_rules.interpolation[ side - 1] = auto_scale ? selection == 'w' ? -1 :
                                                            std::stoi( cm[ size].str()) : std::stoi( cm[ height].str());
                if( cm[ interp].matched)
                {
                    auto s_rep = cm[ interp].str();
                    business_rules.interpolation[ side] = FOUND_STRING( strcasestr( s_rep.data(), cubic));
                }
            }
        }
    }
#endif
    if( ACCESSIBLE(  background_color))
        business_rules.background_color = extractColor( background_color, business_rules.bkroot.get());

    if( ACCESSIBLE(  font_size))
        business_rules.font_size = strtol( font_size, nullptr, 10);

#if defined( CUSTOM_FONT_SUPPORTED)
    if( ACCESSIBLE(  custom_font))
        ( FOUND_STRING( strcmp( custom_font_action, "install")) ? installFont : uninstallFont)( custom_font);
#endif

    if( ACCESSIBLE(  easing_direction))
    {
        std::regex rule( R"(^(row)|(col)$)", std::regex_constants::icase);
        std::cmatch cm;
        if( std::regex_match( easing_direction, easing_direction + strlen( easing_direction), cm, rule))
            business_rules.ease_col = cm[ 2].matched;
    }

    if( ACCESSIBLE(  test_rule))
        testColor( test_rule, bkroot.get());

    if( ac == 1)
        word = *av;
    else if( test_rule == nullptr)
    {
        help:
        std::array<std::string_view, OPTIONS_COUNT> options =
        {
            STRUCTURE_PREFIX( LIST_FONTS),
            STRUCTURE_PREFIX( FONT_PROFILE),
            STRUCTURE_PREFIX( COLOR_RULE),
            STRUCTURE_PREFIX( FONT_SIZE),
            STRUCTURE_PREFIX( DRAWING_CHARACTER),
            STRUCTURE_PREFIX( AS_IMAGE),
            STRUCTURE_PREFIX( OUTPUT),
            STRUCTURE_PREFIX( LIST_EASINGS),
            STRUCTURE_PREFIX( LIST_COMPOSITION_MODES),
            STRUCTURE_PREFIX( LIST_BLEND_MODES),
            STRUCTURE_PREFIX( COMPOSITION_RULE),
            STRUCTURE_PREFIX( COMPOSITION_IMAGE),
            STRUCTURE_PREFIX( DPI),
            STRUCTURE_PREFIX( BACKGROUND_COLOR),
            STRUCTURE_PREFIX( LINE_HEIGHT),
            STRUCTURE_PREFIX( JUSTIFY),
            STRUCTURE_PREFIX( STROKE_WIDTH),
            STRUCTURE_PREFIX( FINAL_SIZE),
            STRUCTURE_PREFIX( UNINSTALL_FONT),
            STRUCTURE_PREFIX( INSTALL_FONT),
            STRUCTURE_PREFIX( QUALITY_INDEX),
            STRUCTURE_PREFIX( EASING_DIRECTION),
            STRUCTURE_PREFIX( PADDING_LEFT),
            STRUCTURE_PREFIX( PADDING_RIGHT),
            STRUCTURE_PREFIX( PADDING_TOP),
            STRUCTURE_PREFIX( PADDING_BOTTOM),
            STRUCTURE_PREFIX( TEST_COLOR),
            STRUCTURE_PREFIX( HELP_PROMPT)
        };
        std::array<std::string_view, OPTIONS_COUNT> options_message =
        {
            MESSAGE( LIST_FONTS),
            MESSAGE( FONT_PROFILE),
            MESSAGE( COLOR_RULE),
            MESSAGE( FONT_SIZE),
            MESSAGE( DRAWING_CHARACTER),
            MESSAGE( AS_IMAGE),
            MESSAGE( OUTPUT),
            MESSAGE( LIST_EASINGS),
            MESSAGE( LIST_COMPOSITION_MODES),
            MESSAGE( LIST_BLEND_MODES),
            MESSAGE( COMPOSITION_RULE),
            MESSAGE( COMPOSITION_IMAGE),
            MESSAGE( DPI),
            MESSAGE( BACKGROUND_COLOR),
            MESSAGE( LINE_HEIGHT),
            MESSAGE( JUSTIFY),
            MESSAGE( STROKE_WIDTH),
            MESSAGE( FINAL_SIZE),
            MESSAGE( UNINSTALL_FONT),
            MESSAGE( INSTALL_FONT),
            MESSAGE( QUALITY_INDEX),
            MESSAGE( EASING_DIRECTION),
            MESSAGE( PADDING_LEFT),
            MESSAGE( PADDING_RIGHT),
            MESSAGE( PADDING_TOP),
            MESSAGE( PADDING_BOTTOM),
            MESSAGE( TEST_COLOR),
            MESSAGE( HELP_PROMPT)
        };

        size_t max_length{};
        std::for_each( std::cbegin( options), std::cend( options),
                       [ &max_length]( auto each){ max_length = std::max( max_length, each.size());});

        auto *prog_ptr = strrchr( program, '/');
        fprintf( stderr, "Usage: %s [OPTION]... FILE|TEXT\n", prog_ptr == nullptr ? program : ++prog_ptr);
        fprintf( stderr, "Convert the content of FILE or TEXT into a format defined by OPTIONs\n\n");
        fprintf( stderr, "The following options can be used to tune the generator:\n");
        for( size_t j = 0, options_size = OPTIONS_COUNT; j < options_size; ++j)
        {
            auto [ help_lines, max_line] = expand( options_message[ j], Justification::Left);
            auto base_spacing = max_length + max_line + ALLOWANCE;
            std::cerr << options[ j] << std::setw( base_spacing - options[ j].size())
                      << help_lines[ 0] <<'\n';
            size_t idx = 1, help_lines_size = help_lines.size();
            while( idx < help_lines_size)
                std::cerr << std::setw( base_spacing) << help_lines[ idx++] <<'\n';
        }
        fprintf( stderr, "NB! The color names available are compliant with those defined\n"
                         "by CSS standard(https://www.w3.org/TR/css-color-3/)\n");
        exit( EXIT_FAILURE);
    }
    else
        exit( EXIT_SUCCESS);

    error = FT_Init_FreeType( &library.get());

    if( error != 0)
    {
      fprintf( stderr, "Unable to startup!");
      exit( EXIT_FAILURE);
    }

    std::unique_ptr<uint8_t, DeleterType> custom_profile(( uint8_t *)nullptr, []( auto *p){ if( p) free( p);});
    if( strrchr( font_profile, '.') == nullptr)
    {
        int64_t size;
        std::tie( size, custom_profile) = useInstalledFont( font_profile);
        if( !UNSET( size))
            error = FT_New_Memory_Face( library.get(), ( const FT_Byte *)custom_profile.get(), size, 0, &face.get());
        else
        {
            auto file = getFontFile( font_profile);
            if( file.empty())
            {
                fprintf( stderr, "Unable to parse font");
                exit( EXIT_FAILURE);
            }
            error = FT_New_Face( library.get(), file.c_str(), 0, &face.get());
        }
    }
    else
        error = FT_New_Face( library.get(), font_profile, 0, &face.get());

    if( error != 0)
    {
        fprintf( stderr, "Font file is invalid!");
        exit( EXIT_FAILURE);
    }

#if IS_LINUX && HAVE_SYS_STAT_H
    struct stat statbuf;
    auto code = stat( word, &statbuf);
    auto is_plain_text = false;
    auto is_file = false;
    std::string mime_type;
    if( code == 0 && S_ISREG( statbuf.st_mode))
    {
        is_file = true;
        using namespace std::string_literals;
        PropertyManager<FILE *> stream_handler( popen(( "file --mime-type "s + word).c_str(), "r"), pclose);
        if( stream_handler.get())
        {
            std::string report( RGB_SCALE, 0);
            fread( &report[ 0], 1, RGB_SCALE, stream_handler.get());
            is_plain_text = report.find( "text/plain") != std::string::npos;
            mime_type = report.substr( report.find( ": ") + 2);
        }
    }

    if( is_file && !is_plain_text)
    {
        std::cerr << "Invalid file specified: " << word << ".\nExpected: text/plain, got: " << mime_type;
        exit( EXIT_FAILURE);
    }

    if( is_plain_text)
    {
#else
    auto ext_pos = strrchr( word, '.');
    if( ext_pos != nullptr && strcasecmp( ext_pos + 1, "txt") == 0)
    {
#endif
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