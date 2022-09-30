#include <iostream>
#include "copywrite.hpp"
#include "renderers/text_colorizer.hpp"
#include "geometry/geo_vector.hpp"
#include "utils/color_utils.hpp"
#include "utils/utils.hpp"

TextColorizer::TextColorizer( ApplicationDirector& manager, TextRenderer &text_renderer)
: app_manager_( manager), text_renderer_( text_renderer)
{
}

FrameBuffer<uint32_t> TextColorizer::paint()
{
    auto frame = text_renderer_.render();
    paintShadow( frame);
    paintText( frame);

    return frame;
}

void TextColorizer::paintShadow( FrameBuffer<uint32_t> &frame)
{
    auto& rasters = text_renderer_.getRasters();
    auto& row_details = text_renderer_.getRowDetails();
    auto n_glyphs = rasters.size();
    auto n_levels = FLOAT_CAST( row_details.size());
    for( auto& raster : rasters)
    {
        auto& [ main, outline] = raster.spans;
        std::clog << "RasterIDX: " << raster.level << ", n_points: " << outline.size() <<'\n';
        auto& rect  = raster.bbox;
        int width   = raster.is_graph ? raster.bbox.width()  : INT_CAST( raster.advance.x),
                height  = raster.is_graph ? raster.bbox.height() : INT_CAST( raster.advance.y);
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
                    end   = UNSET( match->end.x) ? std::max( INT_CAST( n_glyphs) / INT_CAST( n_levels) - 1, 1)
                                                 : match->end.x - match->start.x;
            auto fraction = match->font_easing_fn( FLOAT_CAST( start) / FLOAT_CAST( end));
            fraction = Util::clamp( fraction, 0.f, 1.f);
            bounce_disp = INT_CAST( FLOAT_CAST( match->max_bounce) * fraction);
        }

        int v_offset = row_detail.v_disp - height - row_detail.max_descent - rect.ymin
                       + app_manager_.pad.top + INT_CAST(( match->shadow.y >= 0) * match->shadow.y) - bounce_disp,
                h_offset = INT_CAST( ENUM_CAST( app_manager_.j_mode) > 0)
                           * (( frame.width - row_detail.width + row_detail.max_shadow_y)
                              / ENUM_CAST( app_manager_.j_mode))
                           + app_manager_.pad.left + INT_CAST(( match->shadow.x >= 0) * match->shadow.x);
        for ( auto& s: outline)
        {
            for ( int w = 0; w < s.width; ++w)
            {
                frame.buffer.get()[ (( v_offset + height - 1 - ( s.y - rect.ymin) + pen.y)
                                     * frame.width + s.x - rect.xmin + w + h_offset + pen.x)]
                        = match->shadow_color;
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

void TextColorizer::paintText( FrameBuffer<uint32_t> &frame)
{
    auto& rasters     = text_renderer_.getRasters();
    auto& row_details = text_renderer_.getRowDetails();
    auto n_glyphs     = rasters.size();
    auto n_levels     = INT_CAST( row_details.size());
    int acc_dim{};
    auto iter = std::max_element( row_details.cbegin(), row_details.cend(),
                                  []( auto& left, auto& right){ return right.length > left.length; });
    auto max_length = iter != row_details.cend() ? iter->length : 0;
    for( auto& raster : rasters)
    {
        auto& [ main, outline] = raster.spans;
        auto& rect  = raster.bbox;
        int width   = raster.is_graph ? raster.bbox.width()  : INT_CAST( raster.advance.x),
                height  = raster.is_graph ? raster.bbox.height() : INT_CAST( raster.advance.y);
        auto length = app_manager_.ease_col ? height : width;
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
                acc_dim = app_manager_.ease_col ? row_detail.v_disp - height
                                                : ( raster.pos.x != 1) * ( acc_dim + length);
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
                                            ? std::max( INT_CAST( n_glyphs) / n_levels - 1, 1)
                                            : match->end.x - match->start.x,
                                            UNSET( match->end.y)
                                            ? std::max( n_levels - 1, 1) : match->end.y - match->start.y);
                auto fraction = match->color_easing_fn( app_manager_.ease_col ?
                                                        FLOAT_CAST( start.y) / FLOAT_CAST( end.y)
                                                                              : FLOAT_CAST( start.x) / FLOAT_CAST( end.x));
                inner_color   = ColorUtil::interpolateColor( LOW_DWORD( match->scolor),
                                                             LOW_DWORD( match->ecolor), fraction);
                outline_color = ColorUtil::interpolateColor( HIGH_DWORD( match->scolor),
                                                             HIGH_DWORD( match->ecolor), fraction);
            }
            else
            {
                auto extent = app_manager_.ease_col ? match->gradient->height : match->gradient->width;
                acc_dim = app_manager_.ease_col ? row_detail.v_disp - height
                                                : ( raster.pos.x != 1) * ( acc_dim + length);
                row_colors.reset( new uint64_t[ length]);
                for( int i = 0; i < length; ++i)
                {
                    auto fraction = match->color_easing_fn( FLOAT_CAST( i + acc_dim) / FLOAT_CAST( extent));
                    inner_color   = ColorUtil::interpolateColor( LOW_DWORD( match->scolor),
                                                                 LOW_DWORD( match->ecolor), fraction);
                    outline_color = ColorUtil::interpolateColor( HIGH_DWORD( match->scolor),
                                                                 HIGH_DWORD( match->ecolor), fraction);
                    row_colors.get()[ i] = MAKE_QWORD( outline_color, inner_color);
                }
                /*
                 * Add text advance gap to character to arrive at appropriate color easing value.
                 */
                acc_dim += app_manager_.ease_col ? INT_CAST( raster.advance.y) - height
                                                 : INT_CAST( raster.advance.x) - width;
            }
        }
        /* v_offset: Aligns every row glyph to their respective baselines
        *  h_offset: The adjustment necessary to comply with `Justification::Right` and `Justification::Center`
        */
        int bounce_disp = 0;
        if( match->max_bounce > 0 && match->font_easing_fn)
        {
            auto start = raster.pos.x - match->start.x,
                    end   = UNSET( match->end.x) ? std::max( INT_CAST( n_glyphs) / n_levels - 1, 1)
                                                 : match->end.x - match->start.x;
            auto fraction = match->font_easing_fn( FLOAT_CAST( start) / FLOAT_CAST( end));
            fraction = Util::clamp( fraction, 0.f, 1.f);
            bounce_disp = INT_CAST( FLOAT_CAST( match->max_bounce * fraction));
        }
        int v_offset = row_detail.v_disp - height - row_detail.max_descent - rect.ymin
                       + app_manager_.pad.top - bounce_disp,
                h_offset = INT_CAST( ENUM_CAST( app_manager_.j_mode) > 0)
                           * (( frame.width - row_detail.width) / ENUM_CAST( app_manager_.j_mode))
                           + app_manager_.pad.left;
        auto buffer = frame.buffer.get();
        for ( auto& s: outline)
        {
            // Disable anti-aliasing for console based output
            auto intensity_scaling = app_manager_.as_image
                                     ? static_cast<float>( s.coverage) / static_cast<float>( RGB_SCALE) : 1;
            for ( int w = 0; w < s.width; ++w)
            {
                auto& dest = buffer[ (( v_offset + height - 1 - ( s.y - rect.ymin) + pen.y)
                                      * frame.width + s.x - rect.xmin + w + h_offset + pen.x)];
                int i = s.x - rect.xmin + w,
                        j = s.y - rect.ymin,
                        color_index = app_manager_.ease_col ? height - 1 - j : i;
                if( app_manager_.thickness > 0)
                {
                    if( match->color_easing_fn)
                    {
                        // Paint outline
                        if( match->gradient->gradient_type == GradientType::Linear)
                            dest = ColorUtil::tintColor( match->soak ? HIGH_DWORD( row_colors.get()[ color_index])
                                                                     : outline_color, intensity_scaling);
                        else
                            dest = ColorUtil::tintColor( easeColor( raster, row_detail,
                                                    Vec2D<int>( max_length, n_levels),
                                                    { i + h_offset, height - 1 - j}, pen,
                                                    { HIGH_DWORD( match->scolor), HIGH_DWORD( match->ecolor)}, true),
                                                         intensity_scaling);
                    }
                    else
                        dest = ColorUtil::tintColor( outline_color, intensity_scaling);
                }
                if( i >= app_manager_.thickness && i < ( width - app_manager_.thickness)) // Paint fill
                {
                    int m_index = ( height - 1 - j - INT_CAST( app_manager_.thickness)) * main.width
                                  + ( i - INT_CAST( app_manager_.thickness));
                    if( m_index >= 0 && m_index < main.width * main.height)
                    {
                        if ( main.buffer.get()[ m_index] > 0)
                        {
                            if ( match->color_easing_fn)
                            {
                                if ( match->gradient->gradient_type == GradientType::Linear)
                                    dest = ColorUtil::tintColor( match->soak ?
                                                                 LOW_DWORD( row_colors.get()[ color_index])
                                                                             : inner_color, intensity_scaling);
                                else
                                    dest = ColorUtil::tintColor( easeColor( raster, row_detail,
                                                        Vec2D<int>( max_length, n_levels),
                                                        { i + h_offset, height - 1 - j}, pen,
                                                        { LOW_DWORD( match->scolor), LOW_DWORD( match->ecolor)}, false),
                                                         intensity_scaling);
                            }
                            else
                                dest = ColorUtil::tintColor( inner_color, intensity_scaling);
                        }
                    }
                }
            }
        }

        pen.x += raster.advance.x;
        pen.y += raster.advance.y;
    }
}

uint32_t TextColorizer::easeColor( const MonoGlyph &raster, const RowDetail &row_detail, Vec2D<int> size,
                           Vec2D<int> pos, FT_Vector pen, Vec2D<uint32_t> color_shift, bool is_outline)
{
    auto& match = raster.match;
    auto length = row_detail.length;
    auto glyph_width = raster.bbox.width();
    auto glyph_height = raster.bbox.height();
    auto color = is_outline ? HIGH_DWORD( match->scolor) : LOW_DWORD( match->scolor);
    auto start = Vec2D<int>( raster.pos.x - match->start.x, raster.pos.y - match->start.y),
            end    = Vec2D<int>( UNSET( match->end.x) ? std::max<int>( size.x - 1, 1)
                                                      : match->end.x - match->start.x,
                                 UNSET( match->end.y) ? std::max<int>( size.y - 1, 1)
                                                      : match->end.y - match->start.y);
    auto cwidth  = match->soak ? match->gradient->width : INT_CAST( end.x),
            cheight = match->soak ? match->gradient->height : INT_CAST( end.y);
    if( match->gradient->gradient_type == GradientType::Radial)
    {
        auto& props = reinterpret_cast<RadialGradient *>( match->gradient.get())->props;
        /*
         * Computes the radial-gradient of color starting at `match->gradient->startx`
         */
        auto xy = Vec2D<float>( match->soak ? FLOAT_CAST( pos.x + pen.x) : FLOAT_CAST( start.x),
                                match->soak ? FLOAT_CAST( pos.y + row_detail.v_disp - glyph_height)
                                            : FLOAT_CAST( start.y))
                  / Vec2D<float>( FLOAT_CAST( cwidth), FLOAT_CAST( cheight));
        xy -= Vec2D<float>( props.x, props.y); // Adjust all pixels to push the users origin at (0,0)
        auto scale = ( float)cwidth / ( float)cheight;
        if( cwidth > cheight)         // Converts the resulting elliptical shape into a circle.
            xy.x *= scale;
        else
            xy.y *= scale;
        auto d = xy.length();
        /*
         *  Adjust the spread to smoothen out edges
         */
        auto spread = props.z * FLOAT_CAST( std::sqrt( .25f * scale * scale + .25f));
        auto left   = spread - .2f < 0 ? spread : spread - .2f;
        auto right  = spread - .2f < 0 ? spread : spread + .2f;
        auto c = Util::smoothstep( left, right, d); // Spread the circle around to form a smooth gradient.
        color = ColorUtil::colorLerp( color_shift.y, color_shift.x, match->color_easing_fn( c));
    }
    else if( match->gradient->gradient_type == GradientType::Conic)
    {
        auto gradient = reinterpret_cast<ConicGradient *>( match->gradient.get());
        auto origin = gradient->origin.cast(); // Object construction does not call assignment operator
        Vec2D<float> center( FLOAT_CAST( cwidth) / 2.f, FLOAT_CAST( cheight) / 2.f);
        if( gradient->origin.changed()) // Explicit access is needed is `origin` object is not copy assigned
            center = Vec2D<float>( origin.x * FLOAT_CAST( cwidth - 1), origin.y * FLOAT_CAST( cheight - 1));
        auto diff = ( Vec2D<float>( match->soak ? FLOAT_CAST( pos.x + pen.x)
                                                : ( FLOAT_CAST( start.x) / FLOAT_CAST( length)) * FLOAT_CAST( size.x),
                                    match->soak ? FLOAT_CAST( pos.y + pen.y + row_detail.v_disp - glyph_height)
                                                : FLOAT_CAST( start.y)) - center);
        float angle = diff.angle();
        auto stops = gradient->color_variations;
        if( !stops.empty())
        {
            auto cur_stop = *stops.cbegin(), next_stop = cur_stop;
            for( size_t i = 0, n_stops = stops.size() - 1; i < n_stops; ++i)
            {
                auto &stop  = stops[ i],
                        &other = stops[ i + 1];
                if( angle >= FLOAT_CAST( stop.second) && angle < FLOAT_CAST( other.second))
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
                auto fraction = ( angle - FLOAT_CAST( cur_stop.second))
                                / std::abs( FLOAT_CAST( next_stop.second - cur_stop.second));
                if( match->color_easing_fn)
                    fraction = match->color_easing_fn( fraction);
                return is_outline ? ColorUtil::colorLerp( HIGH_DWORD( cur_stop.first),
                                                          HIGH_DWORD( next_stop.first), fraction)
                                  : ColorUtil::colorLerp( LOW_DWORD( cur_stop.first),
                                                          LOW_DWORD( next_stop.first), fraction);
            }
        }
    }

    return color;
}