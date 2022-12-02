#include <random>
#include "copywrite.hpp"
#include "plugins/plugin.hpp"
#include "plugins/plugin_manager.hpp"
#include "renderers/render-plugins/layer_renderer.hpp"
#include "parsers/rule_parser.hpp"
#include "plugins/image_manager.hpp"
#include "geometry/geo_vector.hpp"
#include "utils/utils.hpp"
#include "codecs/color_space_converter.hpp"
#include "blend_defs.hpp"

LayerRenderer::LayerRenderer( ApplicationDirector& manager, RuleParser& parser)
: Plugin( LAYER_RENDERER), app_manager_( manager),
parser_( parser),
c_rules_( ACCESSIBLE( manager.composition_rule)
          ? RuleParser::parseCompositionRule( manager.composition_rule) : decltype( c_rules_){})
{
}

void LayerRenderer::composite( FrameBuffer<uint32_t> &s_frame)
{
    auto codec = dynamic_cast<ImageManager *>( PluginManager::instance()->get( IMAGE_MANAGER));
    if( codec == nullptr)
        return;
    auto frame_buffer = FrameBuffer<uint32_t>();
    for( auto& c_rule : c_rules_)
    {
        if( c_rule.c_model == MODEL_ENUM( NotApplicable))
            continue;

        auto d_frame = codec->readImage( c_rule.image);
        if( d_frame.buffer == nullptr)
            continue;

        useEffectsOn( s_frame, c_rule, CompositionRule::StickyArena::Top);
        useEffectsOn( d_frame, c_rule, CompositionRule::StickyArena::Base);

        int32_t dwidth  = d_frame.width,
                dheight = d_frame.height;
        auto swidth     = s_frame.width,
                sheight    = s_frame.height;

        auto position = c_rule.position;
        bool needs_adjustment = false;
        if( position.x == INFINITY)
        {
            position = { 0, 0};
            if( c_rule.snap.x != INFINITY)
            {
                position = c_rule.snap;
                needs_adjustment = true;
            }
        }

        auto pos    = Vec2D<float>( position.x * FLOAT_CAST( dwidth - 1)
                                    + ( 1 - position.x) * - FLOAT_CAST( swidth)  + 1,
                                    position.y * FLOAT_CAST( dheight - 1)
                                    + ( 1 - position.y) * - FLOAT_CAST( sheight) + 1);
        // Adjust in case the positioning given by the user is a `snapping` type.
        if( needs_adjustment)
        {
            if( !EQUAL( position.x, .5))
            {
                if( position.x > .5)
                    pos.x -= FLOAT_CAST( swidth);
                else
                    pos.x += FLOAT_CAST( swidth);
            }
            if( !EQUAL( position.y, .5))
            {
                if( position.y > .5)
                    pos.y -= FLOAT_CAST( sheight);
                else
                    pos.y += FLOAT_CAST( sheight);
            }
        }
        auto center = Vec2D<float>( pos.x + FLOAT_CAST( swidth) / 2.f, pos.y + FLOAT_CAST( sheight) / 2.f);

        // Defines the rotated corners of the given image.
        std::array<Vec2D<float>, 4> corners = {
            ( pos                                             - center).rotate( FLOAT_CAST( c_rule.angle)) + center,
            ( pos + Vec2D<float>( FLOAT_CAST( swidth) - 1.f, 0)
              - center).rotate( FLOAT_CAST( c_rule.angle)) + center,
            ( pos + Vec2D<float>( FLOAT_CAST( swidth) - 1.f, FLOAT_CAST( sheight) - 1.f)
              - center).rotate( FLOAT_CAST( c_rule.angle)) + center,
            ( pos + Vec2D<float>( 0, FLOAT_CAST( sheight - 1.f))
              - center).rotate( FLOAT_CAST( c_rule.angle)) + center
        };

        // Defines the corners of the destination image
        std::array<Vec2D<float>, 4> big_corners = {
                Vec2D<float>( 0, 0),
                Vec2D<float>( FLOAT_CAST( dwidth) - 1, 0),
                Vec2D<float>( FLOAT_CAST( dwidth) - 1, FLOAT_CAST( dheight) - 1),
                Vec2D<float>( 0, FLOAT_CAST( dheight) - 1)
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

        // Select the extreme ends of the rotated box
        std::tie( x_min, y_min)  = cherryPick( corners);
        std::tie( x_max, y_max)	 = cherryPick( corners, false);

        auto angle                = ( y_min - x_min).angle();
        auto smallbox_top_edge    = Vec2D<float>((( y_min - x_min).rotate( -angle) + x_min).x, y_min.y);
        auto origin      		 = smallbox_top_edge;
        float width      		 = x_max.x - x_min.x + 1,
                height     		 = y_max.y - y_min.y + 1;
        int   final_width,
                final_height;

        if( COMPOSITON_SIZE( c_rule.c_model))
        { // The final canvas size is the maximum of the width and height of both canvas
            auto bottom_edge = Vec2D<float>( std::max( origin.x + width  - 1.f,  FLOAT_CAST( dwidth)  - 1.f),
                                             std::max( origin.y + height - 1.f, FLOAT_CAST( dheight)  - 1.f));
            origin.x         = std::min( origin.x, 0.f);
            origin.y         = std::min( origin.y, 0.f);
            final_width      = INT_CAST( bottom_edge.x - origin.x + 1.f);
            final_height     = INT_CAST( bottom_edge.y - origin.y + 1.f);
        }
        else if( COMPOSITION_SIDE( c_rule.c_model))
        { // The final canvas size is the size of the bounding box of the source canvas
            final_width  = INT_CAST( width);
            final_height = INT_CAST( height);
        }
        else
        { // The final canvas size is the size of the destination canvas
            origin       = big_corners[ 0];
            final_width  = dwidth;
            final_height = dheight;
        }

        frame_buffer.buffer = std::shared_ptr<uint32_t>(( uint32_t *)calloc( final_width * final_height,
                                                                             sizeof( uint32_t)), []( auto *p) { free( p);});
        auto out_buffer   = frame_buffer.buffer.get();
        frame_buffer.width = final_width,
        frame_buffer.height = final_height;

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
                for( int y = INT_CAST( smallbox_top_edge.y), j = 0; j < final_height; ++y, ++j)
                {
                    for ( int x = INT_CAST( smallbox_top_edge.x), i = 0; i < final_width; ++x, ++i)
                    {
                        auto point = Vec2D<float>( FLOAT_CAST( x), FLOAT_CAST( y));
                        if( intersects( corners, point))
                        {
                            auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle)) + center - pos;
                            int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                            if( index > 0 && index < s_frame_dimension)
                                out_buffer[  j * final_width + i] = s_frame.buffer.get()[ index];
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
                    for( int y = INT_CAST( smallbox_top_edge.y), j = 0; j < final_height; ++y, ++j)
                    {
                        for ( int x = INT_CAST( smallbox_top_edge.x), i = 0; i < final_width; ++x, ++i)
                        {
                            auto point = Vec2D<float>( FLOAT_CAST( x), FLOAT_CAST( y));
                            if constexpr ( std::is_same_v<T, Default>) // DestinationAtop
                            {
                                auto d_index = y * d_frame.width * d_frame.n_channel +  x * d_frame.n_channel;
                                auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle)) + center - pos;
                                int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                if(!c_rule.b_models.empty())
                                {
                                    bool corners_intersects = false;
                                    if(( corners_intersects = ( index >= 0 && index < s_frame_dimension
                                                                && intersects( corners, point)))
                                       && d_index < d_max_index && intersects( big_corners, point))
                                    {
                                        auto top = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                                         d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                                        auto base = s_frame.buffer.get()[ index];
                                        for( auto& blendFn : blendFns)
                                            top = blendFn( top, base);
                                        out_buffer[  j * final_width + i] = top;
                                    }
                                    else if( corners_intersects)
                                        out_buffer[  j * final_width + i] = s_frame.buffer.get()[ index];
                                }
                                else if( d_index < d_max_index && intersects( corners, point)
                                         && intersects( big_corners, point))
                                {
                                    out_buffer[  j * final_width + i] = RGBA( d_buffer[ d_index],
                                                                              d_buffer[d_index + 1],
                                                                              d_buffer[ d_index + 2],
                                                                              d_buffer[ d_index + 3]);
                                }
                                else if( intersects( corners, point))
                                {
                                    if( index >= 0 && index < s_frame_dimension)
                                        out_buffer[ j * final_width + i] = s_frame.buffer.get()[ index];
                                }
                            }
                            else if constexpr ( std::is_same_v<T, Top>) // DestinationIn
                            {
                                auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                                if( d_index < d_max_index && intersects( corners, point)
                                    && intersects( big_corners, point))
                                {
                                    auto top = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                                     d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                                    if( !c_rule.b_models.empty())
                                    {
                                        auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle))
                                                       + center - pos;
                                        int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                        auto base = s_frame.buffer.get()[ index];
                                        for( auto& blendFn : blendFns)
                                            top = blendFn( top, base);
                                    }
                                    out_buffer[ j * final_width + i] = top;
                                }
                            }
                            else if constexpr( std::is_same_v<T, Bottom>) // Source-Out
                            {
                                if( intersects( corners, point) && !intersects( big_corners, point))
                                {
                                    auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle))
                                                   + center - pos;
                                    int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                    if( index > 0 && index < s_frame_dimension)
                                        out_buffer[ j * final_width + i] = s_frame.buffer.get()[ index];
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
                    for( int y = INT_CAST( origin.y), j = 0; j < final_height; ++y, ++j)
                    {
                        for ( int x = INT_CAST( origin.x), i = 0; i < final_width; ++x, ++i)
                        {
                            auto point = Vec2D<float>( FLOAT_CAST( x), FLOAT_CAST( y));
                            auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle)) + center - pos;
                            if constexpr ( !std::is_same_v<T, Default>)
                            {
                                if( auto b_intersects = intersects( big_corners, point),
                                            c_intersects = intersects( corners, point);
                                        b_intersects && c_intersects)
                                {
                                    if constexpr( std::is_same_v<T, Bottom>) // Selection for Lighter
                                    {
                                        auto d_index = y * d_frame.width * d_frame.n_channel
                                                       + x * d_frame.n_channel;
                                        int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                        auto *buffer = d_frame.buffer.get();
                                        auto b_color = RGBA( buffer[ d_index], buffer[ d_index + 1],
                                                             buffer[ d_index + 2], buffer[ d_index + 3]);
                                        uint32_t c_color = b_color;
                                        if( index > 0 && index < s_frame_dimension)
                                            c_color = s_frame.buffer.get()[ index];
                                        auto final_color = ColorUtil::sumMix(b_color, c_color);
                                        auto alpha = ( uint16_t)( c_color & 0xFFu) + ( b_color & 0xFFu);
                                        final_color = final_color | std::min( 0xFFu, alpha);
                                        out_buffer[ j * final_width + i] = final_color;
                                    }
                                }
                                else if( b_intersects) // Bypass the intersection of source and destination
                                {
                                    auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                                    auto *buffer = d_frame.buffer.get();
                                    out_buffer[ j * final_width + i] = RGBA( buffer[ d_index], buffer[d_index + 1],
                                                                             buffer[ d_index + 2],
                                                                             buffer[ d_index + 3]);
                                }
                                else if( c_intersects)
                                {
                                    int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                    if( index > 0 && index < s_frame_dimension)
                                        out_buffer[ j * final_width + i] = s_frame.buffer.get()[ index];
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
                                out_buffer[ j * final_width + i] = top;
                            }
                            else if( intersects( big_corners, point)) // Selection for DestinationOver
                            {
                                auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                                auto *buffer = d_frame.buffer.get();
                                out_buffer[ j * final_width + i] = RGBA( buffer[ d_index], buffer[d_index + 1],
                                                                         buffer[ d_index + 2],
                                                                         buffer[ d_index + 3]);
                            }
                            else if( intersects( corners, point)) // Selection for DestinationOver
                            {
                                int index    = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                                if( index > 0 && index < s_frame_dimension)
                                    out_buffer[ j * final_width + i] = s_frame.buffer.get()[ index];
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
                    for( int y = INT_CAST( origin.y), j = 0; j < final_height; ++y, ++j)
                    {
                        for( int x = INT_CAST( origin.x), i = 0; i < final_width; ++x, ++i)
                        {
                            int s_index     = j * d_frame.width * d_frame.n_channel
                                              + i * d_frame.n_channel,
                                    d_index     = j * final_width + i;
                            auto *d_ptr     = d_frame.buffer.get();
                            uint32_t &pixel = out_buffer[  d_index];
                            auto point      = Vec2D<float>( FLOAT_CAST( x), FLOAT_CAST( y));
                            if constexpr ( std::is_same_v<T, Default>) // SourceAtop
                            {
                                if( intersects( corners, point) && intersects( big_corners, point))
                                {
                                    auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle))
                                                   + center - pos;
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
                                    auto s_coord = ( point - center).rotate( -c_rule.angle)
                                                   + center - pos;
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
                for( int y = INT_CAST( origin.y), j = 0; j < final_height; ++y, ++j)
                {
                    for( int x = INT_CAST( origin.x), i = 0; i < final_width; ++x, ++i)
                    {
                        auto point = Vec2D<float>( INT_CAST( x), INT_CAST( y));
                        auto s_coord = ( point - center).rotate( FLOAT_CAST( -c_rule.angle)) + center - pos;
                        if( intersects( corners, point))
                        {
                            int index = (int)s_coord.y * s_frame.width + (int)s_coord.x;
                            if( index > 0 && index < s_frame_dimension)
                            {
                                if( c_rule.b_models.empty())
                                {
                                    if( ALPHA( s_buffer[ index]) < 180 && intersects( big_corners, point))
                                    {
                                        auto d_index = y * d_frame.width * d_frame.n_channel
                                                       + x * d_frame.n_channel;
                                        out_buffer[ j * final_width + i] = RGBA( d_buffer[ d_index],
                                                                                 d_buffer[d_index + 1],
                                                                                 d_buffer[ d_index + 2],
                                                                                 d_buffer[ d_index + 3]);
                                    }
                                    else
                                        out_buffer[ j * final_width + i] = s_buffer[ index];
                                }
                                else if( intersects( big_corners, point))
                                {
                                    auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                                    auto top  = s_buffer[ index],
                                            base = RGBA( d_buffer[ d_index], d_buffer[ d_index + 1],
                                                         d_buffer[ d_index + 2], d_buffer[ d_index + 3]);
                                    for( auto& blendFn : blendFns)
                                        top = blendFn( top, base);
                                    out_buffer[ j * final_width + i] = top;
                                }
                                else
                                    out_buffer[ j * final_width + i] = s_buffer[ index];
                            }
                        }
                        else if( intersects( big_corners, point))
                        {
                            auto d_index = y * d_frame.width * d_frame.n_channel + x * d_frame.n_channel;
                            out_buffer[ j * final_width + i] = RGBA( d_buffer[ d_index], d_buffer[d_index + 1],
                                                                     d_buffer[ d_index + 2],
                                                                     d_buffer[ d_index + 3]);
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
        models[ ENUM_CAST( c_rule.c_model)]( Default());

        s_frame.buffer = std::move( frame_buffer.buffer);
        s_frame.width  = frame_buffer.width;
        s_frame.height = frame_buffer.height;

        useEffectsOn( s_frame, c_rule, CompositionRule::StickyArena::Both);

        if( c_rule.interpolation[ 0] && c_rule.interpolation[ 1])
            resize( s_frame, c_rule.interpolation);
    }

    if( s_frame.buffer == nullptr)
        return;
    if( app_manager_.interpolation[ 0] && app_manager_.interpolation[ 1])
        resize( s_frame, app_manager_.interpolation);
}

template <typename Tp>
void LayerRenderer::resize( FrameBuffer<Tp> &frame, int *interpolation)
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
                        a_row = INT_CAST( FLOAT_CAST( row * h_scale)),
                        a_col = INT_CAST( FLOAT_CAST( col * w_scale));
                auto index = a_row * o_width * n_channel + a_col * n_channel;
                if( a_row >= 0 && a_col >= 0 && index >= 0 && index < o_size)
                {
                    auto color = s_buffer[ index];
                    if constexpr ( sizeof( Tp) == 4)
                    {
                        red += RED( color);
                        green += GREEN( color);
                        blue += BLUE( color);
                        if( row - j == 0 && col - i == 0)
                            alpha = ALPHA( color);
                    }
                    else
                    {
                        red += s_buffer[ index];
                        green += s_buffer[ index + 1];
                        blue += s_buffer[ index + 2];
                        if( row - j == 0 && col - i == 0)
                            alpha = s_buffer[ index + 3];
                    }
                }
            }
            if constexpr ( sizeof( Tp) == 4)
            {
                auto index = j * new_width * n_channel + i * n_channel;
                d_buffer[ index] = RGBA(( red / n_neighbour),
                                        ( green / n_neighbour),
                                        ( blue / n_neighbour),
                                        alpha);
            }
            else
            {
                auto base_index = j * new_width * n_channel + i * n_channel;
                d_buffer[ base_index]     = INT_CAST( red / n_neighbour);
                d_buffer[ base_index + 1] = INT_CAST( green / n_neighbour);
                d_buffer[ base_index + 2] = INT_CAST( blue / n_neighbour);
                d_buffer[ base_index + 3] = alpha;
            }
        }
    }
    frame.width  = new_width;
    frame.height = new_height;
    frame.buffer = dest;
}

bool LayerRenderer::intersects( std::array<Vec2D<float>, 4> corners, Vec2D<float> test)

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

std::function<uint32_t( uint32_t, uint32_t)> LayerRenderer::selectBlendFn( CompositionRule::BlendModel model)
{
    static std::function<uint32_t( uint32_t, uint32_t)> selector[ NUMBER_OF_BLEND_MODES] =
    {
        []( auto top, auto base)
        {
            auto value = ( rand() % ( 0x100 - ALPHA( top)));
            return value >= 0 && value <= 10 ? top | 0xff : base;
        },
        []( auto top, auto base)
        {
            int t_rgb_min = std::min( std::min( RED( top), GREEN( top)), BLUE( top)),
                    b_rgb_min = std::min( std::min( RED( base), GREEN( base)), BLUE( base)),
                    t_rgb_max = std::max( std::max( RED( top), GREEN( top)), BLUE( top)),
                    b_rgb_max = std::max( std::max( RED( base), GREEN( base)), BLUE( base));
            return ( t_rgb_max + t_rgb_min) < ( b_rgb_max + b_rgb_min) ? top : base;
        },
        []( auto top, auto base)
        {
            uint8_t red   = (( float)RED( top) / RGB_SCALE   * ( float)RED( base) / RGB_SCALE) * RGB_SCALE,
                    green = (( float)GREEN( top) / RGB_SCALE * ( float)GREEN( base) / RGB_SCALE) * RGB_SCALE,
                    blue  = (( float)BLUE( top) / RGB_SCALE  * ( float)BLUE( base) / RGB_SCALE) * RGB_SCALE,
                    alpha = (( float)ALPHA( top) / RGB_SCALE * ( float)ALPHA( base) / RGB_SCALE) * RGB_SCALE;

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = RED( top) == 0 ? 0 :
                            ColorUtil::colorClamp( ( 1.f - ( FLOAT_CAST( RGB_SCALE - RED( base))
                                                             / FLOAT_CAST( RED( top)))) * RGB_SCALE),
                    green = GREEN( top) == 0 ? 0 :
                            ColorUtil::colorClamp( ( 1.f - ( FLOAT_CAST( RGB_SCALE - GREEN( base))
                                                             / FLOAT_CAST( GREEN( top)))) * RGB_SCALE),
                    blue  = BLUE( top) == 0 ? 0 :
                            ColorUtil::colorClamp( ( 1.f - ( FLOAT_CAST( RGB_SCALE - BLUE( base))
                                                             / FLOAT_CAST( BLUE( top)))) * RGB_SCALE),
                    alpha = ALPHA( top) == 0 ? 0 :
                            ColorUtil::colorClamp( ( 1.f - ( FLOAT_CAST( RGB_SCALE - ALPHA( base))
                                                             / FLOAT_CAST( ALPHA( top)))) * RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red_sum   = ColorUtil::colorClamp(( int16_t)RED( base) + RED( top) - RGB_SCALE),
                    green_sum = ColorUtil::colorClamp(( int16_t)GREEN( base) + GREEN( top) - RGB_SCALE),
                    blue_sum  = ColorUtil::colorClamp(( int16_t)BLUE( base) + BLUE( top) - RGB_SCALE),
                    alpha_sum = ColorUtil::colorClamp(( int16_t)ALPHA( base) + ALPHA( top) - RGB_SCALE);

            return RGBA( red_sum, green_sum, blue_sum, alpha_sum);
        },
        []( auto top, auto base)
        {
            constexpr auto factor = .5f;
            return ALPHA( top) > 180 ?
                   RGBA( ColorUtil::colorClamp( RED( top) * factor), ColorUtil::colorClamp( GREEN( top) * factor),
                         ColorUtil::colorClamp( BLUE( top) * factor), ALPHA( top))
                                     : RGBA( ColorUtil::colorClamp( RED( base) * factor),
                                             ColorUtil::colorClamp( GREEN( base) * factor),
                                             ColorUtil::colorClamp( BLUE( base) * factor), ALPHA( base));
        },
        []( auto top, auto base)
        {
            return selector[ ENUM_CAST( CompositionRule::BlendModel::Darken)]( top, base) == top ? base : top;
        },
        []( auto top, auto base)
        {
            constexpr auto factor = 0.00001538f;
            uint8_t red   = ( 1 - factor * (( RGB_SCALE - RED( base)) * ( RGB_SCALE - RED( top)))) * RGB_SCALE,
                    green = ( 1 - factor * (( RGB_SCALE - GREEN( base)) * ( RGB_SCALE - GREEN( top)))) * RGB_SCALE,
                    blue  = ( 1 - factor * (( RGB_SCALE - BLUE( base)) * ( RGB_SCALE - BLUE( top)))) * RGB_SCALE,
                    alpha = ( 1 - factor * (( RGB_SCALE - ALPHA( base)) * ( RGB_SCALE - ALPHA( top)))) * RGB_SCALE;

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = ( float)RED( base) * RGB_SCALE / ( RGB_SCALE - RED( top)),
                    green = ( float)GREEN( base) * RGB_SCALE / ( RGB_SCALE - GREEN( top)),
                    blue  = ( float)BLUE( base) * RGB_SCALE / ( RGB_SCALE - BLUE( top)),
                    alpha = ( float)ALPHA( base) * RGB_SCALE / ( RGB_SCALE - ALPHA( top));

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red_sum   = ColorUtil::colorClamp(( int16_t)RED( base) + RED( top)),
                    green_sum = ColorUtil::colorClamp(( int16_t)GREEN( base) + GREEN( top)),
                    blue_sum  = ColorUtil::colorClamp(( int16_t)BLUE( base) + BLUE( top)),
                    alpha_sum = ColorUtil::colorClamp(( int16_t)ALPHA( base) + ALPHA( top));

            return RGBA( red_sum, green_sum, blue_sum, alpha_sum);
        },
        []( auto top, auto base)
        {
            constexpr auto factor = 2.f;
            return ALPHA( top) > 180 ?
                   RGBA( ColorUtil::colorClamp( RED( top) * factor), ColorUtil::colorClamp( GREEN( top) * factor),
                         ColorUtil::colorClamp( BLUE( top) * factor), ALPHA( top))
                                     : RGBA( ColorUtil::colorClamp( RED( base) * factor),
											 ColorUtil::colorClamp( GREEN( base) * factor),
                                             ColorUtil::colorClamp( BLUE( base) * factor), ALPHA( base));
        },
        []( auto top, auto base)
        {
            uint8_t red   = Util::clamp((( RED( base) > 127.5)
							* ( 1 - ( 1 - 2 * ( ( float)RED( base) / RGB_SCALE - .5f))
                            * ( 1 - ( float)RED( top) / RGB_SCALE)) + ( RED( base) <= 127.5f)
                            * (( 2 * ( float)RED( base) / RGB_SCALE) * RED( top))) * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp((( GREEN( base) > 127.5)
                            * ( 1 - ( 1 - 2 * (( float)GREEN( base) / RGB_SCALE - .5))
                            * ( 1 - ( float)GREEN( top) / RGB_SCALE)) + ( GREEN( base) <= 127.5f)
                            * (( 2 * ( float)GREEN( base) / RGB_SCALE) * GREEN( top))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp((( BLUE( base) > 127.5)
							* ( 1 - ( 1 - 2 * (( float)BLUE( base) / RGB_SCALE - .5))
                            * ( 1 - ( float)BLUE( top) / RGB_SCALE)) + ( BLUE( base) <= 127.5f)
							* (( 2 * ( float)BLUE( base) / RGB_SCALE) * BLUE( top))) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp((( ALPHA( base) > 127.5)
                             * ( 1 - ( 1 - 2 * (( float)ALPHA( base) / RGB_SCALE - .5))
                             * ( 1 - ( float)ALPHA( top) / RGB_SCALE)) + ( ALPHA( base) <= 127.5f)
                             * (( 2 * ( float)ALPHA( base) / RGB_SCALE) * ALPHA( top))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, ALPHA( top) < 180 ? ALPHA( base) : ALPHA( top));
        },
        []( auto top, auto base)
        {
            uint8_t red   = Util::clamp((( RED( top) > 127.5) * ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE)
                            * ( 1 - (( float)RED( top) / RGB_SCALE - .5f))) + ( RED( top) <= 127.5)
                            * (( float)RED( base) / RGB_SCALE * (( float)RED( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp((( GREEN( top) > 127.5) * ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE)
                            * ( 1 - (( float)GREEN( top) / RGB_SCALE - .5f))) + ( GREEN( top) <= 127.5)
                            * ( ( float)GREEN( base) / RGB_SCALE * (( float)GREEN( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp((( BLUE( top) > 127.5) * ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE)
                            * ( 1 - (( float)BLUE( top) / RGB_SCALE - .5f))) + ( BLUE( top) <= 127.5)
                            * (( float)BLUE( base) / RGB_SCALE * (( float)BLUE( top) / RGB_SCALE + .5)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp((( ALPHA( top) > 127.5) * ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE)
                            * ( 1 - (( float)ALPHA( top) / RGB_SCALE - .5f))) + ( ALPHA( top) <= 127.5)
                            * (( float)ALPHA( base) / RGB_SCALE * (( float)ALPHA( top) / RGB_SCALE + .5f)))
                            * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = Util::clamp((( RED( top) > 127.5) * ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)RED( top) / RGB_SCALE - .5f)))
                            + ( RED( top) <= 127.5) * (( float)RED( base) * ( 2 * ( float)RED( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp((( GREEN( top) > 127.5) * ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)GREEN( top) / RGB_SCALE - .5f)))
                            + ( GREEN( top) <= 127.5) * (( float)GREEN( base)
                            * ( 2 * ( float)GREEN( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp((( BLUE( top) > 127.5) * ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)BLUE( top) / RGB_SCALE - .5f)))
                            + ( BLUE( top) <= 127.5) * (( float)BLUE( base)
                            * ( 2 * ( float)BLUE( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp((( ALPHA( top) > 127.5) * ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE)
                            * ( 1 - 2 * (( float)ALPHA( top) / RGB_SCALE - .5f)))
                            + ( ALPHA( top) <= 127.5) * (( float)ALPHA( base)
                            * ( 2 * ( float)ALPHA( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            auto red_f_base   = ( 1 - 2 * (( float)RED( top) / RGB_SCALE - .5f)),
                    red_e_base   = ( 2 * ( float)RED( top) / RGB_SCALE),
                    green_f_base = ( 1 - 2 * (( float)GREEN( top) / RGB_SCALE - .5f)),
                    green_e_base = ( 2 * ( float)GREEN( top) / RGB_SCALE),
                    blue_f_base  = ( 1 - 2 * (( float)BLUE( top) / RGB_SCALE - .5f)),
                    blue_e_base  = ( 2 * ( float)BLUE( top) / RGB_SCALE),
                    alpha_f_base = ( 1 - 2 * (( float)ALPHA( top) / RGB_SCALE - .5f)),
                    alpha_e_base = ( 2 * ( float)ALPHA( top) / RGB_SCALE);
            uint8_t red   = Util::clamp(((( RED( top) > 127.5) * ( ZERO( red_f_base) ? 1 :
                           ((( float)RED( base) / RGB_SCALE) / red_f_base))) + (( RED( top) <= 127.5)
                            * ( ZERO( red_e_base) ? 0 : ( 1 - ( 1 - ( float)RED( base) / RGB_SCALE) / red_e_base))))
                                        * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp(((( GREEN( top) > 127.5) * ( ZERO( green_f_base) ? 1 :
                            ((( float)GREEN( base) / RGB_SCALE) / green_f_base))) + (( RED( top) <= 127.5)
                            * ( ZERO( green_e_base) ? 0 :
                            ( 1 - ( 1 - ( float)GREEN( base) / RGB_SCALE) / green_e_base)))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp(((( BLUE( top) > 127.5) * ( ZERO( blue_f_base) ? 1 :
                            ((( float)BLUE( base) / RGB_SCALE) / blue_f_base))) + (( BLUE( top) <= 127.5)
                            * ( ZERO( blue_e_base) ? 0 : ( 1 - ( 1 - ( float)BLUE( base) / RGB_SCALE) / blue_e_base))))
                                        * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp(((( ALPHA( top) > 127.5) * ( ZERO( alpha_f_base) ? 1 :
                            ((( float)ALPHA( base) / RGB_SCALE) / alpha_f_base))) + (( ALPHA( top) <= 127.5)
                            * ( ZERO( alpha_e_base) ? 0 :
                            ( 1 - ( 1 - ( float)ALPHA( base) / RGB_SCALE) / alpha_e_base)))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red  = Util::clamp((( RED( top) > 127.5) * (( float)RED( base) / RGB_SCALE + 2
                            * (( float)RED( top) / RGB_SCALE - .5f)) + ( RED( top) <= 127.5)
                            * (( float)RED( base) / RGB_SCALE + 2 * (( float)RED( top) / RGB_SCALE) - 1))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp((( GREEN( top) > 127.5) * (( float)GREEN( base) / RGB_SCALE + 2
                            * (( float)GREEN( top) / RGB_SCALE - .5f)) + ( GREEN( top) <= 127.5)
                            * (( float)GREEN( base) / RGB_SCALE + 2 * (( float)GREEN( top) / RGB_SCALE) - 1))
                            * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp((( BLUE( top) > 127.5) * (( float)BLUE( base) / RGB_SCALE + 2
							* (( float)BLUE( top) / RGB_SCALE - .5f)) + ( BLUE( top) <= 127.5)
                            * (( float)BLUE( base) / RGB_SCALE + 2 * (( float)BLUE( top) / RGB_SCALE) - 1))
                            * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp((( ALPHA( top) > 127.5) * (( float)ALPHA( base) / RGB_SCALE + 2
                            * (( float)ALPHA( top) / RGB_SCALE - .5f)) + ( ALPHA( top) <= 127.5)
                            * (( float)ALPHA( base) / RGB_SCALE + 2 * (( float)ALPHA( top) / RGB_SCALE) - 1))
                            * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = Util::clamp((( RED( top) > 127.5) * ( std::max<float>(( float)RED( base) / RGB_SCALE,
                            2 * (( float)RED( top) / RGB_SCALE - .5f))) + ( RED( top) <= 127.5)
                            * ( std::min<float>(( float)RED( base) / RGB_SCALE, 2 * ( float)RED( top) / RGB_SCALE)))
                            * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp((( GREEN( top) > 127.5) * ( std::max<float>(( float)GREEN( base) / RGB_SCALE,
                            2 * (( float)GREEN( top) / RGB_SCALE - .5f))) + ( GREEN( top) <= 127.5)
                            * ( std::min<float>(( float)GREEN( base) / RGB_SCALE,
                            2 * ( float)GREEN( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp((( BLUE( top) > 127.5) * ( std::max<float>(( float)BLUE( base) / RGB_SCALE,
                            2 * (( float)BLUE( top) / RGB_SCALE - .5f))) + ( BLUE( top) <= 127.5)
                            * ( std::min<float>(( float)BLUE( base) / RGB_SCALE,
                            2 * ( float)BLUE( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp((( ALPHA( top) > 127.5) * ( std::max<float>(( float)ALPHA( base)
                            / RGB_SCALE, 2 * (( float)ALPHA( top) / RGB_SCALE - .5f))) + ( ALPHA( top) <= 127.5)
                            * ( std::min<float>(( float)ALPHA( base) / RGB_SCALE,
                            2 * ( float)ALPHA( top) / RGB_SCALE))) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            auto mix = selector[ ENUM_CAST( BLEND_ENUM( LinearDodge))]( top, base);

            return RGBA( RED( mix) < RGB_SCALE ? 0 : RGB_SCALE, GREEN( mix) < RGB_SCALE ? 0 : RGB_SCALE,
                         BLUE( mix) < RGB_SCALE ? 0 : RGB_SCALE, ALPHA( mix) < RGB_SCALE ? 0 : RGB_SCALE);
        },
        []( auto top, auto base)
        {
            uint8_t red   = std::abs(( int16_t)RED( base) - ( int8_t)RED( top)),
                    green = std::abs(( int16_t)GREEN( base) - ( int8_t)GREEN( top)),
                    blue  = std::abs(( int16_t)BLUE( base) - ( int8_t)BLUE( top)),
                    alpha = ColorUtil::colorClamp(( uint16_t)ALPHA( base) + ALPHA( top));

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = Util::clamp(( .5f - 2 * (( float)RED( base) / RGB_SCALE - .5f)
                                                * (( float)RED( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    green = Util::clamp(( .5f - 2 * (( float)GREEN( base) / RGB_SCALE - .5f)
                                                * (( float)GREEN( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    blue  = Util::clamp(( .5f - 2 * (( float)BLUE( base) / RGB_SCALE - .5f)
                                                * (( float)BLUE( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE),
                    alpha = Util::clamp(( .5f - 2 * (( float)ALPHA( base) / RGB_SCALE - .5f)
                                                * (( float)ALPHA( top) / RGB_SCALE - .5f)) * RGB_SCALE, 0, RGB_SCALE);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = RED( top) > RED( base) ? RED( top) - RED( base) : RED( top),
                    green = GREEN( top) > GREEN( base) ? GREEN( top) - GREEN( base) : GREEN( top),
                    blue  = BLUE( top) > BLUE( base) ? BLUE( top) - BLUE( base) : BLUE( top),
                    alpha = ALPHA( top) > ALPHA( base) ? ALPHA( top) - ALPHA( base) : ALPHA( top);

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            uint8_t red   = RED( base) == 0 ? RGB_SCALE
                            : ColorUtil::colorClamp( FLOAT_CAST(RED( top)) * RGB_SCALE / RED( base)),
                    green = GREEN( base) == 0 ? RGB_SCALE
                            : ColorUtil::colorClamp( FLOAT_CAST( GREEN( top)) * RGB_SCALE / GREEN( base)),
                    blue  = BLUE( base) == 0 ? RGB_SCALE
                            : ColorUtil::colorClamp( FLOAT_CAST( BLUE( top)) * RGB_SCALE / BLUE( base)),
                    alpha = ALPHA( base) == 0 ? RGB_SCALE
                            : ColorUtil::colorClamp( FLOAT_CAST( ALPHA( top)) * RGB_SCALE / ALPHA( base));

            return RGBA( red, green, blue, alpha);
        },
        []( auto top, auto base)
        {
            auto top_hsla      = ColorSpaceConverter::rgbaToHsla( top),
                    base_hsla  = ColorSpaceConverter::rgbaToHsla( base),
                    lum_change = HSLA( HUE( top_hsla), SAT( base_hsla), LUMIN( base_hsla), ALPHA( base_hsla));

            return ColorSpaceConverter::hslaToRgba( lum_change);
        },
        []( auto top, auto base)
        {
            auto top_hsla   = ColorSpaceConverter::rgbaToHsla( top),
                    base_hsla  = ColorSpaceConverter::rgbaToHsla( base),
                    lum_change = HSLA( HUE( base_hsla), SAT( top_hsla), LUMIN( base_hsla), ALPHA( base_hsla));

            return ColorSpaceConverter::hslaToRgba( lum_change);
        },
        []( auto top, auto base)
        {
            auto top_hsla   = ColorSpaceConverter::rgbaToHsla( top),
                    lum_change = HSLA( HUE( top_hsla), SAT( top_hsla), LUMIN( top_hsla), ALPHA( base));

            return ColorSpaceConverter::hslaToRgba( lum_change);
        },
        []( auto top, auto base)
            {
                auto top_hsla   = ColorSpaceConverter::rgbaToHsla( top),
                        base_hsla  = ColorSpaceConverter::rgbaToHsla( base),
                        lum_change = HSLA( HUE( base_hsla), SAT( base_hsla), LUMIN( top_hsla), ALPHA( base_hsla));

                return ColorSpaceConverter::hslaToRgba( lum_change);
            }
    };
    return selector[ ENUM_CAST( model)];
}

template <typename Tp>
void LayerRenderer::useEffectsOn( FrameBuffer<Tp> &frame, const CompositionRule& c_rule,
                                  CompositionRule::StickyArena pos)
{
    for( auto& effect : c_rule.s_effects)
    {
        if( std::get<1>( effect) != pos)
            continue;

        SpecialEffectArgs extras;
        switch ( std::get<0>( effect))
        {
            case SpecialEffect::Blur:
            {
                auto kernel = makeGaussian( FLOAT_CAST( std::get<2>( effect)));
                extras.kernel = kernel;
                applyEffect( frame, SpecialEffect::Blur, extras);
                break;
            }
            case SpecialEffect::Sharpen:
            {
                break;
            }
            case SpecialEffect::Emboss:
            {
                auto emboss = makeEmboss( std::get<2>( effect));
                extras.kernel = emboss;
                applyEffect( frame, SpecialEffect::Emboss, extras);
                break;
            }
            case SpecialEffect::Oil:
            {
                applyEffect( frame, SpecialEffect::Oil, extras);
            }
            case SpecialEffect::RequiresKernelSentinel:
                break;
            case SpecialEffect::GrayScale:
                applyEffect( frame, SpecialEffect::GrayScale);
                break;
            case SpecialEffect::Grainy:
            {
                extras.grain_multiplicity = std::get<2>( effect);
                applyEffect( frame, SpecialEffect::Grainy, extras);
                break;
            }
            case SpecialEffect::Twirl:
            {
                auto radius = std::get<2>( effect);
                extras.twirl_radius = std::max( radius, extras.twirl_radius);
                std::string other_args = std::get<3>( effect);
                if( !other_args.empty())
                {
                    std::smatch sm;
                    std::regex key( R"(\s*(\d+)?(?:,\s*(\d+)deg)?(?:,\s*([a-z]+(?:-[a-z]+)?))?\s*)",
                                    std::regex_constants::icase);
                    if( std::regex_match( other_args.cbegin(), other_args.cend(), sm, key))
                    {
                        auto s_twirl_strength  = sm[ 1].str();
                        auto s_twirl_rotation  = sm[ 2].str();
                        auto s_twirl_position  = sm[ 3].str();
                        extras.twirl_strength = s_twirl_strength.empty() ? extras.twirl_strength
                                                 : std::stoi( s_twirl_strength);
                        extras.twirl_rotation = s_twirl_rotation.empty() ? extras.twirl_rotation
                                                 : FLOAT_CAST( std::stoi( s_twirl_rotation) * RAD_SCALE);
                        extras.twirl_center   = s_twirl_position.empty() ? extras.twirl_center
                                                 : Util::getSnapCoordinate( s_twirl_position);
                    }
                }

                applyEffect( frame, SpecialEffect::Twirl, extras);
            }
                break;
        }
    }
}

std::vector<float> LayerRenderer::makeEmboss( int width)
{
    auto size = width * width;
    auto mid = size / 2;
    std::vector<float> emboss( size);
    emboss[ 0] = 1.7;
    emboss[ size - 1] = -1.5;

    for ( int j = 0; j < width; ++j)
    {
        for ( int i = 0; i < width; ++i)
            printf( "%f ", emboss[ j * width + i]);
        putchar( '\n');
    }

    return emboss;
}

// Implementation of guassian filter using erf as cdf
// References:
//[1] https://stackoverflow.com/questions/809362/how-to-calculate-cumulative-normal-distribution
std::vector<float> LayerRenderer::makeGaussian( float radius) noexcept
{
    constexpr auto min_ex_radius = 1.f,
                   max_ex_radius = 200.f,
                   min_radius = 1.f,
                   max_radius = 20.f;
   radius = Util::clamp( radius, min_ex_radius, max_ex_radius);
    auto fraction = (( radius - min_ex_radius) / ( max_ex_radius - min_ex_radius));
    auto sigma   = ( 1 - fraction) * min_radius + fraction * max_radius;
    auto size = INT_CAST( sigma) * 2 + 1;
    std::vector<float> result( size);
    auto sum = 0.f;
    auto r = INT_CAST( sigma);
    for( int i = -r, end = r; i <= end; ++i)
    {
        result[ i + r] = std::exp( -FLOAT_CAST( i * i) / ( 2.f * sigma * sigma));
        sum += result[ i + r];
    }

    for( auto& each : result)
        each /= sum;

    return result;
}

float LayerRenderer::gaussianNoise() noexcept
{
    static auto rd = std::mt19937_64( std::random_device()());
    static std::uniform_real_distribution<float> dist( 0, 1);

    float s, u_one, u_two, v_one, v_two;
    do
    {
        u_one = dist( rd);
        u_two = dist( rd);
        v_one = 2 * u_one - 1;
        v_two = 2 * u_two - 1;
        s     = v_one * v_one + v_two * v_two;
    }
    while( s >= 1);

    return std::sqrt( -2 * std::log( s) / s) * v_one * 7;
}
