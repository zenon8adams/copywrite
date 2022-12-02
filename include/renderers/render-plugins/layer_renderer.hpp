#ifndef LAYER_RENDERER_HPP
#define LAYER_RENDERER_HPP

#include "plugins/plugin.hpp"
#include "parsers/rule_parser.hpp"
#include "geometry/geo_vector.hpp"
#include "utils/color_utils.hpp"
#include "plugins/image_manager.hpp"

class LayerRenderer : public Plugin
{
public:
    explicit LayerRenderer( ApplicationDirector& manager, RuleParser& parser);

    void composite( FrameBuffer<uint32_t> &s_frame);

    template <typename Tp>
    static void resize( FrameBuffer<Tp> &frame, int *interpolation);

    // Implementation of guassian filter using erf as cdf
    // References:
    //[1] https://stackoverflow.com/questions/809362/how-to-calculate-cumulative-normal-distribution
    static std::vector<float> makeGaussian( float radius) noexcept;

    template <typename Tp, typename = std::enable_if_t<std::is_integral_v<Tp>>>
    static void applyEffect( FrameBuffer<Tp> &frame, SpecialEffect effect, const SpecialEffectArgs& extras = {})
    {
        int n_channel = std::max( frame.n_channel, 1);
        auto width    = frame.width,
             height   = frame.height;
        auto x_extent = width,
             y_extent = height;
        if( ENUM_CAST( effect) < ENUM_CAST( SpecialEffect::RequiresKernelSentinel))
        {
            auto kernel  = extras.kernel;
            int k_size   = INT_CAST( kernel.size()),
                k_width  = INT_CAST( std::sqrt( k_size)),
                mid      = k_width / 2;
            auto [ t_frame, interm] = padFrame( frame, k_size - 1, k_size - 1);
            auto buffer   = frame.buffer.get(),
                 t_buffer = t_frame.buffer.get(),
                 d_buffer  = interm.buffer.get();
            auto k_ptr    = kernel.data();

            if( effect == SpecialEffect::Oil)
            {
                int level = 200;
                for( int j = 0; j < height; ++j)
                {
                    for( int i = 0; i < width; ++i)
                    {
                        double red{}, green{}, blue{}, alpha{};
                        if( i < x_extent && j < y_extent)
                        {
                            int max_intensity = 0, max_intensity_index = 0, intensity_freq[ level],
                                    avg_r[ level], avg_g[ level], avg_b[ level];
                            memset( intensity_freq, 0, sizeof( intensity_freq));
                            memset( avg_r, 0, sizeof( avg_r));
                            memset( avg_g, 0, sizeof( avg_g));
                            memset( avg_b, 0, sizeof( avg_b));
                            for( int k = 0; k < k_size; ++k)
                            {
                                int row = ( k_width - k / k_width + 1) - mid + j,
                                    col = ( k_width - k % k_width - 1) - mid + i;
                                if( row >= 0 && row < height && col >= 0 && col < width)
                                {
                                    size_t index = row * width * n_channel + col * n_channel;
                                    uint8_t r{}, g{}, b{}, a{};
                                    readPixel( buffer + index, &r, &g, &b, &a);
                                    int cur_intensity = INT_CAST(( FLOAT_CAST(( r + g + b) * level) / 3.f) / RGB_SCALE);
                                    ++intensity_freq[ cur_intensity];
                                    avg_r[ cur_intensity] += r;
                                    avg_g[ cur_intensity] += g;
                                    avg_b[ cur_intensity] += b;
                                    if( intensity_freq[ cur_intensity] > max_intensity)
                                    {
                                        max_intensity = intensity_freq[ cur_intensity];
                                        max_intensity_index = cur_intensity;
                                    }
                                }
                            }
                            red   = ColorUtil::colorClamp( FLOAT_CAST( avg_r[ max_intensity_index])
                                                           / FLOAT_CAST( max_intensity));
                            green = ColorUtil::colorClamp( FLOAT_CAST( avg_g[ max_intensity_index])
                                                           / FLOAT_CAST( max_intensity));
                            blue  = ColorUtil::colorClamp( FLOAT_CAST( avg_b[ max_intensity_index])
                                                           / FLOAT_CAST( max_intensity));
                        }
                        else
                            readPixel( buffer + j * width * n_channel + i * n_channel, &red, &green, &blue, &alpha);
                        writePixel( t_buffer + j * width * n_channel + i * n_channel,
                                    red, green, blue, buffer + j * width + i);
                    }
                }
            }
            else
            {
                // Row pass
                for( int j = 0; j < height; ++j)
                    for( int i = 0; i < width; ++i)
                        weightedAverageConv( t_buffer, d_buffer, n_channel, k_ptr,
                                             t_frame.width, t_frame.height, t_frame.width, j, i, k_size, 1);

                // Column pass
                for( int j = 0; j < height; ++j)
                    for( int i = 0; i < width; ++i)
                        weightedAverageConv( d_buffer, buffer, n_channel, k_ptr,
                                             t_frame.width, height, width, j, i, 1, k_size, false);
            }
        }
        else if( effect == SpecialEffect::GrayScale || effect == SpecialEffect::Grainy)
        {
            auto buffer = frame.buffer.get();
            auto size = frame.width * frame.height;
            bool is_grainy = effect == SpecialEffect::Grainy;
            int grainy_increment = FLOAT_CAST( std::sqrt( std::max( 1, extras.grain_multiplicity)));
            int red, green, blue, alpha;
            for( size_t i = 0; i < size; ++i)
            {
                float r_scale = is_grainy ? 1.f     : .2126f,
                      g_scale = is_grainy ? r_scale : .7152f,
                      b_scale = is_grainy ? r_scale : .0722f,
                      r_inc   = is_grainy ? gaussianNoise() * FLOAT_CAST( grainy_increment) : 0.f,
                      g_inc   = r_inc,
                      b_inc   = r_inc;
                readPixel( buffer + i * n_channel, &red, &green, &blue, &alpha);
                red   = ColorUtil::colorClamp( FLOAT_CAST( red) * r_scale + r_inc),
                green = ColorUtil::colorClamp( FLOAT_CAST( green) * g_scale + g_inc),
                blue  = ColorUtil::colorClamp( FLOAT_CAST( blue) * b_scale + b_inc);
                writePixel( buffer + i * n_channel, red, green, blue, buffer + i * n_channel);
            }
        }
        else if( effect == SpecialEffect::Twirl)
        {
            std::shared_ptr<Tp> dest(( Tp *)calloc( width * height  * n_channel, sizeof( Tp)), []( auto *p){ free( p);});
            auto d_buffer = dest.get();
            auto s_buffer = frame.buffer.get();
            size_t radius = extras.twirl_radius;
            auto eff_rad  = FLOAT_CAST( std::log( 2)) * FLOAT_CAST( radius) / 5.f;
            auto center = Vec2D<float>( FLOAT_CAST( width)  * extras.twirl_center.x,
                                        FLOAT_CAST( height) * extras.twirl_center.y);
            auto strength = extras.twirl_strength;
            auto rotation = extras.twirl_rotation;
            auto min_radius = 100.f, thickness = 60.f, max_radius = min_radius + thickness;
            for( size_t j = 0; j < height; ++j)
            {
                for( size_t i = 0; i < width; ++i)
                {
                    auto dx      = FLOAT_CAST( i) - center.x,
                         dy      = FLOAT_CAST( j) - center.y;
                    auto theta   = atan2( dy, dx);
                    auto dist    = std::sqrt( dx * dx + dy * dy);
                    auto spin    = FLOAT_CAST( strength) * std::exp( -dist / eff_rad);
                    auto n_theta = (( rotation + spin)/* * FLOAT_CAST( dist >= min_radius && dist <= max_radius)*/) + theta;
                    auto x_dist  = FLOAT_CAST( dist * std::cos( n_theta));
                    auto y_dist  = FLOAT_CAST( dist * std::sin( n_theta));
                    auto x       = INT_CAST( center.x + x_dist),
                         y       = INT_CAST( center.y + y_dist);
                    int d_index = j * width * n_channel + i * n_channel, s_index = 0;
                    if( x >= 0 && x < width && y >= 0 && y < height)
                        s_index = y * width * n_channel + x * n_channel;
                    uint8_t r, g, b, a;
                    readPixel( s_buffer + s_index, &r, &g, &b, &a);
                    writePixel( d_buffer + d_index, r, g, b, s_buffer + s_index);
                }
            }
            frame.buffer = dest;
            /*SpecialEffectArgs args;
            args.inplace = true;
            args.start.x  = INT_CAST(( center.x - coverage) < 0 ? 0 : center.x - coverage);
            args.start.y  = INT_CAST(( center.y - coverage) < 0 ? 0 : center.y - coverage);
            args.extent.x = ( args.start.x + coverage * 2) >  width ?  width : args.start.x + coverage * 2;
            args.extent.y = ( args.start.y + coverage * 2) > height ? height : args.start.y + coverage * 2;
            args.kernel = makeGaussian( 200, 11);
            applyEffect( frame, SpecialEffect::Blur, args);*/
        }
    }

private:
    static bool intersects( std::array<Vec2D<float>, 4> corners, Vec2D<float> test);

    static std::function<uint32_t( uint32_t, uint32_t)> selectBlendFn( CompositionRule::BlendModel model);

    template <typename Tp>
    void useEffectsOn( FrameBuffer<Tp> &frame, const CompositionRule& c_rule, CompositionRule::StickyArena pos);

    static std::vector<float> makeEmboss( int width);

    static float gaussianNoise() noexcept;

    template <typename Src, typename Dst>
    static inline void readPixel( Src *src,  Dst *r, Dst *g, Dst *b, Dst *a) noexcept
    {
        if constexpr ( sizeof( Src) == 4)
        {
            *r = RED( *src);
            *g = GREEN( *src);
            *b = BLUE( *src);
            *a = ALPHA( *src);
        }
        else
        {
            *r = *src;
            *g = *++src;
            *b = *++src;
            *a = *++src;
        }
    }

    template <typename Dst, typename Src>
    static inline void writePixel( Dst *dst, Src r, Src g, Src b, Dst *a) noexcept
    {
        if constexpr ( sizeof( Dst) == 4)
            *dst =  RGBA( r, g, b, ALPHA( *a));
        else
        {
              *dst = r;
            *++dst = g;
            *++dst = b;
            *++dst = a[ 3];
        }
    }

    template<typename Tp>
    static std::pair<FrameBuffer<Tp>, FrameBuffer<Tp>> padFrame( FrameBuffer<Tp>& frame, int row_inc, int col_inc)
    {
        auto n_width   = frame.width + col_inc;
        auto n_height  = frame.height + row_inc;
        auto n_channel = std::max( 1, frame.n_channel);
        auto n_bytes   = sizeof( Tp);
        auto n_stride  = n_width * n_channel;
        auto inc_bytes = col_inc * n_channel;
        auto w_stride  = frame.width * n_channel;
        auto dest = std::shared_ptr<Tp>(( Tp *)calloc( n_width * n_height * n_channel, n_bytes),
                                        []( auto *p){ free( p);});
        auto cache = std::shared_ptr<Tp>(( Tp *)calloc( n_width * n_height * n_channel, n_bytes),
                                        []( auto *p){ free( p);});
        auto d_buffer = dest.get();
        auto c_buffer = cache.get();
        auto s_buffer = frame.buffer.get();
        for( int j = 0; j < frame.height; ++j)
        {
            memcpy( d_buffer + j * n_stride, s_buffer + j * w_stride, w_stride * n_bytes);
            auto s_end = s_buffer + ( j + 1) * w_stride - n_channel * col_inc;
            memcpy( d_buffer + ( j + 1) * n_stride - inc_bytes, s_end, n_channel * col_inc * n_bytes);
            memcpy( c_buffer + ( j + 1) * n_stride - inc_bytes, s_end, n_channel * col_inc * n_bytes);
        }

        memcpy( d_buffer + frame.height * n_stride,
                d_buffer + frame.height * n_stride - n_stride * row_inc, n_stride * row_inc * n_bytes);
        memcpy( c_buffer + frame.height * n_stride,
                c_buffer + frame.height * n_stride - n_stride * row_inc, n_stride * row_inc * n_bytes);

        return {{ std::move( dest), n_width, n_height, n_channel}, { std::move( cache), n_width, n_height, n_channel}};
    }

    template<typename Tp, typename Kv>
    static void weightedAverageConv( Tp *src, Tp *dst, int n_channel, Kv *kernel, int s_cols, int d_rows, int d_cols,
                              int s_row, int s_col, int k_rows, int k_cols, bool row = true)
    {
        using std::min, std::max;
        float red{}, green{}, blue{}, alpha{};
        int k_width = !row ? k_rows : k_cols;

        for( int j = 0; j < k_rows; ++j)
        {
            for( int i = 0; i < k_cols; ++i)
            {
                int index = j * k_width + i;
                float r, g, b, a;
                readPixel( src + ( j + s_row) * s_cols * n_channel + ( i + s_col) * n_channel, &r, &g, &b, &a);
                red   += kernel[ index] * r;
                green += kernel[ index] * g;
                blue  += kernel[ index] * b;
            }
        }

        red = ColorUtil::colorClamp( red);
        green = ColorUtil::colorClamp( green);
        blue = ColorUtil::colorClamp( blue);
        writePixel( dst + s_row * d_cols * n_channel + s_col * n_channel, red, green, blue,
                    src + s_row * s_cols * n_channel + s_col * n_channel);
    }

    std::vector<CompositionRule> c_rules_;
    ApplicationDirector& app_manager_;
    RuleParser& parser_;
};

#endif