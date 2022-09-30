#ifndef LAYER_RENDERER_HPP
#define LAYER_RENDERER_HPP

#include "plugins/plugin.hpp"
#include "parsers/rule_parser.hpp"
#include "geometry/geo_vector.hpp"
#include "utils/color_utils.hpp"

class LayerRenderer : public Plugin
{
public:
    explicit LayerRenderer( ApplicationDirector& manager, RuleParser& parser);

    void composite( FrameBuffer<uint32_t> &s_frame);

    template <typename Tp>
    static void resize( FrameBuffer<Tp> &frame, int *interpolation);

private:
    static bool intersects( std::array<Vec2D<float>, 4> corners, Vec2D<float> test);

    static std::function<uint32_t( uint32_t, uint32_t)> selectBlendFn( CompositionRule::BlendModel model);

    template <typename Tp>
    static void useEffectsOn( FrameBuffer<Tp> &frame, const CompositionRule& c_rule, CompositionRule::StickyArena pos);

    static std::vector<float> makeEmboss( int width);

    // Implementation of guassian filter using erf as cdf
    // References:
    //[1] https://stackoverflow.com/questions/809362/how-to-calculate-cumulative-normal-distribution
    static std::vector<float> makeGaussian( float radius, size_t width = 17);

    static float gaussianNoise();

    template <typename Tp, typename = std::enable_if_t<std::is_integral_v<Tp>>>
    static void applyEffect( FrameBuffer<Tp> &frame, SpecialEffect effect, const SpecialEffectArgs& extras = {})
    {
        int n_channel = std::max( frame.n_channel, 1);
        auto width    = frame.width,
                height   = frame.height;
        auto x_extent = extras.extent.x != -1 ? extras.extent.x : width,
                y_extent = extras.extent.y != -1 ? extras.extent.y : height;
        if( ENUM_CAST( effect) < ENUM_CAST( SpecialEffect::RequiresKernelSentinel))
        {
            auto kernel  = extras.kernel;
            int k_size   = INT_CAST( kernel.size()),
                    k_width  = INT_CAST( std::sqrt( k_size)),
                    mid      = k_width / 2,
                    j_offset = extras.start.y,
                    i_offset = extras.start.x;
            auto inplace = extras.inplace;

            std::shared_ptr<Tp> dest;
            if( inplace)
                dest = frame.buffer;
            else
                dest.reset( ( Tp *)calloc( width * height  * n_channel, sizeof( Tp)), []( auto *p){ free( p);});

            auto buffer   = frame.buffer.get(),
                    d_buffer = dest.get();

            if( effect == SpecialEffect::Oil)
            {
                int intensity = extras.oil_intensity, level = 200;
                for( int j = j_offset; j < height; ++j)
                {
                    for( int i = i_offset; i < width; ++i)
                    {
                        double red{}, green{}, blue{};
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
                                    auto pixel = buffer[ index];
                                    uint8_t r{}, g{}, b{};
                                    if constexpr ( sizeof( Tp) == 4)
                                    {
                                        r = RED( pixel);
                                        g = GREEN( pixel);
                                        b = BLUE( pixel);
                                    }
                                    else
                                    {
                                        r = pixel;
                                        g = buffer[ index + 1];
                                        b = buffer[ index + 2];
                                    }

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
                        {
                            size_t index = j * width * n_channel + i * n_channel;
                            auto pixel = buffer[ index];
                            if constexpr ( sizeof( Tp) == 4)
                            {
                                red = RED( pixel);
                                green = GREEN( pixel);
                                blue = BLUE( pixel);
                            }
                            else
                            {
                                red = pixel;
                                green = buffer[ index + 1];
                                blue = buffer[ index + 2];
                            }
                        }

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
            }
            else
            {
                for( int j = j_offset; j < height; ++j)
                {
                    for( int i = i_offset; i < width; ++i)
                    {
                        double red{}, green{}, blue{};
                        if( i < x_extent && j < y_extent)
                        {
                            for( int k = 0; k < k_size; ++k)
                            {
                                int row = ( k_width - k / k_width + 1) - mid + j,
                                        col = ( k_width - k % k_width - 1) - mid + i;
                                if( row >= 0 && row < height && col >= 0 && col < width)
                                {
                                    size_t index = row * width * n_channel + col * n_channel;
                                    auto pixel = buffer[ index];
                                    uint8_t r{}, g{}, b{};
                                    if constexpr ( sizeof( Tp) == 4)
                                    {
                                        r = RED( pixel);
                                        g = GREEN( pixel);
                                        b = BLUE( pixel);
                                    }
                                    else
                                    {
                                        r = pixel;
                                        g = buffer[ index + 1];
                                        b = buffer[ index + 2];
                                    }

                                    red   += FLOAT_CAST( r) * kernel[ k];
                                    green += FLOAT_CAST( g) * kernel[ k];
                                    blue  += FLOAT_CAST( b) * kernel[ k];
                                }
                            }
                            red = ColorUtil::colorClamp( FLOAT_CAST( red));
                            green = ColorUtil::colorClamp( FLOAT_CAST( green));
                            blue = ColorUtil::colorClamp( FLOAT_CAST( blue));
                        }
                        else
                        {
                            size_t index = j * width * n_channel + i * n_channel;
                            auto pixel = buffer[ index];
                            if constexpr ( sizeof( Tp) == 4)
                            {
                                red = RED( pixel);
                                green = GREEN( pixel);
                                blue = BLUE( pixel);
                            }
                            else
                            {
                                red = pixel;
                                green = buffer[ index + 1];
                                blue = buffer[ index + 2];
                            }
                        }

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
            }
            if( !inplace)
                frame.buffer = dest;
        }
        else if( effect == SpecialEffect::GrayScale || effect == SpecialEffect::Grainy)
        {
            auto buffer = frame.buffer.get();
            auto size = frame.width * frame.height;
            constexpr auto color_change_freq = 8;

            int grainy_increment = FLOAT_CAST( std::sqrt( std::max( 1, extras.grain_multiplicity)));
            for( size_t i = 0; i < size; ++i)
            {
                if constexpr( sizeof( Tp) == 4)
                {
                    if( effect == SpecialEffect::Grainy)
                    {
                        auto& pixel = buffer[ i];
                        auto noise_value = gaussianNoise() * FLOAT_CAST( grainy_increment);
                        auto red   = ColorUtil::colorClamp( RED( pixel) + noise_value),
                                green = ColorUtil::colorClamp( GREEN( pixel) + noise_value),
                                blue  = ColorUtil::colorClamp( BLUE( pixel) + noise_value);
                        pixel = RGBA( red, green, blue, ALPHA( pixel));
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
                    if( effect == SpecialEffect::Grainy)
                    {
                        auto noise_value = gaussianNoise() * FLOAT_CAST( grainy_increment);
                        buffer[ index + 0] = ColorUtil::colorClamp( buffer[ index + 0] + noise_value);
                        buffer[ index + 1] = ColorUtil::colorClamp( buffer[ index + 1] + noise_value);
                        buffer[ index + 2] = ColorUtil::colorClamp( buffer[ index + 2] + noise_value);
                    }
                    else if( effect == SpecialEffect::GrayScale)
                    {
                        auto avg = .2126 * buffer[ index] + .7152 * buffer[ index + 1] + .0722 * buffer[ index + 2];
                        buffer[ index + 0] = avg;
                        buffer[ index + 1] = avg;
                        buffer[ index + 2] = avg;
                    }
                }
            }
        }
        else if( effect == SpecialEffect::Twirl)
        {
            std::shared_ptr<Tp> dest(( Tp *)calloc( width * height  * n_channel, sizeof( Tp)), []( auto *p){ free( p);});
            auto d_buffer = dest.get();
            auto s_buffer = frame.buffer.get();
            size_t radius = extras.twirl_radius;
            auto r       = FLOAT_CAST( std::log( 2)) * FLOAT_CAST( radius) / 5.f;
            auto center = Vec2D<float>( FLOAT_CAST( width)  * extras.twirl_center.x,
                                        FLOAT_CAST( height) * extras.twirl_center.y);
            auto strength = extras.twirl_strength;
            auto rotation = extras.twirl_rotation;
            for( size_t j = 0; j < height; ++j)
            {
                for( size_t i = 0; i < width; ++i)
                {
                    auto dx      = FLOAT_CAST( i) - center.x,
                            dy      = FLOAT_CAST( j) - center.y;
                    auto theta   = atan2( dy, dx);
                    auto dist    = std::sqrt( dx * dx + dy * dy);
                    auto spin    = FLOAT_CAST( strength) * std::exp( -dist / r);
                    auto n_theta = rotation + spin + theta;
                    auto x_dist  = FLOAT_CAST( dist * std::cos( n_theta));
                    auto y_dist  = FLOAT_CAST( dist * std::sin( n_theta));
                    auto x       = INT_CAST( center.x + x_dist),
                            y       = INT_CAST( center.y + y_dist);
                    int d_index = j * width * n_channel + i * n_channel, s_index = 0;
                    if( x >= 0 && x < width && y >= 0 && y < height)
                        s_index = y * width * n_channel + x * n_channel;

                    if constexpr( sizeof( Tp) == 4)
                    {
                        auto pixel = s_buffer[ s_index];
                        d_buffer[ d_index] = RGBA( RED( pixel), GREEN( pixel), BLUE( pixel), ALPHA( pixel));
                    }
                    else
                    {
                        d_buffer[ d_index]     = s_buffer[ s_index];
                        d_buffer[ d_index + 1] = s_buffer[ s_index + 1];
                        d_buffer[ d_index + 2] = s_buffer[ s_index + 2];
                        d_buffer[ d_index + 3] = s_buffer[ s_index + 3];
                    }
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

    std::vector<CompositionRule> c_rules_;
    ApplicationDirector& app_manager_;
    RuleParser& parser_;
};

#endif