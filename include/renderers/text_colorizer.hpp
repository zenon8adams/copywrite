#ifndef TEXT_COLORIZER_HPP
#define TEXT_COLORIZER_HPP

#include "renderers/text_renderer.hpp"
#include "geometry/geo_vector.hpp"

class TextColorizer
{
public:
    explicit TextColorizer( ApplicationDirector& manager, TextRenderer &text_renderer);

    FrameBuffer<uint32_t> paint();

    void paintShadow( FrameBuffer<uint32_t> &frame);

    void paintText( FrameBuffer<uint32_t> &frame);

private:
    static uint32_t easeColor( const MonoGlyph &raster, const RowDetail &row_detail, Vec2D<int> size,
                               Vec2D<int> pos, FT_Vector pen, Vec2D<uint32_t> color_shift, bool is_outline);

    ApplicationDirector& app_manager_;
    TextRenderer& text_renderer_;
};

#endif