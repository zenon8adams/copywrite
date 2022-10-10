#ifndef TEXT_RENDERER_HPP
#define TEXT_RENDERER_HPP

#include FT_FREETYPE_H
#include "parsers/rule_parser.hpp"

class TextRenderer
{
public:
    explicit TextRenderer( ApplicationDirector& manager, RuleParser& parser);

    FrameBuffer<uint32_t> render();

    ~TextRenderer();

    RowDetails& getRowDetails();

    MonoGlyphs& getRasters();

private:
    /*
     *  Passed to the Freetype renderer to compute the outline for the glyph.
     */
    static void spansCallback( int y, int count, const FT_Span *spans, void *user);

    void renderSpans( FT_Outline *outline, Spans *spans);

    FT_Library library_{};
    FT_Face face_{};
    ApplicationDirector& app_manager_;
    RuleParser& parser_;
    MonoGlyphs rasters_;        // Collection of processed characters.
    RowDetails row_details_;    // Holds information common to each row processed.
    std::wstring text_;
};

#endif