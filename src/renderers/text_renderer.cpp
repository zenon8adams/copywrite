#include <iostream>
#include "copywrite.hpp"
#include "parsers/rule_parser.hpp"
#include "renderers/text_renderer.hpp"
#include "plugins/local_font_manager.hpp"
#include "plugins/plugin_manager.hpp"
#include "utils/utils.hpp"
#include "utils/timer.hpp"
#include "codecs/text_codec.hpp"

#include FT_FREETYPE_H

TextRenderer::TextRenderer( ApplicationDirector& manager, RuleParser& parser)
: app_manager_( manager), parser_( parser)
{
    int error = FT_Init_FreeType( &library_);
    if( error != 0)
    {
        fprintf( stderr, "Unable to startup!");
        exit( EXIT_FAILURE);
    }

    std::unique_ptr<uint8_t, DeleterType> custom_profile(( uint8_t *)nullptr, []( auto *p){ if( p) free( p);});
    auto font_profile = app_manager_.font_profile.data();
    if( strrchr( font_profile, '.') == nullptr)
    {
        auto codec = dynamic_cast<LocalFontManager *>( PluginManager::instance()->get( LOCAL_FONT_MANAGER));
        int64_t size{ -1};
        if( codec != nullptr)
        {
            std::tie( size, custom_profile) =  codec->useInstalledFont( font_profile);
            if( !UNSET( size))
                error = FT_New_Memory_Face( library_, ( const FT_Byte *)custom_profile.get(), size, 0, &face_);
        }

        if( error != 0 || size == -1)
        {
            auto file = Util::getFontFile( font_profile);
            if( file.empty())
            {
                fprintf( stderr, "Unable to parse font");
                exit( EXIT_FAILURE);
            }
            error = FT_New_Face( library_, file.c_str(), 0, &face_);
        }
    }
    else
        error = FT_New_Face( library_, font_profile, 0, &face_);

    if( error != 0)
    {
        fprintf( stderr, "Font file is invalid!");
        exit( EXIT_FAILURE);
    }

    auto text = app_manager_.text.data();
#if IS_LINUX && HAVE_SYS_STAT_H
    struct stat statbuf{};
    auto code = stat( text, &statbuf);
    auto is_plain_text = false;
    auto is_file = false;
    std::string mime_type;
    if( code == 0 && S_ISREG( statbuf.st_mode))
    {
        is_file = true;
        using namespace std::string_literals;
        PropertyManager<FILE *> stream_handler( popen(( "file --mime-type "s + text).c_str(), "r"), pclose);
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
        std::cerr << "Invalid file specified: " << text << ".\nExpected: text/plain, got: " << mime_type;
        exit( EXIT_FAILURE);
    }

    if( is_plain_text)
    {
#else
        auto ext_pos = strrchr( text, '.');
        if( ext_pos != nullptr && strcasecmp( ext_pos + 1, "txt") == 0)
        {
#endif
        std::wifstream handle( text, std::ios::in | std::ios::ate);
        if( handle.good())
        {
            auto n_bytes = handle.tellg();
            text_ = std::wstring( n_bytes, L' ');
            handle.seekg( 0);
            handle.read( &text_[ 0], n_bytes);
            handle.close();
        }
    }
    else
        text_ = TextCodec::toWString( text);
}

FrameBuffer<uint32_t> TextRenderer::render()
{
    auto& color_rule = app_manager_.color_rule;
    auto map = [ this]( auto color_rule, auto bkroot)
    {
        auto rules = parser_.parseColorRule( color_rule);
        std::vector<std::shared_ptr<ColorRule>> new_rules( rules.size());
        std::transform( rules.cbegin(), rules.cend(), new_rules.begin(),
                        []( auto rule) { return std::make_shared<ColorRule>( rule);});
        return new_rules;
    };
    auto rules = color_rule == nullptr ? std::vector<std::shared_ptr<ColorRule>>{}
                                       : map( color_rule, app_manager_.bkroot.get());
    auto general = std::make_shared<ColorRule>();
    auto [ text_rows, _] = Util::expand( std::wstring_view( text_.data()), app_manager_.j_mode, false);
    auto total_length = 0;
    for( auto& row : text_rows)
        total_length += INT_CAST( row.size());
    rasters_.resize( total_length);
    row_details_.resize( text_rows.size());

    FT_Glyph o_glyph;
    FT_Stroker stroker;
    FT_Stroker_New( library_, &stroker);
    PropertyManager<FT_Stroker> stroker_deleter( stroker, FT_Stroker_Done);
    FT_Stroker_Set( stroker, To26Dot6( app_manager_.thickness),
                    FT_STROKER_LINECAP_BUTT, FT_STROKER_LINEJOIN_MITER, 0);
    FT_Vector box_size{ 0, 0};
    for( int j = 0, t_row = INT_CAST( text_rows.size()), index = 0; j < t_row; ++j)
    {
        FT_Int max_ascent{};
        FT_Vector max_row_box{ 0, 0}, adjustment;
        auto &baseline    = row_details_[ j].baseline,
                &max_descent = row_details_[ j].max_descent;
        row_details_[ j].length = INT_CAST( text_rows[ j].size());
        int line_height = 0, max_shadow_y = 0, max_row_bounce = 0;
        for( int i = 0, t_len = INT_CAST( text_rows[ j].length()); i < t_len; ++i, ++index)
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

                if( i + 1 >= each->start.x && i + 1 <= each->end.x
                    && j + 1 >= each->start.y && j + 1 <= each->end.y)
                    best = each;
            }

            /*
             * Calculate font size for each glyph
             */
            FT_Int font_size = UNSET( best->font_size_b) ? best->font_size_b = app_manager_.font_size
                                                         : best->font_size_b;
            if( best->font_easing_fn)
            {
                auto start = Vec2D<int>( i + 1 - best->start.x, j + 1 - best->start.y),
                        end   = Vec2D<int>( UNSET( best->end.x)
                                            ? std::max<int>( total_length - 1, 1) : best->end.x - best->start.x,
                                            UNSET( best->end.y)
                                            ? std::max<int>( t_row - 1, 1) : best->end.y - best->start.y);

                float fraction;
                if( app_manager_.ease_col)
                    fraction = best->font_easing_fn( FLOAT_CAST( start.y) / FLOAT_CAST( end.y));
                else
                    fraction = best->font_easing_fn( FLOAT_CAST( start.x) / FLOAT_CAST( end.x));
                if( UNSET( best->font_size_m) && !UNSET( best->font_size_e))
                    font_size = INT_CAST( round( best->font_size_b * ( 1.0 - fraction)
                                                 + fraction * best->font_size_e));
                else if( !UNSET( best->font_size_m) && !UNSET( best->font_size_e))
                    font_size = INT_CAST( round( +2.0 * best->font_size_b * ( fraction - .5) * ( fraction - 1.)
                                                 -4.0 * best->font_size_m * fraction * ( fraction - 1.)
                                                 +2.0 * best->font_size_e * fraction * ( fraction - .5)));
            }

            line_height = INT_CAST( font_size * app_manager_.line_height);

            if ( FT_Set_Char_Size( face_, font_size << 6, 0, app_manager_.dpi, 0) != 0)
                continue;

            if( FT_Load_Char( face_, c_char, FT_LOAD_DEFAULT) != 0
                || FT_Get_Glyph( face_->glyph, &o_glyph) != 0
                || o_glyph->format != FT_GLYPH_FORMAT_OUTLINE)
                continue;

            /*
             * Draw the outline for the glyph
             */
            FT_Glyph_StrokeBorder( &o_glyph, stroker, false, true);

            auto& raster = rasters_[ index];
            renderSpans( &reinterpret_cast<FT_OutlineGlyph>( o_glyph)->outline,
                         &raster.spans.second);
            raster.match = best;
            raster.level = j;
            raster.pos = Vec2D<int>( i + 1, j + 1);
            /*
             * Render bitmap for the glyph
             */
            FT_Render_Glyph( face_->glyph, FT_RENDER_MODE_NORMAL);
            auto slot = face_->glyph;
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
                raster.advance = { static_cast<FT_Pos>(( o_glyph->advance.x >> 16) + app_manager_.thickness * 2),
                                   ( o_glyph->advance.y >> 16)};
                height = rect.height();
                if( i > 0 && FT_HAS_KERNING( face_))
                {
                    FT_Get_Kerning( face_, c_char, text_rows[ j][ i - 1],
                                    FT_KERNING_DEFAULT, &adjustment);
                    max_row_box.x += adjustment.x >> 6;
                    max_row_box.y += adjustment.y >> 6;
                }
                /*
                 * Set the spacing of the glyphs to ensure no extra space occurs
                 * at the far end.
                 */
                max_row_box.x += ( i + 1 == t_len ? rect.width() : INT_CAST( raster.advance.x))
                                 + INT_CAST( std::abs( best->shadow.x));
                max_row_box.y = std::max<long>( max_row_box.y, height);
                max_shadow_y  = std::max( max_shadow_y, INT_CAST( std::abs( best->shadow.y)));
                raster.bbox = rect;
                bearing_y = From26Dot6( slot->metrics.horiBearingY) + app_manager_.thickness * 2;
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
                int default_width  = From26Dot6( slot->metrics.horiAdvance) + app_manager_.thickness * 2;
                max_row_box.x += default_width;
                raster.advance.x = default_width;
            }
            max_row_bounce = std::max( max_row_bounce, INT_CAST( best->max_bounce));

            if( best->color_easing_fn && best != general)
            {
                best->gradient->width = INT_CAST( std::max( max_row_box.x, box_size.x));
                best->gradient->height = INT_CAST( box_size.y) + ( max_row_box.y == 0 ?
                                                                   ( face_->glyph->metrics.vertAdvance >> 6)
                                                                   + app_manager_.thickness * 2
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
            int default_height = From26Dot6( face_->glyph->metrics.vertAdvance) + app_manager_.thickness * 2;
            for( wchar_t c_char : text_rows[ j])
            {
                if( !std::isspace( c_char))
                    continue;
                auto& raster = rasters_[ index];
                raster.advance.y = default_height;
            }
            max_ascent = INT_CAST( default_height);
        }
        max_row_box.y = max_ascent + max_descent + max_shadow_y + max_row_bounce;
        row_details_[ j].width = INT_CAST( max_row_box.x + app_manager_.pad.right
                                           + app_manager_.pad.left - max_shadow_y);
        row_details_[ j].max_shadow_y = max_shadow_y;
        if( max_row_box.y <= 0)
            continue;
        box_size.x = std::max( max_row_box.x, box_size.x); // The width of the largest row
        auto row_line_height = line_height * ( j + 1 != t_row); // The spacing between glyphs
        box_size.y += max_row_box.y + row_line_height;
        // Offset at which each row should start
        row_details_[ j].v_disp += INT_CAST( box_size.y) - row_line_height - max_shadow_y;
    }

    std::clog << "Generated glyphs after: " << Timer::yield( GLOBAL_TIME_ID) <<'\n';

    box_size.x += app_manager_.pad.left + app_manager_.pad.right;
    box_size.y += app_manager_.pad.top  + app_manager_.pad.bottom;
    std::shared_ptr<uint32_t> pixel(( uint32_t *)malloc( sizeof( uint32_t) * box_size.x * box_size.y),
                                    []( auto *p){ free( p);});
    std::fill_n( pixel.get(), box_size.x * box_size.y, app_manager_.background_color.cast());

    return FrameBuffer<uint32_t>{ pixel, static_cast<int32_t>( box_size.x),
                                  static_cast<int32_t>( box_size.y)};
}

TextRenderer::~TextRenderer()
{
    FT_Done_Face( face_);
    FT_Done_FreeType( library_);
}

RowDetails& TextRenderer::getRowDetails()
{
    return row_details_;
}

MonoGlyphs& TextRenderer::getRasters()
{
    return rasters_;
}

void TextRenderer::spansCallback( int y, int count, const FT_Span *spans, void *user)
{
    auto *sptr = static_cast<Spans *>( user);
    for ( int i = 0; i < count; ++i)
        sptr->emplace_back( spans[ i].x, y, spans[ i].len, spans[ i].coverage);
}

void TextRenderer::renderSpans( FT_Outline *outline, Spans *spans)
{
    FT_Raster_Params params;
    memset( &params, 0, sizeof( params));
    params.flags = FT_RASTER_FLAG_AA | FT_RASTER_FLAG_DIRECT;
    params.gray_spans = spansCallback;
    params.user = spans;

    FT_Outline_Render( library_, outline, &params);
}