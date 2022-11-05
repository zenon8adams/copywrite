#include <iostream>
#include <cstdio>
#include <string_view>
#include <string>
#include <regex>
#include <iomanip>
#include <cassert>
#include "copywrite.hpp"
#include "parsers/command_line_parsers.hpp"
#include "blend_defs.hpp"
#include "colors_defs.hpp"
#include "easing_defs.hpp"
#include "parsers/rule_parser.hpp"
#include "plugins/plugin_manager.hpp"
#include "plugins/local_font_manager.hpp"
#include "plugins/image_manager.hpp"
#include "composition_defs.hpp"
#include "utils/color_utils.hpp"
#include "utils/utils.hpp"

CommandLineParser::CommandLineParser( int ac, char **av)
: argc( ac), argv( av)
{
}

ApplicationDirector CommandLineParser::process()
{
    ApplicationDirector director;

    auto& kdroot = director.kdroot;
    auto& bkroot = director.bkroot;
#include "static_kdtree_lut.inserts"
#include "static_bktree_lut.inserts"

    const char *font_profile{ PROJECT_DEFAULT_FONT},
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
            *test_rule{ nullptr},
            *stroke_width{ nullptr},
            *line_height{ nullptr},
            *easing_direction{ nullptr},
            *font_size{ nullptr},
            *background_color{ nullptr},
            *final_size{ nullptr},
            *word{ nullptr},
            *program{ *argv};

    while( --argc > 0 && ( *++argv)[ 0] == '-')
    {
        const char *directive = *argv + 1 + ( *( *argv + 1) == '-'), // Allow for only one hyphen
        *end = strchr( directive, '='),
                *index = nullptr,
                **selection = nullptr;
        size_t count = end == nullptr ? strlen( directive) : end - directive;

        if( FOUND_STRING( strcmp( directive, HELP_PROMPT))
        || FOUND_STRING( strcmp( directive, SHORT( HELP_PROMPT))))
            goto help;
        else if( FOUND_STRING( strcmp( directive, LIST_FONTS))
         || FOUND_STRING( strcmp( directive, SHORT( LIST_FONTS))))
        {
            puts( "Available fonts:\n");
	        // Get the list of all fonts installed on the current system
			auto fonts = Util::requestFontList();
			for( auto &[font, style]: fonts)
				std::cout << "Family: " << font << ", style(s): " << style <<'\n';
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
         || ( strlen( SHORT( COLOR_RULE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( COLOR_RULE), count))))
            selection = &director.color_rule;
        else if( FOUND_STRING( strcmp( directive, OUTPUT))
         || FOUND_STRING( strcmp( directive, SHORT( OUTPUT))) && argc > 0)
        {
            --argc;
            director.src_filename = *++argv;
#if defined( PNG_SUPPORTED) && defined( JPG_SUPPORTED)
            director.out_format = ImageManager::isJPEG( director.src_filename)
                                  ? OutputFormat::JPEG : director.out_format;
#elif defined( PNG_SUPPORTED)
            director.out_format = OutputFormat::PNG;
#elif defined( JPG_SUPPORTED)
            director.out_format = OutputFormat::JPEG;
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
        else if( ( strlen( COMPOSITION_RULE) == count
         && FOUND_STRING( strncmp( directive, COMPOSITION_RULE, count)))
         || ( strlen( SHORT( COMPOSITION_RULE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( COMPOSITION_RULE), count))))
              selection = &director.composition_rule;
        else if( FOUND_STRING( strcmp( directive, AS_IMAGE))
         || FOUND_STRING( strcmp( directive, SHORT( AS_IMAGE))))
                director.as_image = true;
        else if( ( strlen( QUALITY_INDEX) == count && FOUND_STRING( strncmp( directive, QUALITY_INDEX, count)))
         || ( strlen( SHORT( QUALITY_INDEX)) == count
         && FOUND_STRING( strncmp( directive, SHORT( QUALITY_INDEX), count))))
                selection = &image_quality;
        else if( ( strlen( DPI) == count && FOUND_STRING( strncmp( directive, DPI, count)))
         || ( strlen( SHORT( DPI)) == count
         && FOUND_STRING( strncmp( directive, SHORT( DPI), count))))
                selection = &resolution;
        else if(( strlen( EASING_DIRECTION) == count
         && FOUND_STRING( strncmp( directive, EASING_DIRECTION, count)))
         || ( strlen( SHORT( EASING_DIRECTION)) == count
         && FOUND_STRING( strncmp( directive, SHORT( EASING_DIRECTION), count))))
                selection = &easing_direction;
        else if(( strlen( FINAL_SIZE) == count && FOUND_STRING( strncmp( directive, FINAL_SIZE, count)))
         || ( strlen( SHORT( FINAL_SIZE)) == count
         && FOUND_STRING( strncmp( directive, SHORT( FINAL_SIZE), count))))
                selection = &final_size;
        #endif
        else if(( strlen( FONT_SIZE) == count && FOUND_STRING( strncmp( directive, FONT_SIZE, count)))
         || ( strlen( SHORT( FONT_SIZE)) == count && FOUND_STRING( strncmp( directive, SHORT( FONT_SIZE), count))))
            selection = &font_size;
        else if( ( strlen( BACKGROUND_COLOR) == count
         && FOUND_STRING( strncmp( directive, BACKGROUND_COLOR, count)))
         || ( strlen( SHORT( BACKGROUND_COLOR)) == count
         && FOUND_STRING( strncmp( directive, SHORT( BACKGROUND_COLOR), count))))
            selection = &background_color;
        else if(( strlen( DRAWING_CHARACTER) == count
         && FOUND_STRING( strncmp( directive, DRAWING_CHARACTER, count)))
         || ( strlen( SHORT( DRAWING_CHARACTER)) == count
                      && FOUND_STRING( strncmp( directive, SHORT( DRAWING_CHARACTER), count))))
            selection = &director.raster_glyph;
        else if( ( strlen( LINE_HEIGHT) == count && FOUND_STRING( strncmp( directive, LINE_HEIGHT, count)))
         || ( strlen( SHORT( LINE_HEIGHT)) == count
                      && FOUND_STRING( strncmp( directive, SHORT( LINE_HEIGHT), count))))
            selection = &line_height;
        else if( ( strlen( STROKE_WIDTH) == count && FOUND_STRING( strncmp( directive, STROKE_WIDTH, count)))
         || ( strlen( SHORT( STROKE_WIDTH)) == count
                      && FOUND_STRING( strncmp( directive, SHORT( STROKE_WIDTH), count))))
            selection = &stroke_width;
        else if( ( strlen( JUSTIFY) == count && FOUND_STRING( strncmp( directive, JUSTIFY, count)))
         || ( strlen( SHORT( JUSTIFY)) == count && FOUND_STRING( strncmp( directive, SHORT( JUSTIFY), count))))
            selection = &justification;
        #if defined( CUSTOM_FONT_SUPPORTED)
        else if( ( strlen( UNINSTALL_FONT) == count
         && FOUND_STRING( strncmp( directive, UNINSTALL_FONT, count)))
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
         || ( strlen( SHORT( PADDING_TOP)) == count
         && FOUND_STRING( strncmp( directive, SHORT( PADDING_TOP), count))))
                selection = &padding_top;
        else if( ( strlen( PADDING_BOTTOM) == count
         && FOUND_STRING( strncmp( directive, PADDING_BOTTOM, count)))
         || ( strlen( SHORT( PADDING_BOTTOM)) == count
         && FOUND_STRING( strncmp( directive, SHORT( PADDING_BOTTOM), count))))
                selection = &padding_bottom;
        else if( ( strlen( TEST_COLOR) == count && FOUND_STRING( strncmp( directive, TEST_COLOR, count)))
         || ( strlen( SHORT( TEST_COLOR)) == count
         && FOUND_STRING( strncmp( directive, SHORT( TEST_COLOR), count))))
                selection = &test_rule;
        #endif
        if( selection != nullptr && ( index = strchr( directive, '=')) != nullptr)
            *selection = index + 1;
        else if( ACCESSIBLE(  selection))
        {
            argc = -1;
            break;
        }
    }

    if( ACCESSIBLE(  test_rule))
        testColor( test_rule, bkroot.get());

    if( argc == 1)
        word = *argv;
    else if( !ACCESSIBLE( test_rule))
    {
        help:
        helpMe( program);
    }
    else
        exit( EXIT_SUCCESS);

    if( ACCESSIBLE(  line_height))
        director.line_height = strtof( line_height, nullptr);
    if( ACCESSIBLE(  stroke_width))
        director.thickness = strtoul( stroke_width, nullptr, 10);
    if( ACCESSIBLE(  padding_left))
        director.pad.left = INT_CAST( strtol( padding_left, nullptr, 10));
    if( ACCESSIBLE(  padding_right))
        director.pad.right = INT_CAST( strtol( padding_right, nullptr, 10));
    if( ACCESSIBLE(  padding_top))
        director.pad.top = INT_CAST( strtol( padding_top, nullptr, 10));
    if( ACCESSIBLE(  padding_bottom))
        director.pad.bottom = INT_CAST( strtol( padding_bottom, nullptr, 10));

    if( ACCESSIBLE(  justification))
    {
        if( std::cmatch cm; std::regex_match( justification, justification + strlen( justification), cm,
                                              std::regex( R"(^(left)|(right)|(center)$)")), std::regex_constants::icase)
        {
            director.j_mode = cm[ 1].matched ? Justification::Left
                                             : cm[ 2].matched ? Justification::Right : Justification::Center;
        }
    }

#if PNG_SUPPORTED || JPG_SUPPORTED
    if( ACCESSIBLE(  resolution))
                director.dpi = INT_CAST( strtoul( resolution, nullptr, 10));

            if( ACCESSIBLE( image_quality))
                director.image_quality = INT_CAST( strtol( image_quality, nullptr, 10));

            if( ACCESSIBLE( final_size))
                RuleParser::parseFinalSize( final_size, director.interpolation);
#endif
    if( ACCESSIBLE(  background_color))
        director.background_color = ColorUtil::extractColor( background_color, bkroot.get());

    if( ACCESSIBLE(  font_size))
        director.font_size = strtol( font_size, nullptr, 10);

    auto codec = dynamic_cast<LocalFontManager *>( PluginManager::instance()->get( LOCAL_FONT_MANAGER));
    if( codec != nullptr)
    {
        if( FOUND_STRING( strcmp( custom_font_action, "install")))
            codec->installFont( custom_font);
        else
            codec->uninstallFont( custom_font);
    }

    if( ACCESSIBLE(  easing_direction))
    {
        std::regex rule( R"(^(row)|(col)$)", std::regex_constants::icase);
        std::cmatch cm;
        if( std::regex_match( easing_direction, easing_direction + strlen( easing_direction), cm, rule))
            director.ease_col = cm[ 2].matched;
    }

    director.font_profile = font_profile;
    director.text         = word;

    return director;
}

void CommandLineParser::helpMe( std::string program)
{
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

    auto *prog_ptr = strrchr( program.data(), '/');
    fprintf( stdout, "Usage: %s [OPTION]... FILE|TEXT\n", prog_ptr == nullptr ? program.data() : ++prog_ptr);
    fprintf( stdout, "Convert the content of FILE or TEXT into a format defined by OPTIONs\n\n");
    fprintf( stdout, "The following options can be used to tune the generator:\n");
    for( size_t j = 0, options_size = OPTIONS_COUNT; j < options_size; ++j)
    {
        auto [ help_lines, max_line] = Util::expand( options_message[ j], Justification::Left);
        int base_spacing = INT_CAST( max_length) + max_line + ALLOWANCE;
        std::cout << options[ j] << std::setw( INT_CAST( base_spacing) - options[ j].size())
                  << help_lines[ 0] <<'\n';
        size_t idx = 1, help_lines_size = help_lines.size();
        while( idx < help_lines_size)
            std::cout << std::setw( base_spacing) << help_lines[ idx++] <<'\n';
    }
    fprintf( stdout, "NB! The color names available are compliant with those defined\n"
                     "by CSS standard(https://www.w3.org/TR/css-color-3/)\n");
    exit( EXIT_FAILURE);
}

void CommandLineParser::testColor( const char *rule, BKNode *bkroot)
{
    std::shared_ptr<KDNode> kdroot;
    #include "static_kdtree_verifier_lut.inserts"

    auto getName = [ &kdroot]( uint32_t color)
    {
        double initial = INFINITY;
        return getColorNameAt(
                Util::approximate( kdroot.get(), Color{ RED( color), GREEN( color), BLUE( color)},
                                   initial)->index).data();
    };

    auto *given_rule = rule;
    uint64_t first_color{}, second_color{}, color_sum{};
    if( Util::ltrim( rule) && *rule == '(')
        first_color = ColorUtil::extractColor( ++rule, bkroot);
    else if( *rule != 0)
        first_color = ColorUtil::extractColor( rule, bkroot);
    uint16_t ops{};
    if( Util::ltrim( rule) && *rule && rule[ 1] == '/')
    {
        ops = MAKE_WORD( rule[ 2], *rule);
        rule += 3;
    }
    else if( Util::compareOr<std::equal_to<char>>( *rule, '+', '-'))
        ops = INT_CAST( *rule++);
    if( ops != 0)
    {
		// If only one operator is specified, use for both outline and fill
		if( LOW_BYTE( ops) == 0)
			ops = MAKE_WORD( HIGH_BYTE( ops), HIGH_BYTE( ops));
		else if( HIGH_BYTE( ops) == 0)
			ops = MAKE_WORD( LOW_BYTE( ops), LOW_BYTE( ops));

        second_color = ColorUtil::extractColor( rule, bkroot);
        uint8_t alpha{ 0xff};
        if( Util::ltrim( rule) && *rule == ':')
            alpha = Util::getNumber( ++rule, 16);
        color_sum = MAKE_QWORD( HIGH_BYTE( ops) == '+' ?
                                ColorUtil::sumMix( HIGH_DWORD( first_color), HIGH_DWORD( second_color)) | alpha :
                                ColorUtil::subMix( HIGH_DWORD( first_color), HIGH_DWORD( second_color)) | alpha,
                                LOW_BYTE( ops) == '+' ?
                                ColorUtil::sumMix( LOW_DWORD( first_color), LOW_DWORD( second_color)) | alpha :
                                ColorUtil::subMix( LOW_DWORD( first_color), LOW_DWORD( second_color)) | alpha);
        if( Util::compareOr<std::equal_to<char>>( LOW_BYTE( ops), '+', '-'))
            printf( "The result of the rule `%s` is thus:\n"
                    "Background colors: #%x(%s) %c #%x(%s) = #%x(%s)\n", given_rule,
                    LOW_DWORD( first_color), getName( LOW_DWORD( first_color)),
                    LOW_BYTE( ops), LOW_DWORD( second_color), getName( LOW_DWORD( second_color)),
                    LOW_DWORD( color_sum), getName( LOW_DWORD( color_sum)));
        if( Util::compareOr<std::equal_to<char>>( HIGH_BYTE( ops), '+', '-'))
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

std::string_view CommandLineParser::getColorNameAt( size_t pos)
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
