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
#include "parsers/option-builder.hpp"

CommandLineParser::CommandLineParser( int ac, char **av)
: argc( ac), argv( av)
{
}

ApplicationDirector CommandLineParser::process()
{
    ApplicationDirector director;
#if CUSTOM_FONT_SUPPORTED
	PluginManager::instance()->install( std::make_unique<LocalFontManager>( director));
#endif

    auto& kdroot = director.kdroot;
    auto& bkroot = director.bkroot;
#include "static_kdtree_lut.inserts"
#include "static_bktree_lut.inserts"

#define $(key, value) value

	OptionBuilder builder( argc, argv);
	const char *text = nullptr;
	builder.addMismatchConsumer([&text, this]( auto *token, int ac)
    {
		if( ac == argc)
			text = token;
		else
			std::cerr << "Unknown option: " << token <<'\n';
	});
	builder.addOption( HELP_PROMPT,             SHORT( HELP_PROMPT))
		   .addOption( LIST_FONTS,              SHORT( LIST_FONTS))
		   .addOption( LIST_EASINGS,            SHORT( LIST_EASINGS))
		   .addOption( LIST_COMPOSITION_MODES,  SHORT( LIST_COMPOSITION_MODES))
		   .addOption( LIST_BLEND_MODES,        SHORT( LIST_BLEND_MODES))
		   .addOption( AS_IMAGE,                SHORT( AS_IMAGE))
		   .addOption( FONT_PROFILE,            SHORT( FONT_PROFILE), $(DEFAULT, getDefaultFont()))
		   .addOption( COLOR_RULE,              SHORT( COLOR_RULE))
		   .addOption( OUTPUT,                  SHORT( OUTPUT), $(DEFAULT, ""), $(ARG, 1))
		   .addOption( COMPOSITION_RULE,        SHORT( COMPOSITION_RULE))
		   .addOption( QUALITY_INDEX,           SHORT( QUALITY_INDEX), $(DEFAULT, "100"))
		   .addOption( DPI,                     SHORT( DPI), $(DEFAULT, "120"))
		   .addOption( EASING_DIRECTION,        SHORT( EASING_DIRECTION))
		   .addOption( FINAL_SIZE,              SHORT( FINAL_SIZE))
		   .addOption( FONT_SIZE,               SHORT( FONT_SIZE), $(DEFAULT, "10"))
		   .addOption( BACKGROUND_COLOR,        SHORT( BACKGROUND_COLOR))
		   .addOption( DRAWING_CHARACTER,       SHORT( DRAWING_CHARACTER), $(DEFAULT, "\u2589"))
		   .addOption( LINE_HEIGHT,             SHORT( LINE_HEIGHT), $(DEFAULT, "1.15"))
		   .addOption( STROKE_WIDTH,            SHORT( STROKE_WIDTH), $(DEFAULT, "0"))
		   .addOption( JUSTIFY,                 SHORT( JUSTIFY))
		   .addOption( UNINSTALL_FONT,          SHORT( UNINSTALL_FONT))
		   .addOption( INSTALL_FONT,            SHORT( INSTALL_FONT))
		   .addOption( PADDING_LEFT,            SHORT( PADDING_LEFT), $(DEFAULT, "0"))
		   .addOption( PADDING_RIGHT,           SHORT( PADDING_RIGHT), $(DEFAULT, "0"))
		   .addOption( PADDING_TOP,             SHORT( PADDING_TOP), $(DEFAULT, "0"))
		   .addOption( PADDING_BOTTOM,          SHORT( PADDING_BOTTOM), $(DEFAULT, "0"))
		   .addOption( TEST_COLOR,              SHORT( TEST_COLOR))
		   .build();

	auto output = builder.asDefault( OUTPUT);
	if( ACCESSIBLE( output))
	{
		director.src_filename = output;
#if defined( PNG_SUPPORTED) && defined( JPG_SUPPORTED)
		director.out_format = ImageManager::isJPEG( director.src_filename)
		                      ? OutputFormat::JPEG : director.out_format;
#elif defined( PNG_SUPPORTED)
		director.out_format = OutputFormat::PNG;
#elif defined( JPG_SUPPORTED)
            director.out_format = OutputFormat::JPEG;
#endif
	}

	if( builder.asBool( LIST_EASINGS))
		listEasingFunctions();

	if( builder.asBool( LIST_BLEND_MODES))
		listBlendModes();

	if( builder.asBool( LIST_COMPOSITION_MODES))
		listCompositionModes();

	if( builder.asBool( LIST_FONTS))
	{
		puts( "Available fonts:\n");
		// Get the list of all fonts installed on the current system
		auto fonts = Util::requestFontList();
		for( auto &[font, style]: fonts)
			std::cout << "Family: " << font << ", style(s): " << style <<'\n';
	}

    if( builder.asBool( TEST_COLOR))
	    testColor( builder.asDefault( TEST_COLOR), bkroot.get());

	auto callable = [&builder](auto &&param) { return builder.asBool( std::forward<decltype( param)>( param)); };
	auto list_printed = Util::compareOr( callable, LIST_EASINGS, LIST_BLEND_MODES, LIST_COMPOSITION_MODES, LIST_FONTS);
	if( !ACCESSIBLE( text) && !list_printed)
    {
        helpMe( *argv);
		if( !builder.asBool( HELP_PROMPT))
			exit( EXIT_FAILURE);
		exit( EXIT_SUCCESS);
    }
	else if( list_printed)
		exit( EXIT_SUCCESS);

	auto justification = builder.asDefault( JUSTIFY);
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
    director.dpi           = INT_CAST( builder.asInt( DPI));
    director.image_quality = INT_CAST( builder.asInt( QUALITY_INDEX));

    if( builder.asBool( FINAL_SIZE))
        RuleParser::parseFinalSize( builder.asDefault( FINAL_SIZE), director.interpolation);
#endif
	auto background_color = builder.asDefault( BACKGROUND_COLOR);
    if( ACCESSIBLE(  background_color))
        director.background_color = ColorUtil::extractColor( background_color, bkroot.get());

    auto codec = dynamic_cast<LocalFontManager *>( PluginManager::instance()->get( LOCAL_FONT_MANAGER));
    if( ACCESSIBLE( codec))
    {
		auto font_name = builder.asDefault( INSTALL_FONT);
		if( ACCESSIBLE( font_name))
			codec->installFont( font_name);
		else
		{
			font_name = builder.asDefault( UNINSTALL_FONT);
			if( ACCESSIBLE( font_name))
				codec->uninstallFont( font_name);
		}
    }

	auto easing_direction = builder.asDefault( EASING_DIRECTION);
    if( ACCESSIBLE(  easing_direction))
    {
        std::regex rule( R"(^(row)|(col)$)", std::regex_constants::icase);
        std::cmatch cm;
        if( std::regex_match( easing_direction, easing_direction + strlen( easing_direction), cm, rule))
            director.ease_col = cm[ 2].matched;
    }

	if( builder.asBool( COLOR_RULE))
		director.color_rule = builder.asDefault( COLOR_RULE);

	if( builder.asBool( COMPOSITION_RULE))
		director.composition_rule = builder.asDefault( COMPOSITION_RULE);

	director.as_image     = builder.asBool( AS_IMAGE);
	director.font_size    = INT_CAST( builder.asInt( FONT_SIZE));
	director.line_height  = builder.asFloat( LINE_HEIGHT);
	director.thickness    = INT_CAST( builder.asInt( STROKE_WIDTH));
	director.pad.left     = INT_CAST( builder.asInt( PADDING_LEFT));
	director.pad.right    = INT_CAST( builder.asInt( PADDING_RIGHT));
	director.pad.top      = INT_CAST( builder.asInt( PADDING_TOP));
	director.pad.bottom   = INT_CAST( builder.asInt( PADDING_BOTTOM));
	director.raster_glyph = builder.asDefault( DRAWING_CHARACTER);
    director.font_profile = builder.asDefault( FONT_PROFILE);
    director.text         = text;

    return director;
}

void CommandLineParser::listCompositionModes()
{
	printf("Available composition methods:\n");
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

void CommandLineParser::listBlendModes()
{
	printf("Available blend modes:\n");
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

void CommandLineParser::listEasingFunctions()
{
	printf("Available easing functions:\n");
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
