#include <vector>
#include "copywrite.hpp"
#include "utils/utils.hpp"
#include "utils/color_utils.hpp"
#include "composition_defs.hpp"
#include "easing_defs.hpp"
#include "blend_defs.hpp"
#include "special_effects.hpp"
#include "parsers/rule_parser.hpp"

RuleParser::RuleParser( ApplicationDirector& manager)
: app_manager_( manager)
{
}

ConicGradient RuleParser::parseConicGradient( const char *&rule, const ColorRule& color_rule)
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

        auto stops = Util::partition( gradient_part, R"(\s*,\s*(?=[a-zA-Z#]|0[xX]))");
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
                                start_angle = i == 0 ? INT_CAST( end_angle) : start_angle;
                            else
                            {
                                const char *underlying_data = maybe_color.data();
                                auto color_components = ColorUtil::extractColor( underlying_data,
                                                                                 app_manager_.bkroot.get());
                                actual_gradient.color_variations.emplace_back(
                                        color_components, initial_run ? SIZE_MAX : std::abs( INT_CAST( end_angle)));
                                if( end_angle != SIZE_MAX && (int)end_angle < 0)
                                    fprintf( stderr, "Negative position not allowed! |%d| will be used\n",
                                             INT_CAST( end_angle));
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

        int cstops_length = INT_CAST( cstops.size());
        // Adjust cstop colors so that they appear in an increasing sequence.
        for( size_t i = 1; i < cstops_length; ++i)
        {
            if( cstops[ i].second == SIZE_MAX)
            {
                auto start = cstops.cbegin() + INT_CAST( i);
                auto next_valid = std::find_if( start, cstops.cend(), []( auto& el)
                { return el.second != SIZE_MAX;});
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
                    cstops[ i].second = cstops[ i - 1].second
                                        + ( next_valid->second - cstops[ i - 1].second) / ( dist + 1);
                }
            }
            else
                cstops[ i].second = std::max( cstops[ i - 1].second, cstops[ i].second);
        }

        size_t i = 0;
        int rem;
        for( ; i < cstops_length; ++i)
        {
            if(( cstops[ i].second += start_angle) > DEG_MAX)
            {
                rem = i > 0 ? INT_CAST( cstops[ i].second - cstops[ i - 1].second) : 0;
                cstops[ i++].second = DEG_MAX;
                break;
            }
        }
        for( size_t j = i, shift = 0; j < cstops_length; ++j)
        {
            if( cstops[ j + shift - 1].second == DEG_MAX)
                rem = INT_CAST( cstops[ j + shift].second + start_angle -  cstops[ j + shift - 1].second - rem);
            else
                rem = INT_CAST( cstops[ j + shift].second - cstops[ j + shift - 1].second);
            rem = INT_CAST( cstops[ 0].second) - rem;
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
        cstops.erase( cstops.end() - cstops_length + INT_CAST( i), cstops.end());
        if( cstops.back().second != DEG_MAX)
            cstops.emplace_back( cstops.back().first, DEG_MAX);
    }

    return actual_gradient;
}

std::vector<CompositionRule> RuleParser::parseCompositionRule( std::string_view rule)
{
    using namespace std::string_literals;
    std::smatch match_results;
    //Match any of the following:
    //1. from 30deg, mode=source-over
    //2. from 30deg, at .5, 0.45, mode=source-over
    //3. snap=top-left|top-right|top-center| ...|bottom-right
    //4. layer=top|left|both
    //5. mode=source-over or mode=lighter
    //6. blend=dissolve or blend
    //7. effect=blur|(twirl, top, 30, 20, 20deg, top-left)
    //8. size=300w bicubic
    std::string_view base( R"((?:[\s\n]*from[\s\n]+([+-]?\d{1,3})deg)"
						   R"((?:[\s\n]+at[\s\n]+([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?))"
                           R"(,[\s\n]*([+-]?\d*.\d+|[+-]?\d+(?:\.\d*)?))?,[\s\n]*)?[\s\n]*)"
                           R"((?:snap=([a-z]+(?:-[a-z]+)?),[\s\n]*)?)"
                           R"(layer=(.+?)[\s\n]*,[\s\n]*mode=([a-z]+(?:-[a-z]+)?))"
                           R"((?:,[\s\n]*blend=([a-z]+(?:-[a-z]+)?(?:[\s\n]*\|[\s\n]*[a-z]+(?:-[a-z]+)?)*))?)"
                           R"((?:,[\s\n]*effect=((?:[a-z]+|\(.+?\))(?:[\s\n]*\|[\s\n]*(?:[a-z]+|\(.+?\)))*))?)"
                           R"((?:,[\s\n]*size=(.+?))?)");
    auto parts = Util::partition( rule, R"(;[\s\n]*(?=\[))");
    if( parts.empty())
        parts.emplace_back( rule.data());
    std::regex matcher( base.data(), std::regex_constants::icase);
    std::vector<CompositionRule> c_rules;
    for( auto& part : parts)
    {
        if( part.front() == '[')
            part.erase( 0, 1);
        if( part.back() == ']')
            part.erase( part.size() - 1, 1);
        if( std::regex_match( part.cbegin(), part.cend(), match_results, matcher))
        {
            auto x_origin = match_results[ 2].str(),
                    y_origin = match_results[ 3].str(),
                    angle    = match_results[ 1].str();
            c_rules.push_back(
	            CompositionRule{
	                .c_model   = selectCompositionModel( match_results[ 6].str()),
	                .b_models  = selectBlendModels( match_results[ 7].str()),
	                .position  = Vec2D( INT_CAST( x_origin.size()) ? std::stof( x_origin, nullptr)
	                                                               : INFINITY,
	                                    INT_CAST( y_origin.size()) ? std::stof( y_origin, nullptr)
	                                                               : INFINITY),
	                .snap      = Util::getSnapCoordinate( match_results[ 4].str()),
	                .angle     = INT_CAST( angle.size()) ? std::stoi( angle, nullptr, 10) : 0,
	                .image     = match_results[ 5].str(),
	                .s_effects = extractEffects( match_results[ 8].str())
	            }
			);
            parseFinalSize( match_results[ 9].str().data(), c_rules.back().interpolation);
        }
    }

    return c_rules;
}

std::vector<ColorRule> RuleParser::parseColorRule( const char *rule)
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
                    ccolor.start.x = INT_CAST( Util::getNumber( ++rule));
                    if( Util::ltrim( rule) && *rule++ != ',')
                    {
                        fprintf( stderr, "Expected a `,` near -> %s", rule);
                        exit( EXIT_FAILURE);
                    }
                    ccolor.start.y = INT_CAST( Util::getNumber( rule));
                    if( Util::ltrim( rule) && *rule++ != ')')
                    {
                        fprintf( stderr, "Expected a `)` near -> %s", rule);
                        exit( EXIT_FAILURE);
                    }
                }
                else
                    ccolor.start.x = INT_CAST( Util::getNumber( rule));
                if( *rule != '\0')
                {
                    if( *rule == '.' && *( rule + 1) == '.')
                    {
                        rule += 2;
                        if( isdigit( *rule))
                            ccolor.end.x = INT_CAST( Util::getNumber( rule));
                        else if( *rule == '(')
                        {
                            ++rule;
                            // Allow user to specify -1 in case they want to
                            // set `end.y` alone.
                            if( *rule == '-' && rule[ 1] == '1')
                                rule += 2;
                            else
                                ccolor.end.x = INT_CAST( Util::getNumber( rule));
                            if( Util::ltrim( rule) && *rule++ != ',')
                            {
                                fprintf( stderr, "Expected a `,` near -> %s", rule);
                                exit( EXIT_FAILURE);
                            }
                            if( *rule == '-' && rule[ 1] == '1') // A sentinel of -1 is also allowed
                                rule += 2;
                            else
                                ccolor.end.y = INT_CAST( Util::getNumber( rule));
                            if( Util::ltrim( rule) && *rule++ != ')')
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
                    ccolor.font_size_b = Util::getNumber( ++rule);
                    bool mid = false;
                    if( *rule == '-')
                    {
                        ccolor.font_size_m = Util::getNumber( ++rule);
                        mid = true;
                    }
                    if( *rule == '-')
                        ccolor.font_size_e = Util::getNumber( ++rule);
                    else if( mid)
                    {
                        ccolor.font_size_e = ccolor.font_size_m;
                        ccolor.font_size_m = UINT32_MAX;
                    }

                    if( Util::ltrim( rule) && *rule == ';')
                        ccolor.max_bounce = Util::getNumber( ++rule);

                    if( Util::ltrim( rule) && *rule == '-')
                        fillEasingMode( ccolor.font_easing_fn, ++rule, ']');
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
                if( Util::ltrim( rule) && *rule != '\0')
                {
                    setColor( rule, ccolor.scolor);
                    if( Util::ltrim( rule) && *rule == '-')
                    {
                        auto gradient_is_next = Util::compareOr<std::equal_to<char>>(
                                std::tolower( rule[ 1]), 'c', 'r');
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
                            setColor( rule += 2, ccolor.ecolor);
                        }

                        if( Util::ltrim( rule) && *rule == '-'
                            && Util::compareOr<std::equal_to<char>>( std::tolower( rule[ 1]), 'c', 'r'))
                        {
                            Util::ltrim( ++rule);
                            auto lc = std::tolower( *rule);
                            if( lc == 'r')
                            {
                                auto [ x, y, z] = parseFloats<3>( rule += 2);
                                ccolor.gradient.reset( dynamic_cast<BaseGradient *>( new RadialGradient( x, y, z)));
                                if( Util::ltrim( rule) && *rule == ')')
                                    ++rule;
                            }
                            else if( lc == 'c')
                                ccolor.gradient.reset( dynamic_cast<BaseGradient *>(
                                                               new ConicGradient{ parseConicGradient( ++rule, ccolor)}));
                        }
                        if( Util::ltrim( rule) && *rule == '-' && std::tolower( rule[ 1]) == 's')
                        {
                            auto[ x_pos, y_pos, spread] = parseFloats<3>( rule += 2);
                            ccolor.shadow = Vec3D( INT_CAST( x_pos), INT_CAST( y_pos), INT_CAST( spread));
                            if( Util::ltrim( rule) && *rule == ')')
                                ++rule;
                            else if( *rule == ',')
                            {
                                ccolor.shadow_color = ColorUtil::extractColor( ++rule, app_manager_.bkroot.get());
                                if( Util::ltrim( rule) && *rule == ')')
                                    ++rule;
                            }
                        }

                        if( Util::ltrim( rule) && *rule == '+')
                        {
                            ccolor.soak = true;
                            ++rule;
                        }

                        fillEasingMode( ccolor.color_easing_fn, rule, '}');
                    }
                    if( Util::ltrim( rule) && *rule == '}')
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
			else
            {
				fprintf( stderr, "Rule not well defined.\n");
				exit( EXIT_FAILURE);
			}
        }
    }

    return rules;
}

void RuleParser::parseFinalSize( std::string_view rule, int *interpolation)
{
    std::cmatch cm;
    std::regex key( R"(\s*(?:(\d+)(w|h)|(\d+)\s*[x]\s*(\d+))\s*(bi(?:-)?linear|bi(?:-)?cubic)?\s*)",
                    std::regex_constants::icase);
    if( std::regex_search( rule.cbegin(), rule.cend(), cm, key))
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
            interpolation[ size - 1]  = auto_scale ? selection == 'h' ? -1 :
                                                     std::stoi( cm[ size].str()) : std::stoi( cm[ width].str());
            interpolation[ side - 1] = auto_scale ? selection == 'w' ? -1 :
                                                    std::stoi( cm[ size].str()) : std::stoi( cm[ height].str());
            if( cm[ interp].matched)
            {
                auto s_rep = cm[ interp].str();
                interpolation[ side] = FOUND_STRING( strcasestr( s_rep.data(), cubic));
            }
        }
    }
}

CompositionRule::CompositionModel RuleParser::selectCompositionModel( std::string_view given)
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

std::deque<CompositionRule::BlendModel> RuleParser::selectBlendModels( std::string_view given)
{
    if( given.empty())
        return {};
    auto s_models = Util::partition( given, R"(\s*\|\s*)");
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

void RuleParser::fillEasingMode( std::function<float(float)> &function, const char *&rule, char eoc)
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
                               return progress < 0.5 ? 4 * progress * progress * progress
                                                     : 1 - pow( -2 * progress + 2, 3) / 2;
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
            easing += static_cast<char>( std::tolower( *rule));
        ++rule;
    }

    auto fn = easingLookup.find( easing);
    if( fn != easingLookup.cend())
        function = fn->second;
    else
    {
        fprintf( stderr, "Unknown easing function `%s` specified!"
                         " Default easing function will be used\n", easing.c_str());
        auto matches = Util::findWordMatch( app_manager_.bkroot.get(), easing.c_str(), BKNode::Group::Easing, 3);
        if( !matches.empty())
        {
            fprintf( stderr, "Easing function suggestions based on your search: \n");
            for ( size_t i = 0; i < matches.size(); ++i)
                fprintf( stderr, "%zu]. %s\n", i + 1, matches[ i].c_str());
        }
        function = []( float progress){ return progress; };
    }
}

/*
 * Explains what the color specified in color rule is doing.
 */
void RuleParser::setColor( const char *& rule, PropertyProxy<uint64_t> &color)
{
    Util::ltrim( rule);
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
        Util::ltrim( rule);
        prev = rule;
        if( *rule == '(')
            ++rule;
        else
        {
            fprintf( stderr, "Expected `(` after -> %s\n", effect == -1 ? STR_LIGHTER : STR_DARKER);
            exit( EXIT_FAILURE);
        }
    }
    if( Util::ltrim( rule) && *rule == '(')
        color = ColorUtil::mixColor( ++rule, app_manager_.bkroot.get());
    else if( *rule != '-')
    {
        prev = rule;
        color = ColorUtil::extractColor( rule, app_manager_.bkroot.get());
    }
    if( Util::ltrim( rule) && effect != 0)
    {
        auto factor = 2.f;
        if( *rule == ',' && Util::ltrim( ++rule))
            factor = parseFloats<1>( rule)[ 0];
        // Make sure annotation of `Lighter` are values below 1 and those annotated `Darker` are values above 1.
        factor = effect == -1 ? factor : 1.f / factor;
        color = MAKE_QWORD( ColorUtil::tintColor( HIGH_DWORD( color), std::abs( factor)),
                            ColorUtil::tintColor( LOW_DWORD( color), std::abs( factor)));
        if( *rule != ')')
        {
            fprintf( stderr, "Expected `)` to match %s", prev);
            exit( EXIT_FAILURE);
        }
        ++rule;
    }
}

std::deque<std::tuple<SpecialEffect, StickyArena, int, std::string>> RuleParser::extractEffects( std::string_view given)
{
    if( given.empty())
        return {};
    auto parts = Util::partition( given, R"(\s*\|\s*)");
    if( parts.empty())
        parts.emplace_back( given.data());
    static const std::unordered_map<std::string_view, SpecialEffect> possibilities
    {
        { SE_BLUR,      SpecialEffect::Blur},
        { SE_SHARPEN,   SpecialEffect::Sharpen},
        { SE_EMBOSS,    SpecialEffect::Emboss},
        { SE_OIL,       SpecialEffect::Oil},
        { SE_GRAYSCALE, SpecialEffect::GrayScale},
        { SE_GRAINY,    SpecialEffect::Grainy},
        { SE_TWIRL,     SpecialEffect::Twirl}
    };

    std::deque<std::tuple<SpecialEffect, StickyArena, int, std::string>> effects;
    std::smatch sm;
    std::regex key( R"(\(([a-z]+)(?:,\s*(top|base|both))?(?:,\s*(\d+))?(?:,\s*(.+))?\))", std::regex_constants::icase);
    for( auto& part : parts)
    {
        std::transform( part.cbegin(), part.cend(), part.begin(), tolower);
        std::string effect_name, side_name, weight_str, other_params;
        SpecialEffect effect;
        auto weight = 0;
        auto side{ CompositionRule::StickyArena::Base};
        if( std::regex_match( part.cbegin(), part.cend(), sm, key))
        {
            effect_name = sm[ 1].str();
            side_name = sm[ 2].str();
            weight_str = sm[ 3].str();
            std::transform( side_name.cbegin(), side_name.cend(), side_name.begin(), tolower);
            weight = weight_str.empty() ? 0 : std::stoi( weight_str);
            side  =  side_name == "both" ? CompositionRule::StickyArena::Both
                                         : side_name == "top" ? CompositionRule::StickyArena::Top
                                                              : CompositionRule::StickyArena::Base;
            other_params = sm[ 4].str();
        }
        auto pos = possibilities.find( effect_name.empty() ? part : effect_name);
        if( pos == possibilities.cend())
            continue;

        effects.emplace_front( pos->second, side, weight, other_params);
    }

    return effects;
}

template <size_t count>
std::array<float, count> RuleParser::parseFloats( const char *&rule)
{
    std::array<float, count> response{};
    for( auto& result : response)
    {
        result = 0;
        float post_decimal_point_value = 0, shift = 1;
        if( Util::ltrim( rule) && *rule == ')')
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
            if( Util::ltrim( rule) && *rule == '.')
            {
                seen_dot = true;
                ++rule;
                continue;
            }
            seen_dot ? post_decimal_point_value += ( shift *= .1f) * INT_CAST( *rule - '0') :
                    result = result * 10.f + ( *rule - '0');
            ++rule;
        }
        while( *rule && *rule != ')' && *rule != ',');
        result += post_decimal_point_value * sign;
    }

    return response;
}