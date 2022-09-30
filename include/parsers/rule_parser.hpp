#ifndef RULE_PARSER_HPP
#define RULE_PARSER_HPP

class RuleParser
{
public:
    explicit RuleParser( ApplicationDirector& manager);

    ConicGradient parseConicGradient( const char *&rule, const ColorRule& color_rule);

    static std::vector<CompositionRule> parseCompositionRule( std::string_view rule);

    std::vector<ColorRule> parseColorRule( const char *rule);

    static void parseFinalSize( std::string_view rule, int *interpolation);

private:
    static CompositionRule::CompositionModel selectCompositionModel( std::string_view given);

    static std::deque<CompositionRule::BlendModel> selectBlendModels( std::string_view given);

    void fillEasingMode( std::function<float(float)> &function, const char *&rule, char eoc);

    void setColor( const char *& rule, PropertyProxy<uint64_t> &color);

    static std::deque<std::tuple<SpecialEffect, StickyArena, int, std::string>> extractEffects( std::string_view given);

    template <size_t count>
    static std::array<float, count> parseFloats( const char *&rule);

    ApplicationDirector& app_manager_;
};

#endif