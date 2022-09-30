#ifndef COMMAND_LINE_PARSERS_HPP
#define COMMAND_LINE_PARSERS_HPP

class CommandLineParser
{
public:
    CommandLineParser( int ac, char **av);

    [[nodiscard]] ApplicationDirector process();

private:
    static void helpMe( std::string program);

    static void testColor( const char *rule, BKNode *bkroot);

    static std::string_view getColorNameAt( size_t pos);

    int argc;
    char **argv;
};

#endif