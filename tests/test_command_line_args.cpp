#include <memory>
#include "CppUTest/SimpleString.h"
#include "CppUTest/TestPlugin.h"
#include "CppUTest/TestHarness.h"
#include "CppUTest/CommandLineTestRunner.h"

class OptionsPlugin : public TestPlugin
{
public:
    explicit OptionsPlugin( const SimpleString& name)
    : TestPlugin( name)
    {
    }
    ~OptionsPlugin() override = default;

    bool parseArguments( int ac, const char * const *av, int index)
    {
        return false;
    }
};

class CommandLineArgumentsParser
{
public:
    explicit CommandLineArgumentsParser(int ac = 0, const char **av = nullptr, int idx = 0)
    : argc_( ac), argv_( av), index_( idx)
    {
    }

    virtual ~CommandLineArgumentsParser() = default;

    bool parse( TestPlugin *plugin)
    {
        return plugin->parseArguments( argc_, argv_, index_);
    }
private:
    int argc_, index_;
    const char **argv_;
};

TEST_GROUP(CommandLineArgumentsTests)
{
    std::unique_ptr<CommandLineArgumentsParser> args;
    std::unique_ptr<OptionsPlugin> plugin;
    void setup() override
    {
        plugin = std::make_unique<OptionsPlugin>( "options");
    }

    bool newArgumentParser( int ac, const char **av, int idx)
    {
        args = std::make_unique<CommandLineArgumentsParser>( ac, av, idx);
        return args->parse( plugin.get());
    }
};

TEST( CommandLineArgumentsTests, TestColorRule)
{
    const char *av[] = { "copywrite", "-c=[1..:90]{ Orange}"};
    CHECK( newArgumentParser( 2, av, 1));
}

int main( int ac, char **av)
{
    return CommandLineTestRunner::RunAllTests( ac, av);
}