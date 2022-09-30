#include <string>
#include <string_view>
#include "plugins/plugin.hpp"

Plugin::Plugin( std::string name)
 : name_( std::move( name))
{
}

std::string_view Plugin::getName() const
{
    return name_.data();
}
