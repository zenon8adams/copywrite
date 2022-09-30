#ifndef PLUGIN_MANAGER_HPP
#define PLUGIN_MANAGER_HPP

#include "plugins/plugin.hpp"

class PluginManager
{
public:
    static PluginManager *instance();

    void install( std::unique_ptr<Plugin> plugin);

    Plugin *get( const std::string& name) const;

private:
    std::unordered_map<std::string_view, std::shared_ptr<Plugin>> manager_;
};

#endif