#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include "copywrite.hpp"
#include "plugins/plugin_manager.hpp"
#include "plugins/plugin.hpp"


PluginManager *PluginManager::instance()
{
    static PluginManager manager;
    return &manager;
}

void PluginManager::install( std::unique_ptr<Plugin> plugin)
{
    manager_[ plugin->getName()] = std::move( plugin);
}

Plugin *PluginManager::get( const std::string& name) const
{
    auto plugin_handle = manager_.find( name.data());
    return plugin_handle == manager_.cend() ? nullptr : plugin_handle->second.get();
}
