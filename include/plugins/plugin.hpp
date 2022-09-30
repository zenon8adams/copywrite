#ifndef PLUGIN_HPP
#define PLUGIN_HPP

class Plugin
{
public:
    explicit Plugin( std::string name);

    [[nodiscard]] virtual std::string_view getName() const;

    virtual ~Plugin() = default;
private:
    std::string name_;
};

#endif