#ifndef LOCAL_FONT_MANAGER_HPP
#define LOCAL_FONT_MANAGER_HPP

#include "plugins/plugin.hpp"

class LocalFontManager : public Plugin
{
public:
    explicit LocalFontManager( ApplicationDirector& manager);

	bool open();

    void installFont( std::string_view font_file);

    void uninstallFont( std::string_view font);

    std::pair<int64_t, std::unique_ptr<uint8_t, DeleterType>> useInstalledFont( std::string_view font_name);

    static std::pair<std::string, std::string> requestFontInfo( std::string_view font_file);

    ~LocalFontManager() override;

private:
    void executeActionOnFont( std::string_view font_name, FontActivity mode,
                              const std::function<void( std::vector<void *>)>& fn);

    ApplicationDirector& app_manager_;
    #if CUSTOM_FONT_SUPPORTED
    int code_{};
    zip_error_t zip_error_{};
    zip_t *zipper_{ nullptr};
    #endif
};

__attribute__((weak)) const char *fontArchiveDir()
{
	return RESOURCE_DIR "copywrite-installed-fonts.zip";
}

#endif