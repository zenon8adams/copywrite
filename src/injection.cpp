#include <string_view>

// This holds string variables that will used after installation.
std::string_view getDefaultFont()
{
	return PROJECT_DEFAULT_FONT;
}

const char *fontArchiveDir()
{
	return RESOURCE_DIR "copywrite-installed-fonts.zip";
}