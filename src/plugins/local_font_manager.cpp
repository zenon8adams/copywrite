#include <iostream>
#include <fontconfig/fontconfig.h>
#include "copywrite.hpp"
#include "plugins/plugin.hpp"
#include "utils/utils.hpp"
#include "plugins/local_font_manager.hpp"

LocalFontManager::LocalFontManager( ApplicationDirector& manager)
: Plugin( LOCAL_FONT_MANAGER), app_manager_( manager)
{
    #if CUSTOM_FONT_SUPPORTED
        zip_error_init( &zip_error_);
        zipper_ = zip_open( FONT_ARCHIVE, ZIP_CREATE | ZIP_CHECKCONS, &code_);
        //TODO: Report Error
    #endif
}

void LocalFontManager::installFont( std::string_view font_file)
{
    #if CUSTOM_FONT_SUPPORTED
    zip_int64_t response_code;
    if( code_ == -1)
        return;
    auto [ font_family, font_style] = requestFontInfo( font_file);
    if( font_family.empty() || font_style.empty())
        return;

    response_code = zip_dir_add( zipper_, font_family.c_str(), ZIP_FL_ENC_GUESS);
    if( response_code == -1 && zip_error_.zip_err != ZIP_ER_EXISTS)
        return;

    PropertyManager<zip_source *> src( zip_source_file_create( font_file.data(), 0, -1, &zip_error_),
                                       []( auto *src) { zip_source_close( src);  zip_source_free( src);});
    if( src.get() == nullptr)
        return;

    zip_source_keep( src.get());

    std::string modified_font_filename;
    auto *font_filename = strrchr( font_file.data(), '/');
    if( font_filename == nullptr)
        font_filename = font_file.data();
    else
        ++font_filename;

    std::string file( font_family);
    font_family.erase( std::remove_if( font_family.begin(), font_family.end(), isspace), font_family.end());
    if( strcasestr( font_filename, font_style.c_str()) == nullptr)
    {
        auto *p_ext = strrchr( font_filename, '.');
        if( ACCESSIBLE(  p_ext))
            modified_font_filename.append( font_family);
        modified_font_filename.append( "-");
        modified_font_filename.append( font_style);
        if( ACCESSIBLE(  p_ext))
            modified_font_filename.append( p_ext);
    }
    else
        modified_font_filename = font_filename;

    file.append( "/");
    file.append( modified_font_filename);
    response_code = zip_file_add( zipper_, file.c_str(), src.get(), ZIP_FL_ENC_GUESS);
    if( response_code == -1 && zip_error_.zip_err == ZIP_ER_EXISTS)
    {
        printf( "Font file already exists do you want to overwrite(yes/no/y/n)? ");
        std::string reply;
        std::getline( std::cin, reply);
        std::transform( reply.begin(), reply.end(), reply.begin(), tolower);
        if( Util::compareOr<std::equal_to<std::string>>( reply, "yes", "no", "y", "n"))
        {
            response_code = zip_file_add( zipper_, file.c_str(), src.get(), ZIP_FL_OVERWRITE);
            if( response_code == -1)
            {
                fprintf( stderr, "Unable to add font file: (An error occurred!\n");
                return;
            }
        }
    }
    #endif
}

void LocalFontManager::uninstallFont( std::string_view font)
{
    #if CUSTOM_FONT_SUPPORTED
    executeActionOnFont( font, FontActivity::Delete, []( std::vector<void *> params)
    {
        auto *zipper = ( zip_t *)params[ 0];
        auto index = ( int64_t *)params[ 1];
        zip_delete( zipper, *index);
    });
    #endif
}

std::pair<int64_t, std::unique_ptr<uint8_t, DeleterType>> LocalFontManager::useInstalledFont( std::string_view font)
{
    std::unique_ptr<uint8_t, DeleterType> information(( uint8_t *)nullptr, []( auto *p){ if( p) free( p);});
    int64_t size{ -1};
    #if CUSTOM_FONT_SUPPORTED
    executeActionOnFont( font, FontActivity::Read, [ &information, &size]( std::vector<void *> params)
    {
        information.reset(( uint8_t *)params[ 0]);
        size = *( int64_t *)params[ 1];
    });
    #endif

    return { size, std::move( information)};
}

std::pair<std::string, std::string> LocalFontManager::requestFontInfo( std::string_view font_file)
{
    if( !FcInit())
        return {};

    PropertyManager<void *> finalizer( ( void *)nullptr, []( auto *){ FcFini();});
    PropertyManager<FcFontSet *> font_set( FcFontSetCreate(), FcFontSetDestroy);
    PropertyManager<FcStrSet *> font_dirs( FcStrSetCreate(), FcStrSetDestroy);
    if( !FcFileScan( font_set.get(), font_dirs.get(), nullptr, nullptr, ( const FcChar8 *)font_file.data(), false))
        return {};

    if( font_set && font_set->nfont > 0)
    {
        PropertyManager<FcChar8*> font_family(
                FcPatternFormat( font_set->fonts[ 0], ( const FcChar8 *)"%{family}"), free);
        PropertyManager<FcChar8*> font_style(
                FcPatternFormat( font_set->fonts[ 0], ( const FcChar8 *)"%{style}"), free);
        return { ( const char *)font_family.get(), ( const char *)font_style.get()};
    }
    return {};
}

LocalFontManager::~LocalFontManager()
{
    #if CUSTOM_FONT_SUPPORTED
    zip_error_fini( &zip_error_);
    zip_close( zipper_);
    #endif
}

void LocalFontManager::executeActionOnFont( std::string_view font_name, FontActivity mode,
                          const std::function<void( std::vector<void *>)>& fn)
{
    #if CUSTOM_FONT_SUPPORTED
    if( code_ == -1)
        return;

    auto parts = Util::partition( font_name, R"(\s*-\s*)");
    std::string style( "Regular");
    if( parts.empty())
        return;

    if( parts.size() > 1)
        style = parts[ 1];

    auto n_entries = zip_get_num_entries( zipper_, 0);
    if( n_entries == -1)
        return;

    zip_stat_t stat;
    zip_stat_init( &stat);

    std::string font_file( parts[ 0]);
    parts[ 0].erase( std::remove_if( parts[ 0].begin(), parts[ 0].end(), isspace), parts[ 0].end());
    font_file.append( "/");
    font_file.append( parts[ 0]);
    font_file.append( "-");
    font_file.append( style);
    int64_t i = 0;
    for( ; i < n_entries; ++i)
    {
        if( zip_stat_index( zipper_, i, 0, &stat) < 0
            || ( stat.valid & ZIP_STAT_NAME) == 0
            ||  *( stat.name + strlen( stat.name) - 1) == '/')
            continue;
        if( strcasestr( stat.name, font_file.c_str()) != nullptr)
            break;
    }

    if( ( stat.valid & ZIP_STAT_INDEX) == 0)
        return;

    std::vector<void *> params;
    if( mode == FontActivity::Delete)
    {
        params.push_back( ( void *)( zipper_));
        params.push_back( ( void *)&stat.index);
    }
    else
    {
        if ( stat.size == 0)
            return;
        void *buffer = malloc( stat.size);
        if ( buffer == nullptr)
            return;

        PropertyManager<zip_file_t *> handle( zip_fopen_index( zipper_, stat.index, ZIP_FL_UNCHANGED),
                                             [](auto *p) { if ( p != nullptr) zip_fclose( p); });
        if (zip_fread( handle.get(), buffer, stat.size) != stat.size)
        {
            fprintf( stderr, "Read invalid number of bytes\n");
            return;
        }
        params.push_back( buffer);
        params.push_back(( void *)&stat.size);
    }

    fn( params);
    #endif
}