#include <cassert>
#include "copywrite.hpp"
#include "plugins/plugin.hpp"
#include "plugins/image_manager.hpp"

ImageManager::ImageManager( ApplicationDirector& manager)
: Plugin( IMAGE_MANAGER), app_manager_( manager)
{
}

void ImageManager::writeImage( FrameBuffer<uint32_t>& frame) const
{
    if( app_manager_.out_format == OutputFormat::PNG)
    {
        #if PNG_SUPPORTED
        writePNG( app_manager_.src_filename, frame, app_manager_.image_quality);
        #endif
    }
    else
    {
        #if JPG_SUPPORTED
        writeJPEG( app_manager_.src_filename, frame, app_manager_.image_quality);
        #endif
    }
}

[[nodiscard]] FrameBuffer<uint8_t> ImageManager::readImage( std::string_view filename)
{
    ( void )app_manager_; // Silence linter warning
    auto is_jpeg = isJPEG( filename);
    #if PNG_SUPPORTED || JPG_SUPPORTED
        return ( is_jpeg ? readJPEG : readPNG)( filename);
    #elif PNG_SUPPORTED
        return is_jpeg ? {} : readPNG( filename);
    #elif JPG_SUPPORTED
            return is_jpeg ? readJPEG( filename) : {};
    #endif
    return {};
}

 bool ImageManager::isJPEG( std::string_view filename)
{
    auto *extension = strrchr( filename.data(), '.');
    if( extension && ++extension)
    {
        std::regex rule( R"(^jpe?g$)", std::regex_constants::icase);
        return std::regex_match( extension, extension + strlen( extension), rule);
    }

    return false;
}

 FrameBuffer<uint8_t> ImageManager::readJPEG( std::string_view filename)
{
    #if JPG_SUPPORTED
    auto *handle = fopen( filename.data(), "rb");
    if( handle == nullptr)
        return {};
    PropertyManager<FILE *> file_manager( handle, []( auto *p){ fclose( p);});

    jpeg_decompress_struct decompressor{};
    memset( &decompressor, 0, sizeof(decompressor));
    PropertyManager<jpeg_decompress_struct *> manager( &decompressor, jpeg_destroy_decompress);

    jpeg_error_mgr error_mgr{};
    decompressor.err = jpeg_std_error( &error_mgr);

    jpeg_create_decompress( &decompressor);
    jpeg_stdio_src( &decompressor, handle);

    if( jpeg_read_header( &decompressor, TRUE) != JPEG_HEADER_OK)
        return {};

    assert( decompressor.out_color_space == J_COLOR_SPACE::JCS_RGB ||
            decompressor.out_color_space == J_COLOR_SPACE::JCS_GRAYSCALE);

    std::shared_ptr<uint8_t> mem( ( uint8_t *)malloc( decompressor.image_width * decompressor.image_height * 4),
                                  []( auto *p){ free( p);});
    FrameBuffer<uint8_t> frame( mem, INT_CAST( decompressor.image_width), INT_CAST( decompressor.image_height), 4);
    PropertyManager<JSAMPROW> row_manager( new JSAMPLE[ frame.width * decompressor.num_components],
                                           []( auto *p) { delete[] p;});

    auto *s_row_pointer = row_manager.get();
    auto *d_row_pointer = frame.buffer.get();
    auto is_rgb = decompressor.out_color_space == JCS_RGB;

    jpeg_start_decompress( &decompressor);
    while( decompressor.output_scanline < frame.height)
    {
        jpeg_read_scanlines( &decompressor, &s_row_pointer, 1);
        for( size_t i = 0; i < frame.width; ++i)
        {
            auto index = ( decompressor.output_scanline - 1) * frame.width * frame.n_channel + i * frame.n_channel;
            d_row_pointer[ index + 0] = s_row_pointer[ i * decompressor.num_components + 0];
            d_row_pointer[ index + 1] = s_row_pointer[ i * decompressor.num_components + is_rgb];
            d_row_pointer[ index + 2] = s_row_pointer[ i * decompressor.num_components + is_rgb + is_rgb];
            d_row_pointer[ index + 3] = 0xFFu;
        }
    }
    jpeg_finish_decompress( &decompressor);

    return frame;
    #else
    return {};
    #endif
}

 void ImageManager::writeJPEG( std::string filename, FrameBuffer<uint32_t> &frame, int quality)
{
    #if JPG_SUPPORTED
    using namespace std::string_literals;
    filename = std::regex_replace( filename, std::regex( R"(\..+)", std::regex_constants::icase), R"(.jpg)"s);
    auto *handle = fopen( filename.data(), "wb");
    if( handle == nullptr)
        return;
    PropertyManager<FILE *> f_manager( handle, []( auto *p){ fclose( p);});
    jpeg_compress_struct compressor{};
    PropertyManager<jpeg_compress_struct *> manager( &compressor, jpeg_destroy_compress);

    jpeg_error_mgr error_mgr{};
    compressor.err = jpeg_std_error( &error_mgr);

    jpeg_create_compress( &compressor);
    jpeg_stdio_dest(&compressor, handle);

    compressor.image_width = frame.width;
    compressor.image_height = frame.height;
    compressor.input_components = 3;
    compressor.in_color_space = JCS_RGB;
    compressor.write_JFIF_header = TRUE;
    compressor.JFIF_major_version = JPEG_LIB_VERSION_MAJOR;
    compressor.JFIF_minor_version = JPEG_LIB_VERSION_MINOR;

    jpeg_set_defaults( &compressor);
    compressor.dct_method  = JDCT_FLOAT;
    jpeg_set_quality( &compressor, quality, TRUE);
    compressor.raw_data_in = FALSE;
    compressor.smoothing_factor = 100;
//   compressor.optimize_coding = TRUE;

    const auto row_stride = compressor.image_width * 3;
    jpeg_start_compress( &compressor, TRUE);
    auto buffer = frame.buffer.get();
    PropertyManager<uint8_t *> row_manager( new uint8_t[ row_stride], []( auto *p) { delete[] p;});
    auto row_buffer = row_manager.get();
    for( int j = 0; j < compressor.image_height; ++j)
    {
        for( int i = 0; i < compressor.image_width; ++i)
        {
            auto index = j * compressor.image_width + i;
            auto pixel = buffer[ index];
            row_buffer[ i * 3 + 0] = RED( pixel);
            row_buffer[ i * 3 + 1] = GREEN( pixel);
            row_buffer[ i * 3 + 2] = BLUE( pixel);
        }
        jpeg_write_scanlines( &compressor, &row_buffer, 1);
    }

    jpeg_finish_compress( &compressor);
    #endif
}

 FrameBuffer<uint8_t> ImageManager::readPNG( std::string_view filename)
{
    #if PNG_SUPPORTED
    constexpr const auto BYTES_READ = 8;
    char magic[ BYTES_READ];
    PropertyManager<FILE *> handle( fopen( filename.data(), "rb"), []( auto *p) { if( p) fclose( p); p = nullptr;});
    if( handle.get() == nullptr)
        return {};

    if( fread( magic, 1, BYTES_READ, handle.get()) != BYTES_READ)
        return {};

    if( png_sig_cmp( png_const_bytep ( magic), 0, BYTES_READ))
    {
        fprintf( stderr, "This is not a png file.\n");
        return {};
    }

    png_structp png_ptr = png_create_read_struct( PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    if( png_ptr == nullptr)
        return {};

    png_infop info_ptr = png_create_info_struct( png_ptr);
    if( info_ptr == nullptr)
        png_destroy_read_struct( &png_ptr, nullptr, nullptr);

    if( setjmp( png_jmpbuf( png_ptr)))
    {
        png_destroy_read_struct( &png_ptr, &info_ptr, nullptr);
        return {};
    }

    png_init_io( png_ptr, handle.get());
    png_set_sig_bytes( png_ptr, BYTES_READ);
    png_read_info( png_ptr, info_ptr);

    FrameBuffer<uint8_t> frame;
    int32_t bit_depth, color_type;
    png_get_IHDR( png_ptr, info_ptr,
                  reinterpret_cast<uint32_t *>( &frame.width),
                  reinterpret_cast<uint32_t *>( &frame.height),
                  &bit_depth, &color_type, nullptr, nullptr, nullptr);
    Color background_color;
    bool has_background = false;
    if( png_get_valid( png_ptr, info_ptr, PNG_INFO_bKGD))
    {
        png_color_16p background;
        png_get_bKGD( png_ptr, info_ptr, &background);
        auto& [ red, green, blue] = background_color.rgb;
        if( bit_depth == 16)
        {
            red   = background->red   >> 8u;
            green = background->green >> 8u;
            blue  = background->blue  >> 8u;
        }
        else if( color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
        {
            if( bit_depth == 1)
                red = green = blue = background->gray ? 255 : 0;
            else if( bit_depth == 2)
                red = green = blue = ( 255 / 3)  * background->gray;
            else
                red = green = blue = ( 255 / 15) * background->gray;
        }
        else
        {
            red   = background->red;
            green = background->green;
            blue  = background->blue;
        }

        has_background = true;
    }

    if( color_type == PNG_COLOR_TYPE_PALETTE)
        png_set_palette_to_rgb( png_ptr);
    else if( color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8)
        png_set_expand_gray_1_2_4_to_8( png_ptr);
    else if( png_get_valid( png_ptr, info_ptr, PNG_INFO_tRNS))
        png_set_tRNS_to_alpha( png_ptr);

    if( bit_depth == 16)
        png_set_strip_16( png_ptr);
    if( color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
        png_set_gray_to_rgb( png_ptr);

    png_read_update_info( png_ptr, info_ptr);
    uint32_t row_bytes = png_get_rowbytes( png_ptr, info_ptr);
    std::unique_ptr<uint8_t, DeleterType> image_data( ( uint8_t *)malloc( frame.height * row_bytes),
                                                      []( auto *p){ free( p);});
    if( image_data)
    {
        uint8_t * row_pointers[ frame.height];
        for( size_t i = 0; i < frame.height; ++i)
            row_pointers[ i] = image_data.get() + i * row_bytes;
        png_read_image( png_ptr, row_pointers);
        png_read_end( png_ptr, info_ptr);
        frame.buffer = std::move( image_data);
        frame.n_channel = png_get_channels( png_ptr, info_ptr);
    }

    png_destroy_read_struct( &png_ptr, &info_ptr, nullptr);

    return std::move( frame);
    #else
    return {};
    #endif
}

 void ImageManager::writePNG( std::string filename, FrameBuffer<uint32_t> &frame, int quality)
{
    #if PNG_SUPPORTED
    using namespace std::string_literals;
    filename = std::regex_replace( filename, std::regex( R"(\..+)", std::regex_constants::icase), R"(.png)"s);
    png::image<png::rgba_pixel> image( frame.width, frame.height);
    auto buffer = frame.buffer.get();
    for( uint32_t j = 0; j < frame.height; ++j)
    {
        for ( uint32_t i = 0; i < frame.width; ++i)
        {
            auto pixel = PNG_ENDIAN( buffer[ j * frame.width + i]);
            image.set_pixel( i, j, *(( png::rgba_pixel *)&pixel));
        }
    }
    image.write( filename.data());
    #endif
}