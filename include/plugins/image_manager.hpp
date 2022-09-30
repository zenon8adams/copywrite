#ifndef IMAGE_MANAGER_HPP
#define IMAGE_MANAGER_HPP

class ImageManager : public Plugin
{
public:
    explicit ImageManager( ApplicationDirector& manager);

    void writeImage( FrameBuffer<uint32_t>& frame) const;

    [[nodiscard]] FrameBuffer<uint8_t> readImage( std::string_view filename);

    static bool isJPEG( std::string_view filename);

    static FrameBuffer<uint8_t> readJPEG( std::string_view filename);

    static void writeJPEG( std::string filename, FrameBuffer<uint32_t> &frame, int quality);

    static FrameBuffer<uint8_t> readPNG( std::string_view filename);

    static void writePNG( std::string filename, FrameBuffer<uint32_t> &frame, int quality);

private:
    ApplicationDirector& app_manager_;
};

#endif