/***************************************************************************/
/*                                                                         */
/*  copywrite.cpp                                                          */
/*                                                                         */
/*    A terminal based text customization utility					       */
/*                                                                         */
/*  Copyright 2022 by Adesina Meekness                                     */
/*                                                                         */
/*                                                                         */
/*       ##    ## ##                                                       */
/*       ##    ##  #                                                       */
/*       ###  ###  #  ##                                                   */
/*       # # # ##  # #                                                     */
/* ####  # ### ##  ###                                                     */
/*       #  #  ##  # ##                                                    */
/*       #  #  ##  #  ##                                                   */
/*                                                                         */
/*                                                                         */
/*  This file is part of the Copywrite project, and may only be used,      */
/*  modified, and distributed under the terms of the GNU project           */
/*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
/*  this file you indicate that you have read the license and              */
/*  understand and accept it fully.                                        */
/*                                                                         */
/***************************************************************************/

#include <iostream>
#include "copywrite.hpp"
#include "parsers/rule_parser.hpp"
#include "parsers/command_line_parsers.hpp"
#include "plugins/plugin_manager.hpp"
#include "plugins/image_manager.hpp"
#include "plugins/local_font_manager.hpp"
#include "renderers/text_renderer.hpp"
#include "renderers/text_colorizer.hpp"
#include "renderers/render-plugins/layer_renderer.hpp"
#include "utils/utils.hpp"
#include "utils/timer.hpp"

int main( int argc, char *argv[])
{
    Timer::start();
    auto cmd_parser          = CommandLineParser( argc, argv);
    auto activity_director   = cmd_parser.process();
    auto rule_parser         = RuleParser( activity_director);
    auto plugin_manager      = PluginManager::instance();

    #if PNG_SUPPORTED || JPG_SUPPORTED
    plugin_manager->install( std::make_unique<ImageManager>( activity_director));
    plugin_manager->install( std::make_unique<LayerRenderer>( activity_director, rule_parser));
    #endif

    auto text_renderer      = TextRenderer( activity_director, rule_parser);
    auto text_colorizer     = TextColorizer( activity_director, text_renderer);
    auto surface = text_colorizer.paint();

    if( activity_director.as_image)
    {
        auto layer_renderer     = dynamic_cast<LayerRenderer *>( plugin_manager->get( LAYER_RENDERER));
        if( ACCESSIBLE( layer_renderer))
            layer_renderer->composite( surface);

        auto image_manager      = dynamic_cast<ImageManager *>( plugin_manager->get( IMAGE_MANAGER));
        if(  ACCESSIBLE( image_manager))
        {
            std::clog << "Writing to file after: " << Timer::yield( GLOBAL_TIME_ID) <<'\n';
            image_manager->writeImage( surface);
            std::clog << "Finished writing to file after: " << Timer::yield( GLOBAL_TIME_ID) <<'\n';
        }
    }
    else
    {
        PropertyManager<FILE *> destination( stdout, []( FILE *maybe_destructible)
        {
            if( maybe_destructible != stdout)
                fclose( maybe_destructible);
            maybe_destructible = nullptr;
        });

        if( ACCESSIBLE( activity_director.src_filename))
        {
            auto *handle = fopen( activity_director.src_filename, "wb");
            if( handle)
                destination.get() = handle;
        }

        Util::write( surface, activity_director.raster_glyph, destination.get(), activity_director.kdroot.get());
		std::cout << '\n';
    }

  return 0;
}
