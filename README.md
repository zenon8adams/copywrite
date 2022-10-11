
Copywrite
=========
[![Default](https://github.com/zenon8adams/copywrite/actions/workflows/github-ci.yml/badge.svg?branch=master)](https://github.com/zenon8adams/copywrite/actions/workflows/github-ci.yml)

A package for watermarking your code.

## Description
Copywrite allows you to generate watermark for your code. The watermark can be added
to the top of your source files.

## Screenshot

<p align="left">
  <img src="https://github.com/zenon8adams/copywrite/blob/master/screenshot.png"/>
</p>

## Building
```
mkdir cmake-build
cd cmake-build
cmake ../
make
```

## Usage
```
Usage: copywrite [OPTION]... FILE|TEXT
Convert the content of FILE or TEXT into a format defined by OPTIONs

The following options can be used to tune the generator:
-lF, --list-fonts              Lists the fonts installed on your Linux system.
                                                                              
-pF, --font-profile            Set the font family to use for display.    
                               See --install-font                         
                               Example:                                   
                               --font-profile="Noto sans Regular"         
                               If font was installed using --install-font,
                                use: --font-profile="Noto sans - Regular" 
                                                                          
-rC, --color-rule              Set the color rule to use in generating text.                            
                               Usage examples:                                                          
                               SINGLE LINE TEXTS:                                                       
                               1. --color-rule=[1]{Red}                                                 
                                   Set the first character encountered in text to red.                  
                               2. --color-rule=[1..]{Red}                                               
                                   Set all text characters encountered in text to red.                  
                               3. --color-rule=[1..4]{Red}                                              
                                   Set the first to fourth characters in text to red.                   
                               4. --color-rule=[1..4:50]{Red}                                           
                                   Set the first to fourth characters to 50px and color to red.         
                               5. --color-rule=[1..4:50-30 -easeInOutSine]{Red}                         
                                   Set the first to fourth character to tween from 50 to 30px using     
                                easeInOutSine as the easing function. Paint the text red.               
                               6. --color-rule=[1..4:50-30-60 -easeInOutSine]{Red}                      
                                   Set the first to fourth character to tween from 50px to 30px         
                                and then finally, finish at 60px. Paint the font color red.             
                               7. --color-rule=[1..]{Red -> Green}                                      
                                   Set all characters to transition color from red to green using       
                                linear easing function.                                                 
                               Note that the easing will be character based.                            
                                If easing within characters is expected, a `+` should be added          
                                (e.g [1..]{ Red -> Green +}).                                           
                               8. --color-rule=[1..]{Red -> Green +easeInOutSine}                       
                                   Set all characters to transition color from red to green using       
                                easeInOutSine function.                                                 
                               Note that the easing will be character based.                            
                               If easing within characters is expected, a `+` should be added           
                                (e.g [1..]{ Red -> Green +easeInOutSine}).                              
                               9. --color-rule=[1..]{Red -> Green -R(.5, .5, .3) +}                     
                                   Set all characters to transition color from Red to Green using       
                                radial gradient at origin (0.5, 0.5) with spread at 0.3                 
                               10. --color-rule=[1..]{Red -> Green -C +}                                
                                   Set all characters to colors transition from red to green using      
                                conical gradient.                                                       
                               11. --color-rule=[1..]{-C(red, green, blue) +}                           
                                   Set all characters to transition color from red at 0deg              
                                to green at 180deg and finally ending with blue at 360deg.              
                               Any number of colors are allowed as opposed to radial gradient which     
                                allows only two color transition.                                       
                               12. --color-rule=[1..]{-C(from 50deg, red, blue 180deg, orange) +}       
                                   Set all characters to ease using conical gradient.                   
                               The conical angle starts at 50deg, blue starts at 180deg                 
                                and finishes with orange.                                               
                               13. --color-rule=[1..]{red:3f}                                           
                                   Set the font color to red with background transparency of 63.        
                               14. --color-rule=[1..]{(red + blue)}                                     
                                   Set the font color to the result of additive color mixing            
                               of red and blue which is magenta.                                        
                               15. --color-rule=[1..]{(red:3f + blue:2f)}                               
                                   Produces same result as in step `14`.                                
                               To set the opacity of the mixed color, use:                              
                               --color-rule=[1..]{(red:3f + blue: 2f):ff}. Where `0xff`                 
                                is the opacity override override.                                       
                               16. --color-rule=[1..]{(cyan - yellow)}                                  
                                   Set the font color to the result of subtractive mixing of            
                               cyan and yellow which is green.                                          
                               17. --color-rule=[1..]{(red:ff:30 + blue:ff:70)}                         
                                  Set the font color to the result of mixing 30% red with 70%           
                               blue with maximum opacity of 255. If the sum is greater than 100%,       
                               the result is clamped to 100%.                                           
                               18. --color-rule=[1..]{(red::30 + blue::70):ff}                          
                                   Set the color to the sum of red and blue with mixing ratio of        
                               30% red and 70% blue.                                                    
                               19. --color-rule=[1..]{(red/blue +/- green/yellow)}                      
                                   Set the outline color to the result of subtractive color mixing of   
                               blue and yellow (black). Also set the font color to the result of        
                               additive mixture of red and green(yellow).                               
                               MULTILINE TEXTS:                                                         
                               1. --color-rule=[(1,1)]{Red}                                             
                                   Set the character at the first row and first column(only) to red.    
                               2. --color-rule=[(1,1)..]{Red}                                           
                                   Set all characters in text to color red.                             
                                   This command is has same effect as:                                  
                                   --color-rule=[(1,1)..(-1,-1)]{Red}                                   
                               3. --color-rule=[(1,1)..(-1,3)]{Red}                                     
                                   Set all characters that are contained in the bounding box created    
                                by (1, 1) to (3, 3) to red.                                             
                               NB! The color rules defined for single line texts also work here as well.
                                                                                                        
-sF, --font-size               Set the default font size used by characters that don't match any
                               rule or with rules not with no explicit font size specified.     
                                                                                                
-cD, --drawing-character       This option can be used to control character used in
                                drawing glyphs to display in console mode.         
                                                                                   
-iA, --as-image                Prepare output mode to export in image format.                                          
                               This option only works if it is configured with -DPNG_SUPPORTED=ON or -DJPG_SUPPORTED=ON
                                                                                                                       
-o, --output                   Set the filename to write rendering into
                               --output image.png                      
                               NB! jpeg images are also supported.     
                                                                       
-eL, --list-easings             list the easing functions available.
                                                                    
-cL, --list-composition-modes  List all composition modes available.
                                                                    
-bL, --list-blend-modes        List all blend modes available.
                                                              
-c, --composition-rule         Set the method through which the generated text                           
                                is blended with an image provided by --composition-rule                  
                               Usage examples:                                                           
                               1. --composition-rule="mode=source-over"                                  
                               Set the order to which images should be applied on top of each other as   
                               `source-over`.The generated text is place at the top-left                 
                                corner of the output.                                                    
                               2. --composition-rule="from 30deg at .5, .5, mode=source-over"            
                               Set the order of stacking as source-over rotate the source image by 30deg.
                               See also: `--composition-rule`.                                           
                                                                                                         
-iC, --composition-image       Set the image to be used for image composition.
                               Usage:                                         
                               --composition-image=image.png                  
                                                                              
-iR, --dpi                     Set the resolution of the characters to display.
                                                                               
-bG, --background-color        Set the background color of the output image to generated.   
                               This option if only available if source code is compiled with
                               -DPNG_SUPPORTED=ON or -DJPG_SUPPORTED=ON                     
                                                                                            
-hL, --line-height             Set the distance between rows of text.
                                                                     
-j, --justify                  Move multiline text `left`, `right` or `center`.
                               Usage:                                          
                               --justify=left                                  
                                                                               
-wS, --stroke-width            Set the outline thickness for all characters.
                               Usage:                                       
                               --stroke-width=12                            
                                                                            
-uF, --uninstall-font          Removed `font-family - font-face` from the list of installed font.
                               This option is only available if source code is                   
                               compiled with -DCUSTOM_FONT_SUPPORTED=ON                          
                                                                                                 
-iF, --install-font            Add font file to the list to use usable fonts.  
                               This option is only available if source code is 
                               compiled with -DCUSTOM_FONT_SUPPORTED=ON        
                                                                               
-iQ, --quality-index           Set the quality of the output image.
                                                                   
-dE, --easing-direction        Specify the direction for color to flow in linear gradient.
                               Allowed values are: `row` or `col`.                        
                               Usage:                                                     
                               --easing-direction=row                                     
                                                                                          
-pL, --padding-left            Set the spacing left of generated text.
                               Usage:                                 
                               --padding-left=10                      
                                                                      
-pR, --padding-right           Set the spacing right of generated text.
                               Usage:                                  
                               --padding-right=10                      
                                                                       
-pT, --padding-top             Set the spacing above generated text.
                               Usage:                               
                               --padding-top=10                     
                                                                    
-pB, --padding-bottom          Set the spacing below generated text.
                               Usage:                               
                               --padding-bottom=10                  
                                                                    
-cT, --test-color              Inquire about the result of mixing colors.
                               Usage:                                    
                               --test-color=(red::30 + blue)             
                                                                         
-h, --help                     Show this prompt.
                                                
NB! The color names available are compliant with those defined
by CSS standard(https://www.w3.org/TR/css-color-3/)
```
