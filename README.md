
Copywrite
=========
[![Default](https://github.com/zenon8adams/copywrite/actions/workflows/github-ci.yml/badge.svg?branch=master)](https://github.com/zenon8adams/copywrite/actions/workflows/github-ci.yml)
![Licence](https://img.shields.io/github/license/zenon8adams/copywrite)
![Issues](https://img.shields.io/github/issues/zenon8adams/copywrite)

A prompt based image generation tool.

## Description
Copywrite is a command line based tool for image generation.

## Features
> * Colorizable pixel art  
> * Multiline pixel art
> * Layering   
> * Color blend (Color-Burn, Multiply, e.t.c.)
> * Text Transformation
> * Special filter (e.g. Blur, Twirl, Grainy, Paint, e.t.c.)  
> * Gradients (Linear, Radial, Conical)    
> * Color translation  
> * Local font management

## Examples
### Colorizable pixel art
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -wS=1 -rC="[1..]{ Purple/Red}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![linear_gradient](https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_linear_gradient.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Darker(Khaki) -> Darker(Red) +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![outline](https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_outline.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Darker(Purple) -> Purple -C +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![color](https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_color_purple.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Yellow -> LightBlue -C +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![conical_gradient](https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_conical_gradient.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:10-15-10 -easeInOutSine]{ Yellow}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_easing_yellow.png" alt="easing" height="250px" />    

###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -hL=.1 -cD=\# -rC="[1..:10 -easeInOutSine]{ (Yellow + Khaki)}" "Hello World!"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_color_addition.png" alt="addition" height="250px" />    

### Multiline pixel art
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -hL=.1 -rC="[1..:10-15-10 -easeInOutSine]{ (Yellow + Khaki)}" value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result    
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/terminal_color_multiline.png" alt="multiline" height="400px"/>   

### Layering
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" \
           -cT="Khaki/Red" -j="right" \
           -c="[snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, mode=source-over]" \
           -pF="Ubuntu mono" \
           -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
 ```
 ###### &nbsp;&nbsp;&nbsp;&nbsp; Result
 <img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering.png" alt="layering" height="500px"/>   

### Color blend
 ###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
 ```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply];\
           [snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
 <img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering_multiply.png" alt="multiply" height="500px"/>   

### Special Effect
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply]; \
           [snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over, effect=(twirl, 300, 80)|(grainy, 20)]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
 <img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering_twirl_effect.png" alt="twirl" height="500px"/>   

###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
 ```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply];\
           [snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over, effect=(blur, 170)|(blur, 170)|(blur, 170)]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering_blur_effect.png" alt="blur" height="500px"/>   

### Text Transformation
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply]; \
           [from 30deg, snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, mode=source-over]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering_rotate.png" alt="rotate" height="500px"/>   

###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply];\
           [snap=center, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
<img src="https://github.com/zenon8adams/copywrite/blob/master/assets/docs/images/image_layering_centered.png" alt="center" height="500px"/>   

### Color translation  
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -cT="(DarkGreen/Red + DarkBlue/Blue)"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
```
The result of the rule `(DarkGreen/Red + DarkBlue/Blue) -> Blue` is thus:
Background colors: #6400ff(DarkGreen) + #8bff(DarkBlue) = #ffffff(Aqua)
```
## Building
```
mkdir cmake-build
cd cmake-build
cmake ../
make
```
