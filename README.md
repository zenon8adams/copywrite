
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
> * Image composition  
> * Layering   
> * Text Justification
> * Special filter (e.g. Blur, Twirl, Grainy, Paint, e.t.c.)  
> * Gradients (Linear, Radial, Conical)  
> * Color blend (Color-Burn, Multiply, e.t.c.)  
> * Color translation  
> * Local font management

## Examples
### Colorizable pixel art
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Darker(Khaki) -> Darker(Red) +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Darker(Purple) -> Purple -C +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..]{ Yellow -> LightBlue -C +}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:10-15-10 -easeInOutSine]{ Yellow}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:10-15-10 -easeInOutSine]{ (Yellow + Khaki)}" "Hello World"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -hL=.1 -rC="[1..:10-15-10 -easeInOutSine]{ (Yellow + Khaki)}" value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -hL=.1 -cD=\# -rC="[1..:10 -easeInOutSine]{ (Yellow + Khaki)}" "Hello World!"
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" \
           -cT="Khaki/Red" -j="right" \
           -c="[snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, mode=source-over]" \
           -pF="Ubuntu mono" \
           -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
 ```
 ###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
 ###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
 ```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply];\
           [snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over, effect=(blur, 170)|(blur, 170)|(blur, 170)]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply]; \
           [snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, \
           mode=source-over, effect=(twirl, 300, 80)|(grainy, 20)]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)
###### &nbsp;&nbsp;&nbsp;&nbsp; Prompt
```sh
 copywrite -rC="[1..:30]{ Khaki}" -cT="Khaki/Red" -j="right" \
           -c="[snap=center, layer=wood_one.jpg, mode=clip, blend=multiply]; \
           [from 30deg, snap=top-right, layer=pexels-alesia-kozik-6015687-768x1151.jpg, mode=source-over]" \
           -pF="Ubuntu mono" -iA -iR=300 -pT=50 -pR=50 -o image.png value.txt
```
###### &nbsp;&nbsp;&nbsp;&nbsp; Result
![result](https://github.com/zenon8adams/copywrite/blob/master/screenshot.png)

## Building
```
mkdir cmake-build
cd cmake-build
cmake ../
make
```
