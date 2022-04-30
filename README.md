# Copywrite
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
Usage: copywrite [--list-fonts|--font-file=FILE] [--font-size=NUM] [--drawing-character=CHAR] [--output FILE] text
Displays block form of character sequence

Arguments:
        --list-fonts                  List location of all installed fonts.
        --font-file=FILE              Set the font file to be used for display.
        --font-size=NUM               Set the font size for display to NUM pixels.
        --drawing-character=CHAR      Set the character to output in for each block.
        --output FILE                 Write the block of characters into the file FILE.
```
