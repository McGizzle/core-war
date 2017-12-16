# core-wars

An implementation of [core-wars](http://www.corewars.org/) built with Haskell.

## About

Core-Wars is a game which pitts competing programs against eachother by creating a virtual machine for them to run in.
The programs are written in an Assembly Language named RedCode.

This program is written as a [literate haskell](https://wiki.haskell.org/Literate_programming) program. Therefore an explanation of the how the program works is written among the code.
There are numerous design decisions to be made when writing a MARs controller for core-war, and these are detailed in the source.

## Building & Running

`stack build`

`stack exec core-war-exe [<path-to-program>]`



