# core-war
An implementation of [core-war] built with Haskell.

## About

Core-War is game which pits competing programs against eachother by creating a virtual machine for them to run in.
The programs are written in an Assembly Language named RedCode.

This program is written a [lierate haskell]() program. Therefore an explanation of the how it works is written among the code.
There are numerous design decisions to be made when writing a MARs controller for core-war, and these are detailed in the source.

## Building & Running

`stack build`

`stack exec core-war-exe [<path-to-program>]`



