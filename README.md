# haskip8

A CHIP-8 emulator written in Haskell


### Getting Started

#### Prerequisites

To build and run this project you need [stack](https://docs.haskellstack.org/en/stable/README/)

You also need the SDL 2 library: `sudo apt install libsdl2-dev`


#### Build and Run

```bash
  $ cd path/to/repo
  $ stack build
  $ stack run -- dasm data/rng-test.ch8
  $ stack run -- emul data/rng-test.ch8
```


### Acknowledgemnts

The emulator implementation is based on [Cowgod's Chip-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
