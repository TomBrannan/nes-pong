# NES-Pong
A simple Pong game for the Nintendo Entertainment System

# About
This is a Pong game I created thanks to the Nerdy Nights tutorials over at NintendoAge: http://nintendoage.com/forum/messageview.cfm?catid=22&threadid=7155

It covers the basics: a simple game loop, sprite movement, background redrawing, game states, logic, and subroutines.

In the future I may rework this code to not do "everything in NMI" - it's better to run your game engine in the infinite loop after reset using a few flags, but the tutorial I followed didn't do that, so it ended up this way. 

# Usage
The ROM (.nes) file is included and can be played in any NES emulator.  To generate your own ROM file, run your .asm file through an assembler (I used NESASM3)
