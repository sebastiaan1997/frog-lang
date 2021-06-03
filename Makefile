lib.arm: lib.fg
	stack run -- lib

lib.arm: main.fg
	stack run -- main

