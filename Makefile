.PHONY: all build run

all: build run

build:
	stack build

run:
	stack exec survival-game
