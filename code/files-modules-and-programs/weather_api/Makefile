.PHONY: build clean install run test utop
all: build

build:
	jbuilder build

clean:
	jbuilder clean

install:
	jbuilder build @install

run: install
	_build/install/default/bin/weather-api

test:
	jbuilder runtest

utop:
	jbuilder utop src/
