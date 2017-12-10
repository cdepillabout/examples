.PHONY: build clean install run test utop
all: build

build:
	jbuilder build

clean:
	jbuilder clean

install:
	jbuilder build @install

run: install
	_build/install/default/bin/alpha_vantage

test:
	jbuilder runtest

utop:
	jbuilder utop
