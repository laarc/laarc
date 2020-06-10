.PHONY: all clean test

all:
	@mkdir -p src/bcrypt/build
	@cd src/bcrypt/build && cmake .. && make

setup-mac:
	@brew cask install racket
	@raco pkg install gregor-lib

uninstall-mac:
	@brew cask uninstall racket

setup:
	@raco pkg install gregor-lib

clean:
	@rm -rf src/bcrypt/build


test: all
