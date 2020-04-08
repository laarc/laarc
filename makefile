.PHONY: all clean test

all:
	@mkdir -p src/bcrypt/build
	@cd src/bcrypt/build && cmake .. && make

setup:
	@raco pkg install gregor

clean:
	@rm -rf src/bcrypt/build


test: all
