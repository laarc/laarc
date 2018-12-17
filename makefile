.PHONY: all clean test

all:
	@mkdir -p src/bcrypt/build
	@cd src/bcrypt/build && cmake .. && make

clean:
	@rm -rf src/bcrypt/build


test: all
