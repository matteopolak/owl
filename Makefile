all: build

build: configure
	cmake --build build

configure:
	cmake --preset=default -DCMAKE_EXPORT_COMPILE_COMMANDS=1
	ln -sf build/compile_commands.json .

compdb: configure

clean:
	rm -rf build
	rm -f compile_commands.json

.PHONY: all build configure compdb clean
