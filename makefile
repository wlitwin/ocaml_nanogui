.PHONY: all run

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

all:
	dune build bin/main.exe

run: all
	_build/default/bin/main.exe

shell:
	nix-shell -p gcc glfw libffi openssl zlib gmp pulseaudio libglvnd libGLU libGL glew SDL2 SDL2_gfx SDL2_net SDL2_image SDL2_mixer SDL2_ttf


default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

.PHONY: default install uninstall reinstall clean
