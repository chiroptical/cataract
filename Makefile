build: hpack
	cabal build

hpack:
	hpack .

run: hpack
	cabal run	pluto

docker-build:
	nix-build -A docker
	docker load -i result

docker-run:
	docker images
	echo 'docker run --network host pluto:latest-tag'

format-nix:
	alejandra .

format-haskell:
	stylish-haskell -i **/*.hs

format: format-nix format-haskell

ghcid: hpack
	ghcid -c cabal repl

clean: hpack
	cabal clean

.PHONY: build hpack run docker-build docker-run format-nix format-haskell format ghcid clean
