build: hpack
	cabal build

hpack:
	hpack .

run: hpack
	cabal run	cataract

docker-build:
	nix build .#docker
	docker load -i result

deploy:
	bash script/deploy.sh

docker-run:
	docker images
	echo 'docker run --network host cataract:latest-tag'

format-nix:
	alejandra .

format-haskell:
	find app/ src/ test/ -name "*.hs" -exec stylish-haskell -i {} +

format: format-nix format-haskell

ghcid: hpack
	ghcid -c cabal repl

clean: hpack
	cabal clean

hlint: hpack
	hlint .

repl: ghci

ghci: hpack
	cabal repl

.PHONY: build hpack run docker-build deploy docker-run format-nix format-haskell format ghcid clean hlint repl ghci
