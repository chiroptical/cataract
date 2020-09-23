format:
	 $(MAKE) -C frontend format && find src/ app/ test/ -name "*.hs" -exec ormolu -i {} +

run:
	$(MAKE) -C frontend bundle && stack run

build:
	$(MAKE) -C frontend build && stack build

test:
	$(MAKE) -C frontend test && stack test
