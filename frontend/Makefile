format:
	find src/ test/ -name "*.purs" -exec purty format --write {} \;

bundle:
	spago bundle-app --main Main --to ../static/app.js

build:
	spago build

test:
	spago test
