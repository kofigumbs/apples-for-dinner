.PHONY: all

all:
	npm i -g elm@0.19.0
	cd app && elm make --optimize --output ../elm.js Main.elm
	jekyll build
