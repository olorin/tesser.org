deploy: render
	./dist/build/tesser/tesser deploy

render: build
	./dist/build/tesser/tesser rebuild

build:
	cabal build

install:
	cabal install

.PHONY: deploy render build install
