deploy: render
	./dist/build/tesser/tesser deploy

render: clean
	./dist/build/tesser/tesser rebuild

clean: dist/build/tesser/tesser
	./dist/build/tesser/tesser clean

dist/build/tesser/tesser: tesser.hs
	cabal build

.PHONY: deploy render build install
