render: clean
	./dist/build/tesser/tesser rebuild

deploy: render
	./dist/build/tesser/tesser deploy

clean: dist/build/tesser/tesser
	./dist/build/tesser/tesser clean

dist/build/tesser/tesser: main/tesser.hs
	cabal build

format: tesser.hs
	stylish-haskell -i tesser.hs

.PHONY: deploy render build install
