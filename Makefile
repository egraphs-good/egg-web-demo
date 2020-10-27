.PHONY: build
build:
	wasm-pack build --target web --release --out-name wasm --out-dir ./static
	rm static/.gitignore
