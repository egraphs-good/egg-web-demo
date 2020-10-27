.PHONY: build
build:
	wasm-pack build --target web --release --out-name wasm --out-dir ./static/pkg
	rm static/pkg/.gitignore
