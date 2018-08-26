dev:
	elm-live src/Main.elm --output=elm.js --after-build="./copy-static.sh"

prod:
	elm make src/Main.elm --output=elm.js --optimize
	./copy-static.sh
