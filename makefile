BUILD_DIR = build
HASKELL = src/Lexer.hs src/LexerMain.hs
TMP_DIR = "/tmp/compilerponies"

all: $(BUILD_DIR)/lexerbin

$(BUILD_DIR)/lexerbin: $(BUILD_DIR) $(TMP_DIR) $(HASKELL)
	ghc $(HASKELL) -o "$@" -tmpdir $(TMP_DIR) -w -Wall -Wwarn
	rm src/*.hi src/*.o

$(BUILD_DIR):
	mkdir $(BUILD_DIR)
$(TMP_DIR):
	mkdir -p $(TMP_DIR)
	chmod 700 $(TMP_DIR)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(TMP_DIR)
