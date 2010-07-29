BUILD_DIR = build/
HASKELL = src/Lexer.hs src/Main.hs
TMP_DIR = "/tmp/compilerponies"

all: $(BUILD_DIR)/compiler

$(BUILD_DIR)/compiler: $(BUILD_DIR) $(TMP_DIR) $(HASKELL)
	ghc $(HASKELL) -o "$(BUILD_DIR)compiler" -tmpdir $(TMP_DIR) -w -Wall -Wwarn
	rm src/*.hi src/*.o

$(BUILD_DIR):
	mkdir $(BUILD_DIR)
$(TMP_DIR):
	mkdir -p $(TMP_DIR)
	chmod 700 $(TMP_DIR)
