BUILD_DIR = build
HASKELL = src/Lexer.hs src/ParserTypes.hs src/Parser.hs src/Semantics.hs src/IRTypes.hs src/IRHelpers.hs src/IR.hs
LEXER_MAIN = src/LexerMain.hs
MAIN = src/CompilerMain.hs
TMP_DIR = "/tmp/compilerponies"

all: src/Parser.hs $(BUILD_DIR)/lexerbin $(BUILD_DIR)/compiler

src/Parser.hs: src/Parser.y
	happy src/Parser.y -o "$@" -iparser.log

$(BUILD_DIR)/lexerbin: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(LEXER_MAIN)
	ghc $(HASKELL) $(LEXER_MAIN) -o "$@" -tmpdir $(TMP_DIR)
	rm src/*.hi src/*.o

$(BUILD_DIR)/compiler: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(COMPILER_MAIN)
	ghc $(HASKELL) $(MAIN) -o "$@" -tmpdir $(TMP_DIR)
	rm src/*.hi src/*.o

$(BUILD_DIR):
	mkdir $(BUILD_DIR)
$(TMP_DIR):
	mkdir -p $(TMP_DIR)
	chmod 700 $(TMP_DIR)

.PHONY: clean check
clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(TMP_DIR)
	rm -rf src/Parser.hs

check:
	./test.py
	python testrun.py
