BUILD_DIR = build
HASKELL = src/Lexer.hs src/ParserTypes.hs src/Parser.hs src/Semantics.hs src/IRTypes.hs src/IRHelpers.hs src/IR.hs src/Optimiser.hs
LEXER_MAIN = src/LexerMain.hs
PARSER_MAIN = src/ParserMain.hs
SEMANTICS_MAIN = src/SemanticsMain.hs
MAIN = src/CompilerMain.hs
OPT_MAIN = src/OptMain.hs
TMP_DIR = "/tmp/compilerponies"
BINARIES = $(BUILD_DIR)/lexerbin $(BUILD_DIR)/compiler $(BUILD_DIR)/parserbin $(BUILD_DIR)/semanticsbin $(BUILD_DIR)/opt-compiler

all: src/Parser.hs $(BINARIES)

src/Parser.hs: src/Parser.y
	happy src/Parser.y -o "$@" -iparser.log

$(BUILD_DIR)/lexerbin: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(LEXER_MAIN)
	ghc $(HASKELL) $(LEXER_MAIN) -o "$@" -tmpdir $(TMP_DIR)

$(BUILD_DIR)/parserbin: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(PARSER_MAIN)
	ghc $(HASKELL) $(PARSER_MAIN) -o "$@" -tmpdir $(TMP_DIR)

$(BUILD_DIR)/semanticsbin: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(SEMANTICS_MAIN)
	ghc $(HASKELL) $(SEMANTICS_MAIN) -o "$@" -tmpdir $(TMP_DIR)

$(BUILD_DIR)/compiler: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(COMPILER_MAIN)
	ghc $(HASKELL) $(MAIN) -o "$@" -tmpdir $(TMP_DIR)

$(BUILD_DIR)/opt-compiler: $(BUILD_DIR) $(TMP_DIR) $(HASKELL) $(OPT_MAIN)
	ghc $(HASKELL) $(MAIN) -o "$@" -tmpdir $(TMP_DIR)

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

check: all
	./scripts/test.py
	python scripts/testrun.py
	./scripts/say "all tests completed successfully"
