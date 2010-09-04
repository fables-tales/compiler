#!/usr/bin/env python
import subprocess


if __name__ == "__main__":
    tests = ["tests/passall/hello.le",
    "tests/passall/testa.le",
    "tests/passall/testb.le",
    "tests/passall/testc.le",
    "tests/passall/test7.le",
    "tests/passall/testf.le",
    "tests/passall/test4.le",
    "tests/passall/test0.le",
    "tests/passall/hello.le",
    "tests/passall/test6.le",
    "tests/passall/test5.le",
    "tests/passall/test8.le",
    "tests/passall/testd.le",
    "tests/passall/test1.le",
    "tests/passall/testg.le",
    "tests/passall/teste.le",
    "tests/passall/test2.le",
    "tests/passall/test3.le"
    ]
    for test in tests:
        assert subprocess.call(["./build/lexerbin", test]) == 0
        assert subprocess.call(["./build/parserbin", test]) == 0
        assert subprocess.call(["./build/semanticsbin", test]) == 0
        assert subprocess.call(["./build/compiler", test]) == 0
