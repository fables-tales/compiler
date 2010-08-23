#!/usr/bin/env python
import subprocess


if __name__ == "__main__":
    tests = ["tests/lex/pass/hello.le",
    "tests/lex/pass/testa.le",
    "tests/lex/pass/testb.le",
    "tests/lex/pass/testc.le",
    "tests/lex/pass/test7.le",
    "tests/lex/pass/testf.le",
    "tests/lex/pass/test4.le",
    "tests/lex/pass/test0.le",
    "tests/lex/pass/hello.le",
    "tests/lex/pass/test6.le",
    "tests/lex/pass/test5.le",
    "tests/lex/pass/test8.le",
    "tests/lex/pass/testd.le",
    "tests/lex/pass/test1.le",
    "tests/lex/pass/testg.le",
    "tests/lex/pass/teste.le",
    "tests/lex/pass/test2.le",
    "tests/lex/pass/test3.le"
    ]
    for test in tests:
        assert subprocess.call(["./build/lexerbin", test]) == 0

    fails = [
            "tests/lex/fail/fail1.le",
            "tests/lex/fail/fail2.le"
            ]
    for fail in fails:
        assert subprocess.call(["./build/lexerbin", fail]) != 0
