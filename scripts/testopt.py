import os

tests = [
    ("tests/optimiser/testa.le","", "-2\n-1.25\n0.5\n1.25\n40"),
    ("tests/optimiser/testb.le","", "11111"),
    ("tests/optimiser/testc.le","", "1\n20"),
    ("tests/optimiser/testd.le","", "43"),
    ("tests/optimiser/teste.le","", "12"),
    ("tests/optimiser/testf.le","", "2\n16\n0"),
    ("tests/optimiser/testg.le","", "")
]

def main():
    for test in tests:
        filename = os.path.split(test[0])[1]
        print "--- start optimise test " + filename + " ---"
        os.system("./scripts/optcompare.sh %s %s \"%s\" %s" % (test[0], "/tmp/" + filename + ".ass", test[2], test[1]))
        print "--- end   optimise test " + filename + " ---"

if __name__ == "__main__":
    main()
