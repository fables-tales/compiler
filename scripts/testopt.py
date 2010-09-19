import os

tests = [
    ("tests/optimiser/testa.le","", "-2\n-1.25\n0.5\n1.25\n40")
]

def main():
    for test in tests:
        filename = os.path.split(test[0])[1]
        print "--- start optimise test " + filename + " ---"
        os.system("./scripts/optcompare.sh %s %s \"%s\" %s" % (test[0], "/tmp/" + filename + ".ass", test[2], test[1]))
        print "--- end   optimise test " + filename + " ---"

if __name__ == "__main__":
    main()
