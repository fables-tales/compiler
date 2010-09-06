import subprocess
import os
import sys

tests = ["test1","hello","test2", "test3","test4", "test5"]

def do_run_test(test):
    print "running test '" + test + "'"
    test_root = os.path.join("tests","runtests",test)
    input_path = os.path.join(test_root,test) + ".le"
    output_path = input_path + ".ass"
    test_ass_path = input_path + ".test.ass"

    success = 0

    #run output
    output = open(os.path.join(test_root,"run.out"), "w")

    #verify run output matches expected
    a = subprocess.call(["./scripts/testrun.sh", input_path,  output_path], stdout=output)
    output.close()
    result = subprocess.call(["diff",os.path.join(test_root,"run.out"),test_ass_path])

    #if we fail, print an error
    if (result != 0):
        print "failed\n"
        success = 1
    return success

if __name__ == "__main__":
    failbit = 0
    for test in tests:
        failbit |= do_run_test(test)

    sys.exit(failbit)

