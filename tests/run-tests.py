#!/usr/bin/python
from __future__ import print_function
import subprocess
import os

NUM_TESTS = 0
FAILED_TESTS = []
XFAILED_TESTS = []

def debug(m):
    sys.stderr.write('DEBUG: ')
    sys.stderr.write(m)
    sys.stderr.write('\n')

def error(m):
    sys.stderr.write('ERROR: ')
    sys.stderr.write(m)
    sys.stderr.write('\n')

def get_precision(x):
    import re
    import math
    numeric_decimal_pattern = r"""
        [-+]? # optional sign
        (?:
            (?: \d* \. (\d+) ) # .1 .12 .123 etc 9.1 etc 98.1 etc
            |
            (?: \d+ \.? ) # 1. 12. 123. etc 1 12 123 etc
        )
        # followed by optional exponent part if desired
        (?: [Ee] [+-]? (\d+) ) ?
     """
    rx = re.compile(numeric_decimal_pattern, re.VERBOSE)
    m = re.match(rx, x)
    if m:
        dec = m.group(1)
        exp = m.group(2)
        if dec:
            dec = len(dec)
        else:
            dec = 0
        if exp:
            exp = int(exp)
        else:
            exp = 0
        prec = exp - dec
        return math.pow(10,prec)
    else:
        raise ValueError("'{}' is not a number! Cannot get precision.".format(x))


class RevBayes:
    def __init__(self, cmd):
        self.name = "revbayes"
        self.cmd = cmd

    def prefix(self):
        return "rb-"

    def control_file(self):
        return "rb-command.Rev"

    def cmdline(self, tester, test_subdir):
        import re
        return cmd

    def stdin(self, tester, test_subdir):
        test_dir = tester.dir_for_test(test_subdir)
        command_file = os.path.join(test_dir, self.control_file())
        return 'datadir = "{}";source("{}")'.format(tester.data_dir,command_file);

    def read_obtained_likelihood(self, tester, test_subdir):
        import re
        obtained_output = tester.read_obtained(test_subdir, 'output')
        obtained_likelihood = None
        for line in obtained_output.splitlines():
            m = re.match(r".*likelihood =[ \t]+([^ \t]+).*", line)
            if m:
                obtained_likelihood = m.group(1)
        return obtained_likelihood

class BAliPhy:
    def __init__(self, cmd):
        self.name = "bali-phy"
        self.cmd = cmd

    def prefix(self):
        return ""

    def control_file(self):
        return "command.txt"

    def cmdline(self, tester, test_subdir):
        import re
        test_dir = tester.dir_for_test(test_subdir)
        args_filename = os.path.join(test_dir,self.control_file())
        args = open(args_filename,'r').read()
        args = re.sub('<DATA>',tester.data_dir,args)
        return cmd + args.split()

    def stdin(self, tester, test_subdir):
        return ""

    def read_obtained_likelihood(self, tester, test_subdir):
        import re
        obtained_output = tester.read_obtained(test_subdir, 'output')
        obtained_likelihood = None
        for line in obtained_output.splitlines():
            m = re.match(r".* likelihood = ([^ ]+) .*", line)
            if m:
                obtained_likelihood = m.group(1)
        return obtained_likelihood

class Tester:
    def __init__(self, top_test_dir, data_dir, method):
        self.top_test_dir = top_test_dir
        self.data_dir = data_dir
        self.method = method

    def dir_for_test(self, test_subdir):
        return os.path.join(self.top_test_dir, test_subdir)

    def rundir_for_test(self, test_subdir):
        return os.path.join(self.top_test_dir, test_subdir)

    def get_test_dirs(self):
        test_dirs = []
        for root, dirs, files in os.walk(top_test_dir):
            if self.method.control_file() in files:
                path = os.path.relpath(root, top_test_dir)
                test_dirs.insert(0,path)
        return test_dirs

    def run_test_cmd(self,test_subdir):
        print("Running test:",test_subdir," ",end="")
        rundir = self.rundir_for_test(test_subdir)
        prefix = self.method.prefix() + "obtained-"
        obt_outf = os.path.join(rundir, prefix + 'output')
        obt_errf = os.path.join(rundir, prefix + 'error')
        obt_exitf = os.path.join(rundir, prefix +'exit')

        cmd = self.method.cmdline(self, test_subdir)
        stdin = self.method.stdin(self, test_subdir)

        with codecs.open(obt_outf, 'w', encoding='utf-8') as obt_out:
            with codecs.open(obt_errf, 'w', encoding='utf-8') as obt_err:
    #            invocation = '"{}"'.format('" "'.join(cmd))
    #            debug('Running: ' + invocation + ' >"' + obt_outf + '" 2>"' + obt_errf + '" ; echo $? >"' + obt_exitf + '"')
                p = subprocess.Popen(cmd, cwd=rundir, stdin=subprocess.PIPE, stdout=obt_out, stderr=obt_err)
                p.communicate(input=stdin)
                exit_code = p.wait()
                with codecs.open(obt_exitf, 'w', encoding='utf-8') as obt_exit:
                    obt_exit.write('{e:d}\n'.format(e=exit_code))

    def read_expected(self, test_subdir, name):
        test_dir = os.path.join(self.top_test_dir, test_subdir)
        pathname = os.path.join(test_dir, name)
        if not os.path.exists(pathname):
            if name == 'exit':
                return "0"
            else:
                return None
        return codecs.open(pathname, 'r', encoding='utf-8').read().rstrip()

    def read_obtained(self, test_subdir, name):
        rundir = self.rundir_for_test(test_subdir)
        prefix = self.method.prefix()+"obtained-"
        outputf   = os.path.join(rundir, prefix+name)
        return codecs.open(outputf  , 'r', encoding='utf-8').read().rstrip()

    def check_expected(self, test_subdir, name):
        test_dir = self.dir_for_test(test_subdir)
        expected = self.read_expected(test_subdir, name)
        if expected is None:
            return True;
        else:
            return expected == self.read_obtained(test_subdir, name)

    def check_likelihood(self, test_subdir):
        import math

        expected_likelihood = self.read_expected(test_subdir, 'likelihood')

        if not expected_likelihood:
            return None

        obtained_likelihood = self.method.read_obtained_likelihood(self,test_subdir)

        if not obtained_likelihood:
            return "No likelihood found!"

        if expected_likelihood and obtained_likelihood:
            e = float(expected_likelihood);
            o = float(obtained_likelihood);
            diff = o - e
            if abs(diff) < get_precision(expected_likelihood):
                return None
            else:
                return "likelihood is off by {}! (Got {} but expected {})".format(diff,obtained_likelihood,expected_likelihood)

    def check_test_output(self,test_subdir):
        test_dir = os.path.join(self.top_test_dir, test_subdir)
        failures = []
        message = ""
        exit_test_failed = False
        if not self.check_expected(test_subdir, 'output'):
            failures.append('output')
        if not self.check_expected(test_subdir, 'error'):
            failures.append('error')
        if not self.check_expected(test_subdir, 'exit'):
            failures.append('exit')
            exit_test_failed = True
        likelihood_message = self.check_likelihood(test_subdir)
        if likelihood_message:
            message += likelihood_message
            failures.append('likelihood')

        if exit_test_failed:
            expected_exit = self.read_expected(test_subdir, 'exit')
            if expected_exit.rstrip() == "0":
                message = self.read_obtained(test_subdir, 'error')

        xfail = os.path.exists(os.path.join(test_dir,'xfail'))

        return (failures,xfail,message)

    def perform_test(self, test_subdir):
        import re
        global NUM_TESTS, FAILED_TESTS
        NUM_TESTS += 1

        self.run_test_cmd(test_subdir)
        failures,xfail,message = self.check_test_output(test_subdir)
        if not failures:
            print("... ok")
        elif failures:
            print("... FAIL! ",end="")
            if xfail:
                print("(expected) ",end="")
                XFAILED_TESTS.insert(-1,test_subdir)
            else:
                FAILED_TESTS.insert(-1,test_subdir)
                print(failures)
            if message:
                message = re.sub('^','    ',message)
                message = message.rstrip('\n')+"\n"
                print(message)

def prog_name(pathname):
    import re
    filename = os.path.basename(pathname)
    if filename[-4:] == ".exe":
        filename = filename[0:-4]
    return filename

if __name__ == '__main__':
    import codecs
#    import json
    import sys

    script_dir = os.path.split(sys.argv[0])[0]
    script_dir = os.path.abspath(script_dir)

    data_dir = os.path.join(script_dir, 'data')

    top_test_dir = os.getcwd()

    cmd = sys.argv[1:]
    if not cmd:
        print("Please specify which program to test! (e.g. 'bali-phy', 'rb', etc.)")
        exit(1)
    prog = prog_name(cmd[0])
    if prog == 'bali-phy':
        method = BAliPhy(cmd)
    elif prog == 'rb':
        method = RevBayes(cmd)
    else:
        print("I don't recognize program '{}' - cowardly refusing to run tests for it.".format(prog))
        exit(1)

    print("Running tests for '{}':\n".format(method.name))
    if os.path.isabs(cmd[0]) and not os.path.exists(cmd[0]):
        print("Executable '{}' not found!".format(cmd[0]))
        exit(1)

    tester = Tester(top_test_dir, data_dir, method)

    for test_subdir in tester.get_test_dirs():
        tester.perform_test(test_subdir)
    print("Performed {} tests, {} expected failures, {} unexpected failures".format(NUM_TESTS, len(XFAILED_TESTS), len(FAILED_TESTS)))
    if (len(FAILED_TESTS) > 0):
        exit(1)
    else:
        exit(0)
