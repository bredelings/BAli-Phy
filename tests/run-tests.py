#!/usr/bin/env python3
from __future__ import print_function

import os
import subprocess
import re

from collections import defaultdict

def indent(n,s):
    space = ' '*n
    s = space + s
    s = re.sub(r'\n', '\n'+space, s)
    return s

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
        # we need to account for rounding error here
        return math.pow(10,prec)*1.1
    else:
        raise ValueError("'{}' is not a number! Cannot get precision.".format(x))


class Program(object):
    def __init__(self,cmd):
        self.cmd = cmd
        self.name = cmd[0]
        self.exec_file = False
        self.likelihood_regex = r".*likelihood[ \t]*=[ \t]*([^ \t]+).*"
        self.extra_args = []

    def prefix(self):
        return "{}-".format(self.name)

    def control_file(self):
        return "{}command.txt".format(self.prefix())

    def cmdline(self, tester, test_subdir):
        test_dir = tester.dir_for_test(test_subdir)
        args_filename = os.path.join(test_dir,self.control_file())
        if self.exec_file:
            return self.cmd + self.extra_args + [args_filename]
        else:
            args = open(args_filename,'r').read()
            return self.cmd + args.split() + self.extra_args

    def stdin(self, tester, test_subdir):
        return ""

    def read_obtained_likelihood_(self, tester, test_subdir, pat, filename='output'):
        import re
        obtained_output = tester.read_obtained(test_subdir, filename)
        for line in obtained_output.splitlines():
            m = re.match(pat, line)
            if m:
                return m.group(1)
        return None

    def read_obtained_likelihood(self, tester, test_subdir):
        return self.read_obtained_likelihood_(tester, test_subdir, self.likelihood_regex)

class RevBayes(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "revbayes"
        self.exec_file = True
        self.likelihood_regex = r".*likelihood =[ \t]+([^ \t]+)( .*|$)"
        self.extra_args = ['--setOption','outputPrecision=17']

    def control_file(self):
        return "rb-command.Rev"

class Paup(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "paup"
        self.exec_file = True

    def control_file(self):
        return "paup-command.nex"

    def read_obtained_likelihood(self, tester, test_subdir):
        likelihood = Program.read_obtained_likelihood_(self, tester, test_subdir, r"-ln L *([^ \t]+).*")
        if likelihood is not None:
            likelihood = "-"+likelihood
        return likelihood

class BAliPhy(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "bali-phy"
        self.likelihood_regex = r".* likelihood = ([^ ]+) .*"
        self.extra_args = []

    def prefix(self):
        return ""

class IQTREE(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "iqtree"
        self.likelihood_regex = r"1. Initial log-likelihood: ([^ ]+)$"
        self.extra_args = ['--show-lh','-redo','-blmin','1.0e-100','-safe']

class raxml_ng(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "raxml-ng"
        self.likelihood_regex = r"Final LogLikelihood: ([^ ]+)$"
        self.extra_args = ['--loglh','--threads','1','--precision','16']


class PhyML(Program):
    def __init__(self, cmd):
        Program.__init__(self,cmd)
        self.name = "phyml"
        self.y = r". Log likelihood of the current tree: ([^ ]+)\$."
        self.likelihood_regex = r". Log likelihood of the current tree: ([^ ]+)\.$"
        self.extra_args = ['--leave_duplicates','-b','0','-o','n','--l_min','1.0e-100']

class hyphymp(Program):
    def __init__(self, cmd):
        Program.__init__(self, cmd)
        self.name = "hyphymp"
        self.likelihood_regex = r"Log Likelihood = ([^ ]+);"
        self.exec_file = True

    def control_file(self):
        return "hyphymp-command.hbl"


class Tester:
    def __init__(self, top_test_dir, data_dir, method):
        self.top_test_dir = top_test_dir
        self.data_dir = data_dir
        self.method = method
        self.NUM_TESTS = 0
        self.FAILED_TESTS = []
        self.XFAILED_TESTS = []

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
            obtained = self.read_obtained(test_subdir, name)
            return set(expected.splitlines()).issubset(set(obtained.splitlines()))

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

            # expect a relative precision of 1e-14
            prec = abs(e)*1.0e-14
            # but if the likelihood isn't specified that precisely, allow a bigger difference.
            prec = max(prec, get_precision(expected_likelihood))

            if abs(diff) < prec:
                return None
            else:
                rel_diff = abs(diff/e)
                return "likelihood error: absolute={}, relative={} (Got {} but expected {})".format(diff,rel_diff,obtained_likelihood,expected_likelihood)

    def test_xfail(self, test_subdir):
        if os.path.exists(os.path.join(test_subdir, self.method.name, 'xfail')):
            return True;
        return os.path.exists(os.path.join(test_subdir,'xfail'))

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
                message = self.read_obtained(test_subdir, 'error').rstrip()
                if message == "":
                    message = self.read_obtained(test_subdir, 'output').rstrip()
#                message = "\n".join(message.splitlines()[-6:])
                message = message.lstrip()

        if (len(message) > 0):
            message = indent(5,message)

        xfail = self.test_xfail(test_subdir)

        return (failures,xfail,message)

    def perform_test(self, test_subdir):
        import re
        self.NUM_TESTS += 1

        print("Running {} test:".format(self.method.name),test_subdir," ",end="", flush=True)
        self.run_test_cmd(test_subdir)
        failures,xfail,message = self.check_test_output(test_subdir)
        if not failures:
            print("... ok")
        elif failures:
            if xfail:
                expected="(expected)"
                self.XFAILED_TESTS.append(test_subdir)
            else:
                self.FAILED_TESTS.append(test_subdir)
                expected=""
            print("... FAIL! {} {}".format(failures,expected))
            if message:
                message = message.rstrip('\n')+"\n"
                print(message)

    def test_result_string(self, test_subdir):
        print("Running {} test:".format(self.method.name),test_subdir," ",file=sys.stderr)
        self.run_test_cmd(test_subdir)
        failures,xfail,message = self.check_test_output(test_subdir)
        if xfail and failures:
            return 'XFAIL'
        elif failures:
            return 'FAIL'
        else:
            return 'PASS'

def get_test_method(cmd):
    prog = prog_name(cmd[0])
    if prog == 'wine':
        prog = prog_name(cmd[1]);

    if prog.startswith('bali-phy'):
        return BAliPhy(cmd)
    elif prog == 'rb' or prog.startswith('rb-'):
        return RevBayes(cmd)
    elif prog == 'paup':
        return Paup(cmd)
    elif prog == 'phyml':
        return PhyML(cmd)
    elif prog == 'iqtree':
        return IQTREE(cmd)
    elif prog == 'raxml-ng':
        return raxml_ng(cmd)
    elif prog.startswith('hyphy'):
        return hyphymp(cmd)
    else:
        print("I don't recognize program '{}' - cowardly refusing to run tests for it.".format(prog))
        exit(1)
    
def coverage_dict(top_test_dir, data_dir, progs):
    coverage = defaultdict(dict)
    for prog in progs:
        method = get_test_method([prog])
        tester = Tester(top_test_dir, data_dir, method)
        supported_tests = tester.get_test_dirs()
        for test in supported_tests:
            coverage[test][prog] = "X"
    return coverage

def results_dict(top_test_dir, data_dir, progs):
    results = defaultdict(dict)
    for prog in progs:
        method = get_test_method([prog])
        tester = Tester(top_test_dir, data_dir, method)
        supported_tests = tester.get_test_dirs()
        for test in supported_tests:
            results[test][prog] = tester.test_result_string(test)
    return results

def remove_prefix(s,prefix):
    return s[len(prefix):] if s.startswith(prefix) else s


def test_matrix_from_dict(results,progs):
    header = ["test"]+progs
    rows = [header]
    for test in results:
        testname = test
        testname = remove_prefix(testname, 'tests/')
        testname = remove_prefix(testname, 'likelihood/')
        row = [testname]
        for prog in progs:
            if prog in results[test]:
                row.append(results[test][prog])
            else:
                row.append("")
        rows.append(row)
    return rows


def print_test_matrix(rows):
    for row in rows:
        print(','.join(row))

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

    if cmd[0] == 'test':
        cmd = cmd[1:]
    elif cmd[0] == 'coverage':
        progs = cmd[1:]
        print_test_matrix(test_matrix_from_dict(coverage_dict(top_test_dir, data_dir, progs),progs))
        exit(1)
    elif cmd[0] == 'results':
        progs = cmd[1:]
        print_test_matrix(test_matrix_from_dict(results_dict(top_test_dir, data_dir, progs),progs))
        exit(1)

    method = get_test_method(cmd)

    print("Running tests for '{}':\n".format(method.name))
    if os.path.isabs(cmd[0]) and not os.path.exists(cmd[0]):
        print("Executable '{}' not found!".format(cmd[0]))
        exit(1)

    tester = Tester(top_test_dir, data_dir, method)

    for test_subdir in tester.get_test_dirs():
        tester.perform_test(test_subdir)
    if (len(tester.FAILED_TESTS) > 0):
        print("FAIL! ({} unexpected failures, {} expected failures, {} tests total)".format(len(tester.FAILED_TESTS), len(tester.XFAILED_TESTS), tester.NUM_TESTS))
        exit(1)
    else:
        print("SUCCESS! ({} unexpected failures, {} expected failures, {} tests total)".format(len(tester.FAILED_TESTS), len(tester.XFAILED_TESTS), tester.NUM_TESTS))

        exit(0)
