#!/usr/bin/python
import subprocess

NUM_TESTS = 0
FAILED_TESTS = []

def debug(m):
    sys.stderr.write('DEBUG: ')
    sys.stderr.write(m)
    sys.stderr.write('\n')
def error(m):
    sys.stderr.write('ERROR: ')
    sys.stderr.write(m)
    sys.stderr.write('\n')

def get_test_dirs(top_test_dir):
    test_dirs = []
    for root, dirs, files in os.walk(top_test_dir):
        if 'command.txt' in files:
            path = os.path.relpath(root, top_test_dir)
            test_dirs.insert(0,path)
    return test_dirs
    
def run_test_cmd(test_dir, cmd):
    print "Running test:",test_subdir," ",
    obt_outf = os.path.join(test_dir, 'obtained-output')
    obt_errf = os.path.join(test_dir, 'obtained-error')
    obt_exitf = os.path.join(test_dir, 'obtained-exit')
    obt_likef = os.path.join(test_dir, 'obtained-likelihood')
    argsf = os.path.join(test_dir,'command.txt')
    args = open(argsf,'r').read().split()
    cmd = cmd + args
    with codecs.open(obt_outf, 'w', encoding='utf-8') as obt_out:
        with codecs.open(obt_errf, 'w', encoding='utf-8') as obt_err:
            invocation = '"{}"'.format('" "'.join(cmd))
#            debug('Running: ' + invocation + ' >"' + obt_outf + '" 2>"' + obt_errf + '" ; echo $? >"' + obt_exitf + '"')
            p = subprocess.Popen(cmd, cwd=test_dir, stdout=obt_out, stderr=obt_err)
            exit_code = p.wait()
            with codecs.open(obt_exitf, 'w', encoding='utf-8') as obt_exit:
                obt_exit.write('{e:d}\n'.format(e=exit_code))


def check_expected(test_dir, outputf, expectedf):
    outputf   = os.path.join(test_dir, outputf)
    expectedf = os.path.join(test_dir, expectedf)
    if (os.path.exists(expectedf)):
        expected = codecs.open(expectedf, 'r', encoding='utf-8').read()
        output   = codecs.open(outputf  , 'r', encoding='utf-8').read()
        if output != expected:
#            subprocess.call(['diff', expectedf, outputf])
            return False

    return True

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
        

def check_likelihood(test_dir):
    import re
    import math
    expectedf = os.path.join(test_dir, 'likelihood')
    expected_likelihood = None
    if os.path.exists(expectedf):
        expected_likelihood = codecs.open(expectedf, 'r').read()
    else:
        return True

    outputf   = os.path.join(test_dir, 'obtained-output')
    obtained_likelihood = None
    with codecs.open(outputf, 'r') as output:
        for line in output:
            m = re.match(r".* likelihood = ([^ ]+) .*", line)
            if m:
                obtained_likelihood = m.group(1)

    if not obtained_likelihood:
        print "no likelihood found!"
        return False

    if expected_likelihood and obtained_likelihood:
        e = float(expected_likelihood);
        o = float(obtained_likelihood);
        diff = o - e
        if diff < get_precision(expected_likelihood):
            return True
        else:
            print "likelihood is off by",diff,"!"
            return False

                
def check_test_output(test_dir, name):
    failures = []
    if not check_expected(test_dir, 'obtained-output', 'output'):
        failures.append('output')
    if not check_expected(test_dir, 'obtained-error', 'error'):
        failures.append('error')
    if not check_expected(test_dir, 'obtained-exit', 'exit'):
        failures.append('exit')
    if not check_likelihood(test_dir):
        failures.append('likelihood')
    return failures

def perform_test(top_test_dir,test_subdir,cmd):
    global NUM_TESTS, FAILED_TESTS
    test_dir = os.path.join(top_test_dir, test_subdir)
    NUM_TESTS += 1
    obt_outf = os.path.join(test_dir, 'obtained-output')
    obt_errf = os.path.join(test_dir, 'obtained-error')
    obt_exitf = os.path.join(test_dir, 'obtained-exit')
    obt_likef = os.path.join(test_dir, 'obtained-likelihood')
    run_test_cmd(test_dir, cmd)
    failures = check_test_output(test_dir, test_subdir)
    if not failures:
        print "... ok"
    else:
        print "... FAIL!",
        print failures
        FAILED_TESTS.insert(-1,test_subdir)
        


if __name__ == '__main__':
    import codecs
#    import json
    import sys
    import os

    top_test_dir = os.path.split(sys.argv[0])[0]
    top_test_dir = os.path.abspath(top_test_dir)

    cmd = sys.argv[1:]
    if not cmd:
        cmd = ['bali-phy']

    for test_subdir in get_test_dirs(top_test_dir):
        perform_test(top_test_dir, test_subdir, cmd)
    print "Performed {} tests, {} failures".format(NUM_TESTS, len(FAILED_TESTS))
