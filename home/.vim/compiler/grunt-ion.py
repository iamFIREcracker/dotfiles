#!/usr/bin/env python

import sys
import re
import subprocess

# Extract stuff from:
# `>> components/RequestEntry.ts(228,21): error TS1005: ',' expected.`
FILE_LINE_COLUMN_COMPILE_ERROR_PATTERN = re.compile(r'>> ([^(]*)\(([0-9]*),([0-9]*)\): (.*)')

# Extract stuff from:
#
# Module: Parsers Test: time when input is valid
#     at Object.oms.html.oms.html (/home/vagrant/workspace/oms.html/_UnitTest/utils/PresentationTest.js:173:20)
# Actual value:
# 40
# Expected value:
# 4
FILE_LINE_COLUMN_TEST_ERROR_PATTERN = re.compile(r'\s*at [^ ]* \(([^:]*):([0-9]*):([0-9]*)\)')
EMPTY_LINE = re.compile(r'\s*$')

def process(output):
    error = 0;
    testmatch = None
    testmessage = ''
    for l in output.splitlines():
        m = FILE_LINE_COLUMN_COMPILE_ERROR_PATTERN.match(l)
        if m is not None:
            error += 1
            print '%d:%s:%s:%s:%s' % (error, m.group(1), m.group(2),
                                      m.group(3), m.group(4))
        else:
            if testmatch is None:
                m = FILE_LINE_COLUMN_TEST_ERROR_PATTERN.match(l)
                if m is not None:
                    testmatch = m
                    testmessage = ''
            else:
                m = EMPTY_LINE.match(l)
                if m is not None:
                    error + 1
                    print '%d:%s:%s:%s:%s' % (error,
                                              testmatch.group(1),
                                              testmatch.group(2),
                                              testmatch.group(3),
                                              testmessage)
                    testmatch = None
                else:
                    testmessage += ' ' + l


if __name__ == '__main__':
    cmd = 'grunt %s || :' % ' '.join('"%s"' % a for a in sys.argv[1:])
    output = subprocess.check_output(cmd, shell=True)
    process(output)
