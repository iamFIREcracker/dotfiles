#!/usr/bin/env python

import sys
import re
import subprocess

# Matches `12` from: ` 12) UserManager create Should ...`
ERROR_PATTERN = re.compile(r'\s*([0-9]+)\) .*')
# Matches `AssertionError: 1 == 2` in: ` AssertionError: 1 == 2`
MESSAGE_PATTERN = re.compile(r'\s*(AssertionError: .*)')
# Matches `test/users.js:24:20` in:
# ` at Context.<anonymous> (test/users.js:24:20)`
FILE_LINE_COLUMN_PATTERN = re.compile(r'\s*at .* \(([^:]*:[0-9]*:[0-9]*)\)')


def remove_matched_pattern(l, match):
    _, j = match.span()
    return l[j:]


def matched_and_remaining(l, match):
    return match.group(1), remove_matched_pattern(l, match)


def parse(pattern, l):
    m = pattern.match(l)
    if m is None:
        return None, l
    return matched_and_remaining(l, m)


def process(output):
    error = message = file_line_column = None

    for l in output.splitlines():
        if error is None:
            error, l = parse(ERROR_PATTERN, l)
            if error is None:
                continue

        if message is None:
            message, l = parse(MESSAGE_PATTERN, l)
            if message is None:
                continue

        if file_line_column is None:
            file_line_column, l = parse(FILE_LINE_COLUMN_PATTERN, l)
            if file_line_column is None:
                if l.strip():
                    message = message + '\n' + l
                continue

        if all([error, message, file_line_column]):
            print '%s:%s:%s' % (error, file_line_column, message)
            error = message = file_line_column = None


if __name__ == '__main__':
    cmd = 'mocha %s || :' % ' '.join('"%s"' % a for a in sys.argv[1:])
    output = subprocess.check_output(cmd, shell=True)
    process(output)
