#!/usr/bin/python

import re, subprocess

def get_keychain_pass(service=None, account=None):
    params = {
        'keyring': '~/bin/keyring',
        'command': 'get',
        'service': service,
        'account': account
    }
    command = "%(keyring)s %(command)s %(service)s %(account)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = output.splitlines()[0]

    return re.match(r'(.+)', outtext).group(1)
