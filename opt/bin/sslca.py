#!/usr/bin/env python3

import sys, os, shutil, argparse

def is_exe(fpath):
    return os.path.exists(fpath) and os.access(fpath, os.X_OK)

def which(program):
    import os

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

if (os.name == 'nt'):
    inpath=which("openssl.exe")
    if (inpath):
        opensslbin=inpath
    else:
        # check some known locations on Windows
        for path in [r"C:\Program Files\GnuWin32\bin\openssl.exe",
                     r"C:\Program Files (x86)\GnuWin32\bin\openssl.exe",
                     r"C:\Program Files\OpenVPN\bin\openssl.exe",
                     r"C:\Program Files (x86)\OpenVPN\bin\openssl.exe",
                     r"C:\git\bin\openssl.exe"]
            if (is_exe(path)):
                opensslbin=path
    # if after all that we have nothing, exit
    if (!opensslbin):
        print("Can't find OpenSSL binary. Exiting...")
        sys.exit(1)
    # Windows is unlikely to have this variable set, but just in case the user did:
    if (os.environ['EDITOR']):
        myeditor=os.environ['EDITOR']
    else:
        for path in [r"C:\Program Files\Notepad++\notepad++.exe",
                     r"C:\Program Files (x86)\Notepad++\notepad++.exe"]
            if (is_exe(path)):
                myeditor=path
    # if after all that we have nothing, use notepad
    if (!myeditor):
        myeditor="C:\Windows\system32\notepad.exe"

elif (os.name == 'posix'):
    inpath=which("openssl")
    if (inpath):
        opensslbin=inpath
    else:
        print("Can't find OpenSSL binary. Exiting...")
        sys.exit(1)
    myeditor=os.environ['EDITOR']

def genprivkey(args):
    for clientname in args:
        subprocess.check_call([opensslbin, "genrsa", "-out", clientname+".key", "4096"])
        if (!os.path.exists(clientname+".openssl.cnf")):
            shutil.copy2("openssl.cnf.client-DEFAULT", clientname+".openssl.cnf")
            subprocess.check_call([myeditor, clientname+".openssl.cnf"])
        subprocess.check_call([opensslbin, "req", "-new", "-nodes", "-config", 
                               clientname+".openssl.cnf", "-key", 
                               clientname+".key", "-out", clientname+".csr"])

def signcerts(args):
    for clientname in args:
        subprocess.check_call([opensslbin, c"a", "-batch", "-config",
                               "openssl.cnf.ca", "-in", clientname+".csr",
                               "-out", clientname+".cert", "-days", "7300"])

def gensign:
    genprivkey(args)
    signcerts(args)

def main(*args):
    """ORIGINAL HELP: I need to import this more completely into argparse, below. 

sslca: Perform basic tasks for a mini-PKI.

This script can generate private keys and CRLs and sign them with a CA certificate. 

It has several subcommands: 

sslca help              Show this message
sslca genkey            Generate a private key & CRL for a server
sslca sign              Sign a CRL with an existing CA key
sslca gensign           Do both in one step

It expects you to pass it one or more base names, such as "sub.domain.tld", as arguments. For example: 

sslca genkey myserver.tld

It will create files with names based on this, such as myserver.tld.key and myserver.tld.cert. 

Note: the genkey (and also the gensign) subcommand will look for an openssl configuration file named myserver.tld.openssl.cnf. If it does not find one, it will copy openssl.cnf.client-DEFAULT to myserver.tld.openssl.cnf and open your editor on that file. """

    argparser = argparse.ArgumentParser(description='Perform basic tasks for a mini-PKI')
    subparsers = argparser.add_subparsers()
    
    subparser_genkey = subparsers.add_parser('genkey', help='Generate a private key & CRL for a server')
    #TODO: figure out how to add an argument that's required once but optional infinity times
    #that argument should be here. it's a client name to pass to genkey
    subparser_genkey.set_defaults(func=genkey)
    
    subparser_sign = subparsers.add_parser('sign', help='Sign a CRL with an existing CA key')
    #TODO
    subparser_sign.set_defaults(func=sign)

    subparser_gensign = subparsers.add_parser('sign', help='Both generate and sign in one step')
    #TODO
    subparser_gensign.set_defaults(func=gensign)
    
    parsed = argparser.parse_args()
    logging.debug("Parsed arguments: %r" % parsed)
    parsed.func(parsed)

if __name__ == '__main__':
    sys.exit(main(*sys.argv))

