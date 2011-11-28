#!/usr/bin/env python3

import sys, os, shutil, argparse, logging, subprocess
#logging.basicConfig(level=logging.CRITICAL) #show only logging.critical() messages
logging.basicConfig(level=logging.DEBUG) #show all messages up to and including logging.debug() messages

def is_exe(fpath):
    return os.path.exists(fpath) and os.access(fpath, os.X_OK)

def which(program):
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

def genprivkey(args):
    logging.debug("genprivkey args: %r" % args)
    for clientname in args.clientname:
        subprocess.check_call([opensslbin, "genrsa", "-out", clientname+".key", "4096"])
        if not (os.path.exists(clientname+".openssl.cnf")):
            logging.debug("genprivkey: openssl configuration file not present, copying openssl.cnf.client-DEFAULT to %r" % clientname+".openssl.cnf")
            shutil.copy2("openssl.cnf.client-DEFAULT", clientname+".openssl.cnf")
            #input("Your editor was opened on the file %r; press return when you have finished editing this file (REQUIRED).")
            subprocess.check_call([myeditor, clientname+".openssl.cnf"])
        subprocess.check_call([opensslbin, "req", "-new", "-nodes", "-config", 
                               clientname+".openssl.cnf", "-key", 
                               clientname+".key", "-out", clientname+".csr"])

def signcerts(args):
    logging.debug("signcerts args: %r" % args)
    for clientname in args.clientname:
        subprocess.check_call([opensslbin, "ca", "-batch", "-config",
                               "openssl.cnf.ca", "-in", clientname+".csr",
                               "-out", clientname+".cert", "-days", "7300"])

def gensign(args):
    logging.debug("gensign args: %r" % args)
    genprivkey(args)
    signcerts(args)


def main(*args):
    #logging.debug("main args: " + args)
    global opensslbin
    global myeditor
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
                         r"C:\git\bin\openssl.exe"]:
                if (is_exe(path)):
                    opensslbin=path
                    break
            else:
                # if after all that we have nothing, exit
                print("Can't find OpenSSL binary. Exiting...")
                sys.exit(1)
        myeditor="C:\Windows\system32\notepad.exe"
    elif (os.name == 'posix'):
        # for POSIX systems we're just going to assume that openssl is in the path and $EDITOR is an existing env var. 
        inpath=which("openssl")
        if (inpath):
            opensslbin=inpath
        else:
            print("Can't find OpenSSL binary. Exiting...")
            sys.exit(1)
        myeditor=os.environ['EDITOR']

    argparser = argparse.ArgumentParser(description='Perform basic tasks for a mini-PKI')
    subparsers = argparser.add_subparsers()
    
    subparser_genkey = subparsers.add_parser('genkey', help='Generate a private key & CRL for a server')
    subparser_genkey.add_argument('clientname', nargs='+', action='store', help='Supply a clientname, such as myserver or myserver.sub.domain.tld. The filenames for the cert, CRL, etc are based on this name. This subcommand looks for an openssl configuration file named clientname.openssl.cnf; if it does not find one, it will copy openssl.cnf.client-DEFAULT to clientname.openssl.cnf and open your editor on that file.')
    subparser_genkey.set_defaults(func=genprivkey)
    
    subparser_sign = subparsers.add_parser('sign', help='Sign a CRL with an existing CA key')
    subparser_sign.add_argument('clientname', nargs='+', action='store', help='Supply a clientname, such as myserver or myserver.sub.domain.tld. The filenames for the cert, CRL, etc are based on this name.')
    subparser_sign.set_defaults(func=signcerts)

    subparser_gensign = subparsers.add_parser('gensign', help='Both generate and sign in one step')
    subparser_gensign.add_argument('clientname', nargs='+', action='store', help='Supply a clientname, such as myserver or myserver.sub.domain.tld. The filenames for the cert, CRL, etc are based on this name. This subcommand looks for an openssl configuration file named clientname.openssl.cnf; if it does not find one, it will copy openssl.cnf.client-DEFAULT to clientname.openssl.cnf and open your editor on that file.')
    subparser_gensign.set_defaults(func=gensign)
    
    parsed = argparser.parse_args()
    parsed.func(parsed)

if __name__ == '__main__':
    sys.exit(main(*sys.argv))

