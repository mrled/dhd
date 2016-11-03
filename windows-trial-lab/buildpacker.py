#!/usr/bin/env python3

import argparse
import glob
import os
import shutil
import subprocess
import sys


scriptdir = os.path.dirname(os.path.realpath(__file__))
debug = False
verbose = False


# TODO: use logging module?
# TODO: normalize all user paths
# TODO: re-add tagging


def strace():
    import pdb
    pdb.set_trace()


def debugprint(message):
    global debug
    if debug:
        print("DEBUG: " + message)


def verboseprint(message):
    global verbose
    if verbose:
        print("VERBOSE: " + message)


def resolvepath(path):
    return os.path.realpath(os.path.normpath(os.path.expanduser(path)))


def which(commandname):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        exe_file = os.path.join(path, commandname)
        if is_exe(exe_file):
            return exe_file
    print("Exe path is {}".format(exe_file))
    raise Exception("No such command '{}' in %PATH%".format(commandname))


def buildpacker(packerfile, outdir, force=False, whatif=False):
    packerfile = resolvepath(packerfile)
    if not os.path.isfile(packerfile):
        raise Exception("No such packerfile: '{}'".format(packerfile))
    outdir = resolvepath(outdir)
    if not os.path.isdir(outdir):
        os.makedirs(outdir, exist_ok=True)

    logdir = os.path.join(outdir, "packer_log")
    packerdir = os.path.dirname(packerfile)

    oldoutputs = glob.glob("{}/output-*".format(packerdir))
    if len(oldoutputs) > 0:
        if force:
            for oldoutput in oldoutputs:
                shutil.rmtree(oldoutput)
        else:
            raise Exception("A packer output directory exists at '{}'".format(oldoutput))

    cli = 'packer.exe build -var output_directory="{}" {}'.format(outdir, packerfile)

    # NOTE: Packer gives a very weird error if you do not COPY the entire environment
    # When I was setting env to be just a dictionary with the PACKER_* variables I needed,
    # I was seeing errors like this:
    # Failed to initialize build 'virtualbox-iso': error initializing builder 'virtualbox-iso': Unrecognized remote plugin message: Error starting plugin server: Couldn't bind plugin TCP listener
    # Once I copied the entire environment, it was fine. I have no idea why.
    env = os.environ
    env['PACKER_DEBUG'] = '1'
    env['PACKER_LOG'] = '1'
    env['PACKER_LOG_PATH'] = logdir

    verboseprint("Running command:\n    {}\n  from directory: {}\n  with environment:\n    {}".format(cli, packerdir, env))
    if whatif:
        return
    else:
        subprocess.check_call(cli, env=env, cwd=packerdir)

    boxes = glob.glob("{}/*.box".format(outdir))
    if len(boxes) > 1:
        raise Exception("Somehow you came up with more than one box here: '{}'".format(boxes))
    elif len(boxes) < 1:
        raise Exception("Apparently the packer process failed, no boxes were created")

    verboseprint("Packed .box file: '{}'".format(boxes[0]))


def addvagrantbox(vagrantboxname, packedboxpath, force, whatif):
    packedboxdir = os.path.dirname(packedboxpath)
    packedboxname = os.path.basename(packedboxpath)

    # cli = ['vagrant', 'box', 'add', '--force' if force else '', '--name', vagrantboxname, packedboxname]
    cli = "vagrant.exe box add {} --name {} {}".format(
        '--force' if force else '',
        vagrantboxname,
        packedboxname)

    verboseprint("Running command:\n    {}".format(cli))
    if whatif:
        return
    else:
        subprocess.check_call(cli, cwd=packedboxdir)


def main(*args, **kwargs):
    parser = argparse.ArgumentParser(
            description="Windows Trial Lab: build trial Vagrant boxes.",
            epilog="NOTE: requires packer 0.8.6 or higher and vagrant 1.8 or higher. EXAMPLE: buildlab --baseconfigname windows_10_x86; cd vagrant/FreyjaA; vagrant up")

    parser.add_argument(
        "baseconfigname", action='store',
        help="The name of one of the subdirs of this directory, like windows_81_x86")

    parser.add_argument(
        "--base-out-dir", "-o", action='store', default="~/Documents/WinTrialLab",
        help="The base output directory, where Packer does its work and saves its final output. (NOT the VM directory, which is a setting in VirtualBox.)")
    parser.add_argument(
        "--action", "-a", action='store', default="all", choices=['all','packer','vagrant'],
        help="The action to perform. By default, build with packer and add to vagrant.")
    parser.add_argument(
        "--whatif", "-w", action='store_true',
        help="Do not perform any actions, only say what would have been done")
    parser.add_argument(
        "--force", "-f", action='store_true',
        help="Force continue, even if old output directories already exist")
    parser.add_argument(
        "--debug", "-d", action='store_true',
        help="Print debug and verbose messages")
    parser.add_argument(
        "--verbose", "-v", action='store_true',
        help="Print verbose messages")

    parsed = parser.parse_args()
    if parsed.debug:
        global debug
        debug = True
    if parsed.debug or parsed.verbose:
        global verbose
        verbose = True

    if parsed.action == "all":
        actions = ['packer', 'vagrant']
    else:
        actions = [parsed.action]

    fullconfigname = "wintriallab-{}".format(parsed.baseconfigname)
    packeroutdir = os.path.join(resolvepath(parsed.base_out_dir), fullconfigname)
    packerfile = os.path.join(scriptdir, 'packer', parsed.baseconfigname, '{}.packerfile.json'.format(parsed.baseconfigname))
    packedboxpath = os.path.join(packeroutdir, '{}_virtualbox.box'.format(parsed.baseconfigname))

    if 'packer' in actions:
        buildpacker(packerfile, packeroutdir, force=parsed.force, whatif=parsed.whatif)
    if 'vagrant' in actions:
        addvagrantbox(fullconfigname, packedboxpath, force=parsed.force, whatif=parsed.whatif)

if __name__ == '__main__':
    sys.exit(main(*sys.argv))
