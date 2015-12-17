#!/usr/bin/env python3

import argparse
import os
import shutil
import string
import subprocess
import sys

docker_hub_user="mrled"
script_root = os.path.dirname(os.path.realpath(__file__))
marionettist_common_template = """
### START Marionettist Docker configuration

RUN mkdir -p /marionettist/roles 
RUN \
    apt-get -y update && \
    apt-get -y install \
        python \
        ansible
$insert_files
RUN chmod 644 /etc/ansible/hosts
RUN ansible-playbook /marionettist/marionettist-docker-container.yml --connection=local

### END Marionettist Docker configuration
"""

# Key: a path relative to this script 
# Value: a path relative to / on your docker image, but w/o the leading slash
marionettist_common_files = {
    '../ansible/roles/marionettist-docker-users': 'marionettist/roles/marionettist-docker-users',
    '../ansible/marionettist-docker-container.yml': 'marionettist/marionettist-docker-container.yml',
    '../ansible/inventory/marionettist-docker-container': 'etc/ansible/hosts'
}

class AsciiEscapes:
    Blue = '\033[94m'
    Green = '\033[92m'
    Yellow = '\033[93m'
    Red = '\033[91m'
    Default = '\033[0m'
    Bold = '\033[1m'
    Underline = '\033[4m'

def just_fucking_makedirs(dirname):
    """
    Just fucking make a directory
    If you have to make a whole tree, just fucking do it 
    If the leaf directory already exisrts, I don't fucking care
    """
    try: 
        os.makedirs(dirname)
    except FileExistsError:
        pass

def just_fucking_copy(src, dst):
    """
    Just fucking copy this thing
    If it's a directory
    - just fucking use shutil.copytree()
    - if the dst path exists, just fucking copy src to be a subdir of dst
    - if the dst path doesn't exist, just fucking copy src to dst
    If it's a file
    - just fucking use shutil.copy()
    - if the dst path doesn't exist, just fucking create it
    """
    just_fucking_makedirs(os.path.dirname(dst))
    if os.path.isdir(src):
        shutil.copytree(src, dst) # NOTE: this copies data at symlinks into non-symlink files in the dst
    else:
        shutil.copy(src, dst)

def copy_common_files(build_dir):
    for key in marionettist_common_files.keys():
        src = os.path.abspath(key)
        dst = os.path.abspath("{}/{}".format(build_dir, marionettist_common_files[key]))
        just_fucking_copy(src, dst)

def run_docker(docker_args, docker_env={}):

    docker_env['DOCKER_CONTENT_TRUST'] = '1'
    possible_env_vars = [
        'DOCKER_TLS_VERIFY',
        'DOCKER_HOST',
        'DOCKER_CERT_PATH',
        'DOCKER_MACHINE_NAME']
    for var_name in possible_env_vars:
        docker_env[var_name] = os.environ[var_name]
    # Cannot pass an item to subprocess.check_call's 'env=' argument if it is None; set those to empty string instead
    for var_name in docker_env.keys():
        if not docker_env[var_name]: 
            docker_env[var_name] = ""
    print("Docker environment: ")
    for key in docker_env.keys(): print("    {}={}".format(key, docker_env[key]))

    #docker_args.insert(0, 'docker')
    docker_args.insert(0, '/opt/homebrew/bin/docker')
    print("Docker call: {}".format(docker_args))

    subprocess.check_call(docker_args, env=docker_env)

def insert_common_dockerfile(build_dir): 
    print('{}Creating Dockerfile...{}'.format(AsciiEscapes.Green, AsciiEscapes.Default))
    in_df_path  = os.path.abspath("{}/Dockerfile.template".format(build_dir))
    out_df_path = os.path.abspath("{}/Dockerfile".format(build_dir))
    with open(in_df_path) as in_file:
        df_template = string.Template(in_file.read())

    add_stanza = ""
    for file_entry in marionettist_common_files.values():
        add_stanza += "ADD {0} /{0}\n".format(file_entry)
    
    common_mapping = {
        'insert_files': add_stanza
    }
    common_template = string.Template(marionettist_common_template)
    marionettist_common_dockerfile = common_template.safe_substitute(**common_mapping)

    template_mapping = {
        'marionettist_common': marionettist_common_dockerfile
    }

    out_df_contents = df_template.safe_substitute(**template_mapping)

    with open(out_df_path, 'w') as out_file:
        print(out_df_contents, file=out_file)

    print(out_df_contents)

def populate_build_dir(imagename, build_dir):
    print('{}Creating build directory at "{}"...{}'.format(
            AsciiEscapes.Green, build_dir, AsciiEscapes.Default))
    if os.path.exists(build_dir):
        shutil.rmtree(build_dir)
    base_path = os.path.abspath("{}/{}".format(script_root, imagename))
    shutil.copytree(base_path, build_dir)
    copy_common_files(build_dir)
    insert_common_dockerfile(build_dir)

def docker_build(imagename, imagetag, build_dir): 
    print('{}Building docker image...{}'.format(
            AsciiEscapes.Green, AsciiEscapes.Default))
    tagged_image_name = '{user}/{image}:{tag}'.format(
        user=docker_hub_user, image=imagename, tag=imagetag)
    run_docker(['build', '--disable-content-trust=false', '-t', tagged_image_name, build_dir])

def docker_push(imagename, imagetag, build_dir, root_pass, repo_pass): 
    print('{}Pushing docker image...{}'.format(
            AsciiEscapes.Green, AsciiEscapes.Default))
    tagged_image_name = '{user}/{image}:{tag}'.format(
        user=docker_hub_user, image=imagename, tag=imagetag)

    env = {
        'DOCKER_CONTENT_TRUST_ROOT_PASSPHRASE': root_pass,
        'DOCKER_CONTENT_TRUST_REPOSITORY_PASSPHRASE': repo_pass}

    print("This is fucking broken right now, you need to run this manually")
    run_docker(['push', '--disable-content-trust=false', tagged_image_name], env)

def show_docker_commands(imagename, imagetag, build_dir):

    docker_cmds  = "docker build --disable-content-trust=false -t {user}/{image}:{tag} {builddir}"
    docker_cmds += "\ndocker push --disable-content-trust=false  {user}/{image}:{tag}"
    
    print("########")
    print("# Build & Push :{tag}".format(tag=imagetag))
    print(docker_cmds.format(
            user=docker_hub_user, image=imagename, tag=imagetag, builddir=build_dir))
    print("########")
    print("# Build & Push :latest")
    print(docker_cmds.format(
            user=docker_hub_user, image=imagename, tag="latest", builddir=build_dir))
    print("########")

def dmake_main(*args): 
    argparser = argparse.ArgumentParser(description='This is some dumb shit')
    argparser.add_argument(
        'name', action='store',
        help='The name of the docker image to build')
    argparser.add_argument(
        'tag', action='store',
        help='The tag for this version of the docker image')
    argparser.add_argument(
        '--build', action='store_true',
        help='Build the docker image')
    argparser.add_argument(
        '--push', action='store_true',
        help='Push the docker image to the registry. (Does NOT --build.)')
    argparser.add_argument(
        '--root-pass', action='store',
        help='The password for the root private key')
    argparser.add_argument(
        '--repo-pass', action='store',
        help='The password for the repo private key')
    # argparser.add_argument(
    #     '--no-latest','-l', action='store_true',
    #     help='Do not push the :latest tag, just the named tag')
    
    parsedargs = argparser.parse_args()

    src_dir = os.path.abspath("{}/{}".format(script_root, parsedargs.name))
    if not os.path.exists(src_dir):
        raise Exception("No such docker name '{}'".format(parsedargs.name))
    build_dir = os.path.abspath("{}/build/{}".format(script_root, parsedargs.name))
    populate_build_dir(parsedargs.name, build_dir)
    if parsedargs.build:
        docker_build(parsedargs.name, parsedargs.tag, build_dir)
        # if not parsedargs.no_latest:
        #     docker_build(parsedargs.name, 'latest', build_dir)
    if parsedargs.push:
        docker_push(parsedargs.name, parsedargs.tag, build_dir, parsedargs.root_pass, parsedargs.repo_pass)
        # if not parsedargs.no_latest:
        #     docker_push(parsedargs.name, 'latest', build_dir)
    show_docker_commands(parsedargs.name, parsedargs.tag, build_dir)

if __name__ == '__main__':
    sys.exit(dmake_main(*sys.argv))
