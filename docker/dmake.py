#!/usr/bin/env python3

import argparse
import os
import shutil
#import subprocess
import sys

script_root = os.path.dirname(os.path.realpath(__file__))
marionettist_common_template = """
### START Marionettist Docker configuration

RUN mkdir -p /marionettist/roles 
RUN \
    apt-get -y update && \
    apt-get -y install \
        python \
        ansible
{insert_files}
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

# Just fucking make a directory
# If you have to make a whole tree, just fucking do it
# If the leaf directory already exists, I don't fucking care
def just_fucking_makedirs(dirname):
    try: 
        os.makedirs(dirname)
    except FileExistsError:
        pass

# Just fucking copy it
# If it's a directory, just fucking use shutil.copytree()
# If it's a file, just fucking use shutil.copy()
def just_fucking_copy(src, dst):
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

def insert_common_dockerfile(build_dir): 
    in_df_path  = os.path.abspath("{}/Dockerfile.template".format(build_dir))
    out_df_path = os.path.abspath("{}/Dockerfile".format(build_dir))
    with open(in_df_path) as in_file:
        in_df_contents = in_file.read()

    add_stanza = ""
    for file_entry in marionettist_common_files.values():
        add_stanza += "ADD {0} /{0}\n".format(file_entry)
    marionettist_common_dockerfile = marionettist_common_template.format(**{'insert_files': add_stanza})
    out_df_contents  = in_df_contents.format(**{'marionettist_common': marionettist_common_dockerfile})

    with open(out_df_path, 'w') as out_file:
        print(out_df_contents, file=out_file)

def build_docker_dir(dockername, build_dir):
    if os.path.exists(build_dir):
        shutil.rmtree(build_dir)
    base_df_path = os.path.abspath("{}/{}/Dockerfile.template".format(script_root, dockername))
    just_fucking_makedirs(build_dir)
    shutil.copy(base_df_path, build_dir)
    insert_common_dockerfile(build_dir)
    copy_common_files(build_dir)
    print("Build directory created at {}".format(build_dir))

#def run_docker(build_dir):
    

def dmake_main(*args): 
    argparser = argparse.ArgumentParser(description='This is some dumb shit')
    argparser.add_argument('dockername', action='store',
        help='The name of the docker configuration to build')
    parsedargs = argparser.parse_args()

    src_dir = os.path.abspath("{}/{}".format(script_root, parsedargs.dockername))
    if not os.path.exists(src_dir):
        raise Exception("No such docker name '{}'".format(parsedargs.dockername))
    build_dir = os.path.abspath("{}/build/{}".format(script_root, parsedargs.dockername))
    build_docker_dir(parsedargs.dockername, build_dir)

if __name__ == '__main__':
    sys.exit(dmake_main(*sys.argv))
