#!/usr/bin/env python

from __future__ import print_function
from __future__ import with_statement
from string import Template
import os
import sys
import argparse

scriptpath = os.path.realpath(__file__)
scriptroot = os.path.dirname(scriptpath)
scriptname = os.path.basename(scriptpath)
pki = "{}/pki".format(scriptroot)

client_ovpn_template = """
client
dev tun
remote vpn.younix.us 1194 udp
nobind
persist-key
persist-tun
comp-lzo
remote-cert-tls server
redirect-gateway def1
key-direction 1

<key>
$client_key
</key>
<cert>
$client_crt
</cert>
<ca>
$ca_crt
</ca>
<dh>
$dh_pem
</dh>
<tls-auth>
$ta_key
</tls-auth>
"""

def catfile(filename):
    with open(filename) as f:
        contents = f.read()
    return contents

def main(*args):
    argparser = argparse.ArgumentParser(
        description='Pack VPN client configuration.')
    argparser.add_argument(
        '--force', '-f', action='store_true', 
        help='If the output file exists, delete it')
    argparser.add_argument(
        '--output', '-o', default='{}/vpn.ovpn'.format(scriptroot),
        help='The output file')
    parsedargs = argparser.parse_args()

    if os.path.exists(parsedargs.output) and not parsedargs.force: 
        raise Exception("Output file exists at {}".format(parsedargs.output))
    
    t = Template(client_ovpn_template)
    client_ovpn = t.substitute(
        client_key = catfile('{}/private/genericclient.key'.format(pki)),
        client_crt = catfile('{}/issued/genericclient.crt'.format(pki)),
        ca_crt = catfile('{}/ca.crt'.format(pki)),
        dh_pem = catfile('{}/private/dh.pem'.format(pki)),
        ta_key = catfile('{}/private/ta.key'.format(pki)))

    with open(parsedargs.output, "w") as f:
        f.write(client_ovpn)

if __name__ == '__main__':
    sys.exit(main(*sys.argv))
