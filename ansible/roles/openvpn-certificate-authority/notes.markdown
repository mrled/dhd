Here's what I did bleh

most of the configuration is in the `vars` file, which **sets batch mode** among other things. The following assumes batch mode:

    cd easyrsa
    ./easyrsa init-pki
    ./easyrsa build-ca nopass
    ./easyrsa build-server-full vpnserver nopass
    ./easyrsa build-client-full genericclient nopass
    ./easyrsa gen-dh
    mv ../pki/dh.pem ../pki/private/dh.pem
    openvpn --genkey --secret ../pki/ta.key
