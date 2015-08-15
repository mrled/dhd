# Originally from: https://blog.docker.com/2013/09/docker-joyent-openvpn-bliss/
FROM debian:stable

ADD ./bin /usr/local/sbin

VOLUME /etc/openvpn
RUN useradd --system --shell /bin/bash --home-dir /etc/openvpn --user-group openvpn

RUN apt-get update && apt-get install -y openvpn iptables socat curl sudo
RUN /bin/echo 'openvpn ALL=(ALL) NOPASSWD: /sbin/ip' > /etc/sudoers.d/openvpn

EXPOSE 1194/udp
EXPOSE 1194/tcp
CMD run
