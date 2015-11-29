FROM ubuntu
MAINTAINER Micah R Ledbetter <micah@micahrl.com

ENV tshockurl https://github.com/NyxStudios/TShock/releases/download/v4.3.11/tshock_release.zip

ADD ./bin /usr/local/bin

### START Marionettist Docker configuration
RUN \
    apt-get -y update && \
    apt-get -y install \
        python \
        ansible \
ADD ../../ansible/roles/marionettist-users /marionettist/roles/marionettist-users
ADD ../../ansible/marionettist-docker-container.yml /marionettist/marionettist-docker-container.yml
ADD ../../ansible/inventory/marionettist-docker-container /etc/ansible/hosts
RUN ansible-playbook /marionettist/marionettist-docker-container.yml --connection=local
### END Marionettist Docker configuration

RUN \
    apt-get -y update && \
    apt-get -y install \
        wget \
        unzip \
        mono-complete
RUN \
    mkdir /terraria && \
    chown marionettist-1:marionettist-1 /terraria
#VOLUME /terraria

USER marionettist-1

RUN \
    mkdir /terraria/tshock && \
    wget "${tshockurl}" -O /terraria/TShock.zip && \
    unzip /terraria/TShock.zip -d /terraria/tshock
ENTRYPOINT ["mono","/terraria/tshock/TerrariaServer.exe","-configpath","/terraria/tshock/config.json"]

EXPOSE 7777/tcp
