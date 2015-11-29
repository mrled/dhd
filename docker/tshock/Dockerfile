FROM ubuntu
MAINTAINER Micah R Ledbetter <micah@micahrl.com

ENV tshockurl https://github.com/NyxStudios/TShock/releases/download/v4.3.11/tshock_release.zip

ADD ./bin /usr/local/bin

RUN \
    apt-get -y update && \
    apt-get -y install \
        wget \
        unzip \
        mono-complete
RUN useradd --system --shell /bin/bash --home-dir /terraria --user-group --create-home terraria
VOLUME /terraria

USER terraria

RUN \
    mkdir /terraria/tshock && \
    wget "${tshockurl}" -O /terraria/TShock.zip && \
    unzip /terraria/TShock.zip -d /terraria/tshock
ENTRYPOINT ["mono","/terraria/tshock/TerrariaServer.exe","-configpath","/terraria/tshock/config.json"]

EXPOSE 7777/tcp
