FROM httpd:2.4-alpine
LABEL maintainer "me@micahrl.com"
COPY ["index.html", "/usr/local/apache2/htdocs"]
COPY ["httpd.conf", "/usr/local/apache2/conf/httpd.conf"]
EXPOSE 80
CMD ["/usr/bin/python3", "/srv/biblemunger/__main__.py", "--initialize", "init"]
