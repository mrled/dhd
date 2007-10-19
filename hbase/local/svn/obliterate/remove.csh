#!/bin/csh

cat dhd.dump | svndumpfilter --drop-empty-revs --renumber-revs exclude docs/subset.txt.gpg  doc/subset.txt.gpg hbase/.netrc hbase/dot.offlineimaprc hbase/irssi.config host/andraia/openvpn host/AndrAIa/openvpn host/marajade/openvpn hbase/.gnupg hbase/opt/scpt/hpj-login hbase/opt/scpt/pbp-login hbase/opt/scpt/pbp-login.csh hbase/opt/scpt/hpj-login.csh hbase/opt/scp/typhoon-download.pl andraia/micah/.ssh hbase/opt/bin/pbp-login hbase/opt/bin/hpj-login hbase/opt/scpt/typhoon-download.pl hbase/opt/bin/typhoon-download.pl hproblematic doc/dtbox/neuric-proj docs/dtbox/neuric-proj dtbox/neuric-proj projects > secure.dump


# Some notes:
# the projects directory there isn't important from a security standpoint, but it was an unnecessary commit so...
