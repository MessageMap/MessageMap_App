#!/bin/python
import socket
import sys
hostname = socket.gethostname()
if hostname != 'linux-jqn8':
    sys.exit(1)

print(hostname)

#Start server In background
#./local.sh > /tmp/msgmap_$today.log &

#Final Locaiton (Located on WWW Server)
#rsync -a r_msgmap msgmap_`date +%m%d%y`
#tar -cf /tmp/msgmap_`date +%m%d%y`.tar msgmap_`date +%m%d%y`
#scp -i /root/.ssh/id_rsa_transfer  -r /tmp/msgmap_`date +%m%d%y`.tar root@66.42.85.64:~/releases/msgmap_`date +%m%d%y`.tar
#scp -i /root/.ssh/id_rsa_transfer  -r /tmp/msgmap_`date +%m%d%y`.tar root@66.42.85.64:~/releases/msgmap_latest.tar
#rm /tmp/msgmap_`date +%m%d%y`.tar
