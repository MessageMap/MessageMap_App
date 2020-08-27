export u=`echo -n $1 | openssl enc -base64`
export p=`echo -n $2 | openssl enc -base64`
curl -s -X PUT -d 'user='$u'&password='$p''  http://127.0.0.1:8080/admin/moduser
