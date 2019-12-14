curl -v -X POST -d 'user=\'`echo "$1" | openssl enc -base64 -d`\'&password=\'`echo "$2" | openssl enc -base64 -d`\''  http://127.0.0.1:8080/admin/adduser
