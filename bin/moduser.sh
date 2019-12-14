curl -v -X PUT -d 'user=\'`echo "$1" | openssl enc -base64 -d`\'&password=\'`echo "$2" | openssl enc -base64 -d`\''  http://localhost:8080/admin/moduser
