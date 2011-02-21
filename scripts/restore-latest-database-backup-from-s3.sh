#!/bin/bash

echo "Close all programs accessing the vocabulink database."
read
FILE=vocabulink--$(date +%Y-%m-%d).sql.gpg
s3cmd get s3://vocabulink.com-archive/$FILE
echo "DROP DATABASE vocabulink;" | psql -U postgres -h localhost postgres
gpg -d $FILE | psql -U postgres -h localhost postgres
rm $FILE