#!/bin/bash

FILE=vocabulink--$(date +%Y-%m-%d).sql.gpg
pg_dump -U postgres -h localhost --create vocabulink | gpg2 -e -r "chris@forno.us" > $FILE
s3cmd put $FILE s3://vocabulink.com-archive/
rm $FILE