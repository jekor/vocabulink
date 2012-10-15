#!/bin/bash

FILE=vocabulink--$(date +%Y-%m-%d).sql.gpg
pg_dump -U postgres -h localhost --create vocabulink | gpg -e -r "chris@forno.us" --trust-model always > $FILE
s3cmd put $FILE s3://vocabulink.com-archive/sql/
rm $FILE
