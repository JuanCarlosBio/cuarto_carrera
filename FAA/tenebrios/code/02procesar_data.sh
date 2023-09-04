#!/usr/bin/env bash

echo grupo,semana,masa_corp,d,dx

for row in 2 17 32
do
    for field in 1 7 13 19 25 31
    do
        sed 's/,/./g' tenebrios/data/tene_todos_crudos.csv \
            | sed 's/;/,/g' \
            | sed -n $row,+11p \
            | cut -d "," -f $field-$(($field+4))
    done
done 
