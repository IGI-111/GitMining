#!/bin/bash
IFS=$'\n'
DATA=(`git log --since="1 week ago" --until="now" --no-merges --format='"%H","%ct","%an","%ae","%s",'`)
LINES=(`git log --since="1 week ago" --until="now" --no-merges  --pretty=tformat: --shortstat | gawk '(NF > 0){ printf "%s,%s\n",$1,$4 }' -`)

i=0
while [ $i -lt ${#DATA[@]} ]; do
    echo "${DATA[$i]}${LINES[$i]}"
    i=$[i + 1]
done
