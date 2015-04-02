#!/bin/bash

TIME="$1 year ago"


IFS=$'\n'
echo "Load data"
DATA=(`git log --since="$TIME" --until="now" --no-merges --format='%ct,%an,%ae'`)
echo "Load add/remove lines"
LINES=(`git log --since="$TIME" --until="now" --no-merges  --pretty=tformat: --shortstat | gawk '(NF > 0){ printf ",%s,%s\n",$1,$4 }' -`)
echo "Load word"
WORD=(`git log --since="$TIME" --until="now" --no-merges --format=',%s'`)

echo "Create .csv"
touch gitlogy.csv

i=0
while [ $i -lt ${#DATA[@]} ]; do
    echo "${DATA[$i]}${LINES[$i]}${WORD[$i]}" >> gitlogy.csv
    i=$[i + 1]
done

echo "Parse message"
sed -i -e "s/\ /,/g" gitlogy.csv
