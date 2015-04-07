#!/bin/bash

if [ -z $1 ] ; then
	echo "NOTICE : phase0.sh NbYear OutFiles"
	exit 1;
else
	if [ -z $2 ] ; then
		echo "NOTICE : phase0.sh NbYear OutFiles"
		exit 1;
	fi
	if  [ $(echo $1 | grep -v [a-Z] | wc -l) -eq 0 ]; then
		echo "Erreur : arg \$1 non entier positif !"
		exit 1
	fi
fi

TIME="$1 year ago"

IFS=$'\n'
echo "Load data"
DATA=(`git log --since="$TIME" --until="now" --no-merges --format='%ct,%an,%ae'`)
echo "Load add/remove lines"
LINES=(`git log --since="$TIME" --until="now" --no-merges  --pretty=tformat: --shortstat | gawk '(NF > 0){ printf ",%s,%s\n",$1,$4 }' -`)
echo "Load word"
WORD=(`git log --since="$TIME" --until="now" --no-merges --format=',%s'`)

echo "Create $2"
touch $2

i=0
while [ $i -lt ${#DATA[@]} ]; do
    echo "${DATA[$i]}${LINES[$i]}${WORD[$i]}" >> $2
    i=$[i + 1]
done

echo "Parse message"
sed -i -e "s/\ /,/g" $2
