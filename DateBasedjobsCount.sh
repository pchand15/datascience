#!/bin/bash

#date

#read mon day < <(date -d "2 days" "+%m %d")
#read mon day year < <(date -d "2 days" "+%b %_d %Y")
#testdate="$mon $day $year"
#echo "$testdate"


read mon day year < <(date -d "0 days" "+%b %_d %Y")
testdate="$mon $day"
echo "Current date"
echo $testdate

ls -lrt .|grep "$testdate" 
ls -lrt .|grep "$testdate" | wc -l
