#!/bin/bash

COUNT = 0
                  
find ./hask_log -name '*-acc.csv' | sort | while read file
do
	echo $file
	COUNT=$(( COUNT+1 ))

	cp $file $(printf "./acc/%04d-acc.csv" $COUNT)

	# cat "$file"
	#echo -e "= = = = = = = = = = = = =\n"
done
