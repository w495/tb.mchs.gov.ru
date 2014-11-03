#! /usr/bin/env bash

input=$1

Height=`nconvert -info $input | grep Height | awk 'BEGIN { FS = ": " } ; { print $2 }'` # ~3
ret_H=$?

Width=`	nconvert -info $input | grep Width 	| awk 'BEGIN { FS = ": " } ; { print $2 }'` # ~7
ret_W=$?

NewH=$[$Width * 3 / 7 ]
ret_N=$?

Xoffset=$[($Height - $NewH) / 2]
ret_X=$?

#	echo Width  = $Width
#	echo Height = $Height
#	echo NewH = $NewH
#	echo Xoffset = $Xoffset

nconvert -quiet -crop 0 $Xoffset $Width $NewH -o /tmp/crop_$input $input
ret_C=$?
nconvert -quiet -resize 210 90 -o thumb_$input /tmp/crop_$input
ret_R=$?
rm -f /tmp/crop_$input
ret_T=$?
echo -n $[$ret_H + $ret_W + $ret_N + $ret_X + $ret_C + $ret_R]
#echo -n thumb_$input
