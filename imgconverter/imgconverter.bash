#! /usr/bin/env bash

## 
## !!! ИСПОЛЬЗУЕТСЯ КАК ВРЕМЕННОЕ РЕШЕНИЕ
##
## Создает тубнейл заданного размера для файла
##	и изменяет размер исходного файла.
##
## ТРЕБУЕТСЯ:
##	* bash
##	* nconvert
##
## ИСПОЛЬЗОВАНИЕ:
##	imgconverter /path/to/nconvert /path/to/picture
##

executive=$1
input=$2

# Параметры
# -------------------------
target_thumb_width=210
target_thumb_height=90
target_width=240
# -------------------------

# узнаем директорию файла
cur_dirname=$(dirname "$input")
ret_CD=$?

# задаем имя временного файла (на всякий случай уникальное)
tmp_filename=$(basename "$input")
tmp_filename=/tmp/crop_${tmp_filename%.*}_$RANDOM
ret_TE=$?

# задаем имя файла тубнейла
thumb_filename=$input\_L
ret_TF=$?

cp "$input" "$thumb_filename"
ret_CP=$?

# узнаем статус код.
echo -n $[$ret_CD + $ret_TE +  $ret_TF + $ret_CP ]

