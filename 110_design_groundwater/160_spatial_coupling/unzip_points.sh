#!/bin/sh

# This script collects DHMV data (points) to an SQL database.
# DHMV is the "Digital Height/Elevation Model Flanders"
# https://www.vlaanderen.be/digitaal-vlaanderen/onze-diensten-en-platformen/earth-observation-data-science-eodas/het-digitaal-hoogtemodel

# point files are available here:
# https://download.vlaanderen.be/product/56-dhm-vlaanderen-punten
# to be placed in a subfolder named "points".


## delete previous files
# rm ./pointfiles.txt
rm dhmv_points.db

## create empty collection file
# touch ./pointfiles.txt
# echo "x,y,h" >> ./pointfiles.txt # not necessary

## transfer to SQLITE database
sqlite3 dhmv_points.db "CREATE TABLE points (x DECIMAL, y DECIMAL, h DECIMAL);"

# mkdir ./pointfiles
for zipfile in ./points/*.zip;
do
  # file naming
  basepath="${zipfile%.*}"
  basename="${basepath##*/}"
  echo "$basename"

  # unzip
  unzip -o "$zipfile" -d "$basename";

  ## variant 1: move to folder
  # find "./$basename" -type f -name '*.txt' -exec bash -c 'name=$1; mv "$name" "./pointfiles/"' _ {} \;

  ## variant 2: append large text file
  # find "./$basename" -type f -name '*.txt' -exec bash -c 'name=$1; cat "$name" | sed -e "s/\ /,/g" >> "./pointfiles.txt"' _ {} \;

  ## variant 3: direct append sqlite
  find "./$basename" -type f -name '*.txt' -exec bash -c 'name=$1; sed -i "s/\ /,/g" $name && sqlite3 dhmv_points.db ".import --csv $name points"' _ {} \;

  # remove unzipped folder
  rm -rf "$basename"

done


## previous collection attempts
# sed -i "s/\ /,/g" pointfiles.txt

# sqlite3 dhmv_points.db ".import --csv pointfiles.txt points"

# delete the temporary text file
# rm ./pointfiles.txt



# refs:
# https://unix.stackexchange.com/questions/469818/how-can-i-iterate-over-files-in-a-given-directory-and-check-if-file-exist
# https://stackoverflow.com/questions/15317929/load-contents-in-text-files-to-sqlite-table
