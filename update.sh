#!/bin/bash

git checkout master

for f in *.csv 
do
  mv "$f" "archive/$f"
done
Rscript infected.R

git add -A
git commit -m "update data"
git push origin master
