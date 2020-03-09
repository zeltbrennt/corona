#!/bin/bash

#git checkout master

Rscript infected.R

git add -A
git commit -m "update data"
git push origin dev
