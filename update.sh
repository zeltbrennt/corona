#!/bin/bash

for f in *infizierte.csv; do
  mv "$f" "archive/$f"
done

Rscript infected.R
#git add -A
#git commit -m "update data"
