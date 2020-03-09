#!/bin/bash

#git checkout master

Rscript /home/pi/corona/infected.R

cd /home/pi/corona && /usr/bin/git add -A
cd /home/pi/corona && /usr/bin/git commit -m "update data"
cd /home/pi/corona && /usr/bin/git push origin master

