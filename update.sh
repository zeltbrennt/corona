#!/bin/bash

cd /home/pi/corona
/usr/bin/git checkout master
/usr/bin/git pull origin master

/usr/bin/Rscript /home/pi/corona/infected.R

/usr/bin/git add -A
/usr/bin/git commit -m "auto update"
/usr/bin/git push origin master
