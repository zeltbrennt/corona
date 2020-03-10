#!/bin/bash

/usr/bin/Rscript /home/pi/corona/infected.R

cd /home/pi/corona
/usr/bin/git add -A
/usr/bin/git commit -m "update data"
/usr/bin/git push origin master
