#!/bin/sh

mogrify -trim *;
convert -gravity center -append Spatmod.png Taxmod.png Formatmod.png Reviewmod.png Resultmod.png caption.png workflow.png; 
convert workflow.png -fuzz 2% -transparent white workflow.png
exit
