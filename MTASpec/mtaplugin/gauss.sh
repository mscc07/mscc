#! /bin/csh

g16root="/home/$USER/g16"
GAUSS_SCRDIR="/home/$USER/scratch"
export g016root GAUSS_SCRDIR
. $g16root/g16/bsd/g16.profile

g16 $1 $2
