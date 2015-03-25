#!/bin/sh

g="0.85"
taus="0.1 0.2 0.5 1 2 5 10 20 50 100"

for sza in 0 30 60
do
	for tau in $taus
	do
		echo "tau=$tau, sza=$sza"
		fn="./$g-$sza-$tau.log"
		./hopr_t2 $g $tau $sza $1 > $fn &
	done
done
