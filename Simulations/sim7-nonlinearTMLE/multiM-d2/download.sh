#!/bin/bash

#  "bayes" "dnorm"
ssh_options="-o StrictHostKeyChecking=no"
simnumber="7-nonlinearTMLE"


for estimand in "ATT_" ""; do
    for M in "multiM-d2"; do
        for estimator in "TMLE-est2-dnorm" "Onestep-est2-dnorm" "TMLE-est2a" "Onestep-est2a" "TMLE-est2b" "Onestep-est2b"; do
            scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/sim${simnumber}/${M}/${estimator}/${estimand}result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim${simnumber}/${M}/${estimator}/${estimand}result.Rdata"
        done
    done
done
