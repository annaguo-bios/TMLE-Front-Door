#!/bin/bash

#  "bayes" "dnorm"
ssh_options="-o StrictHostKeyChecking=no"
simnumber="3-misspecification"


for estimand in "ATT"; do
    for M in "binaryM"; do
        for emethod in 'CF' 'Linear' 'SL'; do
            for estimator in "TMLE-est1" "Onestep-est1"; do
                scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/sim${simnumber}/${M}/${emethod}/${estimator}/${estimand}_result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim${simnumber}/${M}/${emethod}/${estimator}/${estimand}_result.Rdata"
            done
        done    
    done
done
