#!/bin/bash

ssh_options="-o StrictHostKeyChecking=no"
simnumber="sim5-sensitivity"

for outcome in "continuousYZ"; do
    for m in "0" "1"; do
        for estimator in "AIPW" "IPW" "Gcomp" "g-null"; do
            scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/${simnumber}/${outcome}/result_m${m}_${estimator}.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/${simnumber}/${outcome}/result_m${m}_${estimator}.Rdata"
        done
    done
done


