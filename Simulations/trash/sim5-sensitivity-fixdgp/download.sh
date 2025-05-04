#!/bin/bash

ssh_options="-o StrictHostKeyChecking=no"
simnumber="sim5-sensitivity"

for outcome in "continuousY"; do
    for dgp in "fd_admg1" "nonfd_admg1" "nonfd_admg2" "nonfd_admg3"; do
        for m in "0" "1"; do
            for estimator in "AIPW"; do
                scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/${simnumber}/${outcome}/${dgp}/result_m${m}_${estimator}.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/${simnumber}/${outcome}/${dgp}/result_m${m}_${estimator}.Rdata"
            done
        done
    done
done


