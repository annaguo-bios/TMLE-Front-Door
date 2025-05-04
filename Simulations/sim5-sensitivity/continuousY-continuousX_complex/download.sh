#!/bin/bash

ssh_options="-o StrictHostKeyChecking=no"
simnumber="sim5-sensitivity"
outcome="continuousY-continuousX_complex"

for dgp in "fd_admg1" "fd_admg2" "nonfd_admg1" "nonfd_admg2"; do
    for estimator in "AIPW" ; do
        scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/${simnumber}/${outcome}/${dgp}/result_${estimator}.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/${simnumber}/${outcome}/${dgp}/result_${estimator}.Rdata"
    done
done



