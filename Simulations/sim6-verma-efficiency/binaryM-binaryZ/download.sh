#!/bin/bash

#  "bayes" "dnorm"
ssh_options="-o StrictHostKeyChecking=no"
simnumber="6-verma-efficiency"

for in_input in "binaryM-binaryZ"; do
    for e_method_input in "TMLE" "Onestep"; do
            scp $ssh_options aguo28@clogin01.sph.emory.edu:"/projects/rnabi/annaguo/organized-frontdoor/sim${simnumber}/${in_input}/${e_method_input}/result.Rdata" "/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim${simnumber}/${in_input}/${e_method_input}/result.Rdata"
    done
done
