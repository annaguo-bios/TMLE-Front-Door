This folder contains code for implementing the simulation studies discussed in the paper. The four folders, namely sim1-consistency, sim2-weak-overlap, sim3-misspecification, and sim4-crossfitting, correspond to the four subsections on simulation in the paper. 

###########
FOLDERS
###########

-- binaryM is the univariate binary mediator folder.

-- continuousM is the univariate continuous mediator folder.

-- multiM-d2 is the bivariate mediator folder.

-- multiM-d4 is the four-dimensional mediators folder.

-- DGPs folders contain the data generating code and code for empirically computing the true ACE and its variance. Execute lines in the compute_truth.txt for computing the truth.

-- CF is the Cross Fitting folder.

-- Linear is the estimation with linear regressions folder.

-- SL is the Super Learner folder.

-- RF is the Random Forest folder.

-- Onestep-est1 is the estimation using estimator \psi_1^+ folder.

-- TMLE-est1 is the estimation using estimator \psi_1 folder.

-- Onestep-est2a is the estimation using estimator \psi_{2a}^+ folder.

-- TMLE-est2a is the estimation using estimator \psi_{2a} folder.

-- Onestep-est2b is the estimation using estimator \psi_{2b}^+ folder.

-- TMLE-est2b is the estimation using estimator \psi_{2b} folder.

-- output is the folder that contains estimation results.

###########
FILES
###########

-- joblist*.txt: This is the job file for simulation. Each line corresponding to one simulation. It is recommended to execute the job lists using parallel computing.

-- write_job.R: This is the R script for producing the joblist*.txt files.

-- main.R: Each line in the job list calls this main.R function to perform TMLE and one-step estimation. This file calls the 'fdtmle' package for estimation and save estimation results to the output folders, located under subfolders named after the estimators.

-- organize_onestep.R: This file is used for organizing the output file from one-step estimators. It is called by the organize.txt file within each estimator folder.

-- organize_onestep.R: This file is used for organizing the output file from TMLEs. It is called by the organize.txt file within each estimator folder.

-- organize.txt: This file contains code for summarizing the files in the output folder. Run `bash organize.txt` in terminal to execute.

-- plot.R: This is used for generating plots for sim1-consistency. This file calls plot-sub.R for generating smaller plots.

-- plot-sub.R: This function is called by the plot.R for generating smaller plots.

-- table.R: This is the R script used for generating tables in the paper.




