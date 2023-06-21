# Scripts for *Do habitat refuges make marine protected areas redundant?*

## About
Produces the various analyses and figures associated with "Do habitat refuges make marine protected areas redundant?".

## How to use

These scripts are designed to be used with a high performance computer running the Slurm Workload Manager. They were specifically designed for the University of Cambridge's Icelake nodes. The instructions below are designed for replication in that environment, but with minor changes the analysis should be reproducible on any system. Notes on how to make those changes depending on the system you are on are included at the bottom of this readme.

### To replicate the figures in the manuscript using the University of Cambridge HPC cluster

The instructions below assume you have connected to the HPC and moved the repository there.

1. Navigate your terminal to "scripts"
`cd scripts`
2. Send the main and sensitivity analyses jobs to the HPC
`bash HPC_bash_sens`
3. Once all jobs are complete, run "analysis_sens_summary_figures.R" in R to generate the figures used in the manuscript. The instructions for running the script are included in the script itself.
4. To generate the animations produced by the supplementary model, run "supplementary_model.R" in R. The instructions for running the script are included in the script itself.

## Notes on reproducibility

TBD.
