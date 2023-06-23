# Scripts for *Do natural refuges make marine protected areas redundant for biodiversity conservation and fisheries management?*

## About
Produces the various analyses and figures associated with "Do natural refuges make marine protected areas redundant for biodiversity conservation and fisheries management?".

## How to use

These scripts are designed to be used with a high performance computer running the Slurm Workload Manager. They were specifically designed for the University of Cambridge's Icelake nodes. The instructions below are designed for replication in that environment, but with minor changes the analysis should be reproducible on any system. Notes on how to make those changes depending on the system you are on are included at the bottom of this readme. If you need help with any aspect of reproducibility with the code please get in touch and I'll be happy to help.

### To replicate the figures in the manuscript using the University of Cambridge HPC cluster

The instructions below assume you have connected to the HPC and moved the repository there.

1. Navigate your terminal to "scripts":
`cd scripts`
2. Send the main and sensitivity analyses jobs to the HPC:
`bash HPC_bash_sens`
3. Once all jobs are complete, run "analysis_sens_summary_figures.R" in R to generate the figures used in the manuscript. The instructions for running the script are included in the script itself.
4. To generate the animations produced by the supplementary model, run "supplementary_model.R" in R. The instructions for running the script are included in the script itself.

### Notes on reproducibility outside of the University of Cambridge HPC cluster

Before considering reproducibility outside of the University of Cambridge HPC cluster it is useful to have a sense of the key scripts and how they interact in the workflow.

Each level of overlap for each scenario has 3 unique scripts. E.g. for a particular scenario:
`facil_log_mig_adam_ovr02_fishfav_sens_catch05.R`
`HPC_log_mig_adam_ovr02_fishfav_sens_catch05.slurm`
`pars_log_mig_adam_ovr02_fishfav_sens_catch05.R`

To manually run this particular simulation, one would need to send `HPC_log_mig_adam_ovr02_fishfav_sens_catch05.slurm` to a computer with the Slurm Workload Manager. This would then run `facil_log_mig_adam_ovr02_fishfav_sens_catch05.R` using R on that computer, which itself then calls the scenario's unique parameter file `pars_log_mig_adam_ovr02_fishfav_sens_catch05.R` and the master scripts `species_parameters.R`, `functions.R`, `analysis.R`, and `analysis_figures.R` used in all simulations.

That means, if you need to run the script on a computer without Slurm, all you need to do is run `facil_log_mig_adam_ovr02_fishfav_sens_catch05.R` in R and the eventual output should be identical. To easily do this across all simulation scripts, a simple bash script that sends all of the "facil" scripts to R should be sufficient. If you need help with such a script or any other aspect of reproducibility with the code please get in touch and I'll be happy to help.

To easily run the script on another HPC, the Slurm scripts will need to be modified to accomodate the particular requirements of the HPC in question. This will change depending on the HPC, but will probably involve nominating your chosen partition, making sure job clock times are suitable etc.

### If not rerunning simulations

The repository already contains the processed data files used to generate the final figures included in the manuscript. As such, if you don't want to rerun the simulations themselves, after cloning the repository you can generate all figures if you:

1. Run "analysis_sens_summary_figures.R" in R to generate the figures used in the manuscript. The instructions for running the script are included in the script itself.
2. Run "supplementary_model.R" in R to generate the animations produced by the supplementary model. The instructions for running the script are included in the script itself.
