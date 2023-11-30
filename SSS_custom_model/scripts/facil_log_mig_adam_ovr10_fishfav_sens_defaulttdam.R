# HPC facilitation script

# Save/load packages used ####

# renv::snapshot()
# renv::restore()

# Load packages ####

library(data.table)
library(tidyverse)
library(latex2exp)
library(beepr)
library(progress)
library(ggh4x)
library(foreach)
library(doParallel)
library(doSNOW)
library(profvis)
library(bench)
library(faux)
library(patchwork)
library(ggcorrplot)

# Source and save parameters ####

source("pars_log_mig_adam_ovr10_fishfav_sens_defaulttdam.R") # Change this to custom name

# Save parameters used
par.path <- paste("../logs/simulation_parameters", simulation.name, sep = "/")
dir.create(par.path)
file.copy(from = "pars_log_mig_adam_ovr10_fishfav_sens_defaulttdam.R", 
          to = par.path, overwrite = T)

# Source remaining scripts

source("species_parameters.R")
source("functions.R")
source("analysis.R")
source("analysis_figures.R")