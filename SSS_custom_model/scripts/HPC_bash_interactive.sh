#!/bin/bash

sintr -p icelake -N2 -n2 -t 1:0:0 --qos=INTR

# module load R/4.2.0-icelake
# Rscript analysis_figures.R