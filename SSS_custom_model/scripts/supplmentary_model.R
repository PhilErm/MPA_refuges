# Script details ####

# This script contains the standalone "supplementary model". It produces animated demonstrations of the various
# outcomes associated with different parameters / processes / MPA configurations.

# To use the script:
# 1. Set working directory to the script location.
# 2. Uncomment the scenario parameter block of interest. That is, one of the parameter blocks in the 
#    "Scenarios" code section. Only one can be done at a time.
# 3. Run all lines of code. Figures and animations will be deposited into "../figs/animations".

# Load packages ####

library(data.table)
library(tidyverse)
library(latex2exp)
library(ggh4x)
library(ggview)
library(ggtext)
library(ggrepel)
library(gganimate)
library(gifski)

# Functions ####

# Growth
growth.function <- function(n, r, k){
  n <- n + n*r*(1-(n/k))
  return(n)
}

# Catch
catch.function <- function(bioms, harv){
  loop.ind <- 1
  while(0 < harv){ # Do this so long as there is harvest left to be taken
    if(length(bioms)>1) { # If there is more than one box to be fished
      high <- max(bioms) # Look for the box with the highest population size
      sec.high <- sort(bioms,partial=length(bioms)-loop.ind)[length(bioms)-loop.ind] # Look for the box with the second highest population size
    } else if(length(bioms)==1) { # If there is just one box to be fished
      bioms <- bioms-harv # Fish it
      harv <- harv-harv # Update the amount of catch that needs to be taken (will be 0 since it is all being taken instantly from the one box)
      break # Stop fishing
    } else if(length(bioms)==0) { # If there are no boxes to be fished (full sparing)
      bioms <- bioms
      harv <- harv
      break # Stop fishing
    }
    sec.high <- sort(bioms,partial=length(bioms)-loop.ind)[length(bioms)-loop.ind] # Likely redundant repeat of earlier line. May remove after testing
    harv.diff <- high-sec.high # Find the difference in population size between the largest and second largest box
    if(harv.diff*loop.ind <= harv){ # If there is more harvest left than the amount about to be taken from the largest box
      index <- which(bioms==high) # Find the index of the largest box
      bioms[index] <- bioms[index]-harv.diff # Harvest it
      harv <- harv-(harv.diff*loop.ind) # Update the total harvest that remains to be taken
      bioms
      harv 
      if(var(bioms) != 0){ # If boxes still don't all have identical biomass
        loop.ind <- loop.ind+1 # Increase the loop by one and start from the top
      } else { # If boxes do all have identical biomass
        bioms <- bioms-(harv/length(bioms)) # Take the remaining harvest evenly from the boxes
        harv <- harv-harv # Update harvest to say that there is none left to take
        bioms
        harv}
    } else { # If less harvest needs to be taken than the difference between the two highest boxes (or collections of boxes)
      last.catch <- harv/loop.ind # Divide harvest by the amount of boxes it's to be taken from
      index <- which(bioms==high) # Find index of boxes about to be harvested
      bioms[index] <- bioms[index]-last.catch # Harvest boxes
      harv <- harv-last.catch*loop.ind # Update harvest to 0
      bioms
      harv
    }
  }
  harv
  bioms
}

# Habitat damage
# Recovery
multip.recovery <- function(prist.K, curr.K, dep.K, n.trawls, rep.K, time){
  gain <- ((prist.K-curr.K)*rep.K)
  return(gain)
}

# Depletion
multip.depletion <- function(prist.K, curr.K, dep.K, n.trawls, rep.K, time){
  loss <- (curr.K-(curr.K*(1-dep.K)^n.trawls))
  return(loss)
}

# Recovery + depletion
multip.model <- function(prist.K, curr.K, dep.K, n.trawls, rep.K, time){
  K.after.gain <- curr.K + multip.recovery(prist.K, curr.K, dep.K, n.trawls, rep.K, time)
  final.K <- K.after.gain - multip.depletion(prist.K, K.after.gain, dep.K, n.trawls, rep.K, time)
  return(final.K)
}

# Migration
migration.function <- function(n, k, m){
  migration.change <- vector()
  for(ii in 1:length(n)){
    sum.vect <- vector()
    for(jj in 1:length(n)){
      sum.vect[jj] <- n[jj]
    }
    migration.change[ii] <- ((k[ii]/sum(k))*(sum(sum.vect)-sum.vect[ii]))-(1-((k[ii])/sum(k)))*n[ii]
  }
  migration.final <- m*migration.change
  n <- n + migration.final
  return(n)
}


# Scenarios ####

## No habitat damage ####

# # Case 1: Same parameters, full overlap, no MPAs.
# case <- 1 # S22
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 2: Same parameters, full overlap, 1 MPA.
# case <- 2 #S23
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 3: Same parameters, full overlap, 2 MPAs.
# case <- 3 #S24
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,0,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 4: Weak non-target, full overlap, no MPAs.
# case <- 4 #S25
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818/5 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 5: Weak non-target, full overlap, 1 MPA.
# case <- 5 #S26
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818/5 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 6: Weak non-target, full overlap, 2 MPAs.
# case <- 6 #S27
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818/5 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,0,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 7: Same parameters, partial overlap, no MPAs.
# case <- 7 #S28
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 8: Same parameters, partial overlap, 1 smart MPA.
# case <- 8 #S29
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 9: Same parameters, partial overlap, 1 silly MPA.
# case <- 9 #S30
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(0,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 10: Weak non-target, partial overlap, no MPAs.
# case <- 10 #S31
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818/20 # Non-targeted r
# non.targeted.q <- 0.109*5 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,0,0,1,1,1,1,1,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 11: Weak non-target, partial overlap, 1 smart MPA.
# case <- 11 #S32
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818/20 # Non-targeted r
# non.targeted.q <- 0.109*5 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,0,0,1,1,1,1,1,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

## Habitat damage ####

# # Case 13: Same parameters, full overlap, no MPAs, NT damage = on.
# case <- 13 #S33
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 14: Same parameters, full overlap, 1 MPA, NT damage = on.
# case <- 14 #S34
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 15: Same parameters, full overlap, 2 MPAs, NT damage = on.
# case <- 15 #S35
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,0,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 16: Weak non-target, partial overlap, no MPA, NT damage = on.
# case <- 16 #S36
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 17: Weak non-target, partial overlap, 1 MPA, NT damage = on.
# case <- 17 #S37
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 18: Weak non-target, partial overlap, 1 MPA, NT damage = on.
# case <- 18 #S38
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(0,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 19: Same parameters, full overlap, no MPAs, ALL damage = on.
# case <- 19 #S39
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 20: Same parameters, full overlap, 1 MPA, ALL damage = on.
# case <- 20 #S40
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 21: Same parameters, full overlap, 2 MPAs, ALL damage = on.
# case <- 21 #S41
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,0,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 22: Weak non-target, full overlap, no MPAs, ALL damage = on.
# case <- 22 #S42
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818/10 # Non-targeted r
# non.targeted.q <- 0.109*2 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 23: Weak non-target, full overlap, 2 MPAs, ALL damage = on.
# case <- 23 #S43
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818/10 # Non-targeted r
# non.targeted.q <- 0.109*2 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,0,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 24: Weak non-target, partial overlap, 0 MPAs, ALL damage = on.
# case <- 24 #S44
# n.time <- 300 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818/10 # Non-targeted r
# non.targeted.q <- 0.109*2 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# # Case 25: Weak non-target, partial overlap, 1 smart MPA, ALL damage = on.
# case <- 25 #S45
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818/10 # Non-targeted r
# non.targeted.q <- 0.109*2 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in

# # Case 26: Weak non-target, partial overlap, 1 silly MPA, ALL damage = on.
# case <- 26 #S46
# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0.2 # Targeted habitat recovery
# targeted.habitat.q <- 0.2 # Targeted habitat damage
# non.targeted.r <- 0.818/10 # Non-targeted r
# non.targeted.q <- 0.109*2 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(0,0,1,1,1,1,1,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(0,1,1,1,1,1,1,1,1,1) # Cells to allow fishing in

# A blank scenario for testing.

# n.time <- 150 # Length of simulation
# targeted.r <- 0.818 # Targeted r
# targeted.q <- 0.109 # Targeted q
# targeted.m <- 0.3 # Targeted m
# targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Targeted k in each patch
# targeted.habitat.r <- 0 # Targeted habitat recovery
# targeted.habitat.q <- 0 # Targeted habitat damage
# non.targeted.r <- 0.818 # Non-targeted r
# non.targeted.q <- 0.109 # Non-targeted q
# non.targeted.m <- 0.3 # Non-targeted m
# non.targeted.k <- c(1,1,1,1,1,0,0,0,0,0) # Non-targeted k in each patch
# non.targeted.habitat.r <- 0.2 # Non-targeted habitat recovery
# non.targeted.habitat.q <- 0.2 # Non-targeted habitat damage
# fishable <- c(1,1,1,1,0,1,1,1,1,1) # Cells to allow fishing in


# Other parameters ####

# Catch

targeted.MSY <- targeted.r*sum(targeted.k)/4
catch.constraint <- targeted.MSY # The starting catch constraint in the numerical MSY searcher

# Numerical MSY calculator to set final catch constraint

MSY.search.iter <- 1
repeat{
  # Generate data structure
  n.vect <- vector(length = n.time)
  n.vect[1] <- sum(targeted.k)
  k.vect <- vector(length = n.time)
  k.vect[1] <- sum(targeted.k)
  e.vect <- vector(length = n.time)
  e.vect[1] <- 0
  for(i in 1:(n.time-1)){
    # Growth and catch
    (whole.pop <- n.vect[i])
    (whole.k <- k.vect[i])
    (n.vect[i+1] <- growth.function(n = n.vect[i],
                                    k = k.vect[i],
                                    r = targeted.r) - catch.constraint)
    (n.vect[is.na(n.vect)] <- 0)
    (n.vect[n.vect < 0] <- 0)
    
    # Effort
    (e.vect[i+1] <- catch.constraint/(targeted.q*n.vect[i+1]))
    
    # Habitat
    (k.vect[i+1] <- multip.model(prist.K = k.vect[1],
                                      curr.K = k.vect[i],
                                      dep.K = targeted.habitat.q,
                                      n.trawls = e.vect[i+1],
                                      rep.K = targeted.habitat.r))

    # Restore negative Ks to 0 (populations can't be negative)
    (k.vect[is.na(k.vect)] <- 0)
    (k.vect[k.vect < 0] <- 0)
  }
  if(n.vect[n.time-1] > 0){
    break
  } else {
    (catch.constraint <- catch.constraint*0.99)
    (MSY.search.iter <- MSY.search.iter + 1)
    if(MSY.search.iter == 500){
      (catch.constraint <- targ.species.MSY)
      print("Took too long to find MSY, defaulting to standard calculation.")
      break}
  }
}
(targeted.MSY <- catch.constraint)
(MSY.search.iter)

targeted.catch <- targeted.MSY*0.67 # Catch of targeted species as a fraction of the numerically obtaind MSY

# Prepare simulation ####

# Targeted species
patch <- 1:10 # Number of patches
species <- "targeted" # Target or non-target species
targeted.n <- targeted.k # Starting population
fishable.n <- targeted.n * fishable # Fishable population in each patch
effort <- c(0,0,0,0,0,0,0,0,0,0) # Starting effort
time <- 1 # Starting time
n.before <- targeted.n # Starting population
catch.extracted <- 0 # Starting catch

targeted.species.df <- cbind.data.frame(
  patch,
  species,
  k = targeted.k,
  prist.k = targeted.k,
  n = targeted.n,
  fishable,
  fishable.n,
  effort,
  time,
  n.before,
  catch.extracted
)

# Non-targeted species
patch <- 1:10
species <- "non.targeted"
non.targeted.n <- non.targeted.k
fishable.n <- NA
effort <- c(0,0,0,0,0,0,0,0,0,0)
time <- 1
n.before <- NA
catch.extracted <- NA

non.targeted.species.df <- cbind.data.frame(
  patch,
  species,
  k = non.targeted.k,
  prist.k = non.targeted.k,
  n = non.targeted.n,
  fishable,
  fishable.n,
  effort,
  time,
  n.before,
  catch.extracted
)

# Simulation ####

# Targeted species
targeted.species.save <- data.frame()
for(i in (1:n.time)-1){
  ## Save previous time
  targeted.species.save <- rbind.data.frame(targeted.species.save, targeted.species.df)
  
  ## Time
  targeted.species.df$time <- targeted.species.df$time+1
  
  ## Growth
  targeted.species.df$n <- growth.function(n = targeted.species.df$n,
                                           k = targeted.species.df$k,
                                           r = targeted.r)
  targeted.species.df$n[is.na(targeted.species.df$n)] <- 0

  ## Catch
  targeted.species.df$fishable.n <- targeted.species.df$n *
    targeted.species.df$fishable
  targeted.species.df$n.before <- targeted.species.df$n
  targeted.species.df$fishable.n <- catch.function(bioms = targeted.species.df$fishable.n,
                                          harv = targeted.catch)
  targeted.species.df$n <- ifelse(targeted.species.df$fishable.n != 0, targeted.species.df$fishable.n, targeted.species.df$n)
  targeted.species.df$n[is.na(targeted.species.df$n)] <- 0

  targeted.species.df$catch.extracted <- targeted.species.df$n.before-targeted.species.df$n
  targeted.species.df$effort <- targeted.species.df$catch.extracted/(targeted.q*targeted.species.df$n.before)
  targeted.species.df$effort[is.na(targeted.species.df$effort)] <- 0

  ## Habitat damage
  targeted.species.df$k <- multip.model(prist.K = targeted.species.df$prist.k, 
                                        curr.K = targeted.species.df$k, 
                                        dep.K = targeted.habitat.q,
                                        n.trawls = targeted.species.df$effort, 
                                        rep.K = targeted.habitat.r)
  targeted.species.df$k[is.na(targeted.species.df$k)] <- 0
  targeted.species.df$k[targeted.species.df$k < 0] <- 0

  ## Migration
  targeted.species.df$n <- migration.function(n = targeted.species.df$n,
                                              k = targeted.species.df$k,
                                              m = targeted.m)
  targeted.species.df$n[is.na(targeted.species.df$n)] <- 0
}

# Non-targeted species
non.targeted.species.save <- data.frame()
for(i in (1:n.time)-1){
  ## Save previous time
  non.targeted.species.save <- rbind.data.frame(non.targeted.species.save, non.targeted.species.df)
  
  ## Time
  non.targeted.species.df$time <- non.targeted.species.df$time+1
  
  ## Growth
  non.targeted.species.df$n <- growth.function(n = non.targeted.species.df$n,
                                           k = non.targeted.species.df$k,
                                           r = non.targeted.r)
  non.targeted.species.df$n[is.na(non.targeted.species.df$n)] <- 0

  ## Catch
  # Collect appropriate effort
  current.time <- unique(non.targeted.species.df$time)
  pulled.effort <- targeted.species.save %>%
    filter(time == current.time) %>% 
    pull(effort)
  if(length(pulled.effort) > 0){
    non.targeted.species.df$effort <- pulled.effort
  } else {
    non.targeted.species.df <- NULL
  }
  
  # Subtract appropriate catch
  non.targeted.species.df$n <- non.targeted.species.df$n-(non.targeted.q*
                                                            non.targeted.species.df$effort*
                                                          non.targeted.species.df$n)
  non.targeted.species.df$n
  
  ## Habitat damage
  non.targeted.species.df$k <- multip.model(prist.K = non.targeted.species.df$prist.k, 
                                        curr.K = non.targeted.species.df$k, 
                                        dep.K = non.targeted.habitat.q,
                                        n.trawls = non.targeted.species.df$effort, 
                                        rep.K = non.targeted.habitat.r)
  non.targeted.species.df$k[is.na(non.targeted.species.df$k)] <- 0
  non.targeted.species.df$k[non.targeted.species.df$k < 0] <- 0
  
  ## Migration
  non.targeted.species.df$n <- migration.function(n = non.targeted.species.df$n,
                                              k = non.targeted.species.df$k,
                                              m = non.targeted.m)
  non.targeted.species.df$n[is.na(non.targeted.species.df$n)] <- 0
}

# Process results ####

species.save <- rbind.data.frame(targeted.species.save, non.targeted.species.save)
species.save$fishable <- as.factor(species.save$fishable)
species.save$fishable <- recode(species.save$fishable, "0" = "MPA", "1" = "Open")
species.save$fishable <- factor(species.save$fishable, levels = c("Open", "MPA"))
species.save$fishable <- as.character(species.save$fishable)

species.save$fishable[species.save$species == "targeted" & species.save$prist.k == 0] <- "Not fished"
species.save$fishable[species.save$patch %in% unique(species.save$patch[species.save$species == "targeted" & species.save$prist.k == 0])]  <- "Not fished"

species.save.summary <- species.save %>% 
  group_by(time) %>% 
  filter(species == "non.targeted") %>% 
  summarise(non.targeted.average = mean(n)/mean(prist.k),
            non.targeted.average.round = round(mean(n)/mean(prist.k), digits = 2))

species.habitat.summary <- species.save %>% 
  group_by(time) %>% 
  filter(species == "non.targeted") %>% 
  summarise(non.targeted.average = mean(k)/mean(prist.k),
            non.targeted.average.round = round(mean(k)/mean(prist.k), digits = 2))

# Plot results ####

plot <- ggplot(species.save, aes(x = as.factor(patch), y = n, fill = species, colour = fishable, label = as.factor(fishable))) +
  geom_col(position = "dodge", size = 1.25) +
  geom_text(show.legend = FALSE, size = 5, position = position_dodge(width = .9), hjust = 1.1, angle = 90) +
  scale_y_continuous(name = "Abundance (*n*)", limits = c(0,1.1), expand = expansion(mult = c(0, 0.01)),
                     breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_discrete(name = "Patch") +
  scale_fill_viridis_d(name = "Species", label = c("Non-target", "Target")) +
  scale_colour_manual(name = "MPA present?", values = c("Open" = "#e41a1c", "MPA" = "#33a02c", "Not fished" = "#a6cee3"), guide="none") +
  geom_hline(data = species.save.summary,
             aes(yintercept = non.targeted.average),
             linetype = "dashed") +
  geom_richtext(data = species.save.summary,
            aes(x = 9.8, 
                y = non.targeted.average,
                label = paste("Non-targ. mean<br>*n* = ", scales::number(non.targeted.average, acc = 0.01))),
            colour = "#440154FF",
            fill = NA, 
            label.color = NA,
            vjust = 0,
            size = 4) +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        text = element_text(size = 18))

static <- ggplot(species.save %>% filter(time == max(time)), aes(x = as.factor(patch), y = n, fill = species, colour = fishable, label = as.factor(fishable))) +
  geom_col(position = "dodge", size = 1.25) +
  geom_text(show.legend = FALSE, size = 5, position = position_dodge(width = .9), hjust = 1.1, angle = 90) +
  scale_y_continuous(name = "Abundance (*n*)", limits = c(0,1.1), expand = expansion(mult = c(0, 0.01)),
                     breaks = c(0,0.25,0.5,0.75,1)) +
  scale_x_discrete(name = "Patch") +
  scale_fill_viridis_d(name = "Species", label = c("Non-target", "Target")) +
  scale_colour_manual(name = "MPA present?", values = c("Open" = "#e41a1c", "MPA" = "#33a02c", "Not fished" = "#a6cee3"), guide="none") +
  geom_hline(data = species.save.summary %>% filter(time == max(time)),
             aes(yintercept = non.targeted.average),
             linetype = "dashed") +
  geom_richtext(data = species.save.summary %>% filter(time == max(time)),
                aes(x = 9.8, 
                    y = non.targeted.average,
                    label = paste("Non-targ. mean<br>*n* = ", scales::number(non.targeted.average, acc = 0.01))),
                colour = "#440154FF",
                fill = NA, 
                label.color = NA,
                vjust = 0,
                size = 4) +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        text = element_text(size = 18))
static

# Saving static plot
case.path <- paste("../figs/animations", case, sep = "/")
ggsave(paste(case.path, "pdf", sep = "."), static, width = 10, height = 5, units = "in")

# Saving animation
animation <- plot +
  transition_time(as.integer(time)) +
  labs(title = "Time: {frame_time}") +
  ease_aes(default = "linear")

fps <- 30
duration <- 7
animate(animation, fps = fps, duration = duration, start_pause = fps*0.5, end_pause = fps*1, height = 5, width = 10, res = 225, units = "in")
anim_save(paste(case.path, "gif", sep = "."), animation = last_animation(), path = NULL)
