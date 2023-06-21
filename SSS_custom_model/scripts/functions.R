# Script details ####

# A repository of functions called in the various model scripts. There are many functions are defined in the other scripts and not in here, so if you are looking for 
# something in particular it is also worth checking "analysis.R", "analysis_figures.R", and "analysis_sens_summary_figures.R".

# This script originated as part of a larger, more flexible modelling framework, and so has many options that aren't relevant to 
# the manuscript. Many of these are experimental or outdated and will not necessarily work as described or expected.
# They are best left alone.

# For generating species distributions ####

# Calculate st. dev. from lower confidence interval
sd.from.lower <- function(mean, low.CI){
  sd <- (log(low.CI)-log(mean))/(-2)
  sd
}

# Calculate st. dev. from upper confidence interval
sd.from.upper <- function(mean, up.CI){
  sd <- (log(up.CI)-log(mean))/(2)
  sd
}

# Name species drawn from parameter distributions
namer <- function(name.vect, n){
  names <- NULL
  for(i in 1:length(name.vect)){
    name.iter <- paste(name.vect[i], sep = " ", 1:n)
    names <- c(names, name.iter)
  }
  names
}

# Draw values from parameter distributions
sampler <- function(mean.vec, sd.vec, n){
  pars <- NULL
  for(i in 1:length(mean.vec)){
    pars.iter <- rlnorm(n, meanlog = mean.vec[i], sdlog = sd.vec[i])
    pars <- c(pars, pars.iter)
  }
  pars
}

# Generate data frame of species with parameters drawn from distributions
builder <- function(n, mean.q, sd.q, mean.r, sd.r, name){
  species <- namer(name.vect = name,
                   n = n)
  q <- sampler(mean.vec = log(mean.q),
                     sd.vec = sd.q,
                     n = n)
  r <- sampler(mean.vec = log(mean.r),
                     sd.vec = sd.r,
                     n = n)
  class <- rep(name, each = n)
  samp.pars <- cbind.data.frame(class, species, q, r)
  samp.pars <- as_tibble(samp.pars)
  samp.pars
}

# For calculating biodiversity ####

# Calculate geometric mean
gm.mean <- function(x){
  prod(x, na.rm = F)^(1/length(x))
}

# # For calculating growth ####
# 
# # Logistic growth
# log.growth <- function(n, r, k){
#   new.pop <- n + n*r*(1-(n/k))
#   new.pop
# }
# 
# # Logistic growth with common pool larval dispersal (Takashina version)
# log.growth.larval.NT <- function(local.n, seascape.n, r, n.patches, local.k){
#   new.pop <- local.n + ((1/n.patches)*r*seascape.n) - r*((local.n^2)/local.k)
#   new.pop
# }
# 
# # Logistic growth with common pool larval dispersal (flawed (?) Erm version)
# log.growth.larval.PE <- function(local.n, seascape.n, r, n.patches, local.k){
#   new.pop <- local.n + ((1/n.patches)*r*seascape.n)*(1-(local.n/local.k))
#   new.pop
# }
# 
# # Beverton-Holt growth with common pool larval dispersal (Holden version)
# bev.holt.MH <- function(local.n, seascape.n, r, n.patches, local.k, species.s){
#   new.pop <- species.s*local.n + 
#     (1/n.patches) * ((r*seascape.n)/
#                        (1 + ((r+species.s-1)/((1-species.s)*local.k))*seascape.n))
#   new.pop
# }
# 
# # Beverton-Holt growth with common pool "larval" dispersal (Holden version with seascape.k in denominator)
# bev.holt.MH.2 <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s){
#   new.pop <- species.s*local.n + 
#     ((local.k/seascape.k) * ((r*seascape.n)/
#                                (1 + ((r+species.s-1)/((1-species.s)*seascape.k))*seascape.n)))
#   new.pop
# }
# 
# # Beverton-Holt growth with common pool "larval" dispersal (current best form)
# bev.holt.MH.3 <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s){
#   new.pop <- species.s*local.n +
#     ((1/n.patches) * ((r*seascape.n)/
#                         (1 + ((r+species.s-1)/((1-species.s)*local.k))*seascape.n*(1/n.patches))))
#   new.pop
# }
# 
# # Logistic growth with an Allee effect
# allee.growth <- function(n, r, k, a){
#   new.pop <- n + n*r*(1-n/k)*((n/k)-(a/k))
#   new.pop
# }
# 
# # Logistic growth with common pool larval dispersal (Sala version)
# log.growth.larval.ES <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s){
#   new.pop <- local.n + ((1/n.patches)*r*seascape.n)*(1-(seascape.n/seascape.k))
#   new.pop
# }
# 
# # Hockey stick growth
# exp.growth.hockey <- function(n, r, k){
#   if(n >= k){
#     new.pop <- n
#   } else if (n < k){
#     new.pop <- n + n*r
#   }
# }

# (OPTIMISED) For calculating growth ####

# Logistic growth
log.growth <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- local.n + local.n*r*(1-(local.n/local.k))
  return(new.pop)
}

# Logistic growth with common pool larval dispersal (Takashina version)
log.growth.larval.NT <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- local.n + ((1/n.patches)*r*seascape.n) - r*((local.n^2)/local.k)
  return(new.pop)
}

# Logistic growth with common pool larval dispersal (flawed (?) Erm version)
log.growth.larval.PE <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- local.n + ((1/n.patches)*r*seascape.n)*(1-(local.n/local.k))
  return(new.pop)
}

# Beverton-Holt growth with common pool larval dispersal (Holden version)
bev.holt.MH <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- species.s*local.n + 
    (1/n.patches) * ((r*seascape.n)/
                       (1 + ((r+species.s-1)/((1-species.s)*local.k))*seascape.n))
  return(new.pop)
}

# Beverton-Holt growth with common pool "larval" dispersal (Holden version with seascape.k in denominator)
bev.holt.MH.2 <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- species.s*local.n + 
    ((local.k/seascape.k) * ((r*seascape.n)/
                               (1 + ((r+species.s-1)/((1-species.s)*seascape.k))*seascape.n)))
  return(new.pop)
}

# Beverton-Holt growth with common pool "larval" dispersal (current best form)
bev.holt.MH.3 <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- species.s*local.n +
    ((1/n.patches) * ((r*seascape.n)/
                        (1 + ((r+species.s-1)/((1-species.s)*local.k))*seascape.n*(1/n.patches))))
  return(new.pop)
}

# Logistic growth with an Allee effect
allee.growth <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- local.n + local.n*r*(1-local.n/local.k)*((local.n/local.k)-(species.a/local.k))
  return(new.pop)
}

# Logistic growth with common pool larval dispersal (Sala version)
log.growth.larval.ES <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  new.pop <- local.n + ((1/n.patches)*r*seascape.n)*(1-(seascape.n/seascape.k))
  return(new.pop)
}

# Hockey stick growth
exp.growth.hockey <- function(local.n, seascape.n, seascape.k, r, n.patches, local.k, species.s, species.a){
  if(local.n >= local.k){
    new.pop <- local.n
  } else if (local.n < local.k){
    new.pop <- local.n + local.n*r
  }
  return(new.pop)
}

## Compile list of growth functions ####
# Note: must be in correct order or parameters script will call wrong function
growth.function.list <- list(log.growth,
                             log.growth.larval.NT,
                             log.growth.larval.PE,
                             bev.holt.MH,
                             allee.growth,
                             bev.holt.MH.2,
                             bev.holt.MH.3,
                             log.growth.larval.ES,
                             exp.growth.hockey)

# For generating habitats ####

# Auto-regressive generation of species habitat
patchK <- function(phi,neighbour,epsilon){
  #max(phi*neighbour+epsilon,0) # Consider
  phi*neighbour+epsilon
}

# 1.1 = all non-targeted species share the same habitat
non.targ.habitat.all.shared <- function(n.patches, 
                                        habitat.phi, 
                                        habitat.dev.mean, 
                                        habitat.dev.sd, 
                                        habitat.first.patch.k,
                                        non.targ.species){
  habitat <- vector("numeric", length = n.patches)
  habitat[1] <- habitat.first.patch.k
  for(j in 1:max(n.patches-1)){
    habitat[j+1] <- patchK(habitat.phi,habitat[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
  }
  for (i in seq_along(non.targ.species$species)){
    non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
  }
  non.targ.habitat.list
}

# 3.1 = single variable linear functional response
lin.func.response <- function(m, x){
  y <- m*(x-0.5) + 0.5
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  y
}

# 3.2 = double variable linear functional response
lin.func.response.2 <- function(m.1, m.2, x.1, x.2){
  y <- m.1*(x.1-0.5) + m.2*(x.2-0.5) + 0.5
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  y
}


# For calculating effort ####

# Distribute effort to maintain best CPUE
best.CPUE.finder <- function(bioms, harv){
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

# For calculating carbon loss ####

# Fraction of carbon left in a cell
frac.left.func <- function(c.stored.abs, c.lost.frac){
  output <- c.stored.abs * (1 - c.lost.frac)
  return(output)
}

# Carbon lost
carbon.lost.func <- function(SVR, pcred, plab, k, t){
  output <- SVR * pcred * plab * (1 - exp(-k * t))
  return(output)
}

# Sweep volume ratio
SVR.func <- function(SAR, pdepth){
  output <- SAR * pdepth
  return(output)
}

# Combined carbon function
new.carb.func <- function(SAR, pdepth, pcred, plab, k, t, pres.carbon){
  SVR <- SVR.func(SAR, pdepth)
  frac.lost <- carbon.lost.func(SVR, pcred, plab, k, t)
  new.carbon <- frac.left.func(pres.carbon, frac.lost)
  return(new.carbon)
}

# For calculating habitat damage ####

# Iterative RBS model
# Recovery
itr.RBS.recovery <- function(prist.K, curr.K, dep.K, n.trawls, rep.K, time){
  gain <- curr.K + rep.K*curr.K*(1-(curr.K/prist.K))
  return(gain)
}

# Depletion
itr.RBS.depletion <- function(prist.K, curr.K, dep.K, n.trawls, r, time){
  loss <- (curr.K-(curr.K*(1-dep.K)^n.trawls))
  return(loss)
}

# Recovery + depletion
itr.RBS.model <- function(prist.K, curr.K, dep.K, n.trawls, rep.K, time){
  out <- itr.RBS.recovery(prist.K, curr.K, dep.K, n.trawls, rep.K, time) - itr.RBS.depletion(prist.K, curr.K, dep.K, n.trawls, rep.K, time)
  return(out)
}

# Multiplicative model
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

# Compile list of damage functions
# Note: must be in correct order or parameters script will call wrong function
damage.function.list <- list(itr.RBS.model,
                             multip.model)


