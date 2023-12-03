#XYZ is parallel marker

# SCRIPT BEGINS HERE ####

# Script details ####

# Script for conducting sea sparing/sharing analysis in 
# XXXX

# Script structure ####

# Write structure of loop here

# Run analysis ####

# Create results lists ####

targeted.sims.list <- list()
non.targeted.sims.list <- list()
carbon.sims.list <- list()
all.species.habitat.list <- list() # This list is for saving habitats in matrix form before adjustments so that we can examine how correlated habitats are if using an option with correlation

# Specify plot path
path <- paste("../figs/raw_analysis", simulation.name, sep = "/")
dir.create(path)

# Initialise progress bar ####

# Set parallel settings ####
# From https://www.blasbenito.com/post/02_parallelizing_loops_with_r/

parallel::detectCores() #marker
max.cores <- parallel::detectCores() #marker
if(max.cores < 50){ #marker
  n.cores <- max.cores - 2 #marker
} else { #marker
  n.cores <- 50 #marker
} #marker

my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
print(my.cluster)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()
registerDoSNOW(my.cluster)

# Simulation loop ####

start.time <- Sys.time() # Record start time for main body of script

grand.list <- foreach(w = 1:n.sims, #XYZ
                      .packages = c('data.table', #XYZ
                                    'tidyverse',  #XYZ
                                    'beepr',  #XYZ
                                    'progress', #XYZ
                                    'faux',
                                    'ggcorrplot',
                                    'patchwork'), #XYZ
                      .verbose = T) %dopar% { #XYZ
                        
                        #                         ## Set seed ####
                        #                         
                        #w <- 1 #XYZ
                        set.seed(w) #XYZ
                        source("functions.R") # To avoid missing function error from parallelisation
                        
                        ## Analysis ####
                        
                        ### Generate non-targeted species in seascape ####
                        
                        # SECTION DESCRIPTION/NOTES
                        # Here we generate the parameter distributions from which parameters from individual species will be drawn, and then draw the species. We also specify other species parameters in a variety of ways
                        
                        # Calculate standard deviations from confidence intervals
                        dist.pars$log.sd.q <- sd.from.upper(dist.pars$mean.q, dist.pars$upci.q)
                        dist.pars$log.sd.r <- sd.from.lower(dist.pars$mean.r, dist.pars$lowci.r)
                        
                        # Draw species r and q
                        non.targ.species <- builder(n = n.species, 
                                                    mean.q = dist.pars$mean.q, 
                                                    sd.q = dist.pars$log.sd.q,
                                                    mean.r = dist.pars$mean.r, 
                                                    sd.r = dist.pars$log.sd.r,
                                                    name = dist.pars$class)
                        
                        ### Specify non-targeted species migration rate (m) ####
                        if(mig.rate.option==1){ # 1 = Manually set identical migration rates (m) for all non-targeted species
                          non.targ.species$m <- custom.m
                        } else if (mig.rate.option==2){ # 2 = Migration rates drawn from uniform distribution with specified parameters
                          non.targ.species$m <- runif(nrow(non.targ.species), min = migration.min.m, max = migration.max.m)
                        }
                        
                        # Specify species s
                        non.targ.species$s <- 0.25
                        
                        # Specify species a
                        non.targ.species$a <- non.targeted.a
                        
                        ### Mitigate chaos in non-targeted species ####
                        if (chaos.man.option == 1){ # 1 = Chaotic dynamics are not curtailed in any way
                          NULL
                        } else if (chaos.man.option == 2){ # 2 = Species with r above/below a specified threshold will have their r value reduced/raised to that threshold
                          non.targ.species$r[non.targ.species$r > r.thresh.max] <- r.thresh.max
                          #non.targ.species$r[non.targ.species$r < r.thresh.min] <- r.thresh.min
                        } else if (chaos.man.option == 3){ # 3 = All species' time sensitive values will be divided by a specified number (in effect shortening the time step that r, m etc. represents)
                          non.targ.species$r <- non.targ.species$r/par.division
                          non.targ.species$m <- non.targ.species$m/par.division
                        }
                        
                        ### Produce habitats for each non-targeted species ####
                        non.targ.habitat.list <- list()
                        if(non.targ.habitat.option==1.1){ # 1.1 = all non-targeted species share the same habitat
                          habitat <- vector("numeric", length = n.patches)
                          habitat[1] <- habitat.first.patch.k
                          for(j in 1:max(n.patches-1)){
                            habitat[j+1] <- patchK(habitat.phi,habitat[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                          }
                          habitat[habitat < 0] <- 0
                          habitat[habitat > 1] <- 1
                          for (i in seq_along(non.targ.species$species)){
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          }
                        } else if (non.targ.habitat.option==1.2){ # 1.2 = all non-targeted classes share the same habitat
                          stop("Selected habitat option not built yet. Select habitat option 1 or 3.")
                        } else if (non.targ.habitat.option==1.3){ # 1.3 = no non-targeted species share habitat
                          for (i in seq_along(non.targ.species$species)){
                            habitat <- vector("numeric", length = n.patches)
                            habitat[1] <- habitat.first.patch.k
                            for(j in 1:max(n.patches-1)){
                              habitat[j+1] <- patchK(habitat.phi,habitat[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                            }
                            habitat[habitat < 0] <- 0
                            habitat[habitat > 1] <- 1
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          } 
                        } else if (non.targ.habitat.option==1.4){ # 1.4 = no non-targeted species share habitat, improved behaviour
                          for (i in seq_along(non.targ.species$species)){
                            habitat <- vector("numeric", length = n.patches)
                            habitat[sample(1:length(habitat),1)] <- runif(1, min = 0, max = 1) # Assign a random patch a random value
                            occ.patch <- which(habitat != 0) # Determine which patch is occupied
                            num.to.gen.right <- length(habitat)-occ.patch # Count number of patches above it
                            num.to.gen.left <- occ.patch-1 # Count number of patches below it
                            # Generate values to the right of occupied patch
                            new.right.vals <- c(habitat[occ.patch], rep(0, num.to.gen.right))
                            if(length(new.right.vals) > 1){
                              for(j in 1:num.to.gen.right){
                                new.right.vals[j+1] <- patchK(habitat.phi,new.right.vals[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                              }
                            }
                            # Generate values to the left of occupied patch
                            new.left.vals <- c(rep(0, num.to.gen.left), habitat[occ.patch])
                            if(length(new.left.vals) > 1){
                              for(j in occ.patch:1){
                                new.left.vals[j-1] <- patchK(habitat.phi,new.left.vals[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                              }
                            }
                            new.right.vals <- tail(new.right.vals, -1) # Remove first value
                            new.left.vals <- head(new.left.vals, -1) # Remove last value
                            # Combine values
                            habitat <- c(new.left.vals, habitat[occ.patch], new.right.vals)
                            habitat[habitat < 0] <- 0
                            habitat[habitat > 1] <- 1
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          } 
                        } else if (non.targ.habitat.option==2.1){ # 2.1 = all non-targeted species share the same habitat (manual habitat quality)
                          habitat <- rep(non.targ.manual.habitat.value, n.patches)
                          for (i in seq_along(non.targ.species$species)){
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          }
                        } else if (non.targ.habitat.option==3.1){ # 3.1 = single variable linear functional response
                          non.targ.species$resp <- runif(nrow(non.targ.species), min = response.min, max = response.max)
                          for (i in seq_along(non.targ.species$species)){
                            habitat <- vector("numeric", length = n.patches)
                            habitat <- lin.func.response(non.targ.species$resp[i], env.gradient)
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          }
                        } else if (non.targ.habitat.option==3.2){ # 3.2 = double variable linear functional response
                          non.targ.species$resp.1 <- runif(nrow(non.targ.species), min = response.min.1, max = response.max.1)
                          non.targ.species$resp.2 <- runif(nrow(non.targ.species), min = response.min.2, max = response.max.2)
                          for (i in seq_along(non.targ.species$species)){
                            habitat <- vector("numeric", length = n.patches)
                            habitat <- lin.func.response.2(non.targ.species$resp.1[i],
                                                           non.targ.species$resp.2[i],
                                                           env.gradient.1,
                                                           env.gradient.2)
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          }
                        } else if (non.targ.habitat.option==4.1){ # 4.1 = multivariate normal distribution
                          habitat.search.iter <- 1
                          repeat{
                            hab.mean.vec <- rep(hab.mean.nt, (n.species*8)+1) # Create means vector
                            habitat <- rnorm_multi(n = n.patches,
                                                   mu = hab.mean.vec,
                                                   sd = rep(1, length(hab.mean.vec)),
                                                   r = habitat.correlation)
                            #correlation.before <- cor(habitat)
                            habitat.before <- habitat
                            habitat[habitat < 0] <- 0
                            #habitat[habitat > 1] <- 1
                            habitat.after <- habitat
                            #correlation.after <- cor(habitat)
                            for (i in seq_along(non.targ.species$species)){
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat[,i]
                            }
                            if(all(apply(habitat, 2, FUN = sum) > 0)){ # If no species has a total habitat of 0
                              break
                            } else {
                              (habitat.search.iter <- habitat.search.iter + 1)
                              if(habitat.search.iter == 500){
                                print("Took too long to find MSY, defaulting to standard calculation.")
                                stop("Couldn't generate a set of habitats where all species had at least some habitat despite 500 iterations. Stopping simulation")}
                            }
                          }
                          all.species.habitat.list[[w]] <- habitat.before
                        } else if (non.targ.habitat.option==4.2){ # 4.2 = multivariate normal distribution where non-targeted species are generated based off pre-existing targeted species
                          habitat.search.iter <- 1
                          repeat{
                            
                            # Add in checks to make sure that neither targeted nor non-targeted species have 0 across the board
                            
                            # Generate targeted species habitat
                            targ.habitat <- rnorm(n = n.patches,
                                                  mean = habitat.mean,
                                                  sd = habitat.sd)
                            plot(targ.habitat, type = 'l')
                            targ.habitat[targ.habitat < 0] <- 0 # Replace negative values with 0
                            plot(targ.habitat, type = 'l')
                            
                            # Generate non-targeted habitat values
                            species.loop <- n.species*8
                            habitat.data <- matrix(ncol = species.loop, nrow = n.patches)
                            for(i in 1:species.loop){
                              habitat.data[,i]<- rnorm_pre(x = targ.habitat,
                                                           mu = habitat.mean,
                                                           sd = habitat.sd,
                                                           r = habitat.correlation)
                            }
                            colnames(habitat.data) <- 1:(n.species*8)
                            
                            # Check realised correlations before removing 0s
                            all.habitats.before.0 <- cbind(targ.habitat, habitat.data, deparse.level = 1)
                            all.habitats.before.0
                            cor.before.0 <- cor(all.habitats.before.0)
                            mean.with.targ.before.0 <- round(mean(cor.before.0[2:nrow(cor.before.0),1]),3)
                            mean.calc <- cor.before.0
                            mean.calc[mean.calc == 1] <- NA
                            mean.before <- round(mean(mean.calc, na.rm = T), 3)
                            title.before <- paste("Before adding 0s" , "\nall species avg correlation =", mean.before, "\ntarg species avg correlation =", mean.with.targ.before.0, sep = " ")
                            c1.before <- ggcorrplot(cor.before.0, title = title.before)
                            
                            # Check realised correlations after removing 0s
                            all.habitats.after.0 <- all.habitats.before.0
                            all.habitats.after.0[all.habitats.after.0 < 0] <- 0
                            cor.after.0 <- cor(all.habitats.after.0)
                            mean.with.targ.after.0 <- round(mean(cor.after.0[2:nrow(cor.after.0),1]),3)
                            mean.calc <- cor.after.0
                            mean.calc[mean.calc == 1] <- NA
                            mean.after <- round(mean(mean.calc, na.rm = T), 3)
                            title.after <- paste("After adding 0s" , "\nall species avg correlation =", mean.after, "\ntarg species avg correlation =", mean.with.targ.after.0, sep = " ")
                            c1.after <- ggcorrplot(cor.after.0, title = title.after)
                            
                            # Save habitats into form required by subsequent code
                            habitat <- all.habitats.after.0[,-1] # Remove targeted habitat
                            for (i in seq_along(non.targ.species$species)){
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat[,i]
                            }
                            
                            
                            # Compare correlations
                            p <- c1.before + c1.after
                            
                            plot.name <- "corr.comparison"
                            plot.path <- paste(path, plot.name, sep = "/")
                            pdf.path <- paste(plot.path, ".pdf", sep = "")
                            png.path <- paste(plot.path, ".png", sep = "")
                            ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
                            ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")
                            
                            if(all(apply(all.habitats.after.0, 2, FUN = sum) > 0)){ # If no species has a total habitat of 0
                              break
                            } else {
                              (habitat.search.iter <- habitat.search.iter + 1)
                              if(habitat.search.iter == 500){
                                stop("Couldn't generate a set of habitats where all species had at least some habitat despite 500 iterations. Stopping simulation.")}
                            }
                          }
                        } else if (non.targ.habitat.option==4.3){ # 4.3 = multivariate normal distribution where non-targeted species are generated based off pre-existing targeted species, and targeted species is generated manually
                          
                          habitat.search.iter <- 1
                          repeat{
                            # Generate targeted species habitat
                            num.to.spare <- round(n.patches*proportion.present, digits = 0)
                            targ.habitat <- c(rep(x = k.when.present, times = num.to.spare), rep(x = 0, times = n.patches - num.to.spare))
                            
                            plot(targ.habitat, type = 'l')
                            targ.habitat[targ.habitat < 0] <- 0 # Replace negative values with 0
                            plot(targ.habitat, type = 'l')
                            
                            # Generate non-targeted habitat values
                            species.loop <- n.species*8
                            habitat.data <- matrix(ncol = species.loop, nrow = n.patches)
                            for(i in 1:species.loop){
                              habitat.data[,i]<- rnorm_pre(x = targ.habitat,
                                                           mu = habitat.mean,
                                                           sd = habitat.sd,
                                                           r = habitat.correlation)
                            }
                            colnames(habitat.data) <- 1:(n.species*8)
                            
                            # Check realised correlations before removing 0s
                            all.habitats.before.0 <- cbind(targ.habitat, habitat.data, deparse.level = 1)
                            all.habitats.before.0
                            cor.before.0 <- cor(all.habitats.before.0)
                            mean.with.targ.before.0 <- round(mean(cor.before.0[2:nrow(cor.before.0),1]),3)
                            mean.calc <- cor.before.0
                            mean.calc[mean.calc == 1] <- NA
                            mean.before <- round(mean(mean.calc, na.rm = T), 3)
                            title.before <- paste("Before adding 0s" , "\nall species avg correlation =", mean.before, "\ntarg species avg correlation =", mean.with.targ.before.0, sep = " ")
                            c1.before <- ggcorrplot(cor.before.0, title = title.before)
                            
                            # Check realised correlations after removing 0s
                            all.habitats.after.0 <- all.habitats.before.0
                            all.habitats.after.0[all.habitats.after.0 < 0] <- 0
                            cor.after.0 <- cor(all.habitats.after.0)
                            mean.with.targ.after.0 <- round(mean(cor.after.0[2:nrow(cor.after.0),1]),3)
                            mean.calc <- cor.after.0
                            mean.calc[mean.calc == 1] <- NA
                            mean.after <- round(mean(mean.calc, na.rm = T), 3)
                            title.after <- paste("After adding 0s" , "\nall species avg correlation =", mean.after, "\ntarg species avg correlation =", mean.with.targ.after.0, sep = " ")
                            c1.after <- ggcorrplot(cor.after.0, title = title.after)
                            
                            # Save habitats into form required by subsequent code
                            habitat <- all.habitats.after.0[,-1] # Remove targeted habitat
                            for (i in seq_along(non.targ.species$species)){
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat[,i]
                            }
                            
                            
                            # Compare correlations
                            p <- c1.before + c1.after
                            
                            plot.name <- "corr.comparison"
                            plot.path <- paste(path, plot.name, sep = "/")
                            pdf.path <- paste(plot.path, ".pdf", sep = "")
                            png.path <- paste(plot.path, ".png", sep = "")
                            ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
                            ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")
                            
                            if(all(apply(all.habitats.after.0, 2, FUN = sum) > 0)){ # If no species has a total habitat of 0
                              break
                            } else {
                              (habitat.search.iter <- habitat.search.iter + 1)
                              if(habitat.search.iter == 500){
                                stop("Couldn't generate a set of habitats where all species had at least some habitat despite 500 iterations. Stopping simulation.")}
                            }
                          }
                        } else if (non.targ.habitat.option==5.1){ # 5.1 = Non-targeted species have manually set overlap with targeted species
                          # Generate targeted species habitat
                          num.to.spare <- round(n.patches*proportion.present, digits = 0)
                          targ.habitat <- c(rep(x = k.when.present, times = num.to.spare), rep(x = 0, times = n.patches - num.to.spare))
                          # Specify proportion of overlap desired
                          non.targ.habitat <- targ.habitat
                          # Calculate how many patches are overlapping
                          (n.patches.overlap <- n.patches*proportion.present*overlap)
                          # Invert targ.habitat so there is no overlap
                          non.targ.habitat <- rev(non.targ.habitat)

                          # Add as many 0s to end as required to facilitate overlap
                          non.targ.habitat <- c(non.targ.habitat, rep(0, n.patches.overlap))

                          # Remove as many 0s to end as required to facilitate overlap
                          if(overlap != 0){
                            for(i in 1:n.patches.overlap){
                              non.targ.habitat <- non.targ.habitat[-1] 
                            }
                          }
                          # Save habitat
                          for (i in seq_along(non.targ.species$species)){
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- non.targ.habitat
                          }
                        } else if (non.targ.habitat.option==6.1){ # 6.1 = Pick a random half of patches to be occupied
                          for (i in seq_along(non.targ.species$species)){
                            habitat <- vector("numeric", length = n.patches)
                            occ.patches <- sample(1:n.patches, n.patches/2, replace = FALSE)
                            habitat[occ.patches] <- 1
                            non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- habitat
                          } 
                        } else if (non.targ.habitat.option==7.1){ # 7.1 Random patches but with prescribed overlap
                          # Generate target
                          targ.habitat <- vector("numeric", length = n.patches)
                          targ.occ.patches <- sample(1:n.patches, n.occupied, replace = FALSE)
                          targ.habitat[targ.occ.patches] <- 1

                          # If you end up using a factor other than 1, shuffle species data frame so no bias in multiples less than 1
                          # E.g.
                          #non.targ.species <- non.targ.speces[sample(nrow(non.targ.species)),]
                          # Might cause downstream problems though, so leaving out as long as not using factor.val other than 1
                          
                          # Generate first non-target
                          non.targ.habitat <- vector("numeric", length = n.patches)
                          non.targ.ovr.patches <- sample(targ.occ.patches, random.overlap*n.occupied, replace = FALSE)  
                          non.targ.habitat[non.targ.ovr.patches] <- 1
                          targ.un.patches <- !(1:n.patches  %in%  sort(targ.occ.patches))
                          targ.unocc.patches <- c(1:n.patches)[targ.un.patches]
                          non.targ.free.patches <- sample(targ.unocc.patches , (1-random.overlap)*n.occupied, replace = FALSE)
                          non.targ.habitat[non.targ.free.patches] <- 1
                          for (i in seq_along(non.targ.species$species)){
                            if((i-1) == 0){
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- non.targ.habitat
                            } else if(((i-1)/factor.val)%%1 == 0){
                              non.targ.habitat <- vector("numeric", length = n.patches)
                              non.targ.ovr.patches <- sample(targ.occ.patches, random.overlap*n.occupied, replace = FALSE)  
                              non.targ.habitat[non.targ.ovr.patches] <- 1
                              targ.un.patches <- !(1:n.patches  %in%  sort(targ.occ.patches))
                              targ.unocc.patches <- c(1:n.patches)[targ.un.patches]
                              non.targ.free.patches <- sample(targ.unocc.patches , (1-random.overlap)*n.occupied, replace = FALSE)
                              non.targ.habitat[non.targ.free.patches] <- 1
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- non.targ.habitat
                            } else {
                              non.targ.habitat.list[[paste(non.targ.species$species[i])]] <- non.targ.habitat
                            }
                          } 
                        }
                        
                        ### Define targeted species parameters ####
                        
                        if (targ.spec.option==1){ # 1 = targeted species parameters are set manually
                          targ.species <- bind_cols(
                            class=targeted.class,
                            species=targeted.species,
                            q=targeted.q,
                            r=targeted.r,
                            m=targeted.m,
                            p=targeted.p,
                            c=targeted.c
                          )
                        } else if (targ.spec.option==2){ # 2 = targeted species is selected randomly from any class
                          stop("Selected targeted species option not built yet. Select targeted species option 1.")
                        } else if (targ.spec.option==3){ # 3 = targeted species is selected randomly from particular class
                          stop("Selected targeted species option not built yet. Select targeted species option 1.")
                        } else if (targ.spec.option==4){ # 4 = targeted species is perfectly average species from particular class
                          stop("Selected targeted species option not built yet. Select targeted species option 1.")
                        }
                        
                        ### Mitigate chaos in targeted species ####
                        if (chaos.man.option == 1){ # 1 = Chaotic dynamics are not curtailed in any way
                          NULL
                        } else if (chaos.man.option == 2){ # 2 = Species with r above a specified threshold will have their r value reduced to that threshold
                          NULL # Not applicable to targeted species
                        } else if (chaos.man.option == 3){ # 3 = All species' time sensitive values will be divided by a specified number (in effect shortening the time step that r, m etc. represents)
                          targ.species$r <- targ.species$r/par.division
                          targ.species$m <- targ.species$m/par.division
                        }
                        
                        ### Produce habitat for targeted species ####
                        if (targ.habitat.option==1.1){ # 1.1 = targeted species given same habitat as all non-targeted species
                          targ.habitat <- non.targ.habitat.list[[1]]
                        } else if (targ.habitat.option==1.2){ # 1.2 = targeted species given same habitat as particular non-targeted class
                          stop("Selected habitat option not built yet. Select habitat option 1 or 3.")
                        } else if (targ.habitat.option==1.3){ # 1.3 = targeted species given unique habitat
                          targ.habitat <- vector("numeric", length = n.patches)
                          targ.habitat[1] <- habitat.first.patch.k
                          for(i in 1:max(n.patches-1)){
                            targ.habitat[i+1] <- patchK(habitat.phi,habitat[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                          }
                          targ.habitat[targ.habitat < 0] <- 0
                          targ.habitat[targ.habitat > 1] <- 1
                        } else if (targ.habitat.option==1.4){ # 1.4 = targeted species given unique habitat, improved behaviour
                          targ.habitat <- vector("numeric", length = n.patches)
                          targ.habitat[sample(1:length(targ.habitat),1)] <- runif(1, min = 0, max = 1) # Assign a random patch a random value
                          occ.patch <- which(targ.habitat != 0) # Determine which patch is occupied
                          num.to.gen.right <- length(targ.habitat)-occ.patch # Count number of patches above it
                          num.to.gen.left <- occ.patch-1 # Count number of patches below it
                          # Generate values to the right of occupied patch
                          new.right.vals <- c(targ.habitat[occ.patch], rep(0, num.to.gen.right))
                          if(length(new.right.vals) > 1){
                            for(j in 1:num.to.gen.right){
                              new.right.vals[j+1] <- patchK(habitat.phi,new.right.vals[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                            }
                          }
                          # Generate values to the left of occupied patch
                          new.left.vals <- c(rep(0, num.to.gen.left), targ.habitat[occ.patch])
                          if(length(new.left.vals) > 1){
                            for(j in occ.patch:1){
                              new.left.vals[j-1] <- patchK(habitat.phi,new.left.vals[j],rnorm(1, habitat.dev.mean, habitat.dev.sd))
                            }
                          }
                          new.right.vals <- tail(new.right.vals, -1) # Remove first value
                          new.left.vals <- head(new.left.vals, -1) # Remove last value
                          # Combine values
                          targ.habitat <- c(new.left.vals, targ.habitat[occ.patch], new.right.vals)
                          targ.habitat[targ.habitat < 0] <- 0
                          targ.habitat[targ.habitat > 1] <- 1
                        } else if (targ.habitat.option==2.1){ # 2.1 = given same habitat as all non.targeted species
                          targ.habitat <- rep(targ.manual.habitat.value, n.patches)
                        } else if (targ.habitat.option==3.1){ # 3.1 = single variable linear functional response
                          targ.species$resp <- runif(1, min = response.min, max = response.max)
                          targ.habitat <- lin.func.response(targ.species$resp, env.gradient)
                        } else if (targ.habitat.option==3.2){ # 3.2 = double variable linear functional response
                          targ.species$resp.1 <- runif(1, min = response.min.1, max = response.max.1)
                          targ.species$resp.2 <- runif(1, min = response.min.2, max = response.max.2)
                          targ.habitat <- lin.func.response.2(targ.species$resp.1,
                                                              targ.species$resp.2,
                                                              env.gradient.1,
                                                              env.gradient.2)
                        } else if (targ.habitat.option==4.1){ # 4.1 = multivariate normal distribution
                          targ.habitat <- habitat[,ncol(habitat)]
                        } else if (targ.habitat.option==4.2){ # # 4.2 = multivariate normal distribution where non-targeted species are generated based off pre-existing targeted species
                          targ.habitat <- targ.habitat
                        } else if (targ.habitat.option==4.3){ # 4.3 = manually specifying targeted species
                          targ.habitat <- targ.habitat
                        } else if (targ.habitat.option==5.1){ # 5.1 = manually specifying targeted species
                          targ.habitat <- targ.habitat
                        } else if (targ.habitat.option==6.1){ # 6.1 = Pick a random half of patches to be occupied
                          targ.habitat <- vector("numeric", length = n.patches)
                          occ.patches <- sample(1:n.patches, n.patches/2, replace = FALSE)
                          targ.habitat[occ.patches] <- 1
                        } else if (targ.habitat.option==7.1){ # 7.1 = manually specifying targeted species
                          targ.habitat <- targ.habitat
                        }
                        #### View habitat for particular species ####
                        
                        # Targeted species
                        targ.habitat.plot <- cbind.data.frame(1:n.patches, targ.habitat)
                        ggplot(targ.habitat.plot, aes(x = `1:n.patches`, y = targ.habitat)) +
                          geom_line() +
                          geom_point() +
                          labs(y = "Habitat quality (carrying capacity)", 
                               x = "Patch") +
                          ylim(0,NA) +
                          geom_hline(yintercept = 0, linetype = "dotted") +
                          theme_bw()
                        
                        # Non-targeted species
                        non.targ.habitat.plot <- rbindlist(map(non.targ.habitat.list, cbind.data.frame, patch=1:n.patches), idcol="species")
                        ggplot(non.targ.habitat.plot, aes(x = patch, y = `.x[[i]]`)) +
                          geom_line() +
                          geom_point() +
                          facet_wrap(.~species) +
                          labs(y = "Habitat quality (carrying capacity)", 
                               x = "Patch") +
                          ylim(0,NA) +
                          geom_hline(yintercept = 0, linetype = "dotted") +
                          theme_bw()
                        
                        # Aggregate non-targeted species versus targeted species
                        targ.habitat.plot$species <- "targeted"
                        non.targ.habitat.plot <- non.targ.habitat.plot %>% 
                          group_by(patch) %>% 
                          summarise(value = mean(`.x[[i]]`)) 
                        non.targ.habitat.plot$species <- "non.targeted"
                        targ.habitat.plot <- targ.habitat.plot %>%
                          rename(patch = `1:n.patches`, value = targ.habitat)
                        comb.habitat.plot <- rbind.data.frame(targ.habitat.plot, non.targ.habitat.plot)
                        ggplot(comb.habitat.plot, aes(x = patch, y = value, colour = species)) +
                          geom_line() +
                          geom_point() +
                          labs(y = "Habitat quality (carrying capacity)", 
                               x = "Patch") +
                          ylim(0,NA) +
                          geom_hline(yintercept = 0, linetype = "dotted") +
                          scale_colour_discrete(name = "Species", labels = c("Mean non-targeted", "Targeted")) +
                          theme_bw()
                        
                        ### Calculate maximum population abundance for each species given their particular habitat exposure ####
                        
                        non.targ.species$k <- map_dbl(non.targ.habitat.list, sum)
                        targ.species$k <- sum(targ.habitat)
                        
                        ### Produce spectrum of catches to explore based on targeted species MSY ####
                        
                        targ.species.MSY <- (targ.species$r*targ.species$k)/4/catch.divisor
                        
                        ###### INSERT MSY FINDER HERE ######
                        
                        # Define functions
                        # Define growth function
                        t.growth.function <- growth.function.list[[targeted.growth.option]]
                        
                        # Define habitat damage function
                        t.damage.function <- damage.function.list[[damage.option]]
                        
                        # Set parameters
                        # Misc.
                        #time <- 100 Already set: n.time
                        
                        # Growth
                        #species.r <- 2 Already set: targ.species$r
                        #species.n0 <- 1 Already set: targ.species$k
                        
                        # Catch
                        #catch.q <- 0.1 Already set: targ.species$q
                        #catch.constraint <- 0.05 Already set: targ.species.MSY
                        
                        # Habitat
                        #hab.r <- 0.5 Already set: rep.K
                        #hab.pristine <- 1 Already set: targ.species$k
                        #hab.k0 <- hab.pristine Already set: targ.species$k
                        #hab.d <- 0.2 Already set: dep.K
                        
                        # Calculate upper limit of catch test
                        
                        (targ.species.MSY <- (targ.species$r*targ.species$k)/4/catch.divisor)
                        (catch.constraint <- targ.species.MSY)
                        
                        # Simulate
                        
                        MSY.search.iter <- 1
                        repeat{
                          # Generate data structure
                          n.vect <- vector(length = n.time)
                          n.vect[1] <- targ.species$k
                          k.vect <- vector(length = n.time)
                          k.vect[1] <- targ.species$k
                          e.vect <- vector(length = n.time)
                          e.vect[1] <- 0
                          for(i in 1:(n.time-1)){
                            # Growth and catch
                            (whole.pop <- n.vect[i])
                            (whole.k <- k.vect[i])
                            (n.vect[i+1] <- t.growth.function(local.n = n.vect[i],
                                                              seascape.n = whole.pop,
                                                              seascape.k = whole.k,
                                                              r = targ.species$r,
                                                              n.patches = 1,
                                                              local.k = k.vect[i],
                                                              species.s = global.survival,
                                                              species.a = targeted.a*k.vect[i]) - catch.constraint)
                            (n.vect[is.na(n.vect)] <- 0)
                            (n.vect[n.vect < 0] <- 0)
                            
                            # Effort
                            (e.vect[i+1] <- catch.constraint/(targ.species$q*n.vect[i+1]))
                            
                            # Habitat
                            (k.vect[i+1] <- t.damage.function(prist.K = k.vect[1],
                                                              curr.K = k.vect[i],
                                                              dep.K = dep.Kt,
                                                              n.trawls = e.vect[i+1],
                                                              rep.K = rep.Kt))
                            
                            # Restore negative Ks to 0
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
                        (targ.species.MSY <- catch.constraint)
                        (MSY.search.iter)
                        
                        ##### END MSY FINDER ######
                        
                        # Uncomment below 2 lines to explore broader spectrum of catches
                        #catch.spec <- seq(targ.species.MSY/n.catch.ints, targ.species.MSY, targ.species.MSY/n.catch.ints)
                        #catch.spec <- c(catch.spec, targ.species.MSY*0.67)

                        # Comment out below 1 line if uncommenting two above
                        catch.spec <- targ.species.MSY*0.5

                        ### Produce spectrum of spared patches to explore ####
                        
                        spare.spec <- 0:(n.patches-1)
                        
                        ### Produce results ####
                        
                        # Generate targeted species abundances for t = 1
                        catch <- catch.spec
                        sparing <- spare.spec
                        simulation <- w
                        patch <- 1:n.patches
                        time.step <- 1
                        species <- targ.species$species
                        targ.spec.data <- expand_grid(catch,sparing,simulation,patch,time.step,species)
                        
                        # Generate abundances
                        abun <- vector()
                        for (i in 1:nrow(targ.spec.data)){
                          row <- targ.spec.data[i,]
                          abun[i] <- targ.habitat[row$patch]
                        }
                        targ.spec.data <- cbind.data.frame(targ.spec.data, abun)
                        targ.spec.data$k <- targ.spec.data$abun
                        targ.spec.data$abun <- targ.spec.data$abun*start.pop
                        
                        # Generate non-targeted species abundances for t = 1
                        catch <- catch.spec
                        sparing <- spare.spec
                        simulation <- w
                        patch <- 1:n.patches
                        time.step <- 1
                        non.targ.spec.data <- expand_grid(catch,sparing,simulation,patch,time.step,non.targ.species)
                        
                        # Generate abundances
                        abun <- vector()
                        for (i in 1:nrow(non.targ.spec.data)){
                          row <- non.targ.spec.data[i,]
                          abun[i] <- non.targ.habitat.list[[row$species]][row$patch]
                        }
                        non.targ.spec.data <- cbind.data.frame(non.targ.spec.data, abun)
                        non.targ.spec.data$k <- non.targ.spec.data$abun
                        non.targ.spec.data$effort <- 0
                        non.targ.spec.data$abun <- non.targ.spec.data$k*start.pop
                        non.targ.spec.data$vul <- non.targ.spec.data$q/non.targ.spec.data$r
                        
                        # Split species into individual simulations
                        targ.spec.list <- group_split(targ.spec.data, catch, sparing, simulation)
                        non.targ.spec.list <- group_split(non.targ.spec.data, catch, sparing, simulation, species)
                        
                        ### Generate MPA configuration ####
                        
                        # Create MPA list
                        MPA.list <- list()
                        
                        # Produce MPA configurations under each possible coverage level
                        if (MPA.option==1){ # 1 = MPAs are assigned to patches randomly
                          MPAs <- vector("numeric", length = n.patches)
                          MPA.list[[paste("sparing = 0")]] <- MPAs
                          for(i in 1:(n.patches-1)){ 
                            MPA.patches <- sample(which(MPAs == 0), 1)
                            MPAs[MPA.patches] <- 1
                            MPA.list[[paste("sparing =", i/n.patches)]] <- MPAs
                          }
                        } else if (MPA.option==2){ # 2 = MPAs are assigned to patches with highest aggregate habitat value for non-targeted species
                          patch.values <- non.targ.spec.data %>% 
                            group_by(patch) %>% 
                            summarise(value = sum(abun))
                          MPAs <- vector("numeric", length = n.patches)
                          MPA.list[[paste("sparing = 0")]] <- MPAs
                          ii <- 1
                          for(i in 1:(n.patches-1)){ 
                            MPA.patches <- which(patch.values$value == (sort(patch.values$value, decreasing = TRUE)[1:i])[i])
                            if(length(MPA.patches) > 1){ # Logic for handling case where multiple MPAs meet the criteria for being the next one to spare
                              MPAs[MPA.patches[ii]] <- 1 # Spare the first of the various patches that meet the criteria
                              if(ii == length(MPA.patches)){
                                ii <- 1 # If there are none left to be spared that met the criteria in previous rounds, reset to one so that next time multiple patches meet the criteria we start from scratch in choosing which of those to spare
                              } else {
                                ii <- ii+1 # Otherwise set things up so that the next patch along will be spared when we get back here
                              }
                            } else {
                              MPAs[MPA.patches] <- 1
                              ii <- 1 # Probably redundant
                            }
                            MPA.list[[paste("sparing =", i/n.patches)]] <- MPAs
                          }
                        } else if (MPA.option==3){ # 3 = MPAs are assigned to patches with lowest habitat value to targeted species
                          MPAs <- vector("numeric", length = n.patches)
                          MPA.list[[paste("sparing = 0")]] <- MPAs
                          for(i in 1:(n.patches-1)){ 
                            # Find patches that are candidates to be spared
                            MPA.patches <- which(targ.habitat == (sort(targ.habitat)[1:i])[i])
                            # Eliminate from contention those patches that are already spared
                            ## Look at previous level of sparing
                            already.spared <- MPA.list[[paste("sparing =", (i/n.patches)-1/n.patches)]]
                            ## Check if any of the candidate patches are already spared
                            ### Does the indicated vector have a 1 in any of the following indices?
                            ### If yes, remove any of the trues from MPA.patches
                            elements.to.remove <- MPA.patches[already.spared[MPA.patches] == 1]
                            sparing.candidates <- MPA.patches[!MPA.patches %in% elements.to.remove]
                            # COMMENT BELOW OUT IF WISH TO TURN ON MORE RANDOM SPARING
                            #sparing.candidates <- first(sparing.candidates) # If want to guarantee patches are spared that have non-targ species first
                            # Randomly spare one of the remaining candidates
                            if(length(sparing.candidates) > 1){
                              patch.to.spare <- sample(sparing.candidates, 1)
                              MPAs[patch.to.spare] <- 1
                            } else if(length(sparing.candidates) == 1){
                              MPAs[sparing.candidates] <- 1
                            }
                            # Save that into the list at the appropriate level of sparing
                            MPA.list[[paste("sparing =", i/n.patches)]] <- MPAs
                          }
                        }
                        #MPA.list
                        
                        # Plot MPA configuration
                        MPA.plot <- unlist(MPA.list, use.names = FALSE)
                        MPA.plot <- cbind.data.frame(MPA.plot, 1:n.patches)
                        MPA.plot <- cbind.data.frame(MPA.plot, rep(0:(n.patches-1), each = n.patches))
                        names(MPA.plot) <- c("MPA.present", "patch", "patches.spared")
                        
                        p <- ggplot(MPA.plot, aes(x = patch, y = MPA.present)) +
                          geom_point() +
                          geom_line() +
                          facet_wrap(patches.spared~.)
                        p
                        
                        plot.name <- "MPA.configuration"
                        plot.path <- paste(path, plot.name, sep = "/")
                        pdf.path <- paste(plot.path, ".pdf", sep = "")
                        png.path <- paste(plot.path, ".png", sep = "")
                        ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
                        ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

                        ## Simulate targeted species ####
                        
                        # New code
                        # Define growth function
                        t.growth.function <- growth.function.list[[targeted.growth.option]]
                        
                        # Define habitat damage function
                        t.damage.function <- damage.function.list[[damage.option]]
                        
                        # Create data structures for saving results
                        ready.list <- targ.spec.list
                        all.targ.results <- list()
                        
                        # Mark which cells are spared to facilitate plotting later
                        ready.list <- lapply(ready.list, function(x){
                          x$spared <- MPA.list[[unique(x$sparing)+1]] # Mark which cells are spared
                          return(x)
                        })
                        
                        # Record carrying capacities before habitat is damaged
                        ready.list <- lapply(ready.list, function(x){
                          x$pristine.k <- x$k
                          return(x)
                        })
                        
                        pb <- progress_bar$new(format = "  Simulating targeted species [:bar] :percent ETA: :eta",
                                               total = n.time,
                                               clear = FALSE)
                        
                        pb$tick(0)
                        
                        for(i in 1:n.time){
                          pb$tick()
                          # Take current snapshot
                          all.targ.results[[i]] <- ready.list
                          
                          # Move time forward
                          ready.list <- lapply(ready.list, function(x){
                            x$time.step <- x$time.step+1
                            return(x)
                          })
                          
                          
                          # Add growth
                          ready.list <- lapply(ready.list, function(x){
                            
                            # Add growth
                            whole.pop <- sum(x$abun)
                            whole.k <- sum(x$k)
                            x$abun <- t.growth.function(local.n = x$abun,
                                                        seascape.n = whole.pop,
                                                        seascape.k = whole.k,
                                                        r = targ.species$r,
                                                        n.patches = n.patches, 
                                                        local.k = x$k, 
                                                        species.s = global.survival,
                                                        species.a = targeted.a*x$k)
                            
                            # Restore negative populations to 0
                            x[is.na(x)] <- 0
                            x$abun[x$abun < 0] <- 0
                            
                            return(x)
                          })
                          
                          # Subtract catch
                          ready.list <- lapply(ready.list, function(x){
                            x$prop.abun <- x$abun/sum(x$k) # Calculate proportionate abundance in each cell
                            x$spared <- MPA.list[[unique(x$sparing)+1]] # Mark which cells are spared
                            x$fishable.abun <- sum(x$abun*(1-x$spared)) # Calculate fishable abundance in whole seascape
                            x$prop.fishable.abun <- x$abun/x$fishable.abun*(1-x$spared) # Calculate the proportion of fishable abundance in each cell
                            x$catch.share <- x$catch*x$prop.fishable.abun # Calculate the amount of absolute catch to be taken from the cell under proportional catch rules
                            x$abun.exposed <- x$abun*(1-x$spared) # Calculate fishable abundance in a given cell
                            if(man.option==1){ # 1 = open access dynamics according to Gordon model outside of MPAs
                              x$new.abun <- targeted.c/(targeted.p*targ.species$q)
                              x$new.abun <- ifelse(x$new.abun > x$abun.exposed, x$abun.exposed, x$new.abun)
                            } else if (man.option==2){ # 2 = efficient effort allocation outside of MPA for catch target
                              x$new.abun <- best.CPUE.finder(x$abun.exposed,unique(x$catch))
                            } else if (man.option==3){ # 3 = blanket effort allocation outside of MPA for catch target
                              stop("Selected management option not built yet. Select management option 2.")
                            }
                            x$catch.extracted <- x$abun.exposed-x$new.abun
                            x$effort <- x$catch.extracted/(targ.species$q*x$abun.exposed)
                            x[is.na(x)] <- 0
                            x$abun <- x$abun - (x$abun*x$effort*targ.species$q) # Final catch subtraction
                            return(x)
                          })
                          
                          # Remove any simulations that have produced negative population sizes
                          ready.list <- lapply(ready.list, function(x){
                            # Kill any simulation with negative population size
                            if(any(x$abun < 0 | is.na(x$abun))){
                              x <- NULL
                            } else {
                              x
                            }
                            return(x)
                          })
                          ready.list <- compact(ready.list)
                          
                          # Apply habitat damage
                          ready.list <- lapply(ready.list, function(x){
                            x$k <- t.damage.function(prist.K = x$pristine.k,
                                                     curr.K = x$k,
                                                     dep.K = dep.Kt,
                                                     n.trawls = x$effort,
                                                     rep.K = rep.Kt)
                            
                            # Restore negative Ks to 0
                            x[is.na(x)] <- 0
                            x$k[x$k < 0] <- 0
                            
                            return(x)
                          })
                          
                          # Calculate migration for remaining valid simulations
                          ready.list <- lapply(ready.list, function(x){
                            # Add/subtract migration
                            if(mig.option==1){ # 1 = No migration
                              # Do nothing
                            } else if(mig.option==2){ # 2 = Spatially implicit random diffusion based on patch habitat quality
                              migration.change <- vector()
                              for(ii in 1:nrow(x)){
                                sum.vect <- vector()
                                for(jj in 1:nrow(x)){
                                  sum.vect[jj] <- x$abun[jj]
                                }
                                migration.change[ii] <- ((x$k[ii]/sum(x$k))*(sum(sum.vect)-sum.vect[ii]))-(1-((x$k[ii])/sum(x$k)))*x$abun[ii]
                              }
                              migration.final <- targeted.m*migration.change
                              x$abun <- x$abun + migration.final
                            } else if(mig.option==3){ # 3 = Spatially implicit random diffusion based on patch size
                              migration.change <- vector()
                              for(ii in 1:nrow(x)){
                                sum.vect <- vector()
                                for(jj in 1:nrow(x)){
                                  sum.vect[jj] <- x$abun[jj]
                                }
                                migration.change[ii] <- ((1/n.patches)*(sum(sum.vect)-sum.vect[ii]))-(1-(1/n.patches))*x$abun[ii]
                              }
                              migration.final <- targeted.m*migration.change
                              x$abun <- x$abun + migration.final
                            }
                            return(x)
                          })
                        }
                        
                        # Reduce results to single data frame
                        data <- tibble()
                        for(i in 1:length(all.targ.results)){
                          temp.data <- rbindlist(all.targ.results[[i]])
                          data <- bind_rows(data, temp.data)
                          data
                        }
                        targeted.sims.list[[w]] <- data
                        
                        ## Simulate non-targeted species ####
                        
                        # BENCHMARK START ####
                        start.bench <- Sys.time() 
                        
                        # New code
                        # Define growth function
                        nt.growth.function <- growth.function.list[[non.targeted.growth.option]]
                        
                        # Define habitat damage function
                        nt.damage.function <- damage.function.list[[damage.option]]
                        
                        # Create data structures for saving results
                        nt.ready.list <- non.targ.spec.list
                        all.non.targ.results <- list()
                        
                        # Mark which cells are spared to facilitate plotting later
                        nt.ready.list <- lapply(nt.ready.list, function(x){
                          x$spared <- MPA.list[[unique(x$sparing)+1]] # Mark which cells are spared
                          return(x)
                        })
                        
                        # Record carrying capacities before habitat is damaged
                        nt.ready.list <- lapply(nt.ready.list, function(x){
                          x$pristine.k <- x$k
                          return(x)
                        })
                        
                        pb <- progress_bar$new(format = "  Simulating non-targeted species [:bar] :percent ETA: :eta",
                                               total = n.time,
                                               clear = FALSE)
                        pb$tick(0)
                        
                        for (i in 1:n.time){
                          pb$tick()
                          cat(" Simulating time.step", i, "of", n.time) # Uncomment if required for troubleshooting
                          all.non.targ.results[[i]] <- nt.ready.list
                          
                          # Time step
                          nt.ready.list <- lapply(nt.ready.list, function(x){
                            x$time.step <- x$time.step+1
                            return(x)
                          })
                          
                          # Collect appropriate effort
                          nt.ready.list <- lapply(nt.ready.list, function(x){
                            effort <- data %>%
                              filter(catch == x$catch &
                                       sparing == x$sparing &
                                       simulation == x$simulation &
                                       #patch == x$patch &
                                       time.step == x$time.step) %>% 
                              pull(effort)
                            if(length(effort) > 0){
                              x$effort <- effort
                            } else {
                              x <- NULL
                            }
                            return(x)
                          })
                          nt.ready.list <- compact(nt.ready.list)
                          
                          # Apply model
                          nt.ready.list <- lapply(nt.ready.list, function(x){
                            # New function code
                            # Add growth
                            whole.pop <- sum(x$abun)
                            whole.k <- sum(x$k)
                            x$abun <- nt.growth.function(local.n = x$abun,
                                                         seascape.n = whole.pop,
                                                         seascape.k = whole.k,
                                                         r = x$r,
                                                         n.patches = n.patches,
                                                         local.k = x$k,
                                                         species.s = global.survival,
                                                         species.a = x$a*x$k)
                            
                            # Convert NA rows to 0
                            x[is.na(x)] <- 0
                            
                            # Restore negative populations to 0
                            x$abun[x$abun < 0] <- 0
                            
                            # Subtract catch based on effort in patch
                            x$abun <- x$abun - (x$abun*x$q*x$effort)
                            
                            # Restore negative populations to 0
                            x$abun[x$abun<0] <- 0
                            
                            # Add/subtract migration
                            return(x)
                          })
                          
                          # Apply habitat damage
                          nt.ready.list <- lapply(nt.ready.list, function(x){
                            x$k <- nt.damage.function(prist.K = x$pristine.k,
                                                      curr.K = x$k,
                                                      dep.K = dep.K,
                                                      n.trawls = x$effort,
                                                      rep.K = rep.K)
                            
                            # Restore negative Ks to 0
                            x[is.na(x)] <- 0
                            x$k[x$k < 0] <- 0
                            
                            return(x)
                          })
                          
                          # Calculate migration for remaining valid simulations
                          nt.ready.list <- lapply(nt.ready.list, function(x){
                            # Add/subtract migration
                            if(mig.option==1){ # 1 = No migration
                              # Do nothing
                            } else if(mig.option==2){ # 2 = Spatially implicit random diffusion based on patch habitat quality
                              migration.change <- vector()
                              for(ii in 1:nrow(x)){
                                sum.vect <- vector()
                                for(jj in 1:nrow(x)){
                                  sum.vect[jj] <- x$abun[jj]
                                }
                                migration.change[ii] <- ((x$k[ii]/sum(x$k))*(sum(sum.vect)-sum.vect[ii]))-(1-((x$k[ii])/sum(x$k)))*x$abun[ii]
                              }
                              migration.final <- x$m*migration.change
                              x$abun <- x$abun + migration.final
                            } else if(mig.option==3){ # 3 = Spatially implicit random diffusion based on patch size
                              migration.change <- vector()
                              for(ii in 1:nrow(x)){
                                sum.vect <- vector()
                                for(jj in 1:nrow(x)){
                                  sum.vect[jj] <- x$abun[jj]
                                }
                                migration.change[ii] <- ((1/n.patches)*(sum(sum.vect)-sum.vect[ii]))-(1-(1/n.patches))*x$abun[ii]
                              }
                              migration.final <- x$m*migration.change
                              x$abun <- x$abun + migration.final
                            }
                            return(x)
                          })
                        }
                        
                        # Reduce results to single data frame
                        nt.data <- tibble()
                        for(i in 1:length(all.non.targ.results)){
                          temp.data <- rbindlist(all.non.targ.results[[i]])
                          nt.data <- bind_rows(nt.data, temp.data)
                          nt.data
                        }
                        non.targeted.sims.list[[w]] <- nt.data
                        
                        ## Carbon analysis ####
                        
                        ### Produce data for carbon at t = 1 ####
                        
                        catch <- catch.spec
                        sparing <- spare.spec
                        simulation <- w
                        patch <- 1:n.patches
                        time.step <- 1
                        carbon.start <- carbon.start
                        carbon.data <- expand_grid(catch,sparing,simulation,patch,time.step,carbon.start)
                        carbon.data$carbon.pres <- carbon.data$carbon.start
                        carbon.data$effort <- 0
                        carbon.data$carbon.expo <- carbon.data$carbon.pres * carbon.data$effort
                        carbon.data$carbon.dist <- 0
                        carbon.data$carbon.tran <- carbon.data$carbon.dist * (1 - pcred)
                        carbon.data$carbon.remi <- carbon.data$carbon.dist * eps
                        carbon.data$carbon.labi <- carbon.start * (pdepth - pdepth * pcred - rs)
                        carbon.data$carbon.gain <- ((96.04*(10^6))/(54.89*(10^9)))*carbon.start
                        carbon.data$carbon.lost <- 0
                        
                        # Split data frame into individual simulations
                        carbon.list <- group_split(carbon.data, catch, sparing, simulation)
                        
                        ### Simulate carbon changes ####
                        
                        # Prepare lists
                        carbon.ready.list <- carbon.list
                        all.carbon.results <- list()
                        
                        # Mark which cells are spared to facilitate plotting later
                        carbon.ready.list <- lapply(carbon.ready.list, function(x){
                          x$spared <- MPA.list[[unique(x$sparing)+1]] # Mark which cells are spared
                          return(x)
                        })
                        
                        # pb <- progress_bar$new(format = "  Simulating carbon [:bar] :percent ETA: :eta",
                        #                        total = n.time,
                        #                        clear = FALSE)
                        # pb$tick(0)
                        
                        # Simulate through time
                        for (i in 1:n.time){
                          # pb$tick()
                          #cat(" Simulating time.step", i, "of", n.time) # Uncomment if required for troubleshooting
                          all.carbon.results[[i]] <- carbon.ready.list
                          
                          # Time step
                          carbon.ready.list <- lapply(carbon.ready.list, function(x){
                            x$time.step <- x$time.step+1
                            return(x)
                          })
                          
                          # Collect appropriate effort
                          carbon.ready.list <- lapply(carbon.ready.list, function(x){
                            effort <- data %>%
                              filter(catch == x$catch &
                                       sparing == x$sparing &
                                       simulation == x$simulation &
                                       #patch == x$patch &
                                       time.step == x$time.step) %>% 
                              pull(effort)
                            if(length(effort) > 0){
                              x$effort <- effort
                            } else {
                              x <- NULL
                            }
                            return(x)
                          })
                          carbon.ready.list <- compact(carbon.ready.list)
                          
                          # # Apply carbon function (alternative one)
                          # carbon.ready.list <- lapply(carbon.ready.list, function(x){
                          #   x$carbon.pres <- new.carb.func(SAR = x$effort,
                          #                                   pdepth = pdepth,
                          #                                   pcred = pcred,
                          #                                   plab = plab,
                          #                                   k = carb.k,
                          #                                   t = carb.t,
                          #                                   pres.carbon = x$carbon.pres)
                          #   #x$carbon.pres[x$carbon.pres<0] <- 0
                          #   return(x)
                          # })
                          
                          # Apply carbon function (alternative two)
                          carbon.ready.list <- lapply(carbon.ready.list, function(x){
                            
                            # Calculate carbon.dist
                            # Check to make sure this meets assumptions about limited exposure
                            # May need to multiply whole thing by effort, in which case may not need to multiply by effort later
                            x$carbon.dist <- x$carbon.dist -
                              x$carbon.tran -
                              x$carbon.remi +
                              x$carbon.labi +
                              x$carbon.gain
                            
                            # Calculate carbon.lost
                            x$carbon.lost <- x$carbon.tran + x$carbon.remi
                            
                            # Calculate carbon.pres
                            x$carbon.pres <- x$carbon.pres + x$carbon.gain - (x$carbon.lost *  x$effort)
                            
                            # Update carbon.tran
                            x$carbon.tran <- x$carbon.dist * (1 - pcred)
                            
                            # Update carbon.remi
                            x$carbon.remi <- x$carbon.dist * eps
                            
                            return(x)
                          })
                        }
                        
                        # Reduce results to single data frame
                        carbon.data <- tibble()
                        for(i in 1:length(all.carbon.results)){
                          temp.data <- rbindlist(all.carbon.results[[i]])
                          carbon.data <- bind_rows(carbon.data, temp.data)
                          carbon.data
                        }
                        carbon.sims.list[[w]] <- carbon.data
                        
                        # View carbon change over time
                        
                        ggplot(carbon.data, aes(x = time.step, colour = as.factor(patch), y = carbon.pres, group = as.factor(patch), linetype = as.factor(spared))) +
                          facet_grid(as.factor(sparing)~as.factor(catch/max(catch))) +
                          geom_line() +
                          labs(y = "Carbon", 
                               x = "Time") +
                          geom_line() +
                          scale_colour_discrete(name = "Patch") +
                          scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
                          theme_bw() +
                          scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
                          scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
                          geom_hline(yintercept = 0, linetype = "dotted")
                        
                        ### View sparing/sharing results over time ####
                        
                        summary <- carbon.data %>% 
                          group_by(sparing,catch,time.step) %>% 
                          summarise(total = sum(carbon.pres))
                        
                        ggplot(data = summary,
                               aes(x=time.step, y = total)) +
                          geom_line() +
                          facet_grid(as.factor(sparing)~as.factor(catch/max(catch))) +
                          labs(y = "Carbon", 
                               x = "Time") +
                          theme_bw() +
                          scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
                          scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
                          geom_hline(yintercept = 0, linetype = "dotted")
                        
                        ### View sparing/sharing results at final time ####
                        
                        final.summary <- filter(summary, time.step == n.time)
                        
                        ggplot(data = final.summary,
                               aes(x=sparing/n.patches, y = total, colour = as.factor(catch/max(catch)), group = as.factor(catch/max(catch)))) +
                          geom_line() +
                          geom_point() +
                          scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
                          labs(y = "Carbon", 
                               x = "Proportion of seascape spared") +
                          xlim(0,1) +
                          geom_hline(yintercept = 0, linetype = "dotted") +
                          theme_bw()
                        
                        #list(targeted.sims.list[[w]], non.targeted.sims.list[[w]], carbon.sims.list[[w]], all.species.habitat.list[[w]]) #XYZ
                        list(targeted.sims.list[[w]], non.targeted.sims.list[[w]], carbon.sims.list[[w]]) #XYZ
                      } #XYZ

# Note parallel list object
# grand.list

# End loop auxiliaries #XYZ
stopCluster(my.cluster)

# Split parallel list object into separate lists
targeted.sims.list <- list()  #XYZ
non.targeted.sims.list <- list() #XYZ
carbon.sims.list <- list() #XYZ
for(i in 1:n.sims){ #XYZ
  targeted.sims.list[[i]] <- grand.list[[i]][[1]] #XYZ
  non.targeted.sims.list[[i]]  <- grand.list[[i]][[2]] #XYZ
  carbon.sims.list[[i]]  <- grand.list[[i]][[3]]
  #all.species.habitat.list[[i]]  <- grand.list[[i]][[4]] #XYZ
} #XYZ

# Reduce lists to single data frames
targeted.sim.data <- rbindlist(targeted.sims.list)
non.targeted.sim.data <- rbindlist(non.targeted.sims.list)
carbon.sim.data <- rbindlist(carbon.sims.list)

# Remove lists to free up memory
rm(targeted.sims.list)
rm(non.targeted.sims.list)
rm(carbon.sims.list)
rm(grand.list) #marker

# Change columns to appropriate types and calculate catch as proportion of MSY
# Targeted data
#targeted.sim.data$MSYProp <- as.factor(targeted.sim.data$catch/max(targeted.sim.data$catch))
targeted.sim.data <- targeted.sim.data %>%
  group_by(simulation) %>%
  mutate(MSYProp = catch / max(catch))
targeted.sim.data$MSYProp <- as.factor(targeted.sim.data$MSYProp)
targeted.sim.data$sparedProp <- targeted.sim.data$sparing/n.patches
#targeted.sim.data$catch <- as.factor(targeted.sim.data$catch)
#targeted.sim.data$sparing <- as.factor(targeted.sim.data$sparing)
targeted.sim.data$simulation <- as.factor(targeted.sim.data$simulation)
targeted.sim.data$patch <- as.factor(targeted.sim.data$patch)
targeted.sim.data$spared <- as.factor(targeted.sim.data$spared)
# Non-targeted data
#non.targeted.sim.data$MSYProp <- as.factor(non.targeted.sim.data$catch/max(non.targeted.sim.data$catch))
non.targeted.sim.data <- non.targeted.sim.data %>%
  group_by(simulation) %>%
  mutate(MSYProp = catch / max(catch))
non.targeted.sim.data$MSYProp <- as.factor(non.targeted.sim.data$MSYProp)
non.targeted.sim.data$sparedProp <- non.targeted.sim.data$sparing/n.patches
#non.targeted.sim.data$catch <- as.factor(non.targeted.sim.data$catch)
#non.targeted.sim.data$sparing <- as.factor(non.targeted.sim.data$sparing)
non.targeted.sim.data$simulation <- as.factor(non.targeted.sim.data$simulation)
non.targeted.sim.data$patch <- as.factor(non.targeted.sim.data$patch)
non.targeted.sim.data$spared <- as.factor(non.targeted.sim.data$spared)
# Carbon data
carbon.sim.data$MSYProp <- as.factor(carbon.sim.data$catch/max(carbon.sim.data$catch))
carbon.sim.data$sparedProp <- carbon.sim.data$sparing/n.patches
#carbon.sim.data$catch <- as.factor(carbon.sim.data$catch)
#carbon.sim.data$sparing <- as.factor(carbon.sim.data$sparing)
carbon.sim.data$simulation <- as.factor(carbon.sim.data$simulation)
carbon.sim.data$patch <- as.factor(carbon.sim.data$patch)
carbon.sim.data$spared <- as.factor(carbon.sim.data$spared)

# Optimisation testing zone #### #XYZ
#old.data <- non.targeted.sim.data # XYZ
#all(old.data == non.targeted.sim.data) #XYZ
# XYZ
#gdata::keep(old.data, sure = TRUE) # XYZ # Run this when starting a new optimisation run to see if anything changed in the output
#end.bench <- Sys.time()
#tot.bench <- end.bench - start.bench
#tot.bench
beep()

# BENCHMARK END ####