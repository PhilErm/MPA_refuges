# Script details ####

# Once all simulations have been run, run this script to generate the figures used in the manuscript.

# This script also contains a small calculation of how many species are affected by the cap of growth rate to 2.

# To use the script:
# 1. Set working directory to the script location.
# 2. Make sure you have run all simulations you wish to run. Refer to the instructions in the base directory README in the base directory 
# if you are not sure if you have done so
# 3. Run all lines of code. Figures will be deposited into "../figs/processed".

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
library(ggcorrplot)
library(patchwork)
library(ggview)
library(ggtext)
library(ggrepel)

# Commence loop through sensitivity analyses

sens.names <- list.files("../../rds/hpc-work/SSS_custom_model/summaries")
for(i in 1:length(sens.names)){
  curr.analysis <- sens.names[i]
  summary.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "summary_files", sep = "/")
  effort.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "effort_files", sep = "/")
  fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
  dir.create(fig.path)
  
  # Mean data summary ####
  
  ## Load data ####
  
  # Load biodiversity data
  file.names.sum <- list.files(summary.path)
  data.list <- list()
  data.mean.list <- list()
  for(i in 1:length(file.names.sum)){
    load(paste(summary.path, file.names.sum[i], sep = "/")) # Load the data file (all simulations)
    data.list[[paste(file.names.sum[i])]] <- final.summary.thresh.save # Save the relevant part of it in a list
    data.list[[i]]$overlap <- str_extract(file.names.sum[i], "\\d\\d") # Add the amount of overlap in it to it
    data.list[[i]]$damage <- !str_detect(file.names.sum[i], "_adam_") # Add whether damage is on to it
    data.mean.list[[paste(file.names.sum[i])]] <- final.summary.thresh.mean.save # Load the data file (mean simulation)
    data.mean.list[[i]]$overlap <- str_extract(file.names.sum[i], "\\d\\d") # Save the relevant part of it in a list
    data.mean.list[[i]]$damage <- !str_detect(file.names.sum[i], "_adam_") # Add whether damage is on to it
  }
  
  # Load effort data
  file.names.eff <- list.files(effort.path)
  data.effort.list <- list()
  data.effort.mean.list <- list()
  for(i in 1:length(file.names.eff)){
    load(paste(effort.path, file.names.eff[i], sep = "/")) # Load the data file (all simulations)
    data.effort.list[[paste(file.names.eff[i])]] <- final.effort.save # Save the relevant part of it in a list
    data.effort.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Add the amount of overlap in it to it
    data.effort.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
    data.effort.mean.list[[paste(file.names.eff[i])]] <- final.effort.mean.save # Load the data file (mean simulation)
    data.effort.mean.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Save the relevant part of it in a list
    data.effort.mean.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
  }
  
  ## Process data ####
  
  # Biodiversity data
  # Isolate simulations with highest performing MPA size
  processed.mean.list <- list()
  processed.mean.list <- lapply(data.mean.list, function(x){
    largest.MPA.values <- x %>%
      group_by(MSYProp, biodiversity.measure) %>% # For each catch and biodiversity measure
      filter(sparedProp >= 0.5) %>% # Filter to only when fishery starts getting spared
      filter(mean.mean == max(mean.mean)) # Filter to only highest performing MPA
    share.values <- x %>%
      group_by(MSYProp, biodiversity.measure) %>% # For each catch and biodiversity measure
      filter(sparing == 0) %>% # Filter to when there is no sparing
      mutate(share.bio = mean.mean) %>% # Determine the biodiversity in that case
      select(-sparing, -sparedProp, -time.step, -mean.mean) # Remove to facilitate joining data
    comb.values <- largest.MPA.values %>%
      right_join(share.values, by = c("MSYProp", "biodiversity.measure", "overlap", "damage")) %>% # Join the sparing and sharing datasets
      mutate(percent.change = ((mean.mean/share.bio)*100)-100) %>% # Calculate how much the best MPA improves biodiversity over no MPA
      filter(MSYProp == 1) # In case there is more than one catch present (Note: I think; may now be redundant)
    return(comb.values)
  })
  
  # Convert data from list to data frame
  combined.mean.list <- rbindlist(processed.mean.list)
  
  # Effort data
  processed.effort.mean.list <- list()
  processed.effort.mean.list <- lapply(data.effort.mean.list, function(x){
    effort.processed <- x %>% 
      filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
      filter(biodiversity.measure == "arith.mean") %>% # We are not interested in the geometric mean effort
      filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
      group_by(damage) %>% 
      mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
    return(effort.processed)
  })
  
  # Convert data from list to data frame
  combined.effort.mean.list <- rbindlist(processed.effort.mean.list)
  
  # To facilitate combining our effort data with our biodiversity data, we need effort counts for each level of overlap.
  # Since effort doesn't change with overlap, why just need to replicate our limited effort dataset to apply
  # to each overlap.
  
  # Create new effort datasets
  ovr00 <- combined.effort.mean.list
  ovr02 <- combined.effort.mean.list
  ovr04 <- combined.effort.mean.list
  ovr06 <- combined.effort.mean.list
  ovr08 <- combined.effort.mean.list
  ovr10 <- combined.effort.mean.list
  
  # Identify level of overlap each dataset applies to
  ovr02$overlap <- "02"
  ovr04$overlap <- "04"
  ovr06$overlap <- "06"
  ovr08$overlap <- "08"
  ovr10$overlap <- "10"
  
  # Bind datasets into one data frame
  effort.with.ovr <- rbind.data.frame(ovr00, ovr02, ovr04, ovr06, ovr08, ovr10)
  
  # Combine both the biodiversity and effort datasets into one data frame
  plot.data <- effort.with.ovr %>%
    select(c(-sparing, -time.step, -MSYProp, -biodiversity.measure, -mean.mean)) %>% 
    rename(percent.change.effort = percent.change) %>% # Create independent column for effort
    right_join(combined.mean.list, by = c("sparedProp", "damage", "overlap")) # Combine datasets
  
  stack.data <- plot.data # Rename dataset due to legacy code
  
  # Remove overlap == "00"; the case is trivial
  stack.data <- stack.data %>% 
    filter(overlap != "00")
  
  # Change order of factors to facilitate plotting
  stack.data$damage <- as.factor(stack.data$damage)
  #levels(stack.data$damage)
  stack.data$damage <- factor(stack.data$damage, levels = c("TRUE", "FALSE"))
  
  # Create separate arithmetic mean dataset
  arith <- stack.data %>% 
    filter(biodiversity.measure == "prop.arith")
  
  # Create separate geometric mean dataset
  geom <- stack.data %>% 
    filter(biodiversity.measure == "prop.geom")
  
  
  ## Plot data ####
  
  # Biodiversity, arithmetic
  bio.arith <- ggplot(arith, aes(x = overlap, y = percent.change, group = damage, colour = damage, shape = damage, label = percent.change)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    #geom_text() +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Biodiversity gain (%) at \noptimal MPA size") +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    #coord_flip(clip = "off") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  bio.arith
  
  # Biodiversity, geometric
  bio.geom <- ggplot(geom, aes(x = overlap, y = percent.change, group = damage, colour = damage, shape = damage, label = percent.change)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    #geom_text() +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    #coord_flip(clip = "off") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  bio.geom
  
  # Highest performing MPA, arithmetic
  MPA.arith <- ggplot(arith, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage, colour = damage, shape = damage)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Optimal coverage of fishery \nground by MPAs (%)") +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position="bottom")
  MPA.arith
  
  # Highest performing MPA, geometric
  MPA.geom <- ggplot(geom, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage, colour = damage, shape = damage)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position="bottom")
  MPA.geom
  
  # Effort, arithmetic
  effort.arith <- ggplot(arith, aes(x = overlap, y = percent.change.effort, group = damage, colour = damage, shape = damage)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Effort increase (%) at \noptimal MPA size") +
    scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position="bottom")
  effort.arith
  
  # Effort, geometric
  effort.geom <- ggplot(geom, aes(x = overlap, y = percent.change.effort, group = damage, colour = damage, shape = damage)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(size = 5) +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_colour_viridis_d(name = "Non-target species habitat damage", labels = c("On", "Off"), end = 0.8, guide = guide_legend(reverse = TRUE), direction = -1) +
    scale_shape_manual(name = "Non-target species habitat damage", labels = c("On", "Off"), values = c("\u25E4", "\u25E2"), guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position="top")
  effort.geom
  
  # Compiled plot
  comp.plot <- ((bio.arith) + (bio.geom)) / ((MPA.arith) + (MPA.geom)) / ((effort.arith) + (effort.geom)) +
    plot_annotation(tag_levels = 'A') +
    plot_layout(guides = 'collect') &
    theme(legend.position="bottom",
          legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          plot.tag = element_text(size = 24))
  
  # Test plot size
  # ggview(plot = comp.plot,
  #        device = "png",
  #        width = 7,
  #        height = 10,
  #        units = "in")
  
  cairo_pdf(paste(fig.path, "mean_results.pdf", sep = "/"), family="Arial Unicode MS", width = 7, height = 10)
  print(comp.plot)
  dev.off()
  
  # Detailed data summary ####
  
  ## Process data ####
  
  # Isolate simulations with highest performing MPA size
  # Individual simulations
  processed.list <- list()
  processed.list <- lapply(data.list, function(x){
    x <- x %>% 
      filter(MSYProp == 1) %>% 
      filter(sparedProp >= 0.5)
    return(x)
  })
  
  # Mean simulation
  processed.mean.list <- list()
  processed.mean.list <- lapply(data.mean.list, function(x){
    x <- x %>% 
      filter(MSYProp == 1) %>% 
      filter(sparedProp >= 0.5)
    return(x)
  })
  
  ## Results without habitat damage ####
  
  ### Combine data ####
  
  combined.list <- rbindlist(processed.list)
  combined.mean.list <- rbindlist(processed.mean.list)
  
  combined.list <- combined.list %>% 
    filter(damage == FALSE) %>% 
    filter(overlap != "00")
  combined.mean.list <- combined.mean.list %>% 
    filter(damage == FALSE) %>% 
    filter(overlap != "00")
  
  # combined.list <- combined.list %>% 
  #   filter(geom.mean != 0)
  
  
  ### Plot data ####
  
  if(length(file.names.sum) == 12){
    p1 <- ggplot() +
      ggtitle("Without non-target species habitat damage") +
      geom_line(data = combined.list,
                aes(x=as.factor(sparedProp), 
                    y = biodiversity.value, 
                    group = interaction(simulation, MSYProp),
                    colour = "b"),
                alpha = 0.2) +
      geom_line(data = combined.mean.list,
                aes(x=as.factor(sparedProp),
                    y=mean.mean,
                    group=MSYProp,
                    colour = "a"),
                size = 1.05) +
      facet_grid(biodiversity.measure ~ overlap, labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"),
                                                                     overlap = c("00" = "Range overlap = 0",
                                                                                 "02" = "Range overlap = 0.2",
                                                                                 "04" = "Range overlap = 0.4",
                                                                                 "06" = "Range overlap = 0.6",
                                                                                 "08" = "Range overlap = 0.8",
                                                                                 "10" = "Range overlap = 1"))) +
      labs(y = "Biodiversity") +
      scale_x_discrete(name = element_blank(), labels = c(0, 10, 20, 30, 40, 50, 60, 70)) +
      scale_color_manual(
        name = element_blank(),
        labels = c("Average of all simulations", "Individual simulations"),
        values = c("a" = "#FF6240", "b" = "black")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,1.1)) +
      geom_hline(data = combined.mean.list %>% filter(sparedProp == 0.5), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
      theme_bw() +
      theme(legend.position="none", text = element_text(size = 14))
    p1
  }

  ## Results with habitat damage ####
  
  ### Combine data ####
  
  combined.list <- rbindlist(processed.list)
  combined.mean.list <- rbindlist(processed.mean.list)
  
  combined.list <- combined.list %>% 
    filter(damage == TRUE) %>% 
    filter(overlap != "00")
  combined.mean.list <- combined.mean.list %>% 
    filter(damage == TRUE) %>% 
    filter(overlap != "00")
  
  ### Plot data ####
  
  p2 <- ggplot() +
    ggtitle("With non-target species habitat damage") +
    geom_line(data = combined.list,
              aes(x=as.factor(sparedProp), 
                  y = biodiversity.value, 
                  group = interaction(simulation, MSYProp),
                  colour = "b"),
              alpha = 0.2) +
    geom_line(data = combined.mean.list,
              aes(x=as.factor(sparedProp),
                  y=mean.mean,
                  group=MSYProp,
                  colour = "a"),
              size = 1.05) +
    facet_grid(biodiversity.measure ~ overlap, labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"),
                                                                   overlap = c("00" = "Range overlap = 0",
                                                                               "02" = "Range overlap = 0.2",
                                                                               "04" = "Range overlap = 0.4",
                                                                               "06" = "Range overlap = 0.6",
                                                                               "08" = "Range overlap = 0.8",
                                                                               "10" = "Range overlap = 1"))) +
    labs(y = "Biodiversity") +
    scale_x_discrete(name = "Proportion of fishery ground reserved in MPAs (%)", labels = c(0, 10, 20, 30, 40, 50, 60, 70)) +
    scale_color_manual(
      name = element_blank(),
      labels = c("Average of all simulations", "Individual simulations"),
      values = c("a" = "#FF6240", "b" = "black")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,1.1)) +
    geom_hline(data = combined.mean.list %>% filter(sparedProp == 0.5), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
    theme_bw() +
    theme(legend.position="bottom", text = element_text(size = 14),
          legend.text=element_text(size=14))
  p2
  
  # Combine plots
  if(length(file.names.sum) == 12){
    detailed.plot <- (p1 / p2)
    # ggview(plot = detailed.plot,
    #        device = "png",
    #        width = 30,
    #        height = 20,
    #        units = "cm")
    ggsave(paste(fig.path, "detailed_results.pdf", sep = "/"), units = "cm", width = 30, height = 20)
  } else {
    detailed.plot <- p2
    # ggview(plot = detailed.plot,
    #        device = "png",
    #        width = 30,
    #        height = 10,
    #        units = "cm")
    ggsave(paste(fig.path, "detailed_results.pdf", sep = "/"), units = "cm", width = 30, height = 15)
  }
}

# Effort figure ####

## Load data ####

sens.names <- c("default", "targdam")
curr.analysis <- sens.names[1]
summary.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "summary_files", sep = "/")
effort.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "effort_files", sep = "/")
fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
dir.create(fig.path)

# Load effort data
file.names.eff <- list.files(effort.path)
data.effort.list <- list()
data.effort.mean.list <- list()
for(i in 1:length(file.names.eff)){
  load(paste(effort.path, file.names.eff[i], sep = "/")) # Load the data file (all simulations)
  data.effort.list[[paste(file.names.eff[i])]] <- final.effort.save # Save the relevant part of it in a list
  data.effort.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Add the amount of overlap in it to it
  data.effort.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
  data.effort.mean.list[[paste(file.names.eff[i])]] <- final.effort.mean.save # Load the data file (mean simulation)
  data.effort.mean.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Save the relevant part of it in a list
  data.effort.mean.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
}

# Effort data
processed.effort.mean.list <- list()
processed.effort.mean.list <- lapply(data.effort.mean.list, function(x){
  effort.processed <- x %>% 
    filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
    filter(biodiversity.measure == "arith.mean") %>% # We are not interested in the geometric mean effort
    filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
    group_by(damage) %>% 
    mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
  return(effort.processed)
})

# Convert data from list to data frame
combined.effort.mean.list <- rbindlist(processed.effort.mean.list)
effort.no.damage <- combined.effort.mean.list

sens.names <- c("default", "targdam")
curr.analysis <- sens.names[2]
summary.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "summary_files", sep = "/")
effort.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "effort_files", sep = "/")
fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
dir.create(fig.path)

# Load effort data
file.names.eff <- list.files(effort.path)
data.effort.list <- list()
data.effort.mean.list <- list()
for(i in 1:length(file.names.eff)){
  load(paste(effort.path, file.names.eff[i], sep = "/")) # Load the data file (all simulations)
  data.effort.list[[paste(file.names.eff[i])]] <- final.effort.save # Save the relevant part of it in a list
  data.effort.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Add the amount of overlap in it to it
  data.effort.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
  data.effort.mean.list[[paste(file.names.eff[i])]] <- final.effort.mean.save # Load the data file (mean simulation)
  data.effort.mean.list[[i]]$overlap <- str_extract(file.names.eff[i], "\\d\\d") # Save the relevant part of it in a list
  data.effort.mean.list[[i]]$damage <- !str_detect(file.names.eff[i], "_adam_") # Add whether damage is on to it
}

# Effort data
processed.effort.mean.list <- list()
processed.effort.mean.list <- lapply(data.effort.mean.list, function(x){
  effort.processed <- x %>% 
    filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
    filter(biodiversity.measure == "arith.mean") %>% # We are not interested in the geometric mean effort
    filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
    group_by(damage) %>% 
    mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
  return(effort.processed)
})

# Convert data from list to data frame
combined.effort.mean.list <- rbindlist(processed.effort.mean.list)
effort.damage <- combined.effort.mean.list

effort.both.damage <- bind_rows(effort.no.damage, effort.damage, .id = "targ.damage")
effort.both.damage <- effort.both.damage %>% filter(damage == FALSE)

# Plot
effort.plot <- ggplot(effort.both.damage, aes(x = ((sparedProp-0.5)*2)*100, y = mean.mean, colour = targ.damage, label = paste(round(percent.change), "%", sep = ""))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(name = "Proportion of fishery ground reserved in MPAs (%)", limits = c(0, 52)) +
  ylim(c(0,18)) +
  scale_colour_viridis_d(name = "Target species \nexperiences \nhabitat damage", labels = c("No", "Yes"), end = 0.8, guide = guide_legend(reverse = F), direction = 1) +
  #scale_shape_discrete(name = "Target species \nexperiences \nhabitat damage", labels = c("No", "Yes"), guide = guide_legend(reverse = F)) +
  geom_hline(data = effort.both.damage %>% filter(sparedProp == 0.5) %>% filter(targ.damage == 1), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
  geom_hline(data = effort.both.damage %>% filter(sparedProp == 0.5) %>% filter(targ.damage == 2), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
  geom_text(nudge_y = 0.65, show.legend = FALSE, size = 4) +
  labs(y = "Total seascape-wide fishing effort (*E*)") +
  theme_bw() +
  theme(axis.title.y = element_markdown(size = 14),
        text = element_text(size = 14))
effort.plot

# Test plot size
# ggview(plot = effort.plot,
#        device = "png",
#        width = 6,
#        height = 4,
#        units = "in")

cairo_pdf("../figs/processed/effort_results.pdf", family="Arial Unicode MS", width = 6, height = 4)
print(effort.plot)
dev.off()
ggsave("../figs/processed/effort_results.pdf", effort.plot, units = "in", width = 6, height = 4)

# Collapses figure ####

## Load data ####

sens.names <- c("default")
curr.analysis <- sens.names[1]
summary.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "summary_files", sep = "/")
effort.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "effort_files", sep = "/")
habitat.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "habitat_files", sep = "/")
collapse.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "collapse_files", sep = "/")
fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
dir.create(fig.path)

# Load collapse data
file.names.col <- list.files(collapse.path)
data.collapse.list <- list()
data.collapse.mean.list <- list()
for(i in 1:length(file.names.col)){
  load(paste(collapse.path, file.names.col[i], sep = "/")) # Load the data file (all simulations)
  data.collapse.list[[paste(file.names.col[i])]] <- final.collapsed.save # Save the relevant part of it in a list
  data.collapse.list[[i]]$overlap <- str_extract(file.names.col[i], "\\d\\d") # Add the amount of overlap in it to it
  data.collapse.list[[i]]$damage <- !str_detect(file.names.col[i], "_adam_") # Add whether damage is on to it
  data.collapse.mean.list[[paste(file.names.col[i])]] <- final.collapsed.mean.save # Load the data file (mean simulation)
  data.collapse.mean.list[[i]]$overlap <- str_extract(file.names.col[i], "\\d\\d") # Save the relevant part of it in a list
  data.collapse.mean.list[[i]]$damage <- !str_detect(file.names.col[i], "_adam_") # Add whether damage is on to it
}
# Collapse individual data
processed.collapse.list <- list()
processed.collapse.list <- lapply(data.collapse.list, function(x){
  collapse.processed <- x %>% 
    filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
    filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
    group_by(damage, simulation) %>% 
    #mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
    return(collapse.processed)
})

# Collapse mean data
processed.collapse.mean.list <- list()
processed.collapse.mean.list <- lapply(data.collapse.mean.list, function(x){
  collapse.mean.processed <- x %>% 
    filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
    filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
    group_by(damage) %>% 
    #mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
  return(collapse.mean.processed)
})

# Convert data from list to data frame
combined.collapse.list <- rbindlist(processed.collapse.list)
collapse.damage <- filter(combined.collapse.list, overlap != "00")

combined.collapse.mean.list <- rbindlist(processed.collapse.mean.list)
collapse.mean.damage <- filter(combined.collapse.mean.list, overlap != "00")

#effort.both.damage <- bind_rows(effort.no.damage, effort.damage, .id = "targ.damage")
#habitat.damage <- habitat.damage %>% filter(damage == TRUE) %>% filter(overlap != "00")

# Plot
ggplot(collapse.mean.damage, aes(x = ((sparedProp-0.5)*2)*100, y = prop.collapsed.mean*80, colour = overlap)) +
  geom_line() +
  scale_colour_viridis_d(name = "Range overlap\nbetween target\nand non-target\nspecies", labels = c("20%", "40%", "60%", "80%", "100%")) +
  labs(y = "Mean extirpated non-target species",
       x = "Proportion of fishery ground reserved in MPAs (%)") +
  facet_wrap(~damage, labeller = labeller(damage = c("TRUE" = "With damage", "FALSE" = "Without damage"))) +
  theme_bw()

ggplot(collapse.damage, aes(x = ((sparedProp-0.5)*2)*100, y = prop.collapsed*80, colour = overlap, fill = overlap)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_colour_viridis_d(name = "Range overlap\nbetween target\nand non-target\nspecies", labels = c("20%", "40%", "60%", "80%", "100%")) +
  scale_fill_viridis_d(name = "Range overlap\nbetween target\nand non-target\nspecies", labels = c("20%", "40%", "60%", "80%", "100%")) +
  labs(y = "Mean extirpated non-target species",
       x = "Proportion of fishery ground reserved in MPAs (%)") +
  facet_wrap(.~damage, labeller = labeller(damage = c("TRUE" = "With damage", "FALSE" = "Without damage"))) +
  theme_bw()

cairo_pdf("../figs/processed/habitat_results.pdf", family="Arial Unicode MS", width = 6, height = 6)
print(habitat.plot)
dev.off()
ggsave("../figs/processed/habitat_results.pdf", habitat.plot, units = "in", width = 6, height = 6)

# Habitat figure ####

## Load data ####

sens.names <- c("default")
curr.analysis <- sens.names[1]
summary.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "summary_files", sep = "/")
effort.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "effort_files", sep = "/")
habitat.path <- paste("../../rds/hpc-work/SSS_custom_model/summaries", curr.analysis, "habitat_files", sep = "/")
fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
dir.create(fig.path)

# Load habitat data
file.names.hab <- list.files(habitat.path)
data.habitat.list <- list()
data.habitat.mean.list <- list()
for(i in 1:length(file.names.hab)){
  load(paste(habitat.path, file.names.hab[i], sep = "/")) # Load the data file (all simulations)
  data.habitat.list[[paste(file.names.hab[i])]] <- final.habitat.save # Save the relevant part of it in a list
  data.habitat.list[[i]]$overlap <- str_extract(file.names.hab[i], "\\d\\d") # Add the amount of overlap in it to it
  data.habitat.list[[i]]$damage <- !str_detect(file.names.hab[i], "_adam_") # Add whether damage is on to it
  data.habitat.mean.list[[paste(file.names.hab[i])]] <- final.habitat.mean.save # Load the data file (mean simulation)
  data.habitat.mean.list[[i]]$overlap <- str_extract(file.names.hab[i], "\\d\\d") # Save the relevant part of it in a list
  data.habitat.mean.list[[i]]$damage <- !str_detect(file.names.hab[i], "_adam_") # Add whether damage is on to it
}

# Habitat data
processed.habitat.mean.list <- list()
processed.habitat.mean.list <- lapply(data.habitat.mean.list, function(x){
  habitat.processed <- x %>% 
    filter(MSYProp == 1) %>% # In case there is more than one catch present (Note: I think; may now be redundant)
    filter(biodiversity.measure == "prop.arith") %>% # We are not interested in the geometric mean habitat
    filter(sparedProp >= 0.5) %>% # We are only interested in the case where sparing starts impacting the fishery
    group_by(damage) %>% 
    mutate(percent.change = ((mean.mean/min(mean.mean))*100)-100) # Calculate how much the best MPA increases effort over no MPA
  return(habitat.processed)
})

# Convert data from list to data frame
combined.habitat.mean.list <- rbindlist(processed.habitat.mean.list)
habitat.damage <- combined.habitat.mean.list

habitat.damage <- habitat.damage %>% filter(damage == TRUE) %>% filter(overlap != "00")

# Plot
habitat.plot <- ggplot(habitat.damage, aes(x = ((sparedProp-0.5)*2)*100, y = mean.mean, colour = overlap, label = paste(round(percent.change), "%", sep = ""))) +
  geom_hline(data = habitat.damage %>% filter(sparedProp == 0.5) , aes(yintercept = mean.mean, colour = overlap), linetype = "dotted") +
  geom_line() +
  geom_point() +
  scale_x_continuous(name = "Proportion of fishery ground reserved in MPAs (%)", limits = c(0, 52)) +
  scale_colour_viridis_d(name = "Overlap between target \nand non-target species", end = 0.8, guide = guide_legend(reverse = F), direction = 1,
                         labels = c("0.2", "0.4", "0.6", "0.8", "1")) +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  #geom_text(nudge_y = 0.025, show.legend = FALSE) +
  labs(y = "Occupied patch mean habitat quality (*K*)") +
  theme_bw() +
  theme(axis.title.y = element_markdown(size = 14),
        text = element_text(size = 14),
        legend.position = c(0.5, 0.2))
habitat.plot

# Test plot size
# ggview(plot = habitat.plot,
#        device = "png",
#        width = 6,
#        height = 6,
#        units = "in")

#cairo_pdf("../figs/processed/habitat_results.pdf", family="Arial Unicode MS", width = 6, height = 6)
#print(habitat.plot)
#dev.off()
#ggsave("../figs/processed/habitat_results.pdf", habitat.plot, units = "in", width = 6, height = 6)
# final.collapsed.mean <- final.collapsed.mean.save
# final.collapsed <- final.collapsed.save
# 
# p <- ggplot() +
#   #geom_line(data = final.collapsed,
#    #         aes(x=sparedProp, y = prop.collapsed, colour = MSYProp, group = interaction(simulation, MSYProp)), alpha = 0.2) +
#   #geom_point(data = final.collapsed,
#   #           aes(x=sparedProp, y = prop.collapsed, colour = MSYProp, group = interaction(simulation, MSYProp)), alpha = 0.2) +
#   geom_line(data = final.collapsed.mean,
#             aes(x=sparedProp, y = prop.collapsed.mean, colour = MSYProp), size = 1.05) +
#   geom_point(data = final.collapsed.mean,
#              aes(x=sparedProp, y = prop.collapsed.mean, colour = MSYProp), size = 1.05) +
#   scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
#   labs(y = "Proportion of species below collapse threshold", 
#        x = "Proportion of seascape spared") +
#   ylim(0,1) +
#   xlim(0,1) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme_bw()
# plotly::ggplotly(p)
# 
# # Working correctly. Because catch is only 0.67 we expect to see around 1 extinction. x/80=0.01375 = we see 1.1 extinctions. Bang on.

# How many species are capped if r is limited to 2? ####

# This is a mini-script that checks how many species typically get their growth rate reduced to 2.

source("functions.R")
source("species_parameters.R")
num.spec <- 10
dist.pars$log.sd.q <- sd.from.upper(dist.pars$mean.q, dist.pars$upci.q)
dist.pars$log.sd.r <- sd.from.lower(dist.pars$mean.r, dist.pars$lowci.r)
results.vect <- vector()
for(i in 1:50){
  set.seed(i)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  species.df <- all.species %>% 
    summarise(capped = sum(r > 2)/80)
  results.vect[i] <- species.df$capped
}
results.vect
mean(results.vect)
stderror <- function(x) sd(x)/sqrt(length(x))
stderror(results.vect)
