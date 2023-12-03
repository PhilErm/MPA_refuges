# Script details ####

# Once all simulations have been run, run this script to generate the figures used in the manuscript.

# This script also contains a small calculation of how many species are affected by the cap of growth rate to 2.

# To use the script:
# 1. Set working directory to the script location.
# 2. Make sure you have run all simulations you wish to run. Refer to the instructions in the base directory README in the base directory 
# if you are not sure if you have done so
# 3. Run all lines of code. Figures will be deposited into "../figs/processed".

# An additional note: Please have mercy on this script. It was created in stages, with each stage involving a big scope change and extreme
# time constraints, meaning I had to repeatedly opt for hacky solutions, rather than elegant ones.

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

default.arith.list <- list()
default.geom.list <- list()
tdam.arith.list <- list()
tdam.geom.list <- list()

default.detailed.list <- list()
default.detailed.mean.list <- list()
tdam.detailed.list <- list()
tdam.detailed.mean.list <- list()

sens.names <- list.files("../../rds/hpc-work/SSS_custom_model/summaries")
for(j in 1:length(sens.names)){
  (curr.analysis <- sens.names[j])
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
  
  # If curr.analysis name does not contain "tdam", then save into particular lists
  
  # If curr.analysis name does contain "tdam", then save into other set of lists
  
  if(grepl("tdam", curr.analysis, fixed = TRUE)){
    geom$scenario <- curr.analysis
    arith$scenario <- curr.analysis
    tdam.geom.list[[j]] <- geom
    tdam.arith.list[[j]] <- arith
  } else {
    geom$scenario <- curr.analysis
    arith$scenario <- curr.analysis
    default.geom.list[[j]] <- geom
    default.arith.list[[j]] <- arith
  }
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
  
  # Save data for new approach
  
  combined.list <- rbindlist(processed.list)
  combined.mean.list <- rbindlist(processed.mean.list)
  
  if(grepl("tdam", curr.analysis, fixed = TRUE)){
    combined.list$scenario <- curr.analysis
    combined.mean.list$scenario <- curr.analysis
    
    tdam.detailed.list[[j]] <- combined.list
    tdam.detailed.mean.list[[j]] <- combined.mean.list
  } else {
    combined.list$scenario <- curr.analysis
    combined.mean.list$scenario <- curr.analysis
    
    default.detailed.list[[j]] <- combined.list
    default.detailed.mean.list[[j]] <- combined.mean.list
  }
  
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

# View(default.arith.list)
# View(default.geom.list)
# View(tdam.arith.list)
# View(tdam.geom.list)
# # 
# View(default.detailed.list)
# View(default.detailed.mean.list)
# View(tdam.detailed.list)
# View(tdam.detailed.mean.list)

# New main mean figure ####

(elements <- 1:length(default.arith.list))
spectrum <- elements[c(TRUE,FALSE)]
spectrum

# A script that creates the post review new main figure

for(i in spectrum){
  default.arith <- default.arith.list[[i]]
  default.geom <- default.geom.list[[i]]
  targdam.arith <- tdam.arith.list[[i+1]]
  targdam.geom <- tdam.geom.list[[i+1]]
  curr.analysis <- unique(default.arith$scenario)
  
  # Change damage values to descriptive labels
  default.arith.relabel <- default.arith %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "no.species.damaged",
        damage == T ~ "non.target.species.damaged"
      )
    )
  
  default.geom.relabel <- default.geom %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "no.species.damaged",
        damage == T ~ "non.target.species.damaged"
      )
    )
  
  targdam.arith.relabel <- targdam.arith %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "target.species.damaged",
        damage == T ~ "target.&.non.target.species.damaged"
      )
    )
  
  targdam.geom.relabel <- targdam.geom %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "target.species.damaged",
        damage == T ~ "target.&.non.target.species.damaged"
      )
    )
  
  # Create new combined data frames
  arith.new <- rbind.data.frame(default.arith.relabel, targdam.arith.relabel)
  geom.new <- rbind.data.frame(default.geom.relabel, targdam.geom.relabel)
  
  # Filter new combined data frames
  arith.new <- arith.new %>% filter(damage.label != "target.species.damaged")
  geom.new <- geom.new %>% filter(damage.label != "target.species.damaged")
  
  # Keep only distinct rows
  arith.new <- distinct(arith.new)
  geom.new <- distinct(geom.new)
  
  # Create new plots
  bio.arith <- ggplot(arith.new, aes(x = overlap, y = percent.change, group = damage.label, colour = damage.label, shape = damage.label, label = percent.change)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    #geom_text() +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Biodiversity gain (%) at \noptimal MPA size") +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    #coord_flip(clip = "off") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  bio.arith
  
  # Biodiversity, geometric
  bio.geom <- ggplot(geom.new, aes(x = overlap, y = percent.change, group = damage.label, colour = damage.label, shape = damage.label, label = percent.change)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    #geom_text() +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    #coord_flip(clip = "off") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5))
  bio.geom
  
  # Highest performing MPA, arithmetic
  MPA.arith <- ggplot(arith.new, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage.label, colour = damage.label, shape = damage.label)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Optimal coverage of fishing \nground by MPAs (%)") +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    theme(legend.position="bottom")
  MPA.arith
  
  # Highest performing MPA, geometric
  MPA.geom <- ggplot(geom.new, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage.label, colour = damage.label, shape = damage.label)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    theme(legend.position="bottom")
  MPA.geom
  
  # Effort, arithmetic
  effort.arith <- ggplot(arith.new, aes(x = overlap, y = percent.change.effort, group = damage.label, colour = damage.label, shape = damage.label)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    labs(subtitle = "Arithmetic mean abundance") +
    scale_y_continuous(name = "Effort increase (%) at \noptimal MPA size") +
    scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    theme(legend.position="bottom")
  effort.arith
  
  # Effort, geometric
  effort.geom <- ggplot(geom.new, aes(x = overlap, y = percent.change.effort, group = damage.label, colour = damage.label, shape = damage.label)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    labs(subtitle = "Geometric mean abundance") +
    scale_y_continuous(name = element_blank()) +
    scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    theme_bw() +
    theme(legend.position="top")
  effort.geom
  
  # Compiled plot
  comp.plot <- ((bio.arith) + (bio.geom)) / ((MPA.arith) + (MPA.geom)) / ((effort.arith) + (effort.geom)) +
    plot_annotation(tag_levels = 'A') +
    plot_layout(guides = 'collect') &
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          #legend.box.background = element_rect(colour = "black"),
          plot.tag = element_text(size = 24))
  comp.plot
  
  ggview(plot = comp.plot,
         device = "png",
         width = 7,
         height = 10,
         units = "in")
  
  fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
  ggsave(paste(fig.path, "mean_results.pdf", sep = "/"), comp.plot, units = "in", width = 7, height = 10)
  
  ### Loop for damage scenarios ####
  if(curr.analysis == "q01" | curr.analysis == "q03" | curr.analysis == "r01" | curr.analysis == "r03"){
    # Create new plots
    bio.arith <- ggplot(arith.new, aes(x = overlap, y = percent.change, group = damage.label, colour = damage.label, shape = damage.label, label = percent.change)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      #geom_text() +
      labs(subtitle = "Arithmetic mean abundance") +
      scale_y_continuous(name = "Biodiversity gain (%) at \noptimal MPA size") +
      scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      #coord_flip(clip = "off") +
      theme(legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    bio.arith
    
    # Biodiversity, geometric
    bio.geom <- ggplot(geom.new, aes(x = overlap, y = percent.change, group = damage.label, colour = damage.label, shape = damage.label, label = percent.change)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      #geom_text() +
      labs(subtitle = "Geometric mean abundance") +
      scale_y_continuous(name = element_blank()) +
      scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      #coord_flip(clip = "off") +
      theme(legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    bio.geom
    
    # Highest performing MPA, arithmetic
    MPA.arith <- ggplot(arith.new, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage.label, colour = damage.label, shape = damage.label)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      labs(subtitle = "Arithmetic mean abundance") +
      scale_y_continuous(name = "Optimal coverage of fishing \nground by MPAs (%)") +
      scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      theme(legend.position="bottom")
    MPA.arith
    
    # Highest performing MPA, geometric
    MPA.geom <- ggplot(geom.new, aes(x = overlap, y = ((sparedProp-0.5)*2)*100, group = damage.label, colour = damage.label, shape = damage.label)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      labs(subtitle = "Geometric mean abundance") +
      scale_y_continuous(name = element_blank()) +
      scale_x_discrete(name = element_blank(), labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      theme(legend.position="bottom")
    MPA.geom
    
    # Effort, arithmetic
    effort.arith <- ggplot(arith.new, aes(x = overlap, y = percent.change.effort, group = damage.label, colour = damage.label, shape = damage.label)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      labs(subtitle = "Arithmetic mean abundance") +
      scale_y_continuous(name = "Effort increase (%) at \noptimal MPA size") +
      scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      theme(legend.position="bottom")
    effort.arith
    
    # Effort, geometric
    effort.geom <- ggplot(geom.new, aes(x = overlap, y = percent.change.effort, group = damage.label, colour = damage.label, shape = damage.label)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      labs(subtitle = "Geometric mean abundance") +
      scale_y_continuous(name = element_blank()) +
      scale_x_discrete(name = "Range overlap between target \nand non-target species", labels = c(0.2, 0.4, 0.6, 0.8, 1)) +
      scale_fill_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      theme_bw() +
      theme(legend.position="top")
    effort.geom
    
    # Compiled plot
    comp.plot <- ((bio.arith) + (bio.geom)) / ((MPA.arith) + (MPA.geom)) / ((effort.arith) + (effort.geom)) +
      plot_annotation(tag_levels = 'A') +
      plot_layout(guides = 'collect') &
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            #legend.box.background = element_rect(colour = "black"),
            plot.tag = element_text(size = 24))
    comp.plot
    
    ggview(plot = comp.plot,
           device = "png",
           width = 7,
           height = 10,
           units = "in")
    
    fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
    ggsave(paste(fig.path, "mean_results.pdf", sep = "/"), comp.plot, units = "in", width = 7, height = 10)
  }
  
  
  # Test plot size
  # ggview(plot = comp.plot,
  #        device = "png",
  #        width = 7,
  #        height = 10,
  #        units = "in")
  
  # cairo_pdf(paste(fig.path, "mean_results.pdf", sep = "/"), family="Arial Unicode MS", width = 7, height = 10)
  # print(comp.plot)
  # dev.off()
}

# View(default.detailed.list)
# View(default.detailed.mean.list)
# View(tdam.detailed.list)
# View(tdam.detailed.mean.list)

# New main detailed figure ####

(elements <- 1:length(default.detailed.list))
spectrum <- elements[c(TRUE,FALSE)]

for(i in spectrum){
  
  combined.list <- default.detailed.list[[i]]
  combined.mean.list <- default.detailed.mean.list[[i]]
  (curr.analysis <- unique(combined.list$scenario))
  
  if(curr.analysis == "q01" | curr.analysis == "q03" | curr.analysis == "r01" | curr.analysis == "r03"){
    ## Results without habitat damage ####
    
    ### Combine data ####
    
    combined.list <- default.detailed.list[[i]]
    combined.mean.list <- default.detailed.mean.list[[i]]
    curr.analysis <- unique(combined.list$scenario)
    
    
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
      ggtitle("No explicit habitat damage") +
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
    
    combined.list <- default.detailed.list[[i]]
    combined.mean.list <- default.detailed.mean.list[[i]]
    
    combined.list <- combined.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    combined.mean.list <- combined.mean.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    
    ### Plot data ####
    
    p2 <- ggplot() +
      ggtitle("With explicit habitat damage for non-target species only") +
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
      theme(legend.position="none", text = element_text(size = 14),
            legend.text=element_text(size=14))
    p2
    
    ## Results with habitat damage for target species ####
    
    ### Combine data ####
    
    combined.list <- tdam.detailed.list[[i+1]]
    combined.mean.list <- tdam.detailed.mean.list[[i+1]]
    
    combined.list <- combined.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    combined.mean.list <- combined.mean.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    
    ### Plot data ####
    
    p3 <- ggplot() +
      ggtitle("With explicit habitat damage for target and non-target species") +
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
      scale_x_discrete(name = "Proportion of fishing ground reserved in MPAs (%)", labels = c(0, 10, 20, 30, 40, 50, 60, 70)) +
      scale_color_manual(
        name = element_blank(),
        labels = c("Average of all simulations", "Individual simulations"),
        values = c("a" = "#FF6240", "b" = "black")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,1.1)) +
      geom_hline(data = combined.mean.list %>% filter(sparedProp == 0.5), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
      theme_bw() +
      theme(legend.position="bottom", text = element_text(size = 14),
            legend.text=element_text(size=14))
    p3
    
    detailed.plot <- (p2 / p3)
    detailed.plot
    ggview(plot = detailed.plot,
           device = "png",
           width = 30,
           height = 20,
           units = "cm")
    
    fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
    ggsave(paste(fig.path, "detailed_results.pdf", sep = "/"), units = "cm", width = 30, height = 30)
    
  } else {
    ## Results without habitat damage ####
    
    ### Combine data ####
    
    combined.list <- default.detailed.list[[i]]
    combined.mean.list <- default.detailed.mean.list[[i]]
    curr.analysis <- unique(combined.list$scenario)
    
    
    combined.list <- combined.list %>% 
      filter(damage == FALSE) %>% 
      filter(overlap != "00")
    combined.mean.list <- combined.mean.list %>% 
      filter(damage == FALSE) %>% 
      filter(overlap != "00")
    
    # combined.list <- combined.list %>% 
    #   filter(geom.mean != 0)
    
    
    ### Plot data ####
    
    #if(length(file.names.sum) == 12){
    p1 <- ggplot() +
      ggtitle("No explicit habitat damage") +
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
    #}
    
    ## Results with habitat damage ####
    
    ### Combine data ####
    
    combined.list <- default.detailed.list[[i]]
    combined.mean.list <- default.detailed.mean.list[[i]]
    
    combined.list <- combined.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    combined.mean.list <- combined.mean.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    
    ### Plot data ####
    
    p2 <- ggplot() +
      ggtitle("With explicit habitat damage for non-target species only") +
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
      theme(legend.position="none", text = element_text(size = 14),
            legend.text=element_text(size=14))
    p2
    
    ## Results with habitat damage for target species ####
    
    ### Combine data ####
    
    combined.list <- tdam.detailed.list[[i+1]]
    combined.mean.list <- tdam.detailed.mean.list[[i+1]]
    
    combined.list <- combined.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    combined.mean.list <- combined.mean.list %>% 
      filter(damage == TRUE) %>% 
      filter(overlap != "00")
    
    ### Plot data ####
    
    p3 <- ggplot() +
      ggtitle("With explicit habitat damage for target and non-target species") +
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
      scale_x_discrete(name = "Proportion of fishing ground reserved in MPAs (%)", labels = c(0, 10, 20, 30, 40, 50, 60, 70)) +
      scale_color_manual(
        name = element_blank(),
        labels = c("Average of all simulations", "Individual simulations"),
        values = c("a" = "#FF6240", "b" = "black")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0,1.1)) +
      geom_hline(data = combined.mean.list %>% filter(sparedProp == 0.5), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
      theme_bw() +
      theme(legend.position="bottom", text = element_text(size = 14),
            legend.text=element_text(size=14))
    p3
    
    detailed.plot <- (p1 / p2 / p3)
    detailed.plot
    ggview(plot = detailed.plot,
           device = "png",
           width = 30,
           height = 30,
           units = "cm")
    
    fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
    ggsave(paste(fig.path, "detailed_results.pdf", sep = "/"), units = "cm", width = 30, height = 30)
    
  }
}


# Effort figure ####

## Load data ####

sens.names <- c("default", "defaulttdam")
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

sens.names <- c("default", "defaulttdam")
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
  scale_x_continuous(name = "Proportion of fishing ground reserved in MPAs (%)", limits = c(0, 52)) +
  ylim(c(0,18)) +
  scale_colour_viridis_d(name = "Target species \nexperiences explicit \nhabitat damage", labels = c("No", "Yes"), end = 0.8, guide = guide_legend(reverse = F), direction = 1) +
  #scale_shape_discrete(name = "Target species \nexperiences explicit \nhabitat damage", labels = c("No", "Yes"), guide = guide_legend(reverse = F)) +
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
ggsave("../figs/processed/effort_results.png", effort.plot, units = "in", width = 6, height = 4)

# Collapses figure ####

sens.names <- list.files("../../rds/hpc-work/SSS_custom_model/summaries")
spectrum <- sens.names[c(TRUE, FALSE)]
spectrum

#spectrum <- spectrum[spectrum != "catch09"] # Remove this when done testing. It's here until you have teh catch simulations redone

for(j in spectrum){
  ## Load data ####
  
  curr.analysis <- j
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
  default.collapse <- collapse.mean.damage
  
  #effort.both.damage <- bind_rows(effort.no.damage, effort.damage, .id = "targ.damage")
  #habitat.damage <- habitat.damage %>% filter(damage == TRUE) %>% filter(overlap != "00")
  
  # Do the same for targdam
  curr.analysis <- paste(j, "tdam", sep = "")
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
  targdam.collapse <- collapse.mean.damage
  
  
  ## Combine default and targdam data ####
  
  default.collapse
  targdam.collapse
  
  # Change damage values to descriptive labels
  default.collapse.relabel <- default.collapse %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "no.species.damaged",
        damage == T ~ "non.target.species.damaged"
      )
    )
  
  targdam.collapse.relabel <- targdam.collapse %>% 
    mutate(
      damage.label = case_when(
        damage == F ~ "target.species.damaged",
        damage == T ~ "target.&.non.target.species.damaged"
      )
    )
  
  # Create new combined data frames
  collapse.new <- rbind.data.frame(default.collapse.relabel, targdam.collapse.relabel)
  
  # Filter new combined data frames
  collapse.new <- collapse.new %>% filter(damage.label != "target.species.damaged")
  
  collapse.longer <- collapse.new %>% 
    pivot_longer(cols = prop.collapsed.mean05:prop.collapsed.mean01, values_to = "number.collapsed", names_to = "damage.type")
  
  # Keep only distinct rows
  #collapse.new <- distinct(collapse.new)
  
  
  # Plot
  # # Main text version
  # collapse.plot <- ggplot(collapse.longer, aes(x = ((sparedProp-0.5)*2)*100, y = number.collapsed*80, colour = damage.label, shape = damage.label)) +
  #   geom_line() +
  #   geom_point(aes(size = damage.label)) +
  #   scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
  #   scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
  #   scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
  #   labs(y = "Mean number of species",
  #        x = "Proportion of fishing ground reserved in MPAs (%)") +
  #   #scale_y_continuous(limits=c(0,5)) +
  #   facet_grid(damage.type~overlap, 
  #              switch = "y",
  #              labeller = labeller(overlap = c("00" = "Range overlap = 0",
  #                                              "02" = "Range overlap = 20%",
  #                                              "04" = "Range overlap = 40%",
  #                                              "06" = "Range overlap = 60%",
  #                                              "08" = "Range overlap = 80%",
  #                                              "10" = "Range overlap = 100%"),
  #                                  damage.type = c("prop.collapsed.mean05" = "Below 50% of pristine abundance",
  #                                                  "prop.collapsed.mean01" = "Below 10% of pristine abundance")),
  #              scales = "free_y") +
  #   theme_bw() +
  #   theme(legend.position = c(0.20,0.25),
  #         legend.box.background = element_rect(colour = "black"),
  #         text = element_text(size=14),
  #         strip.placement = "outside",
  #         strip.background = element_blank())
  # collapse.plot
  
  # Sensitivity version
  collapse.plot <- ggplot(collapse.longer, aes(x = ((sparedProp-0.5)*2)*100, y = number.collapsed*80, colour = damage.label, shape = damage.label)) +
    geom_line() +
    geom_point(aes(size = damage.label)) +
    scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
    scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
    scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
    labs(y = "Mean number of species",
         x = "Proportion of fishing ground reserved in MPAs (%)") +
    #scale_y_continuous(limits=c(0,5)) +
    facet_grid(damage.type~overlap, 
               labeller = labeller(overlap = c("00" = "Range overlap = 0",
                                               "02" = "Range overlap = 0.2",
                                               "04" = "Range overlap = 0.4",
                                               "06" = "Range overlap = 0.6",
                                               "08" = "Range overlap = 0.8",
                                               "10" = "Range overlap = 1"),
                                   damage.type = c("prop.collapsed.mean05" = "Below 50% of pristine abundance",
                                                   "prop.collapsed.mean01" = "Below 10% of pristine abundance")),
               scales = "free_y") +
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size=13))
  collapse.plot
  
  curr.analysis <- j
  
  if(curr.analysis == "q01" |curr.analysis == "q03" |curr.analysis == "r01" |curr.analysis == "q03" ){
    collapse.plot <- ggplot(collapse.longer, aes(x = ((sparedProp-0.5)*2)*100, y = number.collapsed*80, colour = damage.label, shape = damage.label)) +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), begin = 0.267, end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("Non-target species only", "Target and non-target species"), values = c(4.5,4), guide = guide_legend()) +
      labs(y = "Mean number of species",
           x = "Proportion of fishing ground reserved in MPAs (%)") +
      #scale_y_continuous(limits=c(0,5)) +
      facet_grid(damage.type~overlap, 
                 labeller = labeller(overlap = c("00" = "Range overlap = 0",
                                                 "02" = "Range overlap = 0.2",
                                                 "04" = "Range overlap = 0.4",
                                                 "06" = "Range overlap = 0.6",
                                                 "08" = "Range overlap = 0.8",
                                                 "10" = "Range overlap = 1"),
                                     damage.type = c("prop.collapsed.mean05" = "Below 50% of pristine abundance",
                                                     "prop.collapsed.mean01" = "Below 10% of pristine abundance")),
                 scales = "free_y") +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size=13))
    collapse.plot
  }
  
  if(curr.analysis == "nspec25"){
    collapse.plot <- ggplot(collapse.longer, aes(x = ((sparedProp-0.5)*2)*100, y = number.collapsed*200, colour = damage.label, shape = damage.label)) +
      geom_line() +
      geom_point(aes(size = damage.label)) +
      scale_colour_viridis_d(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), end = 0.8) +
      scale_shape_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(15, 16, 18)) +
      scale_size_manual(name = "Explicit habitat damage", labels = c("None", "Non-target species only", "Target and non-target species"), values = c(5,4.5,4), guide = guide_legend()) +
      labs(y = "Mean number of species",
           x = "Proportion of fishing ground reserved in MPAs (%)") +
      #scale_y_continuous(limits=c(0,5)) +
      facet_grid(damage.type~overlap, 
                 labeller = labeller(overlap = c("00" = "Range overlap = 0",
                                                 "02" = "Range overlap = 0.2",
                                                 "04" = "Range overlap = 0.4",
                                                 "06" = "Range overlap = 0.6",
                                                 "08" = "Range overlap = 0.8",
                                                 "10" = "Range overlap = 1"),
                                     damage.type = c("prop.collapsed.mean05" = "Below 50% of pristine abundance",
                                                     "prop.collapsed.mean01" = "Below 10% of pristine abundance")),
                 scales = "free_y") +
      theme_bw() +
      theme(legend.position = "bottom",
            text = element_text(size=13))
    collapse.plot
  }
  
  fig.path <- paste("../figs/processed", curr.analysis, sep = "/")
  
  ggview(plot = collapse.plot,
         device = "png",
         width = 12,
         height = 6,
         units = "in")
  #ggsave("../figs/processed/collapse_results.png", collapse.plot, units = "in", width = 12, height = 6)
  
  ggsave(paste(fig.path, "collapse_results.png", sep = "/"), collapse.plot, units = "in", width = 12, height = 6)
  
  #cairo_pdf("../figs/processed/habitat_results.pdf", family="Arial Unicode MS", width = 6, height = 6)
  #print(habitat.plot)
  #dev.off()
  #ggsave("../figs/processed/habitat_results.pdf", habitat.plot, units = "in", width = 6, height = 6)
  
  
  # ggplot(collapse.damage, aes(x = ((sparedProp-0.5)*2)*100, y = prop.collapsed*80, colour = overlap, fill = overlap)) +
  #   geom_smooth(method = "lm", formula = y ~ x) +
  #   scale_colour_viridis_d(name = "Range overlap\nbetween target\nand non-target\nspecies", labels = c("20%", "40%", "60%", "80%", "100%")) +
  #   scale_fill_viridis_d(name = "Range overlap\nbetween target\nand non-target\nspecies", labels = c("20%", "40%", "60%", "80%", "100%")) +
  #   labs(y = "Mean extirpated non-target species",
  #        x = "Proportion of fishery ground reserved in MPAs (%)") +
  #   facet_wrap(.~damage, labeller = labeller(damage = c("TRUE" = "With damage", "FALSE" = "Without damage"))) +
  #   theme_bw()
  
  # cairo_pdf("../figs/processed/habitat_results.pdf", family="Arial Unicode MS", width = 6, height = 6)
  # print(habitat.plot)
  # dev.off()
  # ggsave("../figs/processed/habitat_results.pdf", habitat.plot, units = "in", width = 6, height = 6)
}

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
  #geom_hline(data = habitat.damage %>% filter(sparedProp == 0.5) , aes(yintercept = mean.mean, colour = overlap), linetype = "dotted") +
  geom_line() +
  geom_point() +
  scale_x_continuous(name = "Proportion of fishing ground reserved in MPAs (%)", limits = c(0, 52)) +
  scale_colour_viridis_d(name = "Overlap between target\nand non-target species", end = 0.8, guide = guide_legend(reverse = F), direction = 1,
                         labels = c("0.2", "0.4", "0.6", "0.8", "1")) +
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    #geom_text(nudge_y = 0.025, show.legend = FALSE) +
  labs(y = "Occupied patch mean habitat quality (*K*)") +
  theme_bw() +
  theme(axis.title.y = element_markdown(size = 14),
        text = element_text(size = 14),
        legend.position = c(0.5, 0.19),
        legend.box.background = element_rect(colour = "black"))
habitat.plot

# habitat.plot.1 <- ggplot(habitat.damage, aes(x = ((sparedProp-0.5)*2)*100, y = mean.mean, colour = overlap, label = paste(round(percent.change), "%", sep = ""))) +
#   geom_hline(data = habitat.damage %>% filter(sparedProp == 0.5) , aes(yintercept = mean.mean, colour = overlap), linetype = "dotted") +
#   #geom_line() +
#   #geom_point() +
#   scale_x_continuous(name = "Proportion of fishing ground reserved in MPAs (%)", limits = c(0, 52)) +
#   scale_colour_viridis_d(name = "Habitat quality when no\nMPAs at overlap of", end = 0.8, guide = guide_legend(reverse = F), direction = 1,
#                          labels = c("0.2", "0.4", "0.6", "0.8", "1")) +
#   scale_y_continuous(limits = c(0,1),
#                      breaks = c(0,0.2,0.4,0.6,0.8,1)) +
#   #geom_text(nudge_y = 0.025, show.legend = FALSE) +
#   labs(y = "Occupied patch mean habitat quality (*K*)") +
#   theme_bw() +
#   theme(axis.title.y = element_markdown(size = 14),
#         text = element_text(size = 14),
#         legend.position = c(0.5, 0.19),
#         legend.box.background = element_rect(colour = "black"))
# habitat.plot.1

#Test plot size
ggview(plot = habitat.plot,
       device = "png",
       width = 6,
       height = 6,
       units = "in")

#cairo_pdf("../figs/processed/habitat_results.pdf", family="Arial Unicode MS", width = 6, height = 6)
#print(habitat.plot)
#dev.off()
ggsave("../figs/processed/habitat_results.pdf", habitat.plot, units = "in", width = 6, height = 6)

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

# Movement figure ####

# A script for testing how long it takes a seascape to equilibrate given a certain movement rate

# Load function
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

# Set parameters
initial.sea.n <- c(0,1)
sea.k <- c(1,1)
movement.spec <- c(0.1,0.3,0.9)
time.spec <- seq(1,50,1)

# Run loop
iteration <- 1
sea.n <- vector()
sea.results <- list()
for(i in movement.spec){
  sea.results.j <- vector()
  sea.n <- initial.sea.n
  for(j in time.spec){
  sea.results.j[j] <- sea.n[2]-sea.n[1]
  sea.n <- migration.function(sea.n, sea.k, i)  
  }
  sea.results[[iteration]] <- sea.results.j
  iteration <- iteration+1
}
movement.results <- cbind.data.frame(time.spec, "m01" = sea.results[[1]], "m03" = sea.results[[2]], "m09" = sea.results[[3]])
movement.results <- movement.results %>% pivot_longer(cols = m01:m09, names_to = "movement.rate", values_to = "difference")

movement.fig <- ggplot(movement.results, aes(x = time.spec, y = difference, colour = movement.rate)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(name = "Movement rate (*m*)", option = "A", end = 0.8, labels = c("0.1", "0.3", "0.9")) +
  scale_y_continuous(name = "Difference in patch abundances") +
  scale_x_continuous(name = "Timestep (*t*)") +
  theme_bw() +
  theme(legend.position = c(0.75,0.5),
        legend.box.background = element_rect(colour = "black"),
        axis.title.x = element_markdown(),
        legend.title = element_markdown())
movement.fig

#Test plot size
ggview(plot = movement.fig,
       device = "png",
       width = 6,
       height = 3.5,
       units = "in")

ggsave("../figs/processed/movement_results.png", plot = movement.fig, width = 6, height = 3.5)

# Habitat methods figures ####

## Script details ####

## Script for generating habitat damage methods figure

## Load packages ####

library(tidyverse)
library(patchwork)
library(data.table)
library(ggview)
library(ggtext)
library(latex2exp)

## Load functions ####

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

## Habitat quality through time after one trawl ####

# Specify parameters
sim.length <- 25*2
trawl.vec <- c(0, 1, rep(0, sim.length-2))
pristine.k <- 1
start.k <- 1
hab.rep <- 0.2
hab.dep <- 0.2

# Prepare data
abundance <- vector()
stage <- vector()
abundance[1] <- start.k
stage[1] <- "depletion"

# Generate data
for(i in 1:sim.length){
  even.odd <- (i %% 2 == 0)
  if(even.odd == FALSE){
    gain <- multip.recovery(prist.K = pristine.k,
                            curr.K = abundance[i],
                            dep.K = hab.dep,
                            n.trawls = NULL,
                            rep.K = hab.rep,
                            time = NULL)
    abundance[i+1] <- abundance[i] + gain
    stage[i+1] <- "recovery"
  } else if (even.odd == TRUE){
    loss <- multip.depletion(prist.K = pristine.k,
                             curr.K = abundance[i],
                             dep.K = hab.dep,
                             n.trawls = trawl.vec[i],
                             rep.K = hab.rep,
                             time = NULL)
    abundance[i+1] <- abundance[i] - loss
    stage[i+1] <- "depletion"
  }
}

# Process data
data.one <- cbind.data.frame(abundance, time = seq(0, 25, by = 0.5), stage)

# Plot data
one.trawl <- ggplot(data.one, aes(x = time, y = abundance)) +
  ggtitle("One trawl") +
  geom_line(aes(colour = stage)) +
  scale_colour_discrete(name = element_blank(),
                        label = c("Recovery phase",
                                  "Depletion phase")) +
  scale_y_continuous(limits = c(0.4,1)) +
  labs(y = "Habitat quality", x = "Time") +
  theme_bw() + 
  theme(legend.position = c(0.5,0.3)) +
  aes(group=NA)
one.trawl

## Habitat quality through time with continuous trawls ####

# Specify parameters
sim.length <- 25*2
trawl.vec <- 1
pristine.k <- 1
start.k <- 1
hab.rep <- 0.2
hab.dep <- 0.2

# Prepare data
abundance <- vector()
stage <- vector()
abundance[1] <- start.k
stage[1] <- "depletion"

# Generate data
for(i in 1:sim.length){
  even.odd <- (i %% 2 == 0)
  if(even.odd == FALSE){
    gain <- multip.recovery(prist.K = pristine.k,
                            curr.K = abundance[i],
                            dep.K = hab.dep,
                            n.trawls = NULL,
                            rep.K = hab.rep,
                            time = NULL)
    abundance[i+1] <- abundance[i] + gain
    stage[i+1] <- "recovery"
  } else if (even.odd == TRUE){
    loss <- multip.depletion(prist.K = pristine.k,
                             curr.K = abundance[i],
                             dep.K = hab.dep,
                             n.trawls = trawl.vec,
                             rep.K = hab.rep,
                             time = NULL)
    abundance[i+1] <- abundance[i] - loss
    stage[i+1] <- "depletion"
  }
}

# Process data
data.cont <- cbind.data.frame(abundance, time = seq(0, 25, by = 0.5), stage)

# Plot data
contin.trawl <- ggplot(data.cont, aes(x = time, y = abundance)) +
  ggtitle("Continuous trawling") +
  geom_line(aes(colour = stage)) +
  scale_colour_discrete(name = element_blank(),
                        label = c("Recovery phase",
                                  "Depletion phase")) +
  scale_y_continuous(limits = c(0.4,1)) +
  labs(y = element_blank(), x = "Time") +
  theme_bw() + 
  theme(legend.position = "none") +
  aes(group=NA)
contin.trawl

## Habitat quality through time combined ####

data.cont <- data.cont %>% filter(
  time >= 1.5
)

comb.trawl <- ggplot() +
  geom_line(data = data.cont, aes(x = time, y = abundance, colour = stage)) +
  geom_line(data = data.one, aes(x = time, y = abundance, colour = stage)) +
  geom_point(data = data.cont %>% filter(time == as.integer(time)), aes(x = time, y = abundance), colour = "black") +
  geom_point(data = data.one  %>% filter(time == as.integer(time)), aes(x = time, y = abundance), colour = "black") +
  scale_colour_viridis_d(name = "Phase within timestep", begin = 0.2, end = 0.8,
                         label = c("Recovery",
                                   "Depletion")) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = 0:25) +
  labs(y = "Habitat quality (*K*)", x = "Timestep (*t*)") +
  theme_bw() + 
  theme(legend.position = c(0.75,0.22),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = margin(0, 0, 1, 0, "cm"),
        axis.title.x = element_markdown(size = 14),
        axis.title.y = element_markdown(size = 14)) +
  aes(group=NA)
comb.trawl

## Habitat quality equilibrium continuous trawls (q) ####

# Specify parameters
sim.length <- 25*2
trawl.vec <- seq(0,10,0.05)
pristine.k <- 1
start.k <- 1
hab.rep <- 0.2
hab.dep <- c(0, 0.1, 0.2, 0.3)

new.results.list <- list()
for(k in hab.dep){
  # Prepare data
  results.list <- list()
  
  # Generate data
  for(j in trawl.vec){
    abundance <- vector()
    stage <- vector()
    abundance[1] <- start.k
    stage[1] <- "depletion"
    for(i in 1:sim.length){
      even.odd <- (i %% 2 == 0)
      if(even.odd == FALSE){
        gain <- multip.recovery(prist.K = pristine.k,
                                curr.K = abundance[i],
                                dep.K = k,
                                n.trawls = NULL,
                                rep.K = hab.rep,
                                time = NULL)
        abundance[i+1] <- abundance[i] + gain
        stage[i+1] <- "recovery"
      } else if (even.odd == TRUE){
        loss <- multip.depletion(prist.K = pristine.k,
                                 curr.K = abundance[i],
                                 dep.K = k,
                                 n.trawls = j,
                                 rep.K = hab.rep,
                                 time = NULL)
        abundance[i+1] <- abundance[i] - loss
        stage[i+1] <- "depletion"
      }
    }
    # Save results in list
    results.list[[paste(j)]] <- cbind.data.frame(abundance, time = seq(0, 25, by = 0.5), stage)
  }
  
  # Process data
  data <- rbindlist(results.list, idcol = "trawls")
  data <- data %>% 
    filter(time == max(time) | time == max(time)-0.5) %>% 
    group_by(trawls) %>% 
    summarise(abundance = mean(abundance))
  data$trawls <- as.numeric(data$trawls)
  
  new.results.list[[paste(k)]] <- data
}
data <- rbindlist(new.results.list, idcol = "dep.K")

# Plot data
multip.trawl.q <- ggplot(data, aes(x = trawls, y = abundance, colour = dep.K)) +
  geom_line() +
  labs(y = "Equilibrium habitat quality (*K*)", x = "Trawl passes per timestep (*E*)") +
  ylim(0,1) +
  scale_colour_viridis_d(name = "Habitat **depletion**<br>rate (*δ*)", end = 0.9) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  theme(legend.position = c(0.73,0.625),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_markdown(),
        axis.title.x = element_markdown(size = 14),
        axis.title.y = element_markdown(size = 14))
multip.trawl.q

## Habitat quality equilibrium continuous trawls (r) ####

# Specify parameters
sim.length <- 25*2
trawl.vec <- seq(0,10,0.05)
pristine.k <- 1
start.k <- 1
hab.rep <- c(0, 0.1, 0.2, 0.3)
hab.dep <- 0.2

new.results.list <- list()
for(k in hab.rep){
  # Prepare data
  results.list <- list()
  
  # Generate data
  for(j in trawl.vec){
    abundance <- vector()
    stage <- vector()
    abundance[1] <- start.k
    stage[1] <- "depletion"
    for(i in 1:sim.length){
      even.odd <- (i %% 2 == 0)
      if(even.odd == FALSE){
        gain <- multip.recovery(prist.K = pristine.k,
                                curr.K = abundance[i],
                                dep.K = hab.dep,
                                n.trawls = NULL,
                                rep.K = k,
                                time = NULL)
        abundance[i+1] <- abundance[i] + gain
        stage[i+1] <- "recovery"
      } else if (even.odd == TRUE){
        loss <- multip.depletion(prist.K = pristine.k,
                                 curr.K = abundance[i],
                                 dep.K = hab.dep,
                                 n.trawls = j,
                                 rep.K = k,
                                 time = NULL)
        abundance[i+1] <- abundance[i] - loss
        stage[i+1] <- "depletion"
      }
    }
    # Save results in list
    results.list[[paste(j)]] <- cbind.data.frame(abundance, time = seq(0, 25, by = 0.5), stage)
  }
  
  # Process data
  data <- rbindlist(results.list, idcol = "trawls")
  data <- data %>% 
    filter(time == max(time) | time == max(time)-0.5) %>% 
    group_by(trawls) %>% 
    summarise(abundance = mean(abundance))
  data$trawls <- as.numeric(data$trawls)
  
  new.results.list[[paste(k)]] <- data
}
data <- rbindlist(new.results.list, idcol = "rep.K")

# Plot data
multip.trawl.r <- ggplot(data, aes(x = trawls, y = abundance, colour = rep.K)) +
  geom_line() +
  labs(y = element_blank(), x = "Trawl passes per timestep (*E*)") +
  ylim(0,1) +
  scale_colour_viridis_d(name = c("Habitat **recovery**<br>rate (*ρ*)"), end = 0.9) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  theme(legend.position = c(0.73,0.625),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_markdown(),
        axis.title.x = element_markdown(size = 14))
multip.trawl.r

## Combined plot ####

comb.plot <- (comb.trawl / (multip.trawl.q + multip.trawl.r)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 24),
  )
comb.plot
ggview(plot = comb.plot,
       device = "png",
       width = 18.5,
       height = 18.5,
       units = "cm")
ggsave("../figs/processed/habitat_methods.png", units = "cm", width = 18.5, height = 18.5)
