# Script details ####

# This script is called after analysis.R and generates various diagnostic figures for exploring simulation results. These are not the figures
# used in the manuscript, but are instead saved in "../figs/raw_analysis".

# It also processes and saves certain data files to facilitate the plotting of the manuscript figures.

# This script originated as part of a larger, more flexible modelling framework, and so has many options that aren't relevant to 
# the manuscript. Many of these are experimental or outdated and will not necessarily work as described or expected.
# They are best left alone.

# Plots from single simulation ####

## Review targeted species results ####

targeted.sim.data.slice <- targeted.sim.data %>% 
  filter(simulation == 1)

targeted.sim.data.slice2 <- targeted.sim.data %>% 
  filter(simulation == 2)

targeted.sim.data.slice3 <- targeted.sim.data %>% 
  filter(simulation == 3)

### Abundance over time ####

p <- ggplot(targeted.sim.data.slice, aes(x = time.step, colour = patch, y = abun, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Abundance", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_abun_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Review non-targeted species results ####

non.targeted.sim.data.slice <- non.targeted.sim.data %>% 
  filter(simulation == 1)

non.targeted.sim.data.slice2 <- non.targeted.sim.data %>% 
  filter(simulation == 2)

non.targeted.sim.data.slice3 <- non.targeted.sim.data %>% 
  filter(simulation == 3)

### Particular species over time ####

part.species <- non.targeted.sim.data.slice %>% 
  filter(species == "Asteroidea 1")

p <- ggplot(part.species, aes(x = time.step, colour = patch, y = abun, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  geom_line() +
  labs(y = "Abundance", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ggtitle(part.species$species) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_one_species_abun_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### All species over time under sharing and median catch simulation 1 ####

# # As once off, calculate middle values (not medians so we don't get non-existent catches/sparing proportions if calculating from an even pool)
# middle.catch <- unique(non.targeted.sim.data.slice$catch)[length(unique(non.targeted.sim.data.slice$catch))/2]
# middle.sparing <- unique(non.targeted.sim.data.slice$sparing)[length(unique(non.targeted.sim.data.slice$sparing))/2]

# middle.catch2 <- unique(non.targeted.sim.data.slice2$catch)[length(unique(non.targeted.sim.data.slice2$catch))/2]
# middle.sparing2 <- unique(non.targeted.sim.data.slice2$sparing)[length(unique(non.targeted.sim.data.slice2$sparing))/2]

# middle.catch3 <- unique(non.targeted.sim.data.slice3$catch)[length(unique(non.targeted.sim.data.slice3$catch))/2]
# middle.sparing3 <- unique(non.targeted.sim.data.slice3$sparing)[length(unique(non.targeted.sim.data.slice3$sparing))/2]

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_max_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and max catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_max_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sharing_max_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == middle.sparing) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sparing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == middle.sparing2) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sparing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == middle.sparing3) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.abun = mean(abun))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.abun)) +
#   geom_line() +
#   labs(y = "Abundance", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_abun_time_sparing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time ####

summary <- non.targeted.sim.data.slice %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
  summarise(total = sum(abun),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

p <- ggplot(data = summary,
            aes(x=time.step, y = biodiversity.value, colour = biodiversity.measure)) +
  geom_line() +
  facet_grid(sparing~MSYProp) +
  labs(y = "Biodiversity", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"),begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_ss_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)

p <- ggplot(data = final.summary,
            aes(x=sparedProp, y = biodiversity.value, colour = MSYProp, group = MSYProp)) +
  geom_line() +
  geom_point() +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Biodiversity", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_ss_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time with extinction threshold ####

nt.data.thresh <- non.targeted.sim.data.slice
nt.data.thresh$abun <- if_else(nt.data.thresh$abun/nt.data.thresh$k < ext.thresh, 0, nt.data.thresh$abun)
nt.data.thresh[is.na(nt.data.thresh)] <- 0

summary.thresh <- nt.data.thresh %>% 
  group_by(sparing,sparedProp,catch,MSYProp,time.step,species) %>% 
  summarise(total = sum(abun),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

p <- ggplot(data = summary.thresh,
            aes(x=time.step, y = biodiversity.value, colour = biodiversity.measure)) +
  geom_line() +
  facet_grid(sparing~MSYProp) +
  labs(y = "Biodiversity", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"),begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(limits = c(0,1), sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_ss_time_thresh"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing results at final time accounting for extinction threshold ####

final.summary.thresh <- filter(summary.thresh, time.step == n.time)

p <- ggplot(data = final.summary.thresh,
            aes(x=sparedProp, y = biodiversity.value, colour = MSYProp, group = MSYProp)) +
  geom_line() +
  geom_point() +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Biodiversity", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_ss_final_time_thresh"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species have fallen below the extinction threshold over time ####

summary.thresh <- nt.data.thresh %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
  summarise(total = sum(abun)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total)) %>% 
  pivot_longer(cols = c(arith.mean, geom.mean),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

collapsed.data <- non.targeted.sim.data.slice %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
  summarise(total = sum(abun), carrying = sum(k)) %>% 
  mutate(proportion = total/carrying) %>% 
  mutate(collapse = if_else(proportion < ext.thresh,T,F)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(no.collapsed = sum(collapse), no.healthy = sum(!collapse)) %>%
  mutate(prop.collapsed = no.collapsed/(no.healthy+no.collapsed))

p <- ggplot(collapsed.data, aes(x = time.step, y =prop.collapsed)) +
  facet_grid(sparing~MSYProp) +
  geom_line() +
  labs(y = c("Proportion of species below collapse threshold"), 
       x = "Time step") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(limits = c(0,1), sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  theme_bw()

plot.name <- "single_collapse_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species have fallen below the extinction threshold at final time ####

final.collapsed <- filter(collapsed.data, time.step == n.time)

p <- ggplot(data = final.collapsed,
            aes(x=sparedProp, y = prop.collapsed, colour = MSYProp, group = MSYProp)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Proportion of species below collapse threshold", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_collapse_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species meet IUCN Red List criteria over time ####

# if (IUCN.option == 1){ # 1 = standard categories (CR = 90% reduction etc.)
#   IUCN.data <- non.targeted.sim.data.slice %>% 
#     group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
#     summarise(total = sum(abun), carrying = sum(k)) %>% 
#     mutate(proportion = total/carrying) %>% 
#     mutate(status = case_when(
#       0 == proportion ~ "extinct",
#       0 < proportion & proportion <= (1-0.9) ~ "critically endangered",
#       (1-0.9) < proportion & proportion <= (1-0.7) ~ "endangered",
#       (1-0.7) < proportion & proportion <= (1-0.5) ~ "vulnerable",
#       TRUE ~ "near threatened / least concern")) %>% 
#     group_by(sparing, sparedProp,MSYProp, time.step) %>% 
#     count(status)
# } else if (IUCN.option == 2){ # 2 = conservative categories (CR = 80% reduction etc.)
#   IUCN.data <- non.targeted.sim.data.slice %>% 
#     group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
#     summarise(total = sum(abun), carrying = sum(k)) %>% 
#     mutate(proportion = total/carrying) %>% 
#     mutate(status = case_when(
#       0 == proportion ~ "extinct",
#       0 < proportion & proportion <= (1-0.8) ~ "critically endangered",
#       (1-0.8) < proportion & proportion <= (1-0.5) ~ "endangered",
#       (1-0.5) < proportion & proportion <= (1-0.3) ~ "vulnerable",
#       TRUE ~ "near threatened / least concern")) %>% 
#     group_by(sparing, sparedProp,MSYProp, time.step) %>% 
#     count(status)
# } 
# 
# IUCN.data$status <- factor(IUCN.data$status, 
#                            levels = rev(c("extinct",
#                                           "critically endangered",
#                                           "endangered",
#                                           "vulnerable",
#                                           "near threatened / least concern")))
# 
# p <- ggplot(IUCN.data, aes(x = time.step, y = n/(n.species*8), colour = status, fill = status)) +
#   facet_grid(sparing~MSYProp) +
#   geom_area(position = "stack") +
#   labs(y = c("Proportion of species in category"), 
#        x = "Time step") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#   scale_y_continuous(limits = c(0,1), sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
#   scale_color_viridis_d(name = "IUCN Red List category",
#                         labels = c("Near threatened / least concern",
#                                    "Vulnerable",
#                                    "Endangered",
#                                    "Critically endangered",
#                                    "Extinct")) +
#   scale_fill_viridis_d(name = "IUCN Red List category",
#                        labels = c("Near threatened / least concern",
#                                   "Vulnerable",
#                                   "Endangered",
#                                   "Critically endangered",
#                                   "Extinct")) +
#   theme_bw()
# 
# plot.name <- "single_IUCN_time"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species meet IUCN Red List criteria at final time ####

# COMMENTED OUT DUE TO CAUSING BUG ON HPC

#final.IUCN <- filter(IUCN.data, time.step == n.time) %>% 
#  complete(sparedProp, status, fill = list(n = 0))

#p <- ggplot(data = final.IUCN,
#            aes(x=sparedProp, y = n/(n.species*8), colour = status, fill = status)) +
#  geom_area(position = "stack") +
#  facet_grid(.~MSYProp) +
#  labs(y = "Proportion of species", 
#       x = "Proportion of seascape spared") +
#  ylim(0,1) +
#  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  scale_color_viridis_d(name = "IUCN Red List category",
#                        labels = c("Near threatened / least concern",
#                                   "Vulnerable",
#                                   "Endangered",
#                                   "Critically endangered",
#                                   "Extinct")) +
#  scale_fill_viridis_d(name = "IUCN Red List category",
#                       labels = c("Near threatened / least concern",
#                                  "Vulnerable",
#                                  "Endangered",
#                                  "Critically endangered",
#                                  "Extinct")) +
#  theme_bw()

#plot.name <- "single_IUCN_final_time"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Calculate management costs ####

#costs <- final.summary.thresh %>% 
#  mutate(MPA.cost = ifelse(sparing > 0, ((sparing/n.patches)*time.step*mpa.ongoing.cost)+mpa.init.cost, 0),
#         man.cost = ((1-(sparing/n.patches))*time.step*man.ongoing.cost)+man.init.cost,
#         tot.cost = MPA.cost + man.cost,
#         agg.catch = catch*time.step,
#         cpuc = catch/tot.cost) %>% 
#  pivot_longer(cols = c(MPA.cost, man.cost, tot.cost), names_to = "cost.type", values_to = "cost.cost")

### Costs across strategies ####

#p <- ggplot(costs, aes(x = sparing/n.patches, y = cost.cost, colour = cost.type)) +
#  geom_line() +
#  geom_point() +
#  facet_wrap(~as.factor(catch/max(catch))) +
#  scale_colour_discrete(name = "Cost type", labels = c("Conventional management", "MPAs", "Combined")) +
#  labs(y = "Cost", 
#       x = "Proportion of seascape spared") +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#  theme_bw()

#plot.name <- "single_costs"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### CPUC (catch per unit cost) across catches/strategies ####

#p <- ggplot(costs, aes(x = sparedProp, y = cpuc/max(catch), colour = MSYProp, group = MSYProp)) +
#  geom_line() +
#  geom_point() +
#  scale_colour_discrete(name = "Catch") +
#  labs(y = "Catch per unit cost (CPUC)", 
#       x = "Proportion of seascape spared") +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  theme_bw()
#
#plot.name <- "single_CPUC"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## If using open-access, uncomment this block for visualisation
# 
# open.access.data <- data %>% 
#   group_by(sparing,catch,time.step) %>% 
#   summarise(real.catch = sum(catch.extracted)) %>% 
#   filter(time.step == n.time)
# 
# final.open.access <- left_join(final.time, open.access.data, by = c("sparing"))
# 
# ggplot(data = final.open.access,
#        aes(x=sparing/n.patches, y = biodiversity.value, colour = real.catch)) +
#   geom_line() +
#   facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("arith.mean" = "Arithmetic mean", "geom.mean" = "Geometric mean"))) +
#   scale_colour_viridis_c(name = "Catch") +
#   labs(y = "Biodiversity", 
#        x = "Proportion of seascape spared") +
#   theme_bw()
# beep()

## Habitat plots ####

### Targeted species habitat simulation 1 ####

habitat.summary <- targeted.sim.data.slice %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_targ_habitat_1"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Targeted species habitat simulation 2 ####

habitat.summary <- targeted.sim.data.slice2 %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_targ_habitat_2"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Targeted species habitat simulation 3 ####

habitat.summary <- targeted.sim.data.slice3 %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_targ_habitat_3"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Non-targeted species habitat simulation 1 ####

habitat.summary <- non.targeted.sim.data.slice %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, colour = species, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  scale_colour_discrete(name = "Species") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_all_species_habitat_1"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Non-targeted species habitat simulation 2 ####

habitat.summary <- non.targeted.sim.data.slice2 %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, colour = species, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  scale_colour_discrete(name = "Species") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_all_species_habitat_2"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Non-targeted species habitat simulation 3 ####

habitat.summary <- non.targeted.sim.data.slice3 %>% 
  filter(sparing == 0, catch == min(catch), time.step == 1)

p <- ggplot(habitat.summary, aes(x = patch, y = k, colour = species, group = species)) +
  geom_line() +
  geom_point() +
  labs(y = "Habitat quality (carrying capacity)", 
       x = "Patch") +
  scale_colour_discrete(name = "Species") +
  ylim(0,NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_all_species_habitat_3"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Habitat damage plots ####

### Targeted species k over time ####

p <- ggplot(targeted.sim.data.slice, aes(x = time.step, colour = patch, y = k, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Habitat quality", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_k_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Particular species over time ####

part.species <- non.targeted.sim.data.slice %>% 
  filter(species == "Asteroidea 1")

p <- ggplot(part.species, aes(x = time.step, colour = patch, y = k, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  geom_line() +
  labs(y = "Habitat quality", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ggtitle(part.species$species) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_one_species_k_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### All species over time under sharing and median catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_max_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and max catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_max_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sharing_max_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == middle.sparing) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sparing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == middle.sparing2) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sparing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == middle.sparing3) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.k = mean(k))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.k)) +
#   geom_line() +
#   labs(y = "Habitat quality", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_k_time_sparing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time ####

summary <- non.targeted.sim.data.slice %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
  summarise(total = sum(k),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")


p <- ggplot(data = summary,
            aes(x=time.step, y = biodiversity.value, colour = biodiversity.measure)) +
  geom_line() +
  facet_grid(sparing~MSYProp) +
  labs(y = "Mean habitat quality", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"),begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_ss_k_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)

p <- ggplot(data = final.summary,
            aes(x=sparedProp, y = biodiversity.value, colour = MSYProp, group = MSYProp)) +
  geom_line() +
  geom_point() +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("arith.mean" = "Arithmetic mean", "geom.mean" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Mean habitat quality", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_ss_k_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Effort plots ####

### Targeted species effort over time ####

p <- ggplot(targeted.sim.data.slice, aes(x = time.step, colour = patch, y = effort, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Effort", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_effort_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Particular species over time ####

part.species <- non.targeted.sim.data.slice %>% 
  filter(species == "Asteroidea 1")

p <- ggplot(part.species, aes(x = time.step, colour = patch, y = effort, group = patch, linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  geom_line() +
  labs(y = "Effort", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ggtitle(part.species$species) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_one_species_effort_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### All species over time under sharing and median catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and median catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_max_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and max catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_max_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under sharing and maximum catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == 0) %>% 
#   filter(catch == max(catch)) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sharing_max_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 1 ####

# summary <- non.targeted.sim.data.slice %>% 
#   filter(sparing == middle.sparing) %>% 
#   filter(catch == middle.catch) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sparing_median_1"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 2 ####

# summary <- non.targeted.sim.data.slice2 %>% 
#   filter(sparing == middle.sparing2) %>% 
#   filter(catch == middle.catch2) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sparing_median_2"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# ### All species over time under median sparing and catch simulation 3 ####

# summary <- non.targeted.sim.data.slice3 %>% 
#   filter(sparing == middle.sparing3) %>% 
#   filter(catch == middle.catch3) %>% 
#   group_by(time.step, species) %>% 
#   summarise(mean.effort = mean(effort))

# p <- ggplot(summary, aes(x = time.step, colour = species, y = mean.effort)) +
#   geom_line() +
#   labs(y = "Effort", 
#        x = "Time") +
#   geom_line() +
#   scale_colour_discrete(name = "Species") +
#   theme_bw() +
#   geom_hline(yintercept = 0, linetype = "dotted")

# plot.name <- "single_all_species_effort_time_sparing_median_3"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time ####

summary <- non.targeted.sim.data.slice %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species) %>% 
  summarise(total = sum(effort)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total)) %>% 
  pivot_longer(cols = c(arith.mean, geom.mean),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

p <- ggplot(data = summary,
            aes(x=time.step, y = biodiversity.value, colour = biodiversity.measure)) +
  geom_line() +
  facet_grid(sparing~MSYProp) +
  labs(y = "Mean effort", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"),begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  #ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "single_ss_effort_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)

p <- ggplot(data = final.summary,
            aes(x=sparedProp, y = biodiversity.value, colour = MSYProp, group = MSYProp)) +
  geom_line() +
  geom_point() +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("arith.mean" = "Arithmetic mean", "geom.mean" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Mean effort", 
       x = "Proportion of seascape spared") +
  #ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "single_ss_effort_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# Plots from multiple simulations ####

## Review targeted species results ####

### Abundance over time ####

summary <- targeted.sim.data %>% 
  group_by(sparing,spared,sparedProp,patch,catch,MSYProp,time.step,simulation) %>% 
  summarise(total = sum(abun))

p <- ggplot(summary, aes(x = time.step, colour = patch, y = total, group = interaction(simulation, patch), linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Abundance", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_abun_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Review non-targeted species results ####

### Sparing/sharing over time ####

# Reduces data to that for which all replicates were still going, so we don't take averages from fewer than the max number of replicates
#non.targeted.sim.data <- non.targeted.sim.data %>% 
#  filter(time.step == n.time) %>% 
#  group_by(sparing,MSYProp) %>% 
##  mutate(simulations = length(unique(simulation))) %>% 
# ungroup() %>% 
# filter(simulations == n.sims)

summary <- non.targeted.sim.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species,simulation) %>% 
  summarise(total = sum(abun),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,simulation) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

summary.mean <- summary %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,biodiversity.measure) %>% 
  summarise(mean.mean = mean(biodiversity.value))

p <- ggplot() +
  geom_line(data = summary,
            aes(x=time.step, 
                y = biodiversity.value, 
                colour = biodiversity.measure, 
                group = interaction(simulation, biodiversity.measure)),
            alpha = 0.5) +
  geom_line(data = summary.mean, 
            aes(x = time.step, 
                y = mean.mean,
                colour = biodiversity.measure), 
            size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Biodiversity", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"), begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_ss_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)
final.summary.mean <- filter(summary.mean, time.step == n.time) 

p <- ggplot() +
  geom_line(data = final.summary,
            aes(x=sparedProp, 
                y = biodiversity.value, 
                colour = MSYProp, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  geom_point(data = final.summary,
             aes(x=sparedProp, 
                 y = biodiversity.value, 
                 colour = MSYProp, 
                 group = interaction(simulation, MSYProp)),
             alpha = 0.2) +
  geom_line(data = final.summary.mean,
            aes(x=sparedProp,
                y=mean.mean,
                colour=MSYProp,
                group=MSYProp),
            size = 1.05) +
  geom_point(data = final.summary.mean,
             aes(x=sparedProp,
                 y=mean.mean,
                 colour=MSYProp,
                 group=MSYProp),
             size = 1.05) +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Biodiversity", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_ss_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time with extinction threshold ####

nt.data.thresh <- non.targeted.sim.data
nt.data.thresh$abun <- if_else(non.targeted.sim.data$abun/non.targeted.sim.data$k < ext.thresh, 0, non.targeted.sim.data$abun)
nt.data.thresh[is.na(nt.data.thresh)] <- 0

summary.thresh <- nt.data.thresh %>% 
  group_by(sparing,sparedProp,catch,MSYProp,time.step,species,simulation) %>% 
  summarise(total = sum(abun),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,simulation) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

summary.thresh.mean <- summary.thresh %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,biodiversity.measure) %>% 
  summarise(mean.mean = mean(biodiversity.value))

p <- ggplot() +
  geom_line(data = summary.thresh,
            aes(x=time.step, 
                y = biodiversity.value, 
                colour = biodiversity.measure,
                group = interaction(simulation, biodiversity.measure)),
            alpha = 0.2) +
  geom_line(data = summary.thresh.mean,
            aes(x=time.step, 
                y = mean.mean, 
                colour = biodiversity.measure,
                group = biodiversity.measure),
            size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Biodiversity", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"),begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(limits = c(0,1), sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_ss_time_thresh"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing results at final time accounting for extinction threshold ####

final.summary.thresh <- filter(summary.thresh, time.step == n.time)
final.summary.thresh.mean <- filter(summary.thresh.mean, time.step == n.time) 

# Creating new objects to be saved later
final.summary.thresh.save <- final.summary.thresh
final.summary.thresh.mean.save <- final.summary.thresh.mean

p <- ggplot() +
  geom_line(data = final.summary.thresh,
            aes(x=sparedProp, 
                y = biodiversity.value, 
                colour = MSYProp, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  geom_point(data = final.summary.thresh,
             aes(x=sparedProp, 
                 y = biodiversity.value, 
                 colour = MSYProp, 
                 group = interaction(simulation, MSYProp)),
             alpha = 0.2) +
  geom_line(data = final.summary.thresh.mean,
            aes(x=sparedProp,
                y=mean.mean,
                colour=MSYProp,
                group=MSYProp),
            size = 1.05) +
  geom_point(data = final.summary.thresh.mean,
             aes(x=sparedProp,
                 y=mean.mean,
                 colour=MSYProp,
                 group=MSYProp),
             size = 1.05) +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Biodiversity", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_ss_final_time_thresh"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### 11/10/2022: Alt. SSS thresh visualisation ####

p <- ggplot() +
  geom_line(data = final.summary.thresh,
            aes(x=sparedProp, 
                y = biodiversity.value, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  #geom_point(data = final.summary.thresh,
  #           aes(x=sparedProp, 
  #               y = biodiversity.value, 
  #               group = interaction(simulation, MSYProp)),
  #           alpha = 0.2) +
  geom_line(data = final.summary.thresh.mean,
            aes(x=sparedProp,
                y=mean.mean,
                group=MSYProp),
            size = 1.05,
            colour = "#FF6240") +
  geom_point(data = final.summary.thresh.mean,
            aes(x=sparedProp,
                y=mean.mean,
                group=MSYProp),
            size = 1.05,
            colour = "#FF6240") +
  facet_grid(biodiversity.measure ~ MSYProp, labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Biodiversity", 
       x = "Proportion of seascape spared") +
  ylim(0,1.1) +
  xlim(0,1) +
  geom_hline(data = final.summary.thresh.mean %>% filter(sparedProp == 0), aes(yintercept = mean.mean), linetype = "dotted", colour = "black") +
  theme_bw()

plot.name <- "multi_ss_final_time_thresh_alt"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species have fallen below the extinction threshold over time ####

collapsed.data <- non.targeted.sim.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species, simulation) %>% 
  summarise(total = sum(abun), carrying = 10) %>% # Now hardcoded to 10 since manuscript makes sure all species have 10 patches. Previously `sum(k)`
  mutate(proportion = total/carrying) %>% 
  mutate(collapse = if_else(proportion < ext.thresh,T,F)) %>% 
  mutate(collapse05 = if_else(proportion < 0.5,T,F)) %>% 
  mutate(collapse01 = if_else(proportion < 0.1,T,F)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step, simulation) %>% 
  summarise(no.collapsed = sum(collapse), no.healthy = sum(!collapse),
            no.collapsed05 = sum(collapse05), no.healthy05 = sum(!collapse05),
            no.collapsed01 = sum(collapse01), no.healthy01 = sum(!collapse01)) %>%
  mutate(prop.collapsed = no.collapsed/(no.healthy+no.collapsed)) %>%
  mutate(prop.collapsed05 = no.collapsed05/(no.healthy05+no.collapsed05)) %>%
  mutate(prop.collapsed01 = no.collapsed01/(no.healthy01+no.collapsed01))

collapsed.data.mean <- collapsed.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(prop.collapsed.mean = mean(prop.collapsed),
            prop.collapsed.mean05 = mean(prop.collapsed05),
            prop.collapsed.mean01 = mean(prop.collapsed01)) 

p <- ggplot() +
  geom_line(data = collapsed.data, aes(x = time.step, y =prop.collapsed, group = simulation), alpha = 0.2) +
  geom_line(data = collapsed.data.mean, aes(x = time.step, y = prop.collapsed.mean), size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = c("Proportion of species below collapse threshold"), 
       x = "Time step") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(limits = c(0,1), sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  theme_bw()

plot.name <- "multi_collapse_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species have fallen below the extinction threshold at final time ####

final.collapsed <- filter(collapsed.data, time.step == n.time)
final.collapsed.mean <- filter(collapsed.data.mean, time.step == n.time)

# Creating new objects to be saved later
final.collapsed.save <- final.collapsed
final.collapsed.mean.save <- final.collapsed.mean

p <- ggplot() +
  geom_line(data = final.collapsed,
            aes(x=sparedProp, y = prop.collapsed, colour = MSYProp, group = interaction(simulation, MSYProp)), alpha = 0.2) +
  geom_point(data = final.collapsed,
             aes(x=sparedProp, y = prop.collapsed, colour = MSYProp, group = interaction(simulation, MSYProp)), alpha = 0.2) +
  geom_line(data = final.collapsed.mean,
            aes(x=sparedProp, y = prop.collapsed.mean, colour = MSYProp), size = 1.05) +
  geom_point(data = final.collapsed.mean,
             aes(x=sparedProp, y = prop.collapsed.mean, colour = MSYProp), size = 1.05) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Proportion of species below collapse threshold", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_collapse_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species meet IUCN Red List criteria over time ####

# if (IUCN.option == 1){ # 1 = standard categories (CR = 90% reduction etc.)
#   IUCN.data <- non.targeted.sim.data %>% 
#     group_by(sparing,sparedProp,MSYProp,time.step,species,simulation) %>% 
#     summarise(total = sum(abun), carrying = sum(k)) %>% 
#     mutate(proportion = total/carrying) %>% 
#     mutate(status = case_when(
#       0 == proportion ~ "extinct",
#       0 < proportion & proportion <= (1-0.9) ~ "critically endangered",
#       (1-0.9) < proportion & proportion <= (1-0.7) ~ "endangered",
#       (1-0.7) < proportion & proportion <= (1-0.5) ~ "vulnerable",
#       TRUE ~ "near threatened / least concern")) %>% 
#     group_by(sparing, sparedProp,MSYProp, time.step,simulation) %>% 
#     count(status)
# } else if (IUCN.option == 2){ # 2 = conservative categories (CR = 80% reduction etc.)
#   IUCN.data <- non.targeted.sim.data %>% 
#     group_by(sparing,sparedProp,MSYProp,time.step,species,simulation) %>% 
#     summarise(total = sum(abun), carrying = sum(k)) %>% 
#     mutate(proportion = total/carrying) %>% 
#     mutate(status = case_when(
#       0 == proportion ~ "extinct",
#       0 < proportion & proportion <= (1-0.8) ~ "critically endangered",
#       (1-0.8) < proportion & proportion <= (1-0.5) ~ "endangered",
#       (1-0.5) < proportion & proportion <= (1-0.3) ~ "vulnerable",
#       TRUE ~ "near threatened / least concern")) %>% 
#     group_by(sparing, sparedProp,MSYProp, time.step,simulation) %>% 
#     count(status)
# } 
# 
# IUCN.data$status <- factor(IUCN.data$status, 
#                            levels = rev(c("extinct",
#                                           "critically endangered",
#                                           "endangered",
#                                           "vulnerable",
#                                           "near threatened / least concern")))
# 
# IUCN.data.mean <- IUCN.data %>% 
#   group_by(sparing, sparedProp, MSYProp, time.step, status) %>% 
#   summarise(n.mean = mean(n))
# 
# p <- ggplot(IUCN.data.mean, aes(x = time.step, y = n.mean/(n.species*8), colour = status, fill = status)) +
#   facet_grid(sparing~MSYProp) +
#   geom_area(position = "stack") +
#   labs(y = c("Proportion of species in category"), 
#        x = "Time step") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#   scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
#   scale_color_viridis_d(name = "IUCN Red List category",
#                         labels = c("Near threatened / least concern",
#                                    "Vulnerable",
#                                    "Endangered",
#                                    "Critically endangered",
#                                    "Extinct")) +
#   scale_fill_viridis_d(name = "IUCN Red List category",
#                        labels = c("Near threatened / least concern",
#                                   "Vulnerable",
#                                   "Endangered",
#                                   "Critically endangered",
#                                   "Extinct")) +
#   theme_bw()
# 
# plot.name <- "multi_IUCN_time"
# plot.path <- paste(path, plot.name, sep = "/")
# pdf.path <- paste(plot.path, ".pdf", sep = "")
# png.path <- paste(plot.path, ".png", sep = "")
# ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
# ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### How many species meet IUCN Red List criteria at final time ####

# COMMENTED OUT DUE TO CAUSING HPC BUG

#final.IUCN.mean <- filter(IUCN.data.mean, time.step == n.time) %>% 
#  complete(sparedProp, status, fill = list(n.mean = 0))

#p <- ggplot(data = final.IUCN.mean,
#            aes(x=sparedProp, y = n.mean/(n.species*8), colour = status, fill = status)) +
#  geom_area(position = "stack") +
#  #geom_point(position = "stack") +
#  facet_grid(.~MSYProp) +
#  labs(y = "Proportion of species", 
#       x = "Proportion of seascape spared") +
#  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  scale_color_viridis_d(name = "IUCN Red List category",
#                        labels = c("Near threatened / least concern",
#                                   "Vulnerable",
#                                   "Endangered",
#                                   "Critically endangered",
#                                   "Extinct")) +
#  scale_fill_viridis_d(name = "IUCN Red List category",
#                       labels = c("Near threatened / least concern",
#                                  "Vulnerable",
#                                  "Endangered",
#                                  "Critically endangered",
#                                  "Extinct")) +
#  theme_bw()

#plot.name <- "multi_IUCN_final_time"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Habitat damage plots ####

### Targeted k over time

summary <- targeted.sim.data %>% 
  group_by(sparing,spared,sparedProp,patch,catch,MSYProp,time.step,simulation) %>% 
  summarise(total = sum(k))

p <- ggplot(summary, aes(x = time.step, colour = patch, y = total, group = interaction(simulation, patch), linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Habitat quality", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_k_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time ####

summary <- non.targeted.sim.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species,simulation) %>% 
  summarise(total = sum(k),
            total.k = sum(pristine.k)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,simulation) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            arith.mean.k = mean(total.k),
            geom.mean.k = gm.mean(total.k),
            prop.arith = arith.mean/arith.mean.k,
            prop.geom = geom.mean/geom.mean.k) %>% 
  pivot_longer(cols = c(prop.arith, prop.geom),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

summary.mean <- summary %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,biodiversity.measure) %>% 
  summarise(mean.mean = mean(biodiversity.value))

p <- ggplot() +
  geom_line(data = summary,
            aes(x=time.step, 
                y = biodiversity.value, 
                colour = biodiversity.measure, 
                group = interaction(simulation, biodiversity.measure)),
            alpha = 0.5) +
  geom_line(data = summary.mean, 
            aes(x = time.step, 
                y = mean.mean,
                colour = biodiversity.measure), 
            size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Mean habitat quality", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"), begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_ss_k_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)
final.summary.mean <- filter(summary.mean, time.step == n.time) 

# Creating new objects to be saved later
final.habitat.save <- final.summary
final.habitat.mean.save <- final.summary.mean

p <- ggplot() +
  geom_line(data = final.summary,
            aes(x=sparedProp, 
                y = biodiversity.value, 
                colour = MSYProp, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  geom_point(data = final.summary,
             aes(x=sparedProp, 
                 y = biodiversity.value, 
                 colour = MSYProp, 
                 group = interaction(simulation, MSYProp)),
             alpha = 0.2) +
  geom_line(data = final.summary.mean,
            aes(x=sparedProp,
                y=mean.mean,
                colour=MSYProp,
                group=MSYProp),
            size = 1.05) +
  geom_point(data = final.summary.mean,
             aes(x=sparedProp,
                 y=mean.mean,
                 colour=MSYProp,
                 group=MSYProp),
             size = 1.05) +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("prop.arith" = "Arithmetic mean", "prop.geom" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Mean habitat quality", 
       x = "Proportion of seascape spared") +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_ss_k_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Effort plots ####

### Targeted effort over time ####

summary <- targeted.sim.data %>% 
  group_by(sparing,spared,sparedProp,patch,catch,MSYProp,time.step,simulation) %>% 
  summarise(total = sum(effort))

p <- ggplot(summary, aes(x = time.step, colour = patch, y = total, group = interaction(simulation, patch), linetype = spared)) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Effort", 
       x = "Time") +
  geom_line() +
  scale_colour_discrete(name = "Patch") +
  scale_linetype_discrete(name = "Spared?", labels = c("No", "Yes")) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_effort_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing over time ####

summary <- non.targeted.sim.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,species,simulation) %>% 
  summarise(total = sum(effort)) %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,simulation) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total)) %>% 
  pivot_longer(cols = c(arith.mean, geom.mean),
               names_to = "biodiversity.measure",
               values_to = "biodiversity.value")

summary.mean <- summary %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,biodiversity.measure) %>% 
  summarise(mean.mean = mean(biodiversity.value))

p <- ggplot() +
  geom_line(data = summary,
            aes(x=time.step, 
                y = biodiversity.value, 
                colour = biodiversity.measure, 
                group = interaction(simulation, biodiversity.measure)),
            alpha = 0.5) +
  geom_line(data = summary.mean, 
            aes(x = time.step, 
                y = mean.mean,
                colour = biodiversity.measure), 
            size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Mean effort", 
       x = "Time") +
  scale_colour_viridis_d(name = "Biodiversity measure", labels = c("Arithmetic mean", "Geometric mean"), begin = 0, end = 0.8) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  #ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_ss_effort_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing at final time ####

final.summary <- filter(summary, time.step == n.time)
final.summary.mean <- filter(summary.mean, time.step == n.time) 

# Creating new objects to be saved later
final.effort.save <- final.summary
final.effort.mean.save <- final.summary.mean

p <- ggplot() +
  geom_line(data = final.summary,
            aes(x=sparedProp, 
                y = biodiversity.value, 
                colour = MSYProp, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  geom_point(data = final.summary,
             aes(x=sparedProp, 
                 y = biodiversity.value, 
                 colour = MSYProp, 
                 group = interaction(simulation, MSYProp)),
             alpha = 0.2) +
  geom_line(data = final.summary.mean,
            aes(x=sparedProp,
                y=mean.mean,
                colour=MSYProp,
                group=MSYProp),
            size = 1.05) +
  geom_point(data = final.summary.mean,
             aes(x=sparedProp,
                 y=mean.mean,
                 colour=MSYProp,
                 group=MSYProp),
             size = 1.05) +
  facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("arith.mean" = "Arithmetic mean", "geom.mean" = "Geometric mean"))) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Mean effort", 
       x = "Proportion of seascape spared") +
  #ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_ss_effort_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## Calculate management costs ####
#costs <- final.summary.thresh %>% 
#  mutate(MPA.cost = ifelse(sparing > 0, ((sparing/n.patches)*time.step*mpa.ongoing.cost)+mpa.init.cost, 0),
#         man.cost = ((1-(sparing/n.patches))*time.step*man.ongoing.cost)+man.init.cost,
#         tot.cost = MPA.cost + man.cost,
#         agg.catch = catch*time.step,
#         cpuc = catch/tot.cost) %>% 
#  pivot_longer(cols = c(MPA.cost, man.cost, tot.cost), names_to = "cost.type", values_to = "cost.cost")

### Costs across strategies ####
# No mean required
#p <- ggplot(costs, aes(x = sparing/n.patches, y = cost.cost, colour = cost.type, group = interaction(simulation, cost.type))) +
#  geom_line() +
#  geom_point() +
#  facet_wrap(~as.factor(catch/max(catch))) +
#  scale_colour_discrete(name = "Cost type", labels = c("Conventional management", "MPAs", "Combined")) +
#  labs(y = "Cost", 
#       x = "Proportion of seascape spared") +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
#  theme_bw()

#plot.name <- "multi_costs"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### CPUC (catch per unit cost) across catches/strategies ####
# No mean required
#p <- ggplot(costs, aes(x = sparedProp, y = cpuc/max(catch), colour = MSYProp, group = interaction(simulation, MSYProp))) +
#  geom_line() +
#  geom_point() +
#  scale_colour_discrete(name = "Catch") +
#  labs(y = "Catch per unit cost (CPUC)", 
#       x = "Proportion of seascape spared") +
#  geom_hline(yintercept = 0, linetype = "dotted") +
#  theme_bw()

#plot.name <- "multi_CPUC"
#plot.path <- paste(path, plot.name, sep = "/")
#pdf.path <- paste(plot.path, ".pdf", sep = "")
#png.path <- paste(plot.path, ".png", sep = "")
#ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
#ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

## If using open-access, uncomment this block for visualisation
# Has not been configured for multiple simulations
# 
# open.access.data <- data %>% 
#   group_by(sparing,catch,time.step) %>% 
#   summarise(real.catch = sum(catch.extracted)) %>% 
#   filter(time.step == n.time)
# 
# final.open.access <- left_join(final.time, open.access.data, by = c("sparing"))
# 
# ggplot(data = final.open.access,
#        aes(x=sparing/n.patches, y = biodiversity.value, colour = real.catch)) +
#   geom_line() +
#   facet_wrap(biodiversity.measure ~ ., labeller = labeller(biodiversity.measure = c("arith.mean" = "Arithmetic mean", "geom.mean" = "Geometric mean"))) +
#   scale_colour_viridis_c(name = "Catch") +
#   labs(y = "Biodiversity", 
#        x = "Proportion of seascape spared") +
#   theme_bw()
# beep()

# Plots for carbon ####

### View carbon change over time ####

carbon.sim.data.slice <- carbon.sim.data %>% 
  filter(simulation == 1)

p <- ggplot(carbon.sim.data.slice, aes(x = time.step, colour = as.factor(patch), y = carbon.pres, group = as.factor(patch), linetype = as.factor(spared))) +
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

plot.name <- "multi_carbon_total_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing results over time ####

summary <- carbon.sim.data %>% 
  group_by(sparing,sparedProp,MSYProp,time.step,simulation) %>% 
  summarise(mean.carbon.pres = mean(carbon.pres))

summary.mean <- summary %>% 
  group_by(sparing,sparedProp,MSYProp,time.step) %>% 
  summarise(mean.mean = mean(mean.carbon.pres))

p <- ggplot() +
  geom_line(data = summary,
            aes(x=time.step, 
                y = mean.carbon.pres, 
                group = simulation),
            alpha = 0.5) +
  geom_line(data = summary.mean, 
            aes(x = time.step, 
                y = mean.mean), 
            size = 1.05) +
  facet_grid(sparing~MSYProp) +
  labs(y = "Carbon", 
       x = "Time") +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Catch", labels = NULL, breaks = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Number of patches spared", labels = NULL, breaks = NULL)) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot.name <- "multi_ss_carbon_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

### Sparing/sharing results at final time ####

final.summary <- filter(summary, time.step == n.time)
final.summary.mean <- filter(summary.mean, time.step == n.time) 

p <- ggplot() +
  geom_line(data = final.summary,
            aes(x=sparedProp, 
                y = mean.carbon.pres, 
                colour = MSYProp, 
                group = interaction(simulation, MSYProp)),
            alpha = 0.2) +
  geom_point(data = final.summary,
             aes(x=sparedProp, 
                 y = mean.carbon.pres, 
                 colour = MSYProp, 
                 group = interaction(simulation, MSYProp)),
             alpha = 0.2) +
  geom_line(data = final.summary.mean,
            aes(x=sparedProp,
                y=mean.mean,
                colour=MSYProp,
                group=MSYProp),
            size = 1.05) +
  geom_point(data = final.summary.mean,
             aes(x=sparedProp,
                 y=mean.mean,
                 colour=MSYProp,
                 group=MSYProp),
             size = 1.05) +
  scale_colour_viridis_d(name = "Catch", begin = 0, end = 0.8) +
  labs(y = "Carbon", 
       x = "Proportion of seascape spared") +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw()

plot.name <- "multi_ss_carbon_final_time"
plot.path <- paste(path, plot.name, sep = "/")
pdf.path <- paste(plot.path, ".pdf", sep = "")
png.path <- paste(plot.path, ".png", sep = "")
ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")

# Calculate habitat correlations before and after adjustments ####

if(non.targ.habitat.option == 4.1){
  # Before removal of 0
  correlation.list.before <- lapply(all.species.habitat.list, cor)
  
  mean.calc <- correlation.list.before[[1]]
  mean.calc[mean.calc == 1] <- NA
  mean.before <- round(mean(mean.calc, na.rm = T), 3)
  title.before <- paste("Correlation before 0s, avg =", mean.before, sep = " ")
  
  c1.before <- ggcorrplot(correlation.list.before[[1]]) + ggtitle(title.before)
  # Average correlation
  
  # After removal of 0
  all.species.habitat.list[[1]][all.species.habitat.list[[1]] < 0] <- 0
  correlation.list.after <- lapply(all.species.habitat.list, cor)
  
  mean.calc <- correlation.list.after[[1]]
  mean.calc[mean.calc == 1] <- NA
  mean.after <- round(mean(mean.calc, na.rm = T), 3)
  title.after <- paste("Correlation after 0s, avg =", mean.after, sep = " ")
  
  c1.after <- ggcorrplot(correlation.list.after[[1]]) + ggtitle(title.after)
  
  p <- c1.before + c1.after
  plot.name <- "corr.comparison"
  plot.path <- paste(path, plot.name, sep = "/")
  pdf.path <- paste(plot.path, ".pdf", sep = "")
  png.path <- paste(plot.path, ".png", sep = "")
  ggsave(filename = pdf.path, p, width = 20, height = 20, units = "cm")
  ggsave(filename = png.path, p, width = 20, height = 20, units = "cm")
}

# Save environment ####

# Identify scenario name 
scenario.name <- sub(".*_(.*)", "\\1", simulation.name) 
env.path <- c("../../rds/hpc-work/SSS_custom_model/environments")
env.path <- paste(env.path, scenario.name, sep = "/")
dir.create(env.path, recursive = TRUE)
RData.path <- paste(env.path, simulation.name, sep = "/")
RData.path <- paste(RData.path, "RData", sep = ".")
#save.image(file = RData.path)

# Save summary objects for collated data figure ####

bio.path <- "../../rds/hpc-work/SSS_custom_model/summaries"
bio.path <- paste(bio.path, scenario.name, "summary_files", sep = "/")
dir.create(bio.path, recursive = TRUE)
RData.path <- paste(bio.path, simulation.name, sep = "/")
RData.path <- paste(RData.path, "summary", sep = "_")
RData.path <- paste(RData.path, "RData", sep = ".")
save(final.summary.thresh.save, final.summary.thresh.mean.save, file = RData.path)

# Save effort objects for collated data figure ####

eff.path <- "../../rds/hpc-work/SSS_custom_model/summaries" 
eff.path <- paste(eff.path, scenario.name, "effort_files", sep = "/") 
dir.create(eff.path, recursive = TRUE) 
RData.path <- paste(eff.path, simulation.name, sep = "/")
RData.path <- paste(RData.path, "effort", sep = "_")
RData.path <- paste(RData.path, "RData", sep = ".")
save(final.effort.save, final.effort.mean.save, file = RData.path)

# Save habitat objects for collated data figure ####

hab.path <- "../../rds/hpc-work/SSS_custom_model/summaries"
hab.path <- paste(hab.path, scenario.name, "habitat_files", sep = "/")
dir.create(hab.path, recursive = TRUE) 
RData.path <- paste(hab.path, simulation.name, sep = "/") 
RData.path <- paste(RData.path, "habitat", sep = "_")
RData.path <- paste(RData.path, "RData", sep = ".") 
save(final.habitat.save, final.habitat.mean.save, file = RData.path)

# Save collapse objects for collated data figure

collapse.path <- "../../rds/hpc-work/SSS_custom_model/summaries" 
collapse.path <- paste(collapse.path, scenario.name, "collapse_files", sep = "/") 
dir.create(collapse.path, recursive = TRUE)
RData.path <- paste(collapse.path, simulation.name, sep = "/") 
RData.path <- paste(RData.path, "collapse", sep = "_") 
RData.path <- paste(RData.path, "RData", sep = ".") 
save(final.collapsed.save, final.collapsed.mean.save, file = RData.path)

# Record time taken to run script
end.time <- Sys.time() # Record end time for main body of script
tot.time <- end.time - start.time # Calculate time taken to run script

# Final notes ####
print("The script successfully ran and it took:")
print(tot.time)
beep(sound = 8)
