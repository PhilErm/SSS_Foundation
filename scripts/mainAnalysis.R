# Script details ####

# Script using parameters and functions to undertake main analysis and generate figures in 
# "Meeting wild-caught seafood demand at least cost to biodiversity"

# Load packages ####

library(tidyverse) # For dplyr, ggplot2
library(data.table) # For managing lists
library(ggridges) # For ridgeplots
library(patchwork) # For multipanel plots
library(latex2exp) # For plot labels
library(directlabels) # For plot labels
library(ggrepel) # For plot labels
library(progress) # For progress bars
library(costatcompanion) # For particular statistical calculations
library(beepr) # For beeping
library(ggnewscale) # For multiple colour/fill scales

# Load scripts ####

source("scripts/parameters.R")
source("scripts/functions.R")
source("scripts/dispFunctions.R")

# Generate distributions from parameters ####

# SECTION DESCRIPTION/NOTES
# Here we generate the parameter distributions from which parameters from individual species will
# be drawn

# Calculate standard deviations from confidence intervals
dist.pars$log.sd.q <- sd.from.upper(dist.pars$mean.q, dist.pars$upci.q)
dist.pars$log.sd.r <- sd.from.lower(dist.pars$mean.r, dist.pars$lowci.r)

# Generate distributions from mean and standard deviations
# Catchability distribution
q.values <- seq(0,1,0.001) # Range of values explored
dists.q <- list()
for (i in 1:nrow(dist.pars)){
  probs <- dlnorm(x=q.values, # Generate distribution of probabilities for particular class
                  meanlog=log(dist.pars[i,"mean.q"]),
                  sdlog=dist.pars[i,"log.sd.q"])
  results.df <- cbind.data.frame(q.values, probs)
  dists.q[[dist.pars[i,"class"]]] <- results.df
}
dists.q.df <- rbindlist(dists.q, idcol=T)
dists.q.df <- dists.q.df %>% 
  rename(class = .id)

# Reproduction distribution
r.values <- seq(0,10,0.01) # Range of values explored
dists.r <- list()
for (i in 1:nrow(dist.pars)){
  probs <- dlnorm(x=r.values, # Generate distribution of probabilities for particular class
                  meanlog=log(dist.pars[i,"mean.r"]),
                  sdlog=dist.pars[i,"log.sd.r"])
  results.df <- cbind.data.frame(r.values, probs)
  dists.r[[dist.pars[i,"class"]]] <- results.df
}
dists.r.df <- rbindlist(dists.r, idcol=T)
dists.r.df <- dists.r.df %>% 
  rename(class = .id)

# Plot distributions

# Reproduction distribution
distsA <- ggplot(dists.r.df, aes(x=r.values, height=probs, y=class))+
  geom_ridgeline(alpha = 0.75) +
  labs(tag = "A",
       y = "Class",
       x = TeX("Per capita growth rate $(\\textit{r})$")) +
  xlim(0,5) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
distsA

# Catchability distribution
distsB <- ggplot(dists.q.df, aes(x=q.values, height=probs, y=class))+
  geom_ridgeline(alpha = 0.75, scale = 0.15) +
  labs(tag = "B",
       y = "Class",
       x = TeX("Catchability $(\\textit{q})$")) +
  xlim(0,0.5) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
distsB

# Arrange and save
dists <- 
  distsA / 
  distsB +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
dists
#ggsave(filename = "figs/dists.pdf", dists, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/dists.png", dists, width = 20, height = 20, units = "cm")

# Take random samples from distributions ####

# SECTION DESCRIPTION/NOTES
# Here we sample the distributions to ensure parameterisation is functioning as intended

# Set number of species to be drawn from distribution
num.spec <- 10

# Drawing species
set.seed(1) # The seed used to generate the figures in the manuscript
all.species <- builder(n = num.spec, 
                       mean.q = dist.pars$mean.q, 
                       sd.q = dist.pars$log.sd.q,
                       mean.r = dist.pars$mean.r, 
                       sd.r = dist.pars$log.sd.r,
                       name = dist.pars$class)

# Plot realised distributions
# Note: black lines (individual samples) and red distributions (realised distributions generated from individual
# samples) should begin to approximate theoretical distributions. Due to limitations with geom_density_ridges,
# realised distributions cannot be plotted as lognormal, so they are not compared to lognormal distributions here.
# This means, for instance, that some distributions (both theoretical and realised) will go below 0. However, no
# actual sample (black line) will ever be below 0
# Catchability
ggplot(all.species, aes(x = q.value, y = class)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7) +
  geom_ridgeline(data = dists.q.df, aes(x=q.values, height=probs, y=class, alpha = 0.05, scale = 0.10, fill = "red"))

# Reproduction
ggplot(all.species, aes(x = r.value, y = class)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7)+
  geom_ridgeline(data = dists.r.df, aes(x=r.values, height=probs, y=class, alpha = 0.05, fill = "red"))

# Model without dispersal ####

## Examining yield-density curves (no dispersal) ####

# SECTION DESCRIPTION/NOTES
# Here we see how the abundance of species generated by the sampling process changes with fishing effort
# and catch of the targeted species

# Draw species
# Set number of species to be drawn from distribution
num.spec <- 10

# Drawing species
set.seed(1) # The seed used to generate the figures in the manuscript
all.species <- builder(n = num.spec, 
                       mean.q = dist.pars$mean.q, 
                       sd.q = dist.pars$log.sd.q,
                       mean.r = dist.pars$mean.r, 
                       sd.r = dist.pars$log.sd.r,
                       name = dist.pars$class)

### Abundance from effort for sparing = 0 ####

# Set plot parameters
s <- 0
e <- seq(0,5,0.01)

# Generate non-targeted species data
data <- NULL
for(i in 1:nrow(all.species)){
  class <- all.species$class[i]
  species <- all.species$species[i]
  effort <- e
  total <- abun.eff(s,e,all.species$q.value[i],all.species$r.value[i])
  res <- cbind.data.frame(class,species,effort,total)
  data <- rbind.data.frame(data, res)
}

# Generate targeted species data
class <- "Malacostraca (targeted)"
species <- "Malacostraca (targeted)"
effort <- e
total <- abun.eff(s,e,malacostraca.mean.q,malacostraca.mean.r)
res <- cbind.data.frame(class,species,effort,total)
data <- rbind.data.frame(data, res)

# Plot
abunVsEffA <- ggplot(data, aes(x = effort, y = total, group = species, col = class, size = class)) +
  geom_line() +
  geom_line(data = res) +
  labs(tag = "A",
       y = TeX("Equib. abundance $(\\textit{n/K})$"),
       x = TeX("Fishing effort $(\\textit{E})$")) +
  scale_colour_manual(name = "Species class",
                      values = c("#440154FF", "#46337EFF", "#365C8DFF",
                                 "#277F8EFF", "#1FA187FF", "#4AC16DFF",
                                 "black", "#9FDA3AFF", "#FDE725FF")) +
  scale_size_manual(name = "Species class", values = c(0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24)) +
  guides(colour=guide_legend(ncol=3,bycol=FALSE))
abunVsEffA

### Abundance from effort for sparing = 0.25 ####

# Set plot parameters
s <- 0.25
e <- seq(0,5,0.01)

# Generate non-targeted species data
data <- NULL
for(i in 1:nrow(all.species)){
  class <- all.species$class[i]
  species <- all.species$species[i]
  effort <- e
  total <- abun.eff(s,e,all.species$q.value[i],all.species$r.value[i])
  res <- cbind.data.frame(class,species,effort,total)
  data <- rbind.data.frame(data, res)
}

# Generate targeted species data
class <- "Malacostraca (targeted)"
species <- "Malacostraca (targeted)"
effort <- e
total <- abun.eff(s,e,malacostraca.mean.q,malacostraca.mean.r)
res <- cbind.data.frame(class,species,effort,total)
data <- rbind.data.frame(data, res)

# Plot
abunVsEffB <- ggplot(data, aes(x = effort, y = total, group = species, col = class, size = class)) +
  geom_line() +
  geom_line(data = res) +
  labs(tag = "B",
       y = element_blank(),
       x = TeX("Fishing effort $(\\textit{E})$")) +
  scale_colour_manual(name = "Species class",
                      values = c("#440154FF", "#46337EFF", "#365C8DFF",
                                 "#277F8EFF", "#1FA187FF", "#4AC16DFF",
                                 "black", "#9FDA3AFF", "#FDE725FF")) +
  scale_size_manual(name = "Species class", values = c(0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0.25, linetype = "dotted") +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24)) +
  guides(colour=guide_legend(ncol=3,bycol=FALSE))
abunVsEffB

### Abundance from catch for sparing = 0 ####

# Set targeted species parameters
target.r <- malacostraca.mean.r
target.q <- malacostraca.mean.q
kt <- 1 # Set to 1 because catch is calculated relative to carrying capacity
target.MSY.standard <- (target.r*kt)/4

# Set plot parameters
s <- 0
ct <- c(seq(0,target.MSY.standard,0.001),target.MSY.standard)

# Generate non-targeted species data
data <- NULL
for(i in 1:nrow(all.species)){
  class <- all.species$class[i]
  species <- all.species$species[i]
  catch <- ct
  total <- abun.catch(rt=target.r,
                      qt=target.q,
                      rn=all.species$r.value[i],
                      qn=all.species$q.value[i],
                      s=s,
                      ct=ct,
                      kt=kt)
  res <- cbind.data.frame(class,species,catch,total)
  data <- rbind.data.frame(data, res)
}

# Generate targeted species data
class <- "Malacostraca (targeted)"
species <- "Malacostraca (targeted)"
catch <- ct
total <- abun.catch(rt=target.r,
                    qt=target.q,
                    rn=target.r,
                    qn=target.q,
                    s=s,
                    ct=ct,
                    kt=kt)
res <- cbind.data.frame(class,species,catch,total)
data <- rbind.data.frame(data, res)

# Plot
abunVsCatchA <- ggplot(data, aes(x = catch/target.MSY.standard, y = total, group = species, col = class, size = class)) +
  geom_line()  +
  geom_line(data = res) +
  labs(tag = "B",
       y = TeX("Equib. abundance $(\\textit{n/K})$"),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  scale_colour_manual(name = "Species class",
                      values = c("#440154FF", "#46337EFF", "#365C8DFF",
                                 "#277F8EFF", "#1FA187FF", "#4AC16DFF",
                                 "black", "#9FDA3AFF", "#FDE725FF")) +
  scale_size_manual(name = "Species class", values = c(0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(sec.axis = sec_axis(
    name = TeX("Equib. catch of targeted species $(\\textit{C/K})$"),
    ~ .*target.MSY.standard )) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24)) +
  guides(colour=guide_legend(ncol=3,bycol=FALSE))
abunVsCatchA

### Abundance from catch for sparing = 0.25 ####

# Set targeted species parameters
target.r <- malacostraca.mean.r
target.q <- malacostraca.mean.q
kt <- 1 # Set to 1 because catch is calculated relative to carrying capacity
target.MSY.standard <- (target.r*kt)/4

# Set plot parameters
s <- 0.25
last.catch.value <- target.MSY.standard*(1-0.25)
ct <- c(seq(0,target.MSY.standard,0.001),last.catch.value)

# Generate non-targeted species data
data <- NULL
for(i in 1:nrow(all.species)){
  class <- all.species$class[i]
  species <- all.species$species[i]
  catch <- ct
  total <- abun.catch(rt=target.r,
                      qt=target.q,
                      rn=all.species$r.value[i],
                      qn=all.species$q.value[i],
                      s=s,
                      ct=ct,
                      kt=kt)
  res <- cbind.data.frame(class,species,catch,total)
  data <- rbind.data.frame(data, res)
}

# Generate targeted species data
class <- "Malacostraca (targeted)"
species <- "Malacostraca (targeted)"
catch <- ct
total <- abun.catch(rt=target.r,
                    qt=target.q,
                    rn=target.r,
                    qn=target.q,
                    s=s,
                    ct=ct,
                    kt=kt)
res <- cbind.data.frame(class,species,catch,total)
data <- rbind.data.frame(data, res)

# Plot
abunVsCatchB <- ggplot(data, aes(x = catch/target.MSY.standard, y = total, group = species, col = class, size = class)) +
  geom_line()  +
  geom_line(data = res) +
  labs(tag = "D",
       y = element_blank(),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  scale_colour_manual(name = "Species class",
                      values = c("#440154FF", "#46337EFF", "#365C8DFF",
                                 "#277F8EFF", "#1FA187FF", "#4AC16DFF",
                                 "black", "#9FDA3AFF", "#FDE725FF")) +
  scale_size_manual(name = "Species class", values = c(0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,0.5)) +
  geom_hline(yintercept = 0.25, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(sec.axis = sec_axis(
    name = TeX("Equib. catch of targeted species $(\\textit{C/K})$"),
    ~ .*target.MSY.standard )) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24)) +
  guides(colour=guide_legend(ncol=3,bycol=FALSE))
abunVsCatchB

# Arrange and save
# With sparing included
# abunPlots <- 
#   (abunVsEffA + abunVsEffB) /
#   (abunVsCatchA + abunVsCatchB) +
#   plot_layout(guides = "collect") & 
#   theme(legend.position = 'bottom')
# abunPlots

# Without sparing included
abunPlots <- 
  (abunVsEffA) /
  (abunVsCatchA) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
abunPlots
#ggsave(filename = "figs/abunPlots.pdf", abunPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/abunPlots.png", abunPlots, width = 20, height = 20, units = "cm")

## Perform sparing/sharing analysis across key targets (no dispersal) ####

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Arithmetic mean
sparingVsSharingPlotsA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = element_blank(), 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
sparingVsSharingPlotsA  

# Geometric mean
sparingVsSharingPlotsB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "D", 
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
sparingVsSharingPlotsB

## Calculate effort for sparing/sharing analysis across key targets (no dispersal) ####

# SECTION DESCRIPTION/NOTES
# Since the targeted species never changes, only needs to be done once. For ease of coding, other
# superfluous results (abundances etc.) are still generated. Because the simplest approach is to
# calculate effort in just fished zone and then make adjustment later, equations for calculating 
# zone abundances separately are used

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
kt <- 1

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for target species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- abun.catch.in(k=kt, s=spect.s[i])
    out.pop <- abun.catch.out(r=target.r, k=kt, s=spect.s[i], c=spect.c[j])
    out.pop[Im(out.pop) > 0.0001] <- NA
    out.pop[Im(out.pop) < -0.0001] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
    
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Generate results for non-targeted species
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    effort <- results$e.value
    sparing <- results$s.value
    inside <- abun.eff.in(kt,sparing)
    outside <- abun.eff.out(all.species$r.value[i],kt,sparing,effort,all.species$q.value[i])
    total <- inside+outside
    res <- cbind.data.frame(class,species,effort,catch,sparing,inside,outside,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)
# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual sims
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            effort = unique(effort))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Effort
high.effort <- data
high.effort <- high.effort %>%
  group_by(catch,sim) %>%
  slice_max(effort*(1-sparing), n = 1)

low.effort <- data
low.effort <- low.effort %>%
  group_by(catch,sim) %>%
  slice_min(effort*(1-sparing), n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean
high.effort.t <- high.effort
low.effort.t <- low.effort

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across sims
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            med.effort = median(effort),
            medSE.effort = median_se(effort),
            mean.effort = mean(effort),
            meanSE.effort = mean_se(effort))

# Find highest and lowest
# Mean arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Median effort
high.med.effort <- all.sims
high.med.effort <- high.med.effort %>%
  group_by(catch) %>%
  slice_max(med.effort*(1-sparing), n = 1)

low.med.effort <- all.sims
low.med.effort <- low.med.effort %>%
  group_by(catch) %>%
  slice_min(med.effort*(1-sparing), n = 1)

# Mean effort
high.mean.effort <- all.sims
high.mean.effort <- high.mean.effort %>%
  group_by(catch) %>%
  slice_max(mean.effort*(1-sparing), n = 1)

low.mean.effort <- all.sims
low.mean.effort <- low.mean.effort %>%
  group_by(catch) %>%
  slice_min(mean.effort*(1-sparing), n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean
high.med.effort.t <- high.med.effort
low.med.effort.t <- low.med.effort
high.mean.effort.t <- high.mean.effort
low.mean.effort.t <- low.mean.effort

# Plot
effortFigB <- ggplot(data.t, aes(x = sparing, y = effort*(1-sparing), group = catch/target.MSY.standard)) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Highest values", fill = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Lowest values", fill = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest values" = "purple", "Lowest values" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B",
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  xlim(0,1) +
  ylim(0,3.75) +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24),legend.position="bottom") +
  ggtitle("Without dispersal")
effortFigB

## Comparing a species facing extinction with a species not facing extinction (no dispersal) ####

# SECTION DESCRIPTION/NOTES
# See section title

# From our earlier analysis of the first random assemblage, we know that its most sensitive
# Ascidiacea goes extinct, and that its most sensitive Bivalvia does not go extinct. We will
# use these as our two example species in a deeper examination

# Check parameters for assemblage 1
set.seed(1)
all.species <- builder(n = num.spec, 
                       mean.q = dist.pars$mean.q, 
                       sd.q = dist.pars$log.sd.q,
                       mean.r = dist.pars$mean.r, 
                       sd.r = dist.pars$log.sd.r,
                       name = dist.pars$class)
all.species$sensitivity <- all.species$q.value/all.species$r.value # Calculate sensitivity of species

# From sensitivity check we know that:
# Most sensitive Ascidiacea: Ascidiacea 1
# Most sensitive Bivalvia: Bivalvia 1

# Generate data for each species
# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
kt <- 1

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for target species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- abun.catch.in(k=kt, s=spect.s[i])
    out.pop <- abun.catch.out(r=target.r, k=kt, s=spect.s[i], c=spect.c[j])
    out.pop[Im(out.pop) > 0.0001] <- NA
    out.pop[Im(out.pop) < -0.0001] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
    
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Generate results for non-targeted species
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    effort <- results$e.value
    sparing <- results$s.value
    inside <- abun.eff.in(kt,sparing)
    outside <- abun.eff.out(all.species$r.value[i],kt,sparing,effort,all.species$q.value[i])
    total <- inside+outside
    res <- cbind.data.frame(class,species,effort,catch,sparing,inside,outside,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)
# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# From the main analysis above, filter out only assemblage 1 and Ascidiacea 1 / Bivalvia 1
extinct.dat <- data %>% 
  filter(sim == 1) %>% 
  filter(species == "Ascidiacea 1")

alive.dat <- data %>% 
  filter(sim == 1) %>% 
  filter(species == "Bivalvia 1")

# Process data
combined.dat <- rbind.data.frame(extinct.dat,alive.dat)
combined.dat <- combined.dat %>% 
  mutate(abundance.shared = total-sparing) %>% 
  mutate(abundance.spared = sparing) %>% 
  mutate(abundance.total = total) %>% 
  gather("zone","zone.abundance", c("abundance.shared","abundance.spared","abundance.total")) %>% 
  mutate(catch.MSY = catch/target.MSY.standard)

# Plot
# Specify labels
zone.labs <- c("Fished zone", "Spared zone", "Both zones")
names(zone.labs) <- c("abundance.shared","abundance.spared","abundance.total")
catch.labs <- c("Catch = 0.30", "Catch = 0.60", "Catch = 0.90")
names(catch.labs) <- c("0.3","0.6","0.9")

deepCatch <- ggplot(combined.dat, aes(x = sparing, y = zone.abundance, colour = species)) +
  geom_line() +
  facet_grid(vars(zone), vars(catch.MSY),labeller = labeller(zone = zone.labs, catch.MSY = catch.labs, alpha = 0.8)) +
  labs(y = TeX("Equib. abundance in zone ($\\textit{x/K}$, $\\textit{y/K}$, or $\\textit{n/K}$)"),
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  scale_colour_manual(name = "Species", 
                      labels = c("Most sensitive Ascidiacea (faces possible extinction)",
                                 "Most sensitive Bivalvia (never faces extinction)"),
                      values = c("#46337EFF", 
                                 "#9FDA3AFF")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw(base_size = 15) +
  theme(legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 2))
deepCatch

# Arrange and save
#ggsave(filename = "figs/deepCatch.pdf", deepCatch, width = 16.5, height = 16.5, units = "cm")
#ggsave(filename = "figs/deepCatch.png", deepCatch, width = 16.5, height = 16.5, units = "cm")

# Model with dispersal ####

## Examining yield-density curves (with dispersal) ####

# SECTION DESCRIPTION/NOTES
# Here we see how the abundance of species generated by the sampling process changes with catch of 
# the targeted species for species that can disperse between zones

# Set targeted species parameters
target.r <- malacostraca.mean.r
target.q <- malacostraca.mean.q
kt <- 1 # Set to 1 because catch is calculated relative to carrying capacity
target.MSY.standard <- (target.r*kt)/4

# Set plot parameters
s <- 0.3
last.catch.value <- target.MSY.standard*(1-0.25)
ct <- c(seq(0,target.MSY.standard,0.001),last.catch.value)

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- s
spect.c <- ct

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  in.pop,
                                  out.pop,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:1){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", 1)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- spect.s
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Generate targeted species data
class <- "Malacostraca (targeted)"
species <- "Malacostraca (targeted)"
catch <- results$c.value
total <- results$targ.n
sim <- 1
res <- cbind.data.frame(sim, class,species,catch,sparing,total)
data <- rbind.data.frame(data, res)

# Plot
abunVsCatchMigration <- ggplot(data, aes(x = catch/target.MSY.standard, y = total, group = species, col = class, size = class)) +
  geom_line()  +
  geom_line(data = res) +
  labs(y = element_blank(),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  scale_colour_manual(name = "Species class",
                      values = c("#440154FF", "#46337EFF", "#365C8DFF",
                                 "#277F8EFF", "#1FA187FF", "#4AC16DFF",
                                 "black", "#9FDA3AFF", "#FDE725FF")) +
  scale_size_manual(name = "Species class", values = c(0.5,0.5,0.5,0.5,0.5,0.5,2,0.5,0.5)) +
  geom_hline(yintercept = s, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(sec.axis = sec_axis(
    name = TeX("Equib. catch of targeted species $(\\textit{C/K})$"),
    ~ .*target.MSY.standard )) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24)) +
  xlim(0,1) +
  guides(colour=guide_legend(ncol=3,bycol=FALSE))
abunVsCatchMigration

## Perform sparing/sharing analysis across key targets (with dispersal) ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                             k=kt,
                             s=spect.s[i],
                             m=dispersal,
                             c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                             k=kt,
                             s=spect.s[i],
                             m=dispersal,
                             c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accommodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration formula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))
          
# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
sparingVsSharingPlotsC <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
sparingVsSharingPlotsC  

# Geometric mean
sparingVsSharingPlotsD <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "C", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
sparingVsSharingPlotsD

# Arrange and save
spareSharePlots <-
  ((sparingVsSharingPlotsC + sparingVsSharingPlotsA) / (sparingVsSharingPlotsD + sparingVsSharingPlotsB)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
spareSharePlots

#ggsave(filename = "figs/spareSharePlots.pdf", spareSharePlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/spareSharePlots.png", spareSharePlots, width = 20, height = 20, units = "cm")

# # Individual sims approach (no longer used)
# # Arithmetic mean
# indivA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
#   geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
#   scale_colour_discrete(type = c("#39558c","#1f968b","#bade28")) +
#   guides(colour = "none") +
#   new_scale_color() +
#   geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
#   geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
#   geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
#   scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
#   scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
#   scale_colour_viridis_d(begin = 0, end = 0.8) +
#   labs(tag = "A", 
#        y = "Biodiversity (arithmetic mean abundance)", 
#        x = TeX("Seascape in MPA $(\\textit{s})$")) +
#   ylim(0,1) +
#   xlim(0,1) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   guides(color = "none",
#          shape = guide_legend(order = 2),
#          fill = guide_legend(order = 2)) +
#   theme_bw(base_size = 12) +
#   theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
# indivA
# 
# # Geometric mean
# indivB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
#   geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
#   scale_colour_discrete(type = c("#39558c","#1f968b","#bade28")) +
#   guides(colour = "none") +
#   new_scale_color() +
#   geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
#   geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
#   geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
#   scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
#   scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
#   scale_colour_viridis_d(begin = 0, end = 0.8) +
#   labs(tag = "B", 
#        y = "Biodiversity (geometric mean abundance)", 
#        x = TeX("Seascape spared $(\\textit{s})$")) +
#   ylim(0,1) +
#   xlim(0,1) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   guides(colour = "none",
#          shape = guide_legend(order = 2),
#          fill = guide_legend(order = 2)) +
#   theme_bw(base_size = 12) +
#   theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
# indivB
# 
# # Arrange and save
# indivPlots <- 
#   (indivA + indivB) +
#   plot_layout(guides = "collect") & 
#   theme(legend.position = 'bottom')
# indivPlots

## Calculate effort for sparing/sharing analysis across key targets (with dispersal) ####

# SECTION DESCRIPTION/NOTES
# Since the targeted species never changes, only needs to be done once. For ease of coding, other
# superfluous results (abundances etc.) are still generated. Because the simplest approach is to
# calculate effort in just fished zone and then make adjustment later, equations for calculating 
# zone abundances separately are used

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total,effort)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data.migration <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accommodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration formula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
kt <- 1

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for target species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- abun.catch.in(k=kt, s=spect.s[i])
    out.pop <- abun.catch.out(r=target.r, k=kt, s=spect.s[i], c=spect.c[j])
    out.pop[Im(out.pop) > 0.0001] <- NA
    out.pop[Im(out.pop) < -0.0001] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
    
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Generate results for non-targeted species
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    effort <- results$e.value
    sparing <- results$s.value
    inside <- abun.eff.in(kt,sparing)
    outside <- abun.eff.out(all.species$r.value[i],kt,sparing,effort,all.species$q.value[i])
    total <- inside+outside
    res <- cbind.data.frame(class,species,effort,catch,sparing,inside,outside,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)
# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Combine s = 0 results with migration results
#data <- rbind.data.frame(data.migration,data)
data <- bind_rows(data.migration,data)

# Calculate metrics for individual sims
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total),
            effort = unique(effort))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Effort
high.effort <- data
high.effort <- high.effort %>%
  group_by(catch,sim) %>%
  slice_max(effort*(1-sparing), n = 1)

low.effort <- data
low.effort <- low.effort %>%
  group_by(catch,sim) %>%
  slice_min(effort*(1-sparing), n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean
high.effort.t <- high.effort
low.effort.t <- low.effort

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across sims
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            med.effort = median(effort),
            medSE.effort = median_se(effort),
            mean.effort = mean(effort),
            meanSE.effort = mean_se(effort))

# Find highest and lowest
# Mean arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Median effort
high.med.effort <- all.sims
high.med.effort <- high.med.effort %>%
  group_by(catch) %>%
  slice_max(med.effort*(1-sparing), n = 1)

low.med.effort <- all.sims
low.med.effort <- low.med.effort %>%
  group_by(catch) %>%
  slice_min(med.effort*(1-sparing), n = 1)

# Mean effort
high.mean.effort <- all.sims
high.mean.effort <- high.mean.effort %>%
  group_by(catch) %>%
  slice_max(mean.effort*(1-sparing), n = 1)

low.mean.effort <- all.sims
low.mean.effort <- low.mean.effort %>%
  group_by(catch) %>%
  slice_min(mean.effort*(1-sparing), n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean
high.med.effort.t <- high.med.effort
low.med.effort.t <- low.med.effort
high.mean.effort.t <- high.mean.effort
low.mean.effort.t <- low.mean.effort

# Plot
effortFigA <- ggplot(data.t, aes(x = sparing, y = effort*(1-sparing), group = catch/target.MSY.standard)) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Highest values", fill = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Lowest values", fill = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest values" = "purple", "Lowest values" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A",
       y = TeX("Seascape-wide fishing effort $(\\textit{E})$"), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  xlim(0,1) +
  ylim(0,3.75) +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw() +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24),legend.position="bottom") +
  ggtitle("With dispersal")
effortFigA

# Arrange and save
effortPlots <- 
  (effortFigA + effortFigB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
effortPlots

#ggsave(filename = "figs/effort.png", effortPlots, width = 20, height = 12, units = "cm")
#ggsave(filename = "figs/effort.pdf", effortPlots, width = 20, height = 12, units = "cm")

## Comparing a species facing extinction with a species not facing extinction (with dispersal) ####

# SECTION DESCRIPTION/NOTES
# See section title

# From our earlier analysis of the first random assemblage, we know that its most sensitive
# Ascidiacea goes extinct, and that its most sensitive Bivalvia does not go extinct. We will
# use these as our two example species in a deeper examination


# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    in.abun <- Re(in.abun.eq(r=all.species$r.value[i],
                             k=kt,
                             s=sparing, 
                             q=all.species$q.value[i], 
                             e=effort, 
                             m=dispersal))
    in.abun[in.abun < 0] <- 0
    out.abun <- Re(out.abun.eq(r=all.species$r.value[i],
                               k=kt,
                               s=sparing, 
                               q=all.species$q.value[i], 
                               e=effort, 
                               m=dispersal))
    out.abun[out.abun < 0] <- 0
    total <- in.abun + out.abun
    inside <- in.abun
    outside <- out.abun
    res <- cbind.data.frame(class,species,catch,sparing,total,inside,outside,effort)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data.migration <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 1 # Number of species assemblages to simulate
kt <- 1

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for target species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- abun.catch.in(k=kt, s=spect.s[i])
    out.pop <- abun.catch.out(r=target.r, k=kt, s=spect.s[i], c=spect.c[j])
    out.pop[Im(out.pop) > 0.0001] <- NA
    out.pop[Im(out.pop) < -0.0001] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
    
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Generate results for non-targeted species
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    effort <- results$e.value
    sparing <- results$s.value
    inside <- abun.eff.in(kt,sparing)
    outside <- abun.eff.out(all.species$r.value[i],kt,sparing,effort,all.species$q.value[i])
    total <- inside+outside
    res <- cbind.data.frame(class,species,effort,catch,sparing,inside,outside,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)
# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Combine s = 0 results with migration results
#data <- rbind.data.frame(data.migration,data)
data <- bind_rows(data.migration,data)

# Process results
#data <- drop_na(data)
beep()

# From the main analysis above, filter out only assemblage 1 and Ascidiacea 1 / Bivalvia 1
extinct.dat <- data %>% 
  filter(sim == 1) %>% 
  filter(species == "Ascidiacea 1")

alive.dat <- data %>% 
  filter(sim == 1) %>% 
  filter(species == "Bivalvia 1")

# Process data
combined.dat <- rbind.data.frame(extinct.dat,alive.dat)
combined.dat <- combined.dat %>% 
  mutate(abundance.shared = outside) %>% 
  mutate(abundance.spared = inside) %>% 
  mutate(abundance.total = total) %>% 
  gather("zone","zone.abundance", c("abundance.shared","abundance.spared","abundance.total")) %>% 
  mutate(catch.MSY = catch/target.MSY.standard)

# Plot
# Specify labels
zone.labs <- c("Fished zone", "Spared zone", "Both zones")
names(zone.labs) <- c("abundance.shared","abundance.spared","abundance.total")
catch.labs <- c("Catch = 0.30", "Catch = 0.60", "Catch = 0.90")
names(catch.labs) <- c("0.3","0.6","0.9")

deepCatchMig <- ggplot(combined.dat, aes(x = sparing, y = zone.abundance, colour = species)) +
  geom_line() +
  facet_grid(vars(zone), vars(catch.MSY),labeller = labeller(zone = zone.labs, catch.MSY = catch.labs, alpha = 0.8)) +
  labs(y = TeX("Equib. abundance in zone ($\\textit{x/K}$, $\\textit{y/K}$, or $\\textit{n/K}$)"),
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  scale_colour_manual(name = "Species", 
                      labels = c("Most sensitive Ascidiacea (faces possible extirpation)",
                                 "Most sensitive Bivalvia (never faces extirpation)"),
                      values = c("#46337EFF", 
                                 "#9FDA3AFF")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw(base_size = 15) +
  theme(legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 2))
deepCatchMig

#ggsave(filename = "figs/deepCatchMig.pdf", deepCatchMig, width = 16.5, height = 16.5, units = "cm")
#ggsave(filename = "figs/deepCatchMig.png", deepCatchMig, width = 16.5, height = 16.5, units = "cm")

# Calculating the number of species extirpated/surviving for s = 0 ####

# SECTION DESCRIPTION/NOTES
# See section title

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard,0.6740213*target.MSY.standard, 0.9*target.MSY.standard,target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Per sim, count the number of species that are extirpated
new.data <- data %>% group_by(sparing, catch, sim) %>% 
  summarise(extinct = sum(total == 0),
            alive = sum(total != 0))

# Plot
# Number extirpated
extirp <- ggplot(new.data, aes(x = as.factor(catch/target.MSY.standard), y = extinct)) +
  stat_summary_bin(fun = "mean", geom = "col", fill="grey",color="black") +
  stat_summary_bin(fun.data = mean_se, fun.args=list(mult=1.96), geom = "pointrange") +
  scale_y_continuous(expand = c(0,0.1)) +
  scale_x_discrete(labels = c(0.3,0.6,0.67,0.9,1)) +
  labs(y = TeX('Mean number of non-targeted species extirpated when $\\textit{s} = 0$'),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  theme_bw(base_size = 14)
extirp 
ggplot_build(extirp)

# Number surviving
surv <- ggplot(new.data, aes(x = as.factor(catch/target.MSY.standard), y = alive)) +
  stat_summary_bin(fun = "mean", geom = "col", fill="grey",color="black") +
  stat_summary_bin(fun.data = mean_se, fun.args=list(mult=1.96), geom = "pointrange") +
  scale_y_continuous(limits = c(0,80), expand = expansion(mult = c(0, .1))) +
  scale_x_discrete(labels = c(0.3,0.6,0.67,0.9,1)) +
  labs(y = TeX('Mean number of non-targeted species still present when $\\textit{s} = 0$'),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  theme_bw(base_size = 14)
surv
ggplot_build(surv)

# Arrange and save
#ggsave(filename = "figs/extirp.pdf", extirp, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/extirp.png", extirp, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/surv.pdf", surv, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/surv.png", surv, width = 20, height = 20, units = "cm")

# Alternate visualisation
extirpations <- ggplot(new.data, aes(x = extinct, y = as.factor(catch/target.MSY.standard))) + 
  geom_density_ridges2(stat = "binline", binwidth = 1, scale = 0.95) +
  labs(y = TeX("Equilibrium catch of targeted species $(\\textit{C/C_{MSY}})$"),
       x = "Proportion of non-targeted species extirpated without an MPA") +
  scale_x_continuous(breaks = c(0, 4, 8, 12), labels = c("0.00", "0.05", "0.10", "0.15")) +
  scale_y_discrete(
    labels = c(0.3,0.6,0.67,0.9,1)) +
  theme_bw(base_size = 16) +
  theme(axis.text.y=element_text(hjust=0.5))
extirpations

#ggsave(filename = "figs/extirpations.pdf", extirpations, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/extirpations.png", extirpations, width = 20, height = 20, units = "cm")

# Sensitivity analyses ####

## m = 0.1 ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.1 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# Arrange and save
sensPlots <-
  (dispA + dispB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

beep()

#ggsave(filename = "figs/sensm0_1.pdf", sensPlots, width = 20, height = 12, units = "cm")
#ggsave(filename = "figs/sensm0_1.png", sensPlots, width = 20, height = 12, units = "cm")

## m = 0.9 ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.9 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)


# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# Arrange and save
sensPlots <-
  (dispA + dispB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

#ggsave(filename = "figs/sensm0_9.pdf", sensPlots, width = 20, height = 12, units = "cm")
#ggsave(filename = "figs/sensm0_9.png", sensPlots, width = 20, height = 12, units = "cm")

## 5 species ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 5 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)


# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "C", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Arithmetic mean
noDispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = element_blank(), 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispA  

# Geometric mean
noDispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "D", 
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispB

# Arrange and save
sensPlots <-
  ((dispA + noDispA) / (dispB + noDispB)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

#ggsave(filename = "figs/sens5species.pdf", sensPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/sens5species.png", sensPlots, width = 20, height = 20, units = "cm")

## 20 species ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 20 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "C", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Arithmetic mean
noDispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = element_blank(), 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispA  

# Geometric mean
noDispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "D", 
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispB

# Arrange and save
sensPlots <-
  ((dispA + noDispA) / (dispB + noDispB)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

#ggsave(filename = "figs/sens20species.pdf", sensPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/sens20species.png", sensPlots, width = 20, height = 20, units = "cm")

## Half targeted species r ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r/2
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "C", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Arithmetic mean
noDispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = element_blank(), 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispA  

# Geometric mean
noDispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "D", 
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispB

# Arrange and save
sensPlots <-
  ((dispA + noDispA) / (dispB + noDispB)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

#ggsave(filename = "figs/sensHalfr.pdf", sensPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/sensHalfr.png", sensPlots, width = 20, height = 20, units = "cm")

## Double targeted species r ####

# SECTION DESCRIPTION/NOTES
# See section title. From examination, only the following pairs of equilibria produce biologically sensible results (i.e. no negative / complex abundances):
# - Calculating abundance from effort: Eq. 2
# - Calculating abundance from catch: Eq. 4

# Reset targeted r
target.r <- malacostraca.mean.r*2
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.00125

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accomodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration fomula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s.special <- 0
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Plot
# All sims approach
# Arithmetic mean
dispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispA  

# Geometric mean
dispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                colour = as.factor(catch/target.MSY.standard), 
                                group = as.factor(catch/target.MSY.standard),
                                fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "C", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("With dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
dispB

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate metrics for individual assemblages
data <- data %>% 
  group_by(sparing,catch,sim) %>% 
  summarise(arith.mean = mean(total),
            geom.mean = gm.mean(total))

# Find highest and lowest
# Arithmetic mean
high.arith.mean <- data
high.arith.mean <- high.arith.mean %>%
  group_by(catch,sim) %>%
  slice_max(arith.mean, n = 1)

low.arith.mean <- data
low.arith.mean <- low.arith.mean %>%
  group_by(catch,sim) %>%
  slice_min(arith.mean, n = 1)

# Geometric mean
high.geom.mean <- data
high.geom.mean <- high.geom.mean %>%
  group_by(catch,sim) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- data
low.geom.mean <- low.geom.mean %>%
  group_by(catch,sim) %>%
  slice_min(geom.mean, n = 1)

# Isolating highest/lowest data to facilitate plotting
data.t <- data
high.arith.mean.t <- high.arith.mean
low.arith.mean.t <- low.arith.mean
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean

data.ends.t <- data.t %>% 
  group_by(catch,sim) %>% 
  filter(sparing == max(sparing))

# Calculate metrics across assemblages
all.sims <- data %>% 
  group_by(catch,sparing) %>% 
  summarise(med.arith.mean = median(arith.mean),
            medSE.arith.mean = median_se(arith.mean),
            mean.arith.mean = mean(arith.mean),
            meanSE.arith.mean = mean_se(arith.mean),
            SE.arith.mean = std_mean(arith.mean),
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean),
            SE.geom.mean = std_mean(geom.mean))

# Find highest and lowest
# Median arithmetic mean
high.med.arith.mean <- all.sims
high.med.arith.mean <- high.med.arith.mean %>%
  group_by(catch) %>%
  slice_max(med.arith.mean, n = 1)

low.med.arith.mean <- all.sims
low.med.arith.mean <- low.med.arith.mean %>%
  group_by(catch) %>%
  slice_min(med.arith.mean, n = 1)

# Mean arithmetic mean
high.mean.arith.mean <- all.sims
high.mean.arith.mean <- high.mean.arith.mean %>%
  group_by(catch) %>%
  slice_max(mean.arith.mean, n = 1)

low.mean.arith.mean <- all.sims
low.mean.arith.mean <- low.mean.arith.mean %>%
  group_by(catch) %>%
  slice_min(mean.arith.mean, n = 1)

# Median geometric mean
high.med.geom.mean <- all.sims
high.med.geom.mean <- high.med.geom.mean %>%
  group_by(catch) %>%
  slice_max(med.geom.mean, n = 1)

low.med.geom.mean <- all.sims
low.med.geom.mean <- low.med.geom.mean %>%
  group_by(catch) %>%
  slice_min(med.geom.mean, n = 1)

# Mean geometric mean
high.mean.geom.mean <- all.sims
high.mean.geom.mean <- high.mean.geom.mean %>%
  group_by(catch) %>%
  slice_max(mean.geom.mean, n = 1)

low.mean.geom.mean <- all.sims
low.mean.geom.mean <- low.mean.geom.mean %>%
  group_by(catch) %>%
  slice_min(mean.geom.mean, n = 1)

# Isolating highest/lowest to facilitate plotting
all.sims.t <- all.sims
high.med.arith.mean.t <- high.med.arith.mean
low.med.arith.mean.t <- low.med.arith.mean
high.mean.arith.mean.t <- high.mean.arith.mean
low.mean.arith.mean.t <- low.mean.arith.mean
high.med.geom.mean.t <- high.med.geom.mean
low.med.geom.mean.t <- low.med.geom.mean
high.mean.geom.mean.t <- high.mean.geom.mean
low.mean.geom.mean.t <- low.mean.geom.mean

# Arithmetic mean
noDispA <- ggplot(all.sims.t, aes(x = sparing, y = mean.arith.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_ribbon(aes(ymin = mean.arith.mean-(1.96*SE.arith.mean), ymax = mean.arith.mean+(1.96*SE.arith.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = element_blank(), 
       x = element_blank()) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispA  

# Geometric mean
noDispB <- ggplot(all.sims.t, aes(x = sparing, y = mean.geom.mean, 
                                  colour = as.factor(catch/target.MSY.standard), 
                                  group = as.factor(catch/target.MSY.standard),
                                  fill = as.factor(catch/target.MSY.standard))) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest values", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_ribbon(aes(ymin = mean.geom.mean-(1.96*SE.geom.mean), ymax = mean.geom.mean+(1.96*SE.geom.mean)), colour = NA, alpha = 0.4) +
  geom_line(size = 1.05) +
  scale_shape_manual(element_blank(), values = c("Highest values" = 16, "Lowest values" = 15)) +
  scale_colour_viridis_d(element_blank(), begin = 0, end = 0.8) +
  scale_fill_viridis_d(element_blank(), begin = 0, end = 0.8) +
  labs(tag = "D", 
       y = element_blank(), 
       x = TeX("Seascape in MPA $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  guides(color = "none",
         shape = guide_legend(order = 2),
         fill = "none") +
  theme_bw(base_size = 12) +
  ggtitle("Without dispersal") +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
noDispB

# Arrange and save
sensPlots <-
  ((dispA + noDispA) / (dispB + noDispB)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots

#ggsave(filename = "figs/sensDoubler.pdf", sensPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/sensDoubler.png", sensPlots, width = 20, height = 20, units = "cm")

# Maximum sparing versus catch ####

# SECTION DESCRIPTION/NOTES
# Modified sparing/sharing code to determine the maximum sparing that can still support the catch target.

## Without dispersal ####

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.0025
c.interval <- target.MSY.standard/40

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Plot maximum sparing versus catch
results <- results %>% 
  group_by(c.value) %>% 
  mutate(s.value = max(s.value))
no.disp.results <- results

ggplot(results, aes(x = c.value/target.MSY.standard, y = s.value)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_bw(base_size = 12) +
  labs(y = TeX("Maximum proportion of seascape in MPA $(\\textit{s})$"), 
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))

## With dispersal ####

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.0025
c.interval <- target.MSY.standard/40

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Highly complex equilibria formula for dispersal case cannot
# accommodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration formula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 

# Set granularity of exploration
s.interval <- 0.0025
c.interval <- target.MSY.standard/40

# Define spectra of catch and sparing being explored
spect.s.special <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Drop e.value column so that data frames can be bound
results <- results[,!"e.value"]

# Combine s = 0 results with migration results
results <- rbind.data.frame(results,results.special)

# Plot maximum sparing versus catch
results <- results %>% 
  group_by(c.value) %>% 
  mutate(s.value = max(s.value))
disp.results <- results

# Combine dispersal and no-dispersal results
results.t <- bind_rows(no.disp.results, disp.results, .id = "dispersal")

# Plot results
maxSparePlot <- ggplot(results.t, aes(x = c.value/target.MSY.standard, y = s.value, colour = dispersal, group = dispersal)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_colour_manual(name = element_blank(), label = c("Without dispersal", "With dispersal"), values = c("#440154FF", "#9FDA3AFF")) +
  theme_bw(base_size = 12) +
  labs(y = TeX("Maximum proportion of seascape in MPA $(\\textit{s})$"), 
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24), plot.title = element_text(hjust = 0.5))
maxSparePlot

# Arrange and save
#ggsave(filename = "figs/maxSparePlot.pdf", maxSparePlot, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/maxSparePlot.png", maxSparePlot, width = 20, height = 20, units = "cm")

# Catch versus species extirpated versus sparing ####

# SECTION DESCRIPTION/NOTES
# Code for plotting catch versus species extirpated versus sparing.

## Without dispersal ####

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.025
c.interval <- target.MSY.standard/20

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
list.data <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Calculate extirpations
# Per sim, count the number of species that are extirpated
new.data <- data %>% group_by(sparing, catch, sim) %>% 
  summarise(extinct = sum(total == 0),
            alive = sum(total != 0)) %>% 
  group_by(sparing, catch) %>% 
  summarise(mean.extinct = mean(extinct))
new.data.no.disp <- new.data

## With dispersal ####

# Reset targeted r
target.r <- malacostraca.mean.r
target.MSY.standard <- (target.r*kt)/4

# Specify equilibria functions to be used
# Abundance from catch
in.catch.eq <- d.catch.eq.in.4
out.catch.eq <-  d.catch.eq.out.4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate
dispersal <- 0.3 # Dispersal rate of all species

# Set granularity of exploration
s.interval <- 0.025
c.interval <- target.MSY.standard/20

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list <- list()

# Produce results for targeted species
for (i in seq_along(spect.s)){
  for (j in seq_along(spect.c)){
    # Produce results
    in.pop <- in.catch.eq(r=target.r,
                          k=kt,
                          s=spect.s[i],
                          m=dispersal,
                          c=spect.c[j])
    out.pop <- out.catch.eq(r=target.r,
                            k=kt,
                            s=spect.s[i],
                            m=dispersal,
                            c=spect.c[j])
    in.pop[Im(in.pop) > 0.00000001] <- NA
    in.pop[Im(in.pop) < -0.00000001] <- NA
    out.pop[Im(out.pop) > 0.00000001] <- NA
    out.pop[Im(out.pop) < -0.00000001] <- NA
    in.pop[Re(in.pop) < 0] <- NA
    out.pop[Re(out.pop) < 0] <- NA
    in.pop <- Re(in.pop)
    out.pop <- Re(out.pop)
    targ.n <- in.pop + out.pop
    
    s.value <- spect.s[i]
    c.value <- spect.c[j]
    
    # Use out.pop to calculate e.value
    e.value <- c.value/(target.q*out.pop)
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value,
                                  e.value)
    results.list[[paste("s =", s.value,
                        "c =", c.value,
                        "e =", e.value,
                        sep=" ")]] <- num.eq.df
  }
}

# Store results
results <- rbindlist(results.list)

# Process results
results <- drop_na(results)

# Produce results for all species, all assemblages
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
list.data <- list()
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results$c.value
    sparing <- results$s.value
    effort <- results$e.value
    # total <- abun.catch(rt=target.r,
    #                     qt=target.q,
    #                     rn=all.species$r.value[i],
    #                     qn=all.species$q.value[i],
    #                     s=sparing,
    #                     ct=catch,
    #                     kt=kt)
    total <- tot.abun.eq(r=all.species$r.value[i],
                         k=kt,
                         s=sparing, 
                         q=all.species$q.value[i], 
                         e=effort, 
                         m=dispersal)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data <- rbind.data.frame(data, res)
  }
  list.data[[j]] <- data
}
close(pb)

# Store results
data <- rbindlist(list.data, idcol="sim")

# Process results
#data <- drop_na(data)
beep()

# Highly complex equilibria formula for dispersal case cannot
# accommodate s = 0
# However, s = 0 is simply the case in which there is no dispersal
# and no sparing, i.e., the Schaefer model
# Therefore values for s = 0 can be supplied using equilibria
# formulae for a simple Schaefer model, or our non-migration formula when s = 0

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 

# Set granularity of exploration
s.interval <- 0.025
c.interval <- target.MSY.standard/20

# Define spectra of catch and sparing being explored
spect.s.special <- c(0,seq(s.interval,1-s.interval,s.interval))
#spect.c <- c(0.3*target.MSY.standard, 0.6*target.MSY.standard, 0.9*target.MSY.standard)
spect.c <- c(0,seq(c.interval,target.MSY.standard-c.interval,c.interval),target.MSY.standard)

# Create results list
results.list.special <- list()

# Produce results for targeted species
for (i in seq_along(spect.s.special)){
  for (j in seq_along(spect.c)){
    # Produce results
    targ.n <- abun.catch(rt=target.r,
                         qt=target.q,
                         rn=target.r,
                         qn=target.q,
                         s=spect.s.special[i],
                         ct=spect.c[j],
                         kt=kt)
    s.value <- spect.s.special[i]
    c.value <- spect.c[j]
    
    # Store results
    num.eq.df <- cbind.data.frame(targ.n,
                                  s.value,
                                  c.value)
    results.list.special[[paste("s =", s.value,
                                "c =", c.value,
                                sep=" ")]] <- num.eq.df
  }
}

# Store results
results.special <- rbindlist(results.list.special)

# Process results
results.special <- drop_na(results.special)

# Produce results for all species, all assemblages
list.data.special <- list()
pb <- txtProgressBar(min = 0, max = num.sims, style = 3)
for(j in 1:num.sims){
  setTxtProgressBar(pb, j)
  cat(" Simulating sample", j, "of", num.sims)
  all.species <- builder(n = num.spec, 
                         mean.q = dist.pars$mean.q, 
                         sd.q = dist.pars$log.sd.q,
                         mean.r = dist.pars$mean.r, 
                         sd.r = dist.pars$log.sd.r,
                         name = dist.pars$class)
  data.special <- NULL
  for(i in 1:nrow(all.species)){
    class <- all.species$class[i]
    species <- all.species$species[i]
    catch <- results.special$c.value
    sparing <- results.special$s.value
    total <- abun.catch(rt=target.r,
                        qt=target.q,
                        rn=all.species$r.value[i],
                        qn=all.species$q.value[i],
                        s=sparing,
                        ct=catch,
                        kt=kt)
    res <- cbind.data.frame(class,species,catch,sparing,total)
    data.special <- rbind.data.frame(data.special, res)
  }
  list.data.special[[j]] <- data.special
}
close(pb)

# Store results
data.special <- rbindlist(list.data.special, idcol="sim")

# Combine s = 0 results with migration results
data <- rbind.data.frame(data,data.special)

# Calculate extirpations
# Per sim, count the number of species that are extirpated
new.data <- data %>% group_by(sparing, catch, sim) %>% 
  summarise(extinct = sum(total == 0),
            alive = sum(total != 0)) %>% 
  group_by(sparing, catch) %>% 
  summarise(mean.extinct = mean(extinct))
new.data.disp <- new.data

# Combine dispersal and no-dispersal results
results.t <- bind_rows(new.data.no.disp, new.data.disp, .id = "dispersal")

# Plot
disp.labs <- c("Without dispersal","With dispersal")
names(disp.labs) <- c(1,2)

# Smallest possible MPA that minimises extirpations
# smallestMPA <- ggplot(results.t, aes(x = as.factor(catch/target.MSY.standard), y = sparing, fill = mean.extinct, colour = mean.extinct)) +
#   geom_tile() +
#   scale_fill_viridis_c(name = "Mean extirpated species") +
#   scale_colour_viridis_c(name = "Mean extirpated species") +
#   new_scale_color() + # Data below will require a new colour scale
#   geom_line(data = results.z, aes(x = as.factor(catch/target.MSY.standard), y = sparing, group = dispersal, colour = "Smallest MPA to minimise extirpations")) +
#   scale_colour_manual(element_blank(), values = c("Smallest MPA to minimise extirpations" = "red")) +
#   facet_wrap(dispersal~., labeller = labeller(dispersal = disp.labs)) +
#   labs(y = TeX("Seascape in MPA $(\\textit{s})$"),
#        x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
#   scale_x_discrete(
#     labels = function(x) {
#       x[!(seq_along(x) %% (2) == 1)] <- ""
#       x
#     }
#   ) +
#   theme_bw()
# smallestMPA

# Extract the relationship between extirpated species and catch at particular levels of sparing
results.d <- results.t %>% 
  filter(near(sparing, 0, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.1, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.2, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.3, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.4, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.5, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.6, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.7, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.8, tol = .Machine$double.eps^0.5) |
           near(sparing, 0.9, tol = .Machine$double.eps^0.5))

extirpCatchTrade <- ggplot(results.d, aes(x = catch/target.MSY.standard, y = mean.extinct, colour = as.factor(sparing), group = as.factor(sparing))) +
  geom_line() +
  facet_wrap(dispersal~., labeller = labeller(dispersal = disp.labs)) +
  scale_colour_viridis_d(name = TeX("Seascape in MPA $(\\textit{s})$")) +
  labs(y = "Mean extirpated species",
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  theme_bw()
extirpCatchTrade

# Arrange and save
#ggsave(filename = "figs/extirpCatchTrade.pdf", extirpCatchTrade, width = 20, height = 12, units = "cm")
#ggsave(filename = "figs/extirpCatchTrade.png", extirpCatchTrade, width = 20, height = 12, units = "cm")