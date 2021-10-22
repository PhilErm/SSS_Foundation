# Script details ####

# Script using parameters and functions to undertake main analysis and generate figures in 
# "Meeting wild-caught seafood demand at least cost to biodiversity"

# Load packages ####

library(tidyverse)
library(data.table)
library(ggridges)
library(patchwork)
library(latex2exp)
library(directlabels)
library(ggrepel)
library(progress)
library(costatcompanion)
library(beepr)

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
#ggsave(filename = "figs/finalised/dists.pdf", dists, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/finalised/dists.png", dists, width = 20, height = 20, units = "cm")

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

# Analyse characteristics of one random species assemblage ####

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

## Abundance from effort for sparing = 0 ####

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

## Abundance from effort for sparing = 0.25 ####

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

## Abundance from catch for sparing = 0 ####

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
  labs(tag = "C",
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

## Abundance from catch for sparing = 0.25 ####

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
abunPlots <- 
  (abunVsEffA + abunVsEffB) /
  (abunVsCatchA + abunVsCatchB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
abunPlots
#ggsave(filename = "figs/abunPlots.pdf", abunPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/abunPlots.png", abunPlots, width = 20, height = 20, units = "cm")

# Perform sparing/sharing analysis across all catch targets ####

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across all catch targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

# Set granularity of exploration
s.interval <- 0.01
c.interval <- target.MSY.standard/150 # For ms figure

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- seq(c.interval,target.MSY.standard,c.interval)

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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

# Arithmetic mean
fullCatchA <- ggplot(all.sims, aes(x = catch/target.MSY.standard, y = sparing, fill = mean.arith.mean)) +
  geom_tile() +
  geom_line(data = high.mean.arith.mean, aes(x = catch/target.MSY.standard, y = sparing, colour = "Highest biodiversity"), size = 1) +
  geom_line(data = low.mean.arith.mean, aes(x = catch/target.MSY.standard, y = sparing, colour = "Lowest biodiversity"), size = 1) +
  labs(tag = "A",
       y = TeX("Seascape spared $(\\textit{s})$"),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  scale_fill_viridis_c(name = "Biodiversity \n(arithmetic mean abundance)",
                       limits = c(0,1)) +
  scale_colour_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  ylim(0,1) +
  scale_x_continuous(sec.axis = sec_axis(
    name = TeX("Equib. catch of targeted species $(\\textit{C/K})$$"),
    ~ .*target.MSY.standard )) +
  guides(colour = guide_legend(order = 2),
         fill = guide_colourbar(order = 1)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
fullCatchA

# Geometric mean
fullCatchB <- ggplot(all.sims, aes(x = catch/target.MSY.standard, y = sparing, fill = mean.geom.mean)) +
  geom_tile() +
  geom_line(data = high.mean.geom.mean, aes(x = catch/target.MSY.standard, y = sparing, colour = "Highest biodiversity"), size = 1) +
  geom_line(data = low.mean.geom.mean, aes(x = catch/target.MSY.standard, y = sparing, colour = "Lowest biodiversity"), size = 1) +
  labs(tag = "B",
       y = TeX("Seascape spared $(\\textit{s})$"),
       x = TeX("Equib. catch of targeted species $(\\textit{C/C_{MSY}})$")) +
  scale_fill_viridis_c(name = "Biodiversity \n(geometric mean abundance)",
                       limits = c(0,1)) +
  scale_colour_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  ylim(0,1) +
  scale_x_continuous(sec.axis = sec_axis(
    name = TeX("Equib. catch of targeted species $(\\textit{C/K})$$"),
    ~ .*target.MSY.standard )) +
  guides(colour = guide_legend(order = 2),
         fill = guide_colourbar(order = 1)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
fullCatchB

# Collate
fullCatchPlots <- 
  (fullCatchA / fullCatchB) +
  plot_layout(guides = "collect")
fullCatchPlots
#ggsave(filename = "figs/fullCatchPlots.pdf", fullCatchPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/fullCatchPlots.png", fullCatchPlots, width = 20, height = 20, units = "cm")

# Perform sparing/sharing analysis across key targets ####

# SECTION DESCRIPTION/NOTES
# The main sea sparing/sharing analysis across key targets. Warnings will be thrown every
# time sqrt() produces a complex number. These occur when a catch and sparing proportion combination
# are input into a function which is impossible to obtain (e.g. MSY catch while sparing 90% of the
# seascape)

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
# Arithmetic mean
sparingVsSharingA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sparingVsSharingA

# Geometric mean
sparingVsSharingB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sparingVsSharingB

# Arrange and save
sparingVsSharingPlots <- 
  (sparingVsSharingA + sparingVsSharingB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sparingVsSharingPlots
#ggsave(filename = "figs/sparingVsSharingPlots.pdf", sparingVsSharingPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/sparingVsSharingPlots.png", sparingVsSharingPlots, width = 20, height = 14, units = "cm")

# Examining variability and comparing max. sparing vs sharing ####
# See section title

# Create new data frames just for sharing and max. sparing
max.sparing <- data.t %>% 
  group_by(catch,sim) %>% 
  slice_max(sparing, n = 1)

sharing <- data.t %>% 
  group_by(catch,sim) %>% 
  slice_min(sparing, n = 1)

# Rescale catch so relative to MSY
max.sparing$catch <- max.sparing$catch/target.MSY.standard
sharing$catch <- sharing$catch/target.MSY.standard

# Set labels for figures
max.sparing$scen <- "Conventional\n fisheries\n management\n with largest\n possible MPA\n (max. sparing)"
sharing$scen <- "Conventional \n fisheries\n management\n alone (sharing)"

# Process data
new.data.arith <- rbind.data.frame(sharing, max.sparing)
new.data.geom <- rbind.data.frame(sharing, max.sparing)

# Arithmetic
catch.labs <- c("Catch = 0.30", "Catch = 0.60", "Catch = 0.90")
names(catch.labs) <- c("0.3","0.6","0.9")

varA <- ggplot(new.data.arith, aes(x = arith.mean)) +
  facet_grid(vars(scen), vars(catch), labeller = labeller(catch = catch.labs),
             scales = "fixed") +
  geom_histogram(bins = 25, fill = "black") +
  #scale_fill_viridis_d() +
  labs(tag = "A", 
       y = "Number of species assemblages",
       x = "Biodiversity (arithmetic mean abundance)") +
  scale_x_continuous(expand = expansion(mult = c(0,0.05)), limits = c(-0.05,1)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)), limits = c(0,100)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), 
        plot.tag = element_text(size=34),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)) +
  guides(fill = FALSE)
varA

# Geometric
varB <- ggplot(new.data.geom, aes(x = geom.mean)) +
  facet_grid(vars(scen), vars(catch), labeller = labeller(catch = catch.labs),
             scales = "fixed") +
  geom_histogram(bins = 25, fill = "black") +
  #scale_fill_viridis_d() +
  labs(tag = "B", 
       y = "Number of species assemblages",
       x = "Biodiversity (geometric mean abundance)") +
  scale_x_continuous(expand = expansion(mult = c(0,0.05)), limits = c(-0.05,1)) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)), limits = c(0,100)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), 
        plot.tag = element_text(size=34),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)) +
  guides(fill = FALSE)
varB

# Arrange amd save
varPlots <- 
  varA / 
  varB
varPlots
#ggsave(filename = "figs/varPlots.pdf", varPlots, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/varPlots.png", varPlots, width = 20, height = 20, units = "cm")


# Calculate effort for sparing/sharing analysis across key targets ####

# SECTION DESCRIPTION/NOTES
# Since the targeted species never changes, only needs to be done once. For ease of coding, other
# superfluous results (abundances etc.) are still generated. Because the simplest approach is to
# calculate effort in just fished zone and then make adjustment later, equations for calculating 
# zone abundances separately are used

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate
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
effortFig <- ggplot(data.t, aes(x = sparing, y = effort*(1-sparing), group = catch/target.MSY.standard)) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Highest seascape-wide fishing effort", fill = "Highest seascape-wide fishing effort", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.effort.t, aes(x = sparing, y = mean.effort*(1-sparing), shape = "Lowest seascape-wide fishing effort", fill = "Lowest seascape-wide fishing effort", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest seascape-wide fishing effort" = "purple", "Lowest seascape-wide fishing effort" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest seascape-wide fishing effort" = 16, "Lowest seascape-wide fishing effort" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(y = TeX("Seascape-wide fishing effort $(\\textit{E})$"), 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 18) +
  theme(legend.text = element_text(size=14), plot.tag = element_text(size=24),legend.position="bottom")
effortFig

# Arrange and save
#ggsave(filename = "figs/effort.png", effortFig, width = 20, height = 20, units = "cm")
#ggsave(filename = "figs/effort.pdf", effortFig, width = 20, height = 20, units = "cm")

# Comparing a species facing extinction with a species not facing extinction ####

# SECTION DESCRIPTION/NOTES
# See section title

# From our earlier analysis of the first random assemblage, we know that its most sensitive
# Ascidiacea goes extinct, and that its most sensitive Bivalvia does not go extinct. We will
# use these as our two example species

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
       x = TeX("Seascape spared $(\\textit{s})$")) +
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

# Sens. analysis: species = 5 ####

# SECTION DESCRIPTION/NOTES
# See section title

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 5 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/sensA5species.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/sensA5species.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: species = 50 ####

# SECTION DESCRIPTION/NOTES
# See section title

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 50 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/sensA50species.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/sensA50species.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: targeted r halved ####

# SECTION DESCRIPTION/NOTES
# See section title

# Adjust targeted r
target.r <- malacostraca.mean.r/2
target.MSY.standard <- (target.r*kt)/4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/halfr.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/halfr.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: targeted r doubled ####

# SECTION DESCRIPTION/NOTES
# See section title

# Adjust targeted r
target.r <- malacostraca.mean.r*2
target.MSY.standard <- (target.r*kt)/4

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard)), alpha = 0.20, colour = "gray70") +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/doubler.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/doubler.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: model with dispersal m = 0.1 ####

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

# Abundance from effort
in.abun.eq <- d.eq.in.2
out.abun.eq <- d.eq.out.2
tot.abun.eq <- function(r, k, s, q, e, m){
  in.abun <- Re(in.abun.eq(r, k, s, q, e, m))
  in.abun[in.abun < 0] <- 0
  out.abun <- Re(out.abun.eq(r, k, s, q, e, m))
  out.abun[out.abun < 0] <- 0
  abun <- in.abun + out.abun
  abun
}

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate
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
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/dispersal01.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/dispersal01.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: model with dispersal m = 0.5 ####

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

# Abundance from effort
in.abun.eq <- d.eq.in.2
out.abun.eq <- d.eq.out.2
tot.abun.eq <- function(r, k, s, q, e, m){
  in.abun <- Re(in.abun.eq(r, k, s, q, e, m))
  in.abun[in.abun < 0] <- 0
  out.abun <- Re(out.abun.eq(r, k, s, q, e, m))
  out.abun[out.abun < 0] <- 0
  abun <- in.abun + out.abun
  abun
}

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate
dispersal <- 0.5 # Dispersal rate of all species

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
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/dispersal05.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/dispersal05.png", sensPlots, width = 20, height = 14, units = "cm")

# Sens. analysis: model with dispersal m = 1 ####

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

# Abundance from effort
in.abun.eq <- d.eq.in.2
out.abun.eq <- d.eq.out.2
tot.abun.eq <- function(r, k, s, q, e, m){
  in.abun <- Re(in.abun.eq(r, k, s, q, e, m))
  in.abun[in.abun < 0] <- 0
  out.abun <- Re(out.abun.eq(r, k, s, q, e, m))
  out.abun[out.abun < 0] <- 0
  abun <- in.abun + out.abun
  abun
}

# Set parameters
set.seed(1) # Random seed used in manuscript
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate
dispersal <- 1 # Dispersal rate of all species

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
num.sims <- 100 # Number of species assemblages to simulate

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
            med.geom.mean = median(geom.mean),
            medSE.geom.mean = median_se(geom.mean),
            mean.geom.mean = mean(geom.mean),
            meanSE.geom.mean = mean_se(geom.mean))

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
sensA <- ggplot(data.t, aes(x = sparing, y = arith.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.arith.mean.t, aes(x = sparing, y = mean.arith.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "A", 
       y = "Biodiversity (arithmetic mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(color = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensA

# Geometric mean
sensB <- ggplot(data.t, aes(x = sparing, y = geom.mean, group = catch/target.MSY.standard)) +
  geom_line(data = data.t, aes(group=interaction(sim, catch/target.MSY.standard), colour = as.factor(catch/target.MSY.standard)), alpha = 0.20) +
  geom_line(stat = "summary", fun = "mean", aes(colour = as.factor(catch/target.MSY.standard)), size = 1.05) +
  geom_point(data = high.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Highest biodiversity", fill = "Highest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  geom_point(data = low.mean.geom.mean.t, aes(x = sparing, y = mean.geom.mean, shape = "Lowest biodiversity", fill = "Lowest biodiversity", colour = as.factor(catch/target.MSY.standard)), size = 4) +
  scale_fill_manual(element_blank(), values = c("Highest biodiversity" = "purple", "Lowest biodiversity" = "red")) +
  scale_shape_manual(element_blank(), values = c("Highest biodiversity" = 16, "Lowest biodiversity" = 15)) +
  scale_colour_viridis_d(begin = 0, end = 0.8) +
  labs(tag = "B", 
       y = "Biodiversity (geometric mean abundance)", 
       x = TeX("Seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  guides(colour = FALSE,
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(legend.text = element_text(size=12), plot.tag = element_text(size=24))
sensB

# Arrange and save
sensPlots <- 
  (sensA + sensB) +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
sensPlots
#ggsave(filename = "figs/dispersal1.pdf", sensPlots, width = 20, height = 14, units = "cm")
#ggsave(filename = "figs/dispersal1.png", sensPlots, width = 20, height = 14, units = "cm")

# Illustrative methods figures ####

# SECTION DESCRIPTION/NOTES
# See section title

# Specify parameters

# Catchability
target.q <- 0.25
non.target1.q <- 0.25
non.target2.q <- 0.25

# Growth rate
target.r <- 1
non.target1.r <- 2
non.target2.r <- 0.17

## Abundance from effort for sparing = 0 ####

# Plot parameters
kt <- 1
s <- 0
e <- seq(0,5,0.01)
catch1 <- 0.35
catch2 <- 0.65

# Generate data
target.abun <- abun.eff(r=target.r,s=s,e=e,q=target.q)
non.target1.abun <- abun.eff(r=non.target1.r,s=s,e=e,q=non.target1.q)
non.target2.abun <- abun.eff(r=non.target2.r,s=s,e=e,q=non.target2.q)

# Process data
data <- cbind.data.frame(
  target.abun, 
  non.target1.abun, 
  non.target2.abun,
  e) %>% gather(taxa, value, c("target.abun", 
                               "non.target1.abun", 
                               "non.target2.abun"))
data$taxa <- factor(data$taxa, levels = c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))

# Plot data
taxa.label <- c("Targeted", 
                "Non-targeted 1",
                "Non-targeted 2")
methodsA <- ggplot(data, aes(x = e, y = value, col = taxa)) +
  geom_line() +
  labs(
    y = TeX("Equib. abundance"),
    x = TeX("Fishing effort")) +
  scale_colour_manual(name = "Taxon", labels = taxa.label, values = c("forestgreen","black","black")) +
  scale_size_manual(name = "Taxon", values = c(0.5,0.5,0.5,0.5,0.5,2,0.5,0.5), labels = taxa.label) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw(base_size = 12) +
  theme(legend.position="none",
        legend.text = element_text(size=12), 
        plot.tag = element_text(size=24), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        plot.margin = margin(10,10,10,10)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  ggtitle("Abundance vs fishing effort")
methodsA

## Abundance from catch for sparing = 0 ####

# Calculate MSY and set plot catch spectrum
target.MSY.standard <- (target.r*kt)/4
target.catch <- c(seq(0,target.MSY.standard,0.001),target.MSY.standard)
kt <- 1
s <- 0

# Generate data
target.abun <- abun.catch(rt=target.r,
                          qt=target.q,
                          rn=target.r,
                          qn=target.q,
                          s=s,
                          ct=target.catch,
                          kt=kt)
non.target1.abun <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target1.r,
                               qn=non.target1.q,
                               s=s,
                               ct=target.catch,
                               kt=kt)
non.target2.abun <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target2.r,
                               qn=non.target2.q,
                               s=s,
                               ct=target.catch,
                               kt=kt)

# Process data
data <- cbind.data.frame(
  target.abun, 
  non.target1.abun, 
  non.target2.abun,
  target.catch) %>% gather(taxa, value, c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))
data$taxa <- factor(data$taxa, levels = c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))

# Plot data
taxa.label <- c("Targeted", 
                "Non-targeted 1",
                "Non-targeted 2")
methodsC <- ggplot(data, aes(x = target.catch/target.MSY.standard, y = value, col = taxa)) +
  geom_line() +
  labs(
    y = TeX("Equib. abundance"),
    x = TeX("Targeted equib. catch")) +
  scale_colour_manual(name = "Taxon", labels = taxa.label, values = c("forestgreen","black","black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = catch1, linetype = "dashed", colour = "blue2") +
  geom_vline(xintercept = catch2, linetype = "dashed", colour = "darkorange1") +
  theme_bw(base_size = 12) +
  theme(legend.position="none",legend.text = element_text(size=12), 
        plot.tag = element_text(size=24), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = c("grey30", "blue2", "darkorange1", "grey30")),
        axis.title.x = element_text(colour = "forestgreen"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        plot.margin = margin(10,10,10,10)
  ) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, catch1, catch2, 1)) +
  ggtitle("Abundance vs catch")
methodsC

## Abundance from catch for sparing = 0.25 ####

# Plot parameters
kt <- 1
s <- 0.25
e <- seq(0,5,0.01)
catch1 <- 0.35
catch2 <- 0.65

# Generate data
target.abun <- abun.eff(r=target.r,s=s,e=e,q=target.q)
non.target1.abun <- abun.eff(r=non.target1.r,s=s,e=e,q=non.target1.q)
non.target2.abun <- abun.eff(r=non.target2.r,s=s,e=e,q=non.target2.q)

# Process data
data <- cbind.data.frame(
  target.abun, 
  non.target1.abun, 
  non.target2.abun,
  e) %>% gather(taxa, value, c("target.abun", 
                               "non.target1.abun", 
                               "non.target2.abun"))
data$taxa <- factor(data$taxa, levels = c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))

# Plot data
taxa.label <- c("Targeted", 
                "Non-targeted 1",
                "Non-targeted 2")
methodsB <- ggplot(data, aes(x = e, y = value, col = taxa)) +
  geom_line() +
  labs(
    y = TeX("Equib. abundance"),
    x = TeX("Fishing effort")) +
  scale_colour_manual(name = "Taxon", labels = taxa.label, values = c("forestgreen","black","black")) +
  scale_size_manual(name = "Taxon", values = c(0.5,0.5,0.5,0.5,0.5,2,0.5,0.5), labels = taxa.label) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  theme_bw(base_size = 12) +
  theme(legend.position="none",legend.text = element_text(size=12), 
        plot.tag = element_text(size=24), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        plot.margin = margin(10,10,10,10)
  ) +
  scale_x_continuous(expand = c(0,0)) +
  ggtitle("Abundance vs fishing effort")
methodsB

## Abundance from catch for sparing = 0.25 ####

# Calculate MSY and set plot catch spectrum
target.MSY.standard <- (target.r*kt)/4
target.catch <- c(seq(0,target.MSY.standard,0.001),target.MSY.standard)
kt <- 1
s <- 0.25

# Generate data
target.abun <- abun.catch(rt=target.r,
                          qt=target.q,
                          rn=target.r,
                          qn=target.q,
                          s=s,
                          ct=target.catch,
                          kt=kt)
non.target1.abun <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target1.r,
                               qn=non.target1.q,
                               s=s,
                               ct=target.catch,
                               kt=kt)
non.target2.abun <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target2.r,
                               qn=non.target2.q,
                               s=s,
                               ct=target.catch,
                               kt=kt)

# Process data
data <- cbind.data.frame(
  target.abun, 
  non.target1.abun, 
  non.target2.abun,
  target.catch) %>% gather(taxa, value, c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))
data$taxa <- factor(data$taxa, levels = c("target.abun", 
                                          "non.target1.abun", 
                                          "non.target2.abun"))

# Plot data
taxa.label <- c("Targeted", 
                "Non-targeted 1",
                "Non-targeted 2")
methodsD <- ggplot(data, aes(x = target.catch/target.MSY.standard, y = value, col = taxa)) +
  geom_line() +
  labs(
    y = TeX("Equib. abundance"),
    x = TeX("Targeted equib. catch")) +
  scale_colour_manual(name = "Taxon", labels = taxa.label, values = c("forestgreen","black","black")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = catch1, linetype = "dashed", colour = "blue2") +
  geom_vline(xintercept = catch2, linetype = "dashed", colour = "darkorange1") +
  theme_bw(base_size = 12) +
  theme(legend.position="none",legend.text = element_text(size=12), 
        plot.tag = element_text(size=24), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = c("grey30", "blue2", "darkorange1", "grey30")),
        axis.title.x = element_text(colour = "forestgreen"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        plot.margin = margin(10,10,10,10)
  ) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, catch1, catch2, 1)) +
  ggtitle("Abundance vs catch")
methodsD

## Plot biodiversity vs sparing vs catch (geometric mean) ####

# Using approach that feeds that calculates 

# Plot constrained results
# Set granularity of exploration
s.interval <- 0.005
c.interval <- 0.01

# Define spectra of catch and sparing being explored
spect.s <- c(0,seq(s.interval,1-s.interval,s.interval))
spect.c <- c(catch1*target.MSY.standard, catch2*target.MSY.standard)

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

# Produce results for non-targeted species
non.target1.n <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target1.r,
                               qn=non.target1.q,
                               s=results$s.value,
                               ct=results$c.value,
                               kt=kt)
non.target2.n <- abun.catch(rt=target.r,
                               qt=target.q,
                               rn=non.target2.r,
                               qn=non.target2.q,
                               s=results$s.value,
                               ct=results$c.value,
                               kt=kt)

# Join all outputs
# target species allowance
target.n <- results$targ.n

results <- cbind.data.frame(results, 
                            target.n, 
                            non.target1.n,
                            non.target2.n)

# Add column for catch relative to MSY
results$c.value.MSY <- results$c.value/target.MSY.standard

# Calculate biodiversity

# Geometric mean
results$geom.mean <- (results$non.target1.n *
                        results$non.target2.n)^(1/2)

# Find highest and lowest biodiversity
# Geometric mean
high.geom.mean <- results
high.geom.mean <- high.geom.mean %>%
  group_by(c.value) %>%
  slice_max(geom.mean, n = 1)

low.geom.mean <- results
low.geom.mean <- low.geom.mean %>%
  group_by(c.value) %>%
  slice_min(geom.mean, n = 1)

results.t <- results
high.geom.mean.t <- high.geom.mean
low.geom.mean.t <- low.geom.mean
data.ends.t <- results.t %>% 
  group_by(c.value) %>% 
  filter(s.value == max(s.value))

# Plot data
methodsE <- ggplot(results.t, aes(x = s.value, y = geom.mean, group = as.character(c.value.MSY), colour = as.character(c.value.MSY))) +
  geom_line() +
  scale_colour_manual(element_blank(), values = c("blue2","darkorange1")) +
  geom_point(data = high.geom.mean.t, aes(x = s.value, y = geom.mean, shape = "H", fill = "H"), size = 1) +
  geom_point(data = low.geom.mean.t, aes(x = s.value, y = geom.mean, shape = "L", fill = "L"), size = 1) +
  scale_fill_manual(element_blank(), values = c("H" = "purple", "L" = "red")) +
  scale_shape_manual(element_blank(), values = c("H" = 21, "L" = 22)) +
  labs(
    y = "Biodiversity \n(geometric mean abundance)", 
    x = TeX("Proportion of seascape spared $(\\textit{s})$")) +
  ylim(0,1) +
  xlim(0,0.75) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  guides(colour = guide_colourbar(order = 1),
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 2)) +
  theme_bw(base_size = 13) +
  theme(legend.text = element_text(size=12), 
        plot.tag = element_text(size=24), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        plot.margin = margin(10,10,10,10),
        #axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 11)
  ) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  ggtitle("Biodiversity vs sparing vs catch for C = 0.35, C = 0.65")
methodsE

# Arrange and save
#ggsave(filename = "figs/methodsA.png", methodsA, width = 6, height = 6, units = "cm", bg = "transparent")
#ggsave(filename = "figs/methodsB.png", methodsB, width = 6, height = 6, units = "cm", bg = "transparent")
#ggsave(filename = "figs/methodsC.png", methodsC, width = 6, height = 6, units = "cm", bg = "transparent")
#ggsave(filename = "figs/methodsD.png", methodsD, width = 6, height = 6, units = "cm", bg = "transparent")
#ggsave(filename = "figs/methodsE.png", methodsE, width = 13.63636, height = 6, units = "cm", bg = "transparent")