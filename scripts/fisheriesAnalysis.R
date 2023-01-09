# Script details ####

# Script for undertaking real-world fisheries analysis in
# "Meeting wild-caught seafood demand at least cost to biodiversity"

# Load packages ####

library(tidyverse)
library(data.table)

# Load scripts ####

source("scripts/parameters.R")
source("scripts/functions.R")

# Load data ####

# The global fisheries database used here must be acquired before running the script.
# It's construction is described in: Costello, C. et al. Global fishery prospects under 
# contrasting management regimes. (2016). Proc.Natl Acad.
# The version that this script is designed to be used with can be found in the data repository 
# for: Costello, C. et al. The future of food from the sea. (2020). Nature.
# The repository can be found here: https://datadryad.org/stash/dataset/doi:10.25349/D96G6H.

# After downloading the data, the data sheet that is needed is "ProjectionData.csv".

# Importing data
data <- read.csv("data/costelloData/m_capture_data/ProjectionData.csv") # Modify this line to point to your copy of ProjectionData.csv

# Perform analysis ####

## Step 1. Filter fisheries according to criteria outlined in methods ####

# Code for four different scenarios is provided below.
# To obtain the results for a particular scenario, uncomment the code block associated with it.
# The script as provided has already done this for scenario 1.
# To change scenarios, comment out scenario 1 and uncomment your chose scenario.

# Scenario 1: RAM + FAO at 2012 mortality, projected forward (default)
proc.data <- data %>%
  filter(Year == 2012) %>% # Only year of historical data
  filter(FvFmsy <= 1) %>%  # Only well-regulated fisheries
  filter(SpeciesCatName == "Crabs, sea-spiders" | # Only Malacostraca fisheries
           SpeciesCatName == "King crabs, squat-lobsters" |
           SpeciesCatName == "Lobsters, spiny-rock lobsters" |
           SpeciesCatName == "Shrimps, prawns")

# Scenario 2: RAM alone at 2012 mortality, projected forward (sensitivity)
# proc.data <- data %>%
#   filter(Year == 2012) %>% # Only year of historical data
#   filter(Dbase == "RAM") %>%
#   filter(FvFmsy <= 1) %>%  # Only well-regulated fisheries
#   filter(SpeciesCatName == "Crabs, sea-spiders" | # Only Malacostraca fisheries
#            SpeciesCatName == "King crabs, squat-lobsters" |
#            SpeciesCatName == "Lobsters, spiny-rock lobsters" |
#            SpeciesCatName == "Shrimps, prawns")

# Scenario 3: RAM + FAO at 2050 BAU (sensitivity)
# proc.data <- data %>%
#   filter(Policy == "BAU") %>%
#   filter(Scenario == "All Stocks") %>%
#   filter(Year == 2050) %>% # Only year of historical data
#   filter(FvFmsy <= 1) %>%  # Only well-regulated fisheries
#   filter(SpeciesCatName == "Crabs, sea-spiders" | # Only Malacostraca fisheries
#            SpeciesCatName == "King crabs, squat-lobsters" |
#            SpeciesCatName == "Lobsters, spiny-rock lobsters" |
#            SpeciesCatName == "Shrimps, prawns")

# Scenario 4: RAM + FAO at 2050 RBFM (sensitivity)
proc.data <- data %>%
  filter(Policy == "RBFM") %>%
  filter(Scenario == "All Stocks") %>%
  filter(Year == 2050) %>% # Only year of historical data
  filter(FvFmsy <= 1) %>%  # Only well-regulated fisheries
  filter(SpeciesCatName == "Crabs, sea-spiders" | # Only Malacostraca fisheries
           SpeciesCatName == "King crabs, squat-lobsters" |
           SpeciesCatName == "Lobsters, spiny-rock lobsters" |
           SpeciesCatName == "Shrimps, prawns")


## Step 2. Identify trawl fisheries ####

# Gear is not included in the database, so this step has to be completed manually.
# Trawl fisheries are identified using Sea Around Us according to the criteria outlined in the 
# paper's methods.

# Species in which 90% of catch is from trawling
valid <- c("Aristaeomorpha foliacea",
           "Aristeus antennatus",
           "Callinectes sapidus", 
           "Cancer borealis", 
           "Cancer pagurus",
           "Carcinus aestuarii", 
           "Carcinus maenas", 
           "Cervimunida Johni", 
           "Crangon crangon",
           "Farfantepenaeus aztecus", 
           "Farfantepenaeus duorarum", 
           "Heterocarpus reedi",
           "Litopenaeus setiferus", 
           "Pandalus borealis", 
           "Pandalus borealis", 
           "Pandalus hypsinotus",
           "Pandalus kessleri",
           "Parapenaeus longirostris",
           "Penaeus aztecus",
           "Penaeus brevirostris", 
           "Penaeus monodon", 
           "Penaeus duorarum", 
           "Penaeus japonicus",
           "Penaeus notialis", 
           "Penaeus occidentalis", 
           "Penaeus penicillatus", 
           "Penaeus setiferus",
           "Penaeus vannamei",
           "Pleuroncodes monodon",
           "Plesiopenaeus edwardsianus", 
           "Portunus trituberculatus", 
           "Scylla serrata",
           "Trachypenaeus curvirostris",
           "Xiphopenaeus kroyeri",
           "Pandalus kessleri", 
           "Haliporoides triarthrus", 
           "Geryon longipes", 
           "Pleoticus robustus", 
           "Solenocera agassizii", 
           "Penaeus californiensis", 
           "Pleoticus muelleri", 
           "Artemesia longinaris", 
           "Penaeus stylirostris")

# Species in which 90% of catch is not from trawling
invalid <- c("Callinectes danae", 
             "Cancer edwardsii", 
             "Cancer magister", 
             "Cancer productus",
             "Chaceon affinis", 
             "Chaceon fenneri", 
             "Chaceon maritae", 
             "Chaceon notialis",
             "Chaceon geryons nei", 
             "Chionoecetes bairdi",
             "Chionoecetes japonicus",
             "Chionoecetes opilio",
             "Homarus americanus", 
             "Homarus gammarus", 
             "Jasus edwardsii",
             "Jasus lalandii", 
             "Lithodes santolla", 
             "Maja squinado", 
             "Menippe mercenaria",
             "Metanephrops challengeri", 
             "Mithrax armatus", 
             "Nephrops norvegicus",
             "Palaemon serratus",
             "Palinurus gilchristi",
             "Panulirus argus", 
             "Paralithodes brevipes", 
             "Paralithodes camtschaticus",
             "Paralithodes platypus", 
             "Paralomis granulosa", 
             "Penaeus latisulcatus",
             "Penaeus monodon",
             "Penaeus semisulcatus",
             "Portunus pelagicus", 
             "Sicyonia brevirostris",
             "Penaeus merguiensis", 
             "Metapenaeus endeavouri", 
             "Scyllarides latus", 
             "Necora puber", 
             "Thenus orientalis", 
             "Erimacrus isenbeckii", 
             "Penaeus brasiliensis", 
             "Jasus tristani", 
             "Metapenaeus monoceros", 
             "Palinurus delagoae", 
             "Penaeus kerathurus", 
             "Palinurus elephas", 
             "Ibacus ciliatus", 
             "Panulirus longipes", 
             "Nematopalaemon hastatus", 
             "Cancer irroratus", 
             "Panulirus homarus", 
             "Haliporoides diomedeae", 
             "Jasus frontalis", 
             "Acetes japonicus", 
             "Penaeus chinensis", 
             "Aristeus varidens", 
             "Palinurus mauritanicus", 
             "Parapenaeopsis atlantica", 
             "Panulirus cygnus", 
             "Nematopalaemon schmitti", 
             "Metapenaeus joyneri", 
             "Metanephrops mozambicus", 
             "Panulirus gracilis")

# Species not in Sea Around Us or not resolved at species level
unknown <- c("Chaceon spp",
             "Chionoecetes spp",
             "Galatheidae", 
             "Homalaspis plana",
             "Jasus verreauxi",
             "Lithodes aequispina",
             "Penaeus setiferus",
             "Pseudocarcinus gigas",
             "Sicyonia ingentis",
             "Upogebia pugettensis", 
             "Xiphopenaeus riveti", 
             "Pandalus jordani", 
             "Chaceon quinquedens", 
             "Jasus novaehollandiae", 
             "Jasus paulensis", 
             "Acetes erythraeus", 
             "Natantia", 
             "Panulirus spp", 
             "Brachyura", 
             "Metapenaeus spp", 
             "Parapenaeopsis spp", 
             "Penaeus spp", 
             "Sergestidae", 
             "Scyllaridae", 
             "Palinurus spp", 
             "Palaemonidae", 
             "Portunus spp", 
             "Reptantia", 
             "Pandalus spp", 
             "Crangonidae", 
             "Metanephrops spp", 
             "Palinuridae")

# To aid in seeing which species of the current fisheries data being examined have/haven't been checked
species.checked <- c(valid, invalid, unknown)
vect <- proc.data$SciName[!(proc.data$SciName %in% species.checked)]
unique(vect)

# Tool for checking unchecked species

# If there are any unchecked species then complete the procedure below to
# confirm if they are mostly caught in trawl fisheries or not.

## Step 1. Download data for species from Sea Around Us
## Step 2. Use code below to verify % of species take that came from trawling
## Step 3. If over 90%, call all fisheries capturing species trawl fisheries
## Step 4. Repeat from Step 1 for each species until completed for all species

species <- read.csv("data/costelloData/species.csv") # Modify this line to point to your copy of species data from Sea Around Us

species.proc <- species %>%
  filter(area_type == "eez") %>%
  filter(year == 2012) %>%
  filter(fishing_sector == "Industrial") %>%
  filter(catch_type == "Landings") %>%
  filter(reporting_status == "Reported") %>% 
  group_by(scientific_name, common_name, gear_type) %>% 
  summarise(tot.landings = sum(tonnes)) %>% 
  mutate(percent = tot.landings/sum(tot.landings)) %>% 
  mutate(inclusion = case_when(
    gear_type == "beam trawl" ~ "include",
    gear_type == "bottom trawl" ~ "include",
    gear_type == "otter trawl" ~ "include",
    gear_type == "shrimp trawl" ~ "include",
    gear_type == "dredge" ~ "include",
    TRUE ~ "no include")
  )

check <- species.proc %>% 
  group_by(inclusion) %>% 
  summarise(percent.trawl = sum(percent)) %>% 
  print()

# If percent of shrimp trawl + bottom trawl + otter trawl + beam trawl + dredge (i.e. include) is > 90%, include fisheries targeting that species in analysis
# (I.e., add them to the large vector of valid species above.)

# From manual identification process above, filter fisheries to those targeting species that met criteria

trawl.data <- proc.data %>% 
  filter(SciName %in% valid)

## Step 3. Find equilibrium state of fisheries at 2012 mortality ####

## Define function for finding equilibrium
PT.eq <- function(k,g,f,p){
  k*((g-(f*p)+(g*p))/(g*(1+p)))^(1/p)
}

## Find equilibria
eq.data <- trawl.data %>%
  mutate(f = Catch / Biomass) %>% # Find fishing mortality
  mutate(Biomass.msy = Biomass/BvBmsy) %>% # Find MSY biomass
  mutate(Biomass.eq = PT.eq(k=.data$k, # Find equilibrium biomass
                            g=.data$g,
                            f=.data$f,
                            p=.data$phi)) %>% 
  mutate(rel.Biomass.eq = Biomass.eq / k) %>% # Find eq. biomass relative to K
  mutate(BvBmsy.eq = Biomass.eq / Biomass.msy) %>% # Find eq. B/Bmsy
  mutate(CvCmsy.eq.prac = (Biomass.eq * f) / MSY) %>%  # Find eq. C/Cmsy
  mutate(CvCmsy.given = (Catch / MSY)) # # Find C/Cmsy from values given by Costello at 2050

(meanValue <- mean(eq.data$CvCmsy.eq.prac)) # The mean catch value of well-regulated fisheries
(meanValue <- mean(eq.data$CvCmsy.given)) # (Costello values)

## Step 4. Generate species assemblages used in manuscript ####

# Calculate standard deviations from confidence intervals
dist.pars$log.sd.q <- sd.from.upper(dist.pars$mean.q, dist.pars$upci.q)
dist.pars$log.sd.r <- sd.from.lower(dist.pars$mean.r, dist.pars$lowci.r)

# Set parameters
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 250 # Number of species assemblages to simulate

# Generate assemblages
set.seed(1) # Random seed used in manuscript
list.data <- list()
for(j in 1:num.sims){
   all.species <- builder(n = num.spec, 
                          mean.q = dist.pars$mean.q, 
                          sd.q = dist.pars$log.sd.q,
                          mean.r = dist.pars$mean.r, 
                          sd.r = dist.pars$log.sd.r,
                          name = dist.pars$class)
   list.data[[j]] <- all.species
}

# Store results
data <- rbindlist(list.data, idcol="sim")

# Measure sensitivity of species
data$sensitivity <- data$q.value/data$r.value

## Step 5. Find and isolate the most sensitive species in each assemblage ####

most.sens <- data %>% 
   group_by(sim) %>% 
   slice_max(sensitivity, n = 1)

## Step 6. Apply each assemblage to each fishery ####

# Function for finding catch threshold of particular assemblage
# If a fishery takes a C/C_MSY above the identified threshold, then sparing is required
# Note: obtained by solving rule 2 for C/K
thresh <- function(qt,rt,qn,rn){
   (qt*(-qt+(rt*(qn/rn))))/(rt*((qn/rn)^2))
}

# Set targeted species parameters
target.r <- malacostraca.mean.r
target.q <- malacostraca.mean.q
target.MSY <- (target.r*1)/4 # MSY is calculated relative to carrying capacity, so K = 1

# Find catch thresholds above which sparing is required for all assemblages
outcomes <- sort(thresh(qt = target.q, 
                        rt = target.r,
                        qn = most.sens$q.value,
                        rn = most.sens$r.value))
outcomes <- outcomes/target.MSY # Make relative to MSY since eq.data catches we will be comparing to are relative to MSY

# For each fishery, count how many assemblages are above catch thresholds
data <- NULL
for(i in 1:length(eq.data$CvCmsy.eq.prac)){
   num.req.sparing <- sum(eq.data$CvCmsy.eq.prac[i] > outcomes)
   num.req.sparing.given <- sum(eq.data$CvCmsy.given[i] > outcomes)
   fish.name <- eq.data$IdOrig[i]
   location <- eq.data$Country[i]
   CvCmsy.eq.prac <- eq.data$CvCmsy.eq.prac[i]
   CvCmsy.given <- eq.data$CvCmsy.given[i]
   targ.species <- eq.data$SciName[i]
   new <- cbind.data.frame(targ.species,fish.name,num.req.sparing, num.req.sparing.given,location,CvCmsy.eq.prac,CvCmsy.given)
   data <- rbind.data.frame(data,new)
}
#View(data) # For filling table in appendix

# Filter Costello data with 2050 catch = 0 (i.e. fisheries that no longer exist)
data <- data %>% 
  filter(CvCmsy.given != 0)

# Diagnostics
# All ".given" results apply to scenarios 3 and 4. Costello el al. (2016) gives the data, meaning the equilibrium 
# calculation isn't required.
# All non-".given" results apply to scenarios 1 and 2.

hist(outcomes) # Threshold of each assemblage
hist(eq.data$CvCmsy.eq.prac) # C/C_MSY of each fishery to compare with thresholds
hist(eq.data$CvCmsy.given) # (Costello version)
mean(eq.data$CvCmsy.eq.prac) # Mean C/C_MSY taken
mean(eq.data$CvCmsy.given) # (Costello version)
hist(data$num.req.sparing) # Number of thresholds each C/C_MSY exceeds
hist(data$num.req.sparing.given) # (Costello version)
mean <- mean(data$num.req.sparing) # Mean number of thresholds each C/C_MSY exceeds
mean.given <- mean(data$num.req.sparing.given) # (Costello version)
sd(data$num.req.sparing) # SD of number of thresholds each C/C_MSY exceeds
sd(data$num.req.sparing.given) # (Costello version)
std <- function(x) sd(x)/sqrt(length(x)) # Function for calculating SE
sem <-std(data$num.req.sparing) # SE of number of thresholds each C/C_MSY exceeds
sem.given <-std(data$num.req.sparing.given) # (Costello version)
CI <- sem*1.96 # 95% confidence interval
CI.given <- sem.given*1.96 # (Costello version)
sum(data$num.req.sparing>(num.sims/2)) # Number of fisheries that require sparing under majority of assemblages
sum(data$num.req.sparing.given>(num.sims/2)) # (Costello version)
sum(data$num.req.sparing>(num.sims/2))/length(data$num.req.sparing) # Proportion of fisheries that require sparing under majority of assemblages
sum(data$num.req.sparing.given>(num.sims/2))/length(data$num.req.sparing.given) # (Costello version)
