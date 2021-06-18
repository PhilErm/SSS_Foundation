# Script details ####

# Script for undertaking real-world fisheries analysis in
# "Meeting wild-caught seafood demand at least cost to biodiversity"

# Load packages ####

library(tidyverse)

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

proc.data <- data %>%
  filter(Year == 2012) %>% # Only year of historical data
  filter(FvFmsy <= 1) %>%  # Only well managed fisheries
  filter(SpeciesCatName == "Crabs, sea-spiders" |
           SpeciesCatName == "King crabs, squat-lobsters" |
           SpeciesCatName == "Lobsters, spiny-rock lobsters" |
           SpeciesCatName == "Shrimps, prawns") # Only Malacostraca fisheries

## Step 2. Identify trawl fisheries ####

# Gear is not included in the database, so this step has to be completed manually.
# Trawl fisheries are identified using Sea Around Us according to the criteria outlined in the 
# paper's methods.

## Step 1. Download data for each species from Sea Around Us
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
  View()
# If percent of shrimp trawl + bottom trawl + dredge is > 90%, call all fisheries capturing species trawl fisheries

# From manual identification process above, filter trawl fisheries that met criteria

trawl.data <- proc.data %>% 
  filter(IdOrig == "13813-FAO-37-45"|
   IdOrig == "CBP-BCRABCHESB-1990-2010-HIVELY"|
   IdOrig == "16799-FAO-31-42"|
   IdOrig == "17526-FAO-31-42"|
   IdOrig == "2612-FAO-31-42"|
   IdOrig == "8832-FAO-31-42"|
   IdOrig == "16956-FAO-21-42"|
   IdOrig == "11149-FAO-27-42"|
   IdOrig == "16423-FAO-27-42"|
   IdOrig == "6071-FAO-27-42"|
   IdOrig == "9356-FAO-27-42"|
   IdOrig == "17068-FAO-77-42"|
   IdOrig == "13981-FAO-37-42"|
   IdOrig == "4027-FAO-37-42"|
   IdOrig == "4948-FAO-37-42"|
   IdOrig == "13830-FAO-27-42"|
   IdOrig == "16918-FAO-21-42"|
   IdOrig == "3955-FAO-27-42"|
   IdOrig == "IFOP-BSQLOBSTERCH-1979-2012-CHING"|
   IdOrig == "13708-FAO-27-45"|
   IdOrig == "13709-FAO-37-45"|
   IdOrig == "3872-FAO-27-45"|
   IdOrig == "SEFSC-BRNSHRIMPGM-1984-2011-HIVELY"|
   IdOrig == "SEFSC-PINKSHRIMPGM-1984-2011-HIVELY"|
   IdOrig == "IFOP-NSHRIMPCH-1949-2012-CHING"|
   IdOrig == "SEFSC-WSHRIMPGM-1984-2011-HIVELY"|
   IdOrig == "ASMFC-PANDALGOM-1960-2009-IDOINE"|
   IdOrig == "14007-FAO-21-45"|
   IdOrig == "3651-FAO-21-45"|
   IdOrig == "7887-FAO-21-45"|
   IdOrig == "11135-FAO-34-45"|
   IdOrig == "182-FAO-47-45"|
   IdOrig == "4302-FAO-34-45"|
   IdOrig == "5238-FAO-34-45"|
   IdOrig == "10350-FAO-77-45"|
   IdOrig == "14701-FAO-61-45"|
   IdOrig == "2678-FAO-31-45"|
   IdOrig == "6764-FAO-61-45"|
   IdOrig == "7430-FAO-61-45"|
   IdOrig == "4328-FAO-34-45"|
   IdOrig == "2252-FAO-87-45"|
   IdOrig == "14786-FAO-61-45"|
   IdOrig == "3300-FAO-77-45"|
   IdOrig == "14151-FAO-34-45"|
   IdOrig == "2001-FAO-61-42"|
   IdOrig == "6684-FAO-61-42"|
   IdOrig == "7352-FAO-61-42"|  
   IdOrig == "10266-FAO-71-42"|
   IdOrig == "10442-FAO-71-42"|
   IdOrig == "14716-FAO-61-42"|
   IdOrig == "15027-FAO-71-42"|
   IdOrig == "3703-FAO-71-42"|
   IdOrig == "5672-FAO-57-42"|
   IdOrig == "8764-FAO-51-42"|
   IdOrig == "9001-FAO-71-42"|
   IdOrig == "14847-FAO-61-45"|
   IdOrig == "16748-FAO-31-45")

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
  mutate(CvCmsy.eq.prac = (Biomass.eq * f) / MSY) # Find eq. C/Cmsy

(meanValue <- mean(eq.data$CvCmsy.eq.prac)) # The mean catch value of well-regulated fisheries

## Step 4. Generate species assemblages used in manuscript ####

# Calculate standard deviations from confidence intervals
dist.pars$log.sd.q <- sd.from.upper(dist.pars$mean.q, dist.pars$upci.q)
dist.pars$log.sd.r <- sd.from.lower(dist.pars$mean.r, dist.pars$lowci.r)

# Set parameters
num.spec <- 10 # Number of species to draw from each distribution 
num.sims <- 100 # Number of species assemblages to simulate

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
   fish.name <- eq.data$IdOrig[i]
   targ.species <- eq.data$SciName[i]
   new <- cbind.data.frame(targ.species,fish.name,num.req.sparing)
   data <- rbind.data.frame(data,new)
}
View(data) # For filling table in appendix

# Diagnostics
hist(outcomes) # Threshold of each assemblage
hist(eq.data$CvCmsy.eq.prac) # C/C_MSY of each fishery to compare with thresholds
hist(data$num.req.sparing) # Number of thresholds each C/C_MSY exceeds
mean(data$num.req.sparing) # Mean number of thresholds each C/C_MSY exceeds
sd(data$num.req.sparing) # SD of number of thresholds each C/C_MSY exceeds
std <- function(x) sd(x)/sqrt(length(x))
std(data$num.req.sparing) # SE of number of thresholds each C/C_MSY exceeds
sum(data$num.req.sparing>(num.sims/2)) # Number of fisheries that require sparing under majority of assemblages
sum(data$num.req.sparing>(num.sims/2))/length(data$num.req.sparing) # Proportion of fisheries that require sparing under majority of assemblages