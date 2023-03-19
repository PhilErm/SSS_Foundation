# Script details ####

# Script with class-level parameters used in 
# "The biodiversity benefits of marine protected areas in well-regulated fisheries"

# Load parameters ####

# Catchability
anthozoa.mean.q <- 0.097
anthozoa.upci.q <- 0.229

ascidiacea.mean.q <- 0.012
ascidiacea.upci.q <- 0.193

asteroidea.mean.q <- 0.067
asteroidea.upci.q <- 0.17

bivalvia.mean.q <- 0.207
bivalvia.upci.q <- 0.276

gastropoda.mean.q <- 0.094
gastropoda.upci.q <- 0.19

malacostraca.mean.q <- 0.109
malacostraca.upci.q <- 0.172

ophiuroidea.mean.q <- 0.137
ophiuroidea.upci.q <- 0.239

polychaeta.mean.q <- 0.127
polychaeta.upci.q <- 0.194

# Reproduction
anthozoa.mean.r <- 0.679
anthozoa.lowci.r <- 0.358

ascidiacea.mean.r <- 0.123
ascidiacea.lowci.r <- 0.042

asteroidea.mean.r <- 1.429
asteroidea.lowci.r <- 0.482

bivalvia.mean.r <- 1.567
bivalvia.lowci.r <- 0.923

gastropoda.mean.r <- 1.364
gastropoda.lowci.r <- 0.489

malacostraca.mean.r <- 0.818
malacostraca.lowci.r <- 0.475

ophiuroidea.mean.r <- 3.955
ophiuroidea.lowci.r <- 0.727

polychaeta.mean.r <- 0.879
polychaeta.lowci.r <- 0.538

# Collate parameters into a data frame ####

class <- c("Anthozoa", 
           "Ascidiacea", 
           "Asteroidea", 
           "Bivalvia", 
           "Gastropoda",
           "Malacostraca", 
           "Ophiuroidea", 
           "Polychaeta")
mean.q <- c(anthozoa.mean.q,
            ascidiacea.mean.q,
            asteroidea.mean.q,
            bivalvia.mean.q,
            gastropoda.mean.q,
            malacostraca.mean.q,
            ophiuroidea.mean.q,
            polychaeta.mean.q)
upci.q <- c(anthozoa.upci.q,
            ascidiacea.upci.q,
            asteroidea.upci.q,
            bivalvia.upci.q,
            gastropoda.upci.q,
            malacostraca.upci.q,
            ophiuroidea.upci.q,
            polychaeta.upci.q)
mean.r <- c(anthozoa.mean.r,
            ascidiacea.mean.r,
            asteroidea.mean.r,
            bivalvia.mean.r,
            gastropoda.mean.r,
            malacostraca.mean.r,
            ophiuroidea.mean.r,
            polychaeta.mean.r)
lowci.r <- c(anthozoa.lowci.r,
             ascidiacea.lowci.r,
             asteroidea.lowci.r,
             bivalvia.lowci.r,
             gastropoda.lowci.r,
             malacostraca.lowci.r,
             ophiuroidea.lowci.r,
             polychaeta.lowci.r)

dist.pars <- cbind.data.frame(class, mean.q, upci.q, mean.r, lowci.r)
