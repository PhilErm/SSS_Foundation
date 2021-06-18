# Script details ####

# Script with functions used to undertake analysis and generate figures in 
# "Meeting wild-caught seafood demand at least cost to biodiversity"

# For generating distributions ####

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
  q.value <- sampler(mean.vec = log(mean.q),
                     sd.vec = sd.q,
                     n = n)
  r.value <- sampler(mean.vec = log(mean.r),
                     sd.vec = sd.r,
                     n = n)
  class <- rep(name, each = n)
  samp.pars <- cbind.data.frame(class, species, q.value, r.value)
  samp.pars
}

# For sparing/sharing framework ####

## Which calculate the inside/outside reserve populations separately ####

# Abundance inside reserve from effort
abun.eff.in <- function(k,s){
  k*s
}

# Abundance outside reserve from effort
abun.eff.out <- function(r,k,s,e,q){
  out <- (k*((q*e)-r)*(-1+s))/r
  out[out < 0] <- 0
  out
}

# Abundance inside reserve from catch
abun.catch.in <- function(k,s){
  k*s
}

# Abundance outside reserve from catch
abun.catch.out <- function(r,k,s,c){
  -((sqrt(k)*sqrt(as.complex(4*c + k*r*(-1 + s)))*sqrt(as.complex(-1 + s)))/(2*sqrt(as.complex(r)))) - (1/2)*k*(-1 + s)
}

## Which calculate the inside/outside reserve populations together ####

# Abundance from effort
abun.eff <- function(s,e,q,r){
  pop <- ifelse(((s-1)*((q*e)-r))/r>0, (((s-1)*((q*e)-r))/r)+s, s)
}

# Abundance from catch
abun.catch <- function(rt,qt,rn,qn,s,ct,kt){
  ifelse(1-s-((2*qn*rt)/(qt*rn*(rt+sqrt(rt*((4/(s-1))*(ct/kt)+rt)))))*(ct/kt)>0,
         1-((2*qn*rt)/(qt*rn*(rt+sqrt(rt*((4/(s-1))*(ct/kt)+rt)))))*(ct/kt),
         s)
}

## Miscellaneous ####

# Geometric mean
gm.mean <- function(x){
  prod(x)^(1/length(x))
}