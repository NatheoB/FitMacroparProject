###############################################################################
###############################   LIBRARIES   #################################
###############################################################################

Install_and_Load <- function(packages) {
  k <- packages[!(packages %in% installed.packages()[, "Package"])];
  if(length(k)) {
    install.packages(k, repos = "https://cran.rstudio.com/");
  }
  for(package_name in packages) {
    library(package_name, character.only = TRUE, quietly = TRUE);
  }
}

# Install only if they are not already installed
# Use the library
Install_and_Load(c("shiny","shinydashboard", "DT", "fitdistrplus", "rhandsontable", "ggplot2"))

###############################################################################
###########################   DATA IMPORTATION   ##############################
###############################################################################

data.description <- read.csv2("www/resources/results/data_description.csv")
data.khi2 <- read.csv2("www/resources/results/data_khi2.csv")
data.ranking <- read.csv2("www/resources/results/data_ranking.csv")
data.param <- read.csv2("www/resources/results/data_param.csv")

data.null <- data.frame(
  paraPerHost = as.integer(0),
  freq = as.integer(0)
)

data.names <- list(
  "Brattey 1988", "Chubb 1963", "Milne 1943",
  "Schmid and Robinson 1972", "Stromberg et al. 1978"
)

plot.grades <- c("Poisson", "Negative binomial", "Geom", "HLmodel", "Observed")
plot.cols <- c("blue", "red", "yellow", "black", "green")

###############################################################################
##############################   FUNCTIONS   ##################################
###############################################################################

# App
freq.group.index <- function(distrib.freq) {
  # Search for k min
  k.min = 1
  if (distrib.freq[k.min] < 5) {
    k.min <- k.min + 1
    tail.upper.sum <- sum(distrib.freq[1:k.min])
    while (k.min < (length(distrib.freq) - 1) && tail.upper.sum < 5) {
      k.min <- k.min + 1
      tail.upper.sum <- sum(distrib.freq[1:k.min])
    }
  }
  
  # Search for k max
  k.max = length(distrib.freq)
  if (distrib.freq[k.max] < 5) {
    k.max <- k.max - 1
    tail.lower.sum <- sum(distrib.freq[k.max:length(distrib.freq)])
    while (k.max > (k.min + 1) && tail.lower.sum < 5) {
      k.max <- k.max - 1
      tail.lower.sum <- sum(distrib.freq[k.max:length(distrib.freq)])
    }
  }
  
  return(c(k.min, k.max))
}

freq.group.distrib <- function(distrib.freq, index.min, index.max) {
  distrib.freq <- c(sum(distrib.freq[1:index.min]),
                    distrib.freq[(index.min+1):(index.max-1)],
                    sum(distrib.freq[index.max:length(distrib.freq)]))
  return(distrib.freq)
}

chisq.calc.statistic <- function(distrib.freq.grouped, observed.freq.grouped) {
  return(sum((observed.freq.grouped - distrib.freq.grouped)^2 / distrib.freq.grouped))
}

chisq.calc.pvalue <- function(statistic, df) {
  if (df < 0) {return(NA)}
  return(pchisq(statistic, df, lower.tail = FALSE))
}

calc.AIC <- function(distrib, pars.number) {
  return(2*pars.number - 2*sum(log(distrib)))
}

calc.BIC <- function(distrib, pars.number, sample.size) {
  return(pars.number*log(sample.size) - 2*sum(log(distrib)))
}

rank.models <- function(BICs) {
  index <- sort(BICs, index.return = TRUE)$ix
  rank <- rep(0, length(BICs))
  for (i in 1:length(BICs)) {
    rank[index[i]] <- i
  }
  return(rank)
}

Mk.geom <- function(k, M0) {
  return((1 - M0)^k * M0)
}

# (Log) Density function with given M0 and input data
M.geom <- function(x, M0, log = FALSE) {
  if (log) {
    return(sapply(x, function(k){log(Mk.geom(k, M0))}))
  }
  return(sapply(x, function(k){Mk.geom(k, M0)}))
}

# Calculate Negative Log-Likelihood
NLL.geom <- function(M0, data) {
  -sum(M.geom(x = data, M0, log = TRUE))
}

# 0 <= prob <= 1
distrib.verif <- function(distrib) {
  if (sum(distrib < 0) || sum(distrib > 1)) {
    return(FALSE)
  }
  return(TRUE)
}

# Give a vector of n random parameters between min and max
pars.random <- function(n, min, max) {
  return(runif(n, min, max))
}

# Check if parameters are all positives
pars.check <- function(pars) {
  return(ifelse(sum(pars<0) == 0, TRUE, FALSE))
}

dM.HLmodel <- function(time, M, pars) {
  # Calculate dMk/dt at time T with initial state M
  with(as.list(pars), {
    dM <- mu - (force + mu) * M[1] + gamma * M[2]
    k <- 2:K
    dM <- c(dM,
            force*M[k-1] - (force + mu + (k-1)*gamma)*M[k] + k*gamma*M[k+1],
            force*M[K] - (force + mu + K*gamma)*M[K+1]
    )
    return(list(dM))
  })
}

M.HLmodel <- function(x, pars, log = FALSE) {
  # Calculate distribution from k=0 to 2*max_k
  # After, M_k = 0
  pars <- c(pars, K = max(x)*2)
  
  # Init Mk equals to 0 except M0 = 1
  Mini = c(1, rep(0, pars["K"][[1]]))
  
  # Find Mk at equilibrium by solving ode
  preci <- 0.01
  times = seq(0, 10, by = preci)
  M <- deSolve::ode(Mini, times, dM.HLmodel, pars)
  M <- as.vector(M[dim(M)[1], 2:dim(M)[2]])
  
  # If precision is not enough and distribution is not mathematically possible
  # Increase precision and resolve ode
  while (!distrib.verif(M)) {
    preci <- preci / 10
    times = seq(0, 10, by = preci)
    M <- deSolve::ode(Mini, times, dM.HLmodel, pars)
    M <- as.vector(M[dim(M)[1], 2:dim(M)[2]])
  }
  
  # Tolerance and avoid prob = 0
  M[which(M < 1e-12)] = 1e-12
  
  # Return vector of log (or not) probability density according to the data
  # assuming the model distribution
  if (log) {
    return(log(M[x+1]))
  }
  return(M[x+1])
}

NLL.HLmodel <- function(pars, data) {
  # If parameters are not positives,
  if (!pars.check(pars)) {
    return(Inf)
  }
  
  # Calculate Negative Log-Likelihood of sampling data following the model distribution
  -sum(M.HLmodel(x = data, pars, log = TRUE))
}


# Maximum likelihood estimation of the model
MLE.HLmodel <- function(data) {
  # Start optim with random initial parameters
  pars <- pars.random(3, 0, 10)
  names(pars) <- c("force", "mu", "gamma")
  
  # Search for the minimum of negative log Likelihood = maximum likelihood ratio
  # Use a tryCatch to avoid script to stop while an error occurs
  pars.mle <- tryCatch(
    optim(par = pars,
          fn = NLL.HLmodel,
          data = data,
          control = list(parscale = pars))$par
    ,
    error = function(e) {
      print(e)
      return(c(NA, NA, NA))}
  )
  return(pars.mle)
}

# Search for Maximum likelihood estimation
# For nit number of random initial parameter
MLE.all.HLmodel <- function(data, nit) {
  mat <- NULL
  
  withProgress(message = "Fitting HLmodel", min = 0, max = nit, value = 0, {
  
    setProgress(detail = "0%")
    
    mle <- MLE.HLmodel(data)
    if (!anyNA(mle)) {mat <- mle}
    incProgress(1, detail = paste(round(1/nit*100), "%"))
  
    if (nit > 1) {
      for (i in 2:nit){
          # Fitting
          mle <- MLE.HLmodel(data)
          if (!anyNA(mle)) {mat <- c(mat, mle)}
          
          # Update progress bar
          incProgress(1, detail = paste(round(i/nit*100), "%"))
      }
    }
    
  })
  
  mat <- matrix(mat, ncol = 3, byrow = TRUE)
  colnames(mat) <- c("force", "mu", "gamma")
  return(mat)
}

# Find parameters that give the maximum likelihood ratio
# Don't find the local minimum of gradient curve but the general one
# Best parameter to fit with data
MLE.best.HLmodel <- function(pars.mat, data) {
  best.pars <- pars.mat[1,]
  best.NLL <- NLL.HLmodel(best.pars, data)
  for (i in 2:nrow(pars.mat)){
    try.pars <- pars.mat[i,]
    try.NLL <- NLL.HLmodel(try.pars, data)
    if (try.NLL < best.NLL) {
      best.pars <- try.pars
      best.NLL <- try.NLL
    }
  }
  return(best.pars)
}

# Give the best parameters to fit with data
find.estimate.HLmodel <- function(data, nit) {
  pars.mat <- MLE.all.HLmodel(data, nit)
  best.pars <- MLE.best.HLmodel(pars.mat, data)
  return(best.pars)
}

