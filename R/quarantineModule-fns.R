# Working functions
# Functions for generating probability distributions
getGenDist <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(pweibull(q = times, shape = params$shape, scale = params$scale))
  } else {
    dweibull(x = times, shape = params$shape, scale = params$scale)
  }
}

getInfectivityProfile <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(pt(q = (times - params$shift) / params$scale, params$df))
  } else {
    return(dt(x = (times - params$shift) / params$scale, params$df) / params$scale)
  }
}

# Incubation period distribution is averaged across 7 reported distributions:
incParamsDefault <- rbind(
  data.frame(study = "Bi", n = 183, meanlog = 1.570, sdlog = 0.650), # Reported
  data.frame(study = "Lauer", n = 181, meanlog = 1.621, sdlog = 0.418), # Reported
  data.frame(study = "Li", n = 10, meanlog = 1.434, sdlog = 0.661), # From source code of He et al.
  data.frame(study = "Linton", n = 158, meanlog = 1.611, sdlog = 0.472), # with(list(mu = 5.6, sigma = 2.8), c(mean = log(mu^2 / sqrt(mu^2 + sigma^2)), sd = sqrt(log(1 + sigma^2 / mu^2))))
  data.frame(study = "Ma", n = 587, meanlog = 1.857, sdlog = 0.547), # with(list(mu = 7.44, sigma = 4.39), c(mean = log(mu^2 / sqrt(mu^2 + sigma^2)), sd = sqrt(log(1 + sigma^2 / mu^2))))
  data.frame(study = "Zhang", n = 49, meanlog = 1.540, sdlog = 0.470), # Reported
  data.frame(study = "Jiang", n = 2015, meanlog = 1.530, sdlog = 0.464) # Unknown...
)
incParamsDefault$study <- factor(incParamsDefault$study)
#' Mean and SD of this incubation time
incParamsDefault$mean <- mean(exp(incParamsDefault$meanlog + (incParamsDefault$sdlog^2)/2))

getIncubationPeriod <- function(times, params, CDF = FALSE) {
  y <- sapply(levels(params$study), function(study) {
    if (CDF) { # Cumulative density function
      return(plnorm(q = times,
                    meanlog = params[params$study == study, "meanlog"],
                    sdlog = params[params$study == study, "sdlog"]))
    } else { # Probability density function
      return(dlnorm(x = times,
                    meanlog = params[params$study == study, "meanlog"],
                    sdlog = params[params$study == study, "sdlog"]))
    }
  })

  #' Average over the studies
  if (length(times) == 1) return(mean(y))
  else return(apply(y, 1, mean))
}


getIntegral <- function(upper, lower, tE, params) {
  getGenDist(times = upper - tE, params = params, CDF = T) -
    getGenDist(times = lower - tE, params = params, CDF = T)
}

getUtility <- function(s = 1, efficacy, time) {
  s * efficacy / time
}

dayLabels <- function(x) {
  labs <- paste(x, ifelse(x == 1, "day", "days"))
  names(labs) <- x
  return(labs)
}

# False negative probabilities
falseNeg <- approxfun(
  x = c(0, 1, 4, 5, 6, 7, 8, 9, 21),
  y = c(1, 1, 0.67, 0.38, 0.25, 0.21, 0.20, 0.21, 0.66)
)
