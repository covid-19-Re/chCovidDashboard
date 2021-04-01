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

#' To enable parallelisation, we pass everything as arguments
getTertiaryCasesNoTracing <- function(paramList, infProf, integral) {
  #' We want to vectorise everything in terms of Delta1
  indexDelta1 <- sapply(paramList$Delta1, function(Delta1) which.min(abs(infProf$t - Delta1)), USE.NAMES = F)
  #' Loop over Re values
  out.df <- lapply(paramList$R, function(R) {
    #' Loop over f values
    lapply(paramList$f, function(f) {
      #' Loop over alpha values
      lapply(paramList$alpha, function(alpha) {
        #' Compute cases
        secondaryCases <- R * ((1-alpha)*f * infProf[indexDelta1, "CDF"] + (1 - (1-alpha)*f))
        tertiaryCases <- R^2 * ((1-alpha)*f * infProf[indexDelta1, "CDF"] + (1 - (1-alpha)*f)) * ((1-alpha)*f * integral[indexDelta1, "J"] + (1 - (1-alpha)*f))

        data.frame(
          alpha = factor(alpha, levels = paramList$alpha),
          f = factor(f, levels = paramList$f),
          Delta1 = factor(paramList$Delta1, levels = paramList$Delta1),
          R = factor(R, levels = paramList$R),
          secondaryCases = secondaryCases,
          tertiaryCases = tertiaryCases,
          ter.per.sec = ifelse(secondaryCases > 0, tertiaryCases/secondaryCases, 0)
        )
      }) %>% bind_rows()
    }) %>% bind_rows()
  }) %>% bind_rows()
  return(out.df)
}

# Calculate the number of tertiary cases per index case under TTIQ
getTertiaryCases <- function(paramList, infProf, integral, genDist, stepSize) {
  #' Compute functions and integrals for each (Delta_1, tau, Delta_2) combination
  functions <- lapply(paramList$Delta1, function(Delta1) {
    index_Delta1 <- which.min(abs(infProf$t - Delta1))
    P_Delta1 <- infProf[[index_Delta1, "CDF"]]
    J_Delta1 <- integral[[index_Delta1, "J"]]

    lapply(paramList$tau, function(tau) {
      index_minusTau <- which.min(abs(infProf$t + tau))
      P_minusTau <- infProf[[index_minusTau, "CDF"]]
      P_diff <- P_Delta1 - P_minusTau

      lapply(paramList$Delta2, function(Delta2) {
        integralTerm <- 0
        if(any(paramList$g > 0)) {
          p <- infProf[seq(index_minusTau,index_Delta1),"pdf"]
          index_lower <- which.min(abs(infProf$t - (Delta1 + Delta2 + tau)))
          index_upper <- which.min(abs(infProf$t - Delta2))
          Q <- genDist[seq(index_lower,index_upper),"CDF"]
          integralTerm <- sum(p * Q) * stepSize
        }
        data.frame(
          Delta1 = factor(Delta1, levels = paramList$Delta1),
          tau = factor(tau, levels = paramList$tau),
          Delta2 = factor(Delta2, levels = paramList$Delta2),
          P_Delta1 = P_Delta1,
          P_minusTau = P_minusTau,
          P_diff = P_diff,
          J_Delta1 = J_Delta1,
          integralTerm = integralTerm
        )
      }) %>%bind_rows()
    }) %>%bind_rows()
  }) %>%bind_rows()

  #' Finally loop over the values of (alpha,f,g,R) and sum up all the cases per parameter set
  paramDF <- expand.grid(R = paramList$R, alpha = paramList$alpha, f = paramList$f, g = paramList$g, KEEP.OUT.ATTRS = F)
  tertiaryCases <- lapply(seq_len(nrow(paramDF)), function(i) {
    R <- paramDF[[i,"R"]]
    alpha <- paramDF[[i,"alpha"]]
    f <- paramDF[[i,"f"]]
    g <- paramDF[[i,"g"]]

    secondaryCases <- R * ((1-alpha)*f * functions$P_Delta1 + (1 - (1-alpha)*f))

    F_f_Delta1 <- (1-alpha)*f * functions$J_Delta1 + (1 - (1-alpha)*f)

    tertiaryCases_detected <- R^2 * (1-alpha)*f*g * functions$integralTerm
    tertiaryCases_undetected <- R^2 * (1-alpha)*f*(1-g) * functions$P_diff * F_f_Delta1
    tertiaryCases_outside <- R^2 * (1-alpha)*f * functions$P_minusTau * F_f_Delta1
    tertiaryCases_indexUndetected <- R^2 * (1 - (1-alpha)*f) * F_f_Delta1

    tertiaryCases <- tertiaryCases_detected + tertiaryCases_undetected +
      tertiaryCases_outside + tertiaryCases_indexUndetected


    # Now tertiary cases caused by asymptomatic secondary cases only
    #tertiaryCases_asymptomatic <- alpha * tertiaryCases_detected +
    #  (alpha / F_f_Delta1) * (tertiaryCases_undetected + tertiaryCases_outside + tertiaryCases_indexUndetected)

    data.frame(
      R = factor(R, levels = paramList$R),
      alpha = factor(alpha, levels = paramList$alpha),
      f = factor(f, levels = paramList$f),
      g = factor(g, levels = paramList$g),
      Delta1 = functions$Delta1,
      tau = functions$tau,
      Delta2 = functions$Delta2,
      secondaryCases = secondaryCases,
      tertiaryCases = tertiaryCases,
      ter.per.sec = ifelse(secondaryCases > 0, tertiaryCases/secondaryCases, 0)#,
      #tertiaryCases_asymptomatic = tertiaryCases_asymptomatic,
      #frac_asymptomatic = ifelse(tertiaryCases > 0, tertiaryCases_asymptomatic/tertiaryCases, 0)
    )
  }) %>% bind_rows()
  return(tertiaryCases)
}

perturbParam <- function(paramList, param, values, infProf, integral, genDist, stepSize) {
  # Perturb values
  paramList[[param]] <- values
  # Compute cases
  tertiaryCases <- getTertiaryCases(paramList, infProf, integral, genDist, stepSize)
  # Format and return
  df <- data.frame(
    param = factor(param, levels = c("f", "Delta1", "tau", "g", "Delta2")),
    value = values,
    ter.per.sec = tertiaryCases$ter.per.sec
  )
  return(df)
}

