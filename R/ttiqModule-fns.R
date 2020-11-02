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

getIncubationPeriod <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(plnorm(q = times, meanlog = params$meanlog, sdlog = params$sdlog))
  } else {
    return(dlnorm(x = times, meanlog = params$meanlog, sdlog = params$sdlog))
  }
}

# Calculate the number of secondary cases per index case under testing and isolation
getSeconaryCases <- function(paramList, infProf) {
  # We want to vectorise everything in terms of Delta1
  indexDelta1 <- sapply(paramList$Delta1, function(Delta1) which.min(abs(infProf$t - Delta1)), USE.NAMES = F)
  # Loop over Re values
  secondaryCases <- lapply(paramList$Re, function(Re) {
    k <- Re
    # Loop over f values
    lapply(paramList$f, function(f) {
      # Compute cases
      secondaryCases <- k * (f * infProf[indexDelta1, "CDF"] + (1 - f))
      data.frame(
        f = factor(f, levels = paramList$f),
        Delta1 = factor(paramList$Delta1, levels = paramList$Delta1),
        Re = factor(Re, levels = paramList$Re),
        secondaryCases = secondaryCases
      )
    }) %>% bind_rows()
  }) %>% bind_rows()
  return(secondaryCases)
}

# Calculate the number of tertiary cases per index case under testing and isolation
getTertiaryCasesNoTracing <- function(paramList, infProf, integral) {
  # We want to vectorise everything in terms of Delta1
  indexDelta1 <- sapply(paramList$Delta1, function(Delta1) which.min(abs(infProf$t - Delta1)), USE.NAMES = F)
  # Loop over Re values
  tertiaryCases <- lapply(paramList$Re, function(Re) {
    k1 <- Re
    k2 <- Re
    # Loop over f values
    lapply(paramList$f, function(f) {
      #' Compute cases
      tertiaryCases <- k1 * k2 * (f * infProf[indexDelta1, "CDF"] + (1 - f)) * (f * integral[indexDelta1, "J"] + (1 - f))
      data.frame(
        f = factor(f, levels = paramList$f),
        Delta1 = factor(paramList$Delta1, levels = paramList$Delta1),
        Re = factor(Re, levels = paramList$Re),
        tertiaryCases = tertiaryCases
      )
    }) %>% bind_rows()
  }) %>% bind_rows()
  return(tertiaryCases)
}

# Calculate the number of tertiary cases per index case under TTIQ
getTertiaryCases <- function(paramList, infProf, integral, genDist, stepSize) {
  # Compute functions and integrals for each (Delta_1, tau, Delta_2) combination
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
          p <- infProf[seq(index_minusTau,index_Delta1), "pdf"]
          index_lower <- which.min(abs(infProf$t - (Delta1 + Delta2 + tau)))
          index_upper <- which.min(abs(infProf$t - Delta2))
          Q <- genDist[seq(index_lower,index_upper), "CDF"]
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
      }) %>% bind_rows()
    }) %>% bind_rows()
  }) %>% bind_rows()

  # Finally loop over the values of (f,g,Re) and sum up all the cases per parameter set
  paramDF <- expand.grid(Re = paramList$Re, f = paramList$f, g = paramList$g, KEEP.OUT.ATTRS = F)
  tertiaryCases <- lapply(seq_len(nrow(paramDF)), function(i) {
    Re <- paramDF[[i,"Re"]]
    f <- paramDF[[i,"f"]]
    g <- paramDF[[i,"g"]]

    k1 <- Re
    k2 <- Re
    F_f_Delta1 <- f * functions$J_Delta1 + (1 - f)

    tertiaryCases_detected <- f * g * k1 * k2 * functions$integralTerm
    tertiaryCases_undetected <- f * (1 - g) * k1 * k2 * functions$P_diff * F_f_Delta1
    tertiaryCases_outside <- f * k1 * k2 * functions$P_minusTau * F_f_Delta1
    tertiaryCases_indexUndetected <- (1 - f) * k1 * k2 * F_f_Delta1
    tertiaryCases <- tertiaryCases_detected + tertiaryCases_undetected +
      tertiaryCases_outside + tertiaryCases_indexUndetected
    data.frame(
      Re = factor(Re, levels = paramList$Re),
      f = factor(f, levels = paramList$f),
      g = factor(g, levels = paramList$g),
      Delta1 = functions$Delta1,
      tau = functions$tau,
      Delta2 = functions$Delta2,
      tertiaryCases_detected = tertiaryCases_detected,
      tertiaryCases_undetected = tertiaryCases_undetected,
      tertiaryCases_outside = tertiaryCases_outside,
      tertiaryCases_indexUndetected = tertiaryCases_indexUndetected,
      tertiaryCases = tertiaryCases
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
    tertiaryCases = tertiaryCases$tertiaryCases
  )
  return(df)
}

