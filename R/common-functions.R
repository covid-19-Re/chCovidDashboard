# functions
dgammaShift <- function(x, shape, rate = 1, shift, ...) {
  dgamma(x = x + shift, shape = shape, rate = rate, ...)
}

pgammaShift <- function(x, shape, rate = 1, shift, ...) {
  pgamma(q = x + shift, shape = shape, rate = rate, ...)
}

getInfectivityProfile <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(pt(q = (times - params$shift) / params$scale, params$df))
  } else {
    return(dt(x = (times - params$shift) / params$scale, params$df) / params$scale)
  }
}

# Function to systematically generate infectivity profiles
getInfectivityProfile2 <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(pgammaShift(x = times, shape = params$shape, rate = params$rate, shift = params$shift))
  } else {
    return(dgammaShift(x = times, shape = params$shape, rate = params$rate, shift = params$shift))
  }
}

# Function to systematically generate incubation period distribution
getIncubationPeriod <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(plnorm(q = times, meanlog = params$meanlog, sdlog = params$sdlog))
  } else {
    return(dlnorm(x = times, meanlog = params$meanlog, sdlog = params$sdlog))
  }
}

getGenDist <- function(times, params, CDF = FALSE) {
  if (CDF) {
    return(pweibull(q = times, shape = params$shape, scale = params$scale))
  } else {
    dweibull(x = times, shape = params$shape, scale = params$scale)
  }
}

computeCasesInf <- function(paramList) {
  load("data/quantities.RData")
  #' Compute the number of tertiary cases
  paramDF <- expand.grid(Re = paramList$Re, f = paramList$f, g = paramList$g, KEEP.OUT.ATTRS = F)
  #' We want to vectorise everything in terms of Delta1
  indexDelta1 <- sapply(paramList$Delta1, function(Delta1) which.min(abs(quantitiesMLE$t - Delta1)), USE.NAMES = F)
  #' Loop over the different Re Values
  tertiaryCasesMLE <- lapply(paramList$tau, function(tau) {
    #' Define values that are purely functions of tau and Delta1
    Iinfty <- quantitiesMLE[[iMax, "integral"]]
    PDelta1 <- quantitiesMLE[indexDelta1, "infProfileCDF"] # vector
    indexMinusTau <- which.min(abs(quantitiesMLE$t - (-tau)))
    Ptau <- quantitiesMLE[[indexMinusTau, "infProfileCDF"]]
    Itau <- quantitiesMLE[[indexMinusTau, "integral"]]
    Gtau <- incDist[[which.min(abs(incDist$t - tau)), "CDF"]]

    casesDetectedPart2 <- (PDelta1 - (1 - Gtau) * Ptau - Itau) * Iinfty # * f * g * k1 * k2
    casesUndetected <- (PDelta1 - (1 - Gtau) * Ptau - Itau) # * f * k1 * (1 - g) * n2
    casesOutsideDetection <- ((1 - Gtau) * Ptau - Iinfty + Itau) # * f * k1 * n2

    #' Loop over contact isolation time Delta2
    cases <- lapply(paramList$Delta2, function(Delta2) {
      indexDelta2 <- which.min(abs(quantitiesMLE$t - Delta2))
      #' Perform the integral part for the vector of Delta1's
      casesDetectedPart1 <- unlist(lapply(indexDelta1, function(iDelta1) {
        (1 - Gtau) * sum(
          sapply(seq(indexMinusTau, iDelta1), function(k) {
            quantitiesMLE[k, "infProfile"] * sum(incDist[seq(i0, iMax), "pdf"] * quantitiesMLE[(iDelta1 + indexDelta2 - k) - (seq(i0, iMax) - i0), "infProfileCDF"])
          })
        ) * stepSize^2 + sum(
          sapply(seq(indexMinusTau + 1, i0), function(j) {
            incDist[i0 + (i0 - j), "pdf"] * sum(
              sapply(seq(j, iDelta1), function(k) {
                quantitiesMLE[k, "infProfile"] * sum(incDist[seq(i0, iMax), "pdf"] * quantitiesMLE[(iDelta1 + indexDelta2 - k) - (seq(i0, iMax) - i0), "infProfileCDF"])
              })
            )
          })
        ) * stepSize^3
      })) # * f * g * k1 * k2

      #' Finally loop over the values of f,g,Re and sum up all the cases per parameter set
      cases <- lapply(seq_len(nrow(paramDF)), function(i) {
        Re <- paramDF[[i, "Re"]]
        f <- paramDF[[i, "f"]]
        g <- paramDF[[i, "g"]]

        k1 <- Re / (1 - Iinfty)
        k2 <- k1
        n2 <- k2 * ((1 - f) + f * PDelta1 - Iinfty)
        casesIndexUndetected <- (1 - f) * k1 * (1 - Iinfty) * n2

        c <- f * g * k1 * k2 * (casesDetectedPart1 - casesDetectedPart2) +
          f * k1 * (1 - g) * casesUndetected * n2 +
          f * k1 * casesOutsideDetection * n2 +
          casesIndexUndetected

        data.frame(
          Re = factor(Re, levels = paramList$Re),
          f = factor(f, levels = paramList$f),
          Delta1 = factor(paramList$Delta1, levels = paramList$Delta1),
          tau = factor(tau, levels = paramList$tau),
          g = factor(g, levels = paramList$g),
          Delta2 = factor(Delta2, levels = paramList$Delta2),
          cases = c
        )
      })
      do.call(rbind, cases)
    })
    do.call(rbind, cases)
  })
  tertiaryCasesMLE <- do.call(rbind, tertiaryCasesMLE)
  return(tertiaryCasesMLE)
}
