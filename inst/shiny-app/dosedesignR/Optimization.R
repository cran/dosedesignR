#' Optimal design for a linear model
#'
#' @param modelparam Not needed, can be set to NA
#' @param regimen Is the maximum dose only given or the actual dose levels?
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#' @param levels A vector of dose levels.
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the selected doses, the second row the number of
#'         patients for each dose level.
#' @export
#'
#' @examples OptLinear(NA, "maxOnly", 0, 100, NA)

OptLinear <- function(
  modelparam,
  regimen = c("maxOnly", "levels"),
  minD = 0,
  maxD = NA,
  doselevels = NA
) {
  if (regimen == "maxOnly") {
    doses <- c(minD,maxD)
  } else if(regimen == "levels") {
    doses <- doselevels
  }

  model <- Mods(linear = NULL, doses = doses)
  opt <- optDesign(model, probs = 1)

  idx <- round(opt$design,3) != 0
  res <- rbind(opt$doses[idx], opt$design[idx])

  return(
    list(
      res = res,
      mods = model,
      optD = opt
    )
  )
}

#' Optimal design for an Emax model
#'
#' @param modelparam A named list of model parameters for the emax model:
#'                   list(ED50=)
#' @param regimen Is the maximum dose only given or the actual dose levels?
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#' @param levels A vector of dose levels.
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the selected doses, the second row the number of
#'         patients for each dose level.
#' @export
#'
#' @examples OptEmax(list(ED50=20), "levels", 0, NA, c(0,5,40,100))
OptEmax <- function(
  modelparam,
  regimen = c("maxOnly", "levels"),
  minD = 0,
  maxD = NA,
  doselevels = NA
){
  ED50 <- modelparam$ED50

  if (regimen == "maxOnly") {
    doses <- c(minD:maxD, maxD)
    model <- Mods(emax = ED50, doses = doses)
    dopt <- numeric(3)
    dopt[1] <- a <- min(doses)
    dopt[3] <- b <- max(doses)
    dopt[2] <- (b*(a+ED50)+a*(b+ED50))/(a+b+2*ED50)
    design <- c(1/3, 1/3, 1/3)

    cc <- calcCrit(
      design,
      doses = dopt,
      models = model,
      probs = 1
    )
    opt <- NULL
    opt$crit <- cc

    res <- rbind(dopt,design)
  } else if( regimen == "levels") {
    doses <- doselevels
    model <- Mods(emax = ED50, doses = doses)
    opt <- optDesign(model, probs = 1)
    idx <- round(opt$design,3) != 0
    res <- rbind(opt$doses[idx], opt$design[idx])
  }

  return(
    list(
      res = res,
      mods = model,
      optD = opt
    )
  )
}

#' Optimal design for an Sigmoidal Emax model
#'
#' @param modelparam A named list of model parameters for the emax model:
#'                   list(ED50=, Hill=)
#' @param regimen Is the maximum dose only given or the actual dose levels?
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#' @param levels A vector of dose levels.
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the selected doses, the second row the number of
#'         patients for each dose level.
#' @export
#'
#' @examples OptEmax(list(ED50=20, Hill=3), "levels", 0, NA, c(0,5,40,100))

OptSigEmax <- function(
  modelparam,
  regimen = c("maxOnly", "levels"),
  minD = 0,
  maxD = NA,
  doselevels = NA
){
  ED50 <- modelparam$ED50
  Hill <- modelparam$Hill

  if(regimen == "maxOnly") {
    doses <- c(minD:maxD, maxD)
    model <- Mods(
      sigEmax = c(ED50, Hill),
      doses = doses
    )
    weights <- rep(0.25, 4) #equal weights on all 4 support points

    dopt <- numeric(4)
    dopt[1] <- min(doses)
    dopt[4] <- max(doses)

    if (ED50 < maxD) {
      a <- (ED50 + minD)/2
      b <- min(c(ED50 + (ED50-a), (ED50+maxD)/2))
    } else {
      a <- (maxD-minD)/3    #initial values for middle support points - set rounding level accordingly
      b <- (maxD-minD)*2/3
    }
    dcrit <- function(x) {
      calcCrit(
        design = weights,
        models = model,
        probs = 1,
        doses = c(minD, x, maxD),
        designCrit = "Dopt"
      )
    }

    optx <- try(optim(c(a,b), dcrit, method="L-BFGS-B", lower=minD, upper=maxD), silent = T)
    if("try-error" %in% class(optx)){
      optx <- try(optim(c(a,b), dcrit, method="L-BFGS-B", lower=minD, upper=maxD, control=list(fnscale=10)), silent = T)
      if("try-error" %in% class(optx)){
        optx <- try(optim(c(a,b), dcrit, method="L-BFGS-B", lower=minD, upper=maxD, control=list(fnscale=0.1)), silent = T)
        if("try-error" %in% class(optx)){
          optx <- try(optim(c(a,b), dcrit, method="L-BFGS-B", lower=minD, upper=maxD, control=list(fnscale=100)), silent = T)
          if("try-error" %in% class(optx)){
            optx <- try(optim(c(a,b), dcrit, method="L-BFGS-B", lower=minD, upper=maxD, control=list(fnscale=0.01)), silent = F)
          }
        }
      }
    }

    dopt[2:3] <- sort(optx$par)
    design <- weights

    cc <- calcCrit(design, doses=dopt, models=model, probs=1)
    opt <- NULL
    opt$crit <- cc

    res <- rbind(dopt,design)
  } else if(regimen == "levels") {
    doses <- doselevels
    model <- Mods(
      sigEmax = c(ED50, Hill),
      doses = doses
    )
    opt <- optDesign(model, probs = 1)

    idx <- round(opt$design,3) != 0
    res <- rbind(opt$doses[idx], opt$design[idx], 3)
  }

  return(
    list(
      res = res,
      mods = model,
      optD = opt
    )
  )
}

ComputeOptimal <- function(
  input,
  regimen,
  levels,
  dmin,
  dmax
){
  if (regimen == "Min and max dose only") {
    regimen_tmp <- "maxOnly"
  } else if (regimen == "Specify dose levels") {
    regimen_tmp <- "levels"
    doselevels <- as.numeric(strsplit(levels, ";")[[1]])
    # ignore Dose levels if fewer than one level
    if (length(doselevels) < 2) {
      regimen_tmp <- "maxOnly"
    }
  }
  model <- input$model
  if (model == "Linear") {
    x <- OptLinear(NA, regimen_tmp, dmin, dmax, doselevels)
  } else if(model == "Emax"){
    ED50 <- input$parameter2

    x <- OptEmax(
      list(ED50 = ED50),
      regimen_tmp,
      dmin,
      dmax,
      doselevels
    )
  } else if(model == "Sigmoidal Emax") {
    ED50 <- input$parameter2
    Hill <- input$parameter4

    x <- OptSigEmax(
      list(ED50 = ED50, Hill = Hill),
      regimen_tmp,
      dmin,
      dmax,
      doselevels
    )

  }

  return(x)
}

#For markdown report.Rmd
ComputeOptimal2 <- function(
  model,
  regimen,
  levels,
  dmin,
  dmax,
  parameter1,
  parameter2,
  parameter3,
  parameter4
){
  if (regimen == "Min and max dose only") {
    regimen_tmp <- "maxOnly"
  } else if (regimen == "Specify dose levels") {
    regimen_tmp <- "levels"
    doselevels <- as.numeric(strsplit(levels, ";")[[1]])
    # ignore Dose levels if fewer than one level
    if (length(doselevels) < 2) {
      regimen_tmp <- "maxOnly"
    }
  }
  if(model == "Linear") {
    x <- OptLinear(NA, regimen_tmp, dmin, dmax, doselevels)
  } else if(model == "Emax"){
    ED50 <- parameter2
    x <- OptEmax(
      list(ED50 = ED50),
      regimen_tmp,
      dmin,
      dmax,
      doselevels
    )
  } else if(model == "Sigmoidal Emax") {
    ED50 <- parameter2
    Hill <- parameter4

    x <- OptSigEmax(
      list(ED50 = ED50, Hill = Hill),
      regimen_tmp,
      dmin,
      dmax,
      doselevels
    )
  }


  return(x)
}

ComputeEfficiency <- function(
  UserValue,
  optValue,
  input,
  tab,
  npats
) {
  doses <- UserValue$doses1
  pats <- UserValue$pats1
  models <- optValue$mods1
  optD <- optValue$optD1
  model <- input$model

  if (length(doses) > 0 & length(doses) == length(pats)) {

    if (model == "Linear" & length(unique(doses)) < 2) {
      dat <- matrix("Need at least 2 different dose levels to calculate Efficiency")
    } else if (model == "Linear" & length(unique(doses)) >= 2) {
      cc <- calcCrit(pats/sum(pats), doses = doses, models = models, probs = 1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow = TRUE)
    }

    if (model == "Emax" & length(unique(doses)) < 3) {
      dat <- matrix("Need at least 3 different dose levels to calculate Efficiency")
    } else if (model == "Emax" & length(unique(doses)) >= 3) {
      cc <- calcCrit(pats/sum(pats), doses=doses, models=models, probs=1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow = TRUE)
    }
    if(model == "Sigmoidal Emax" & length(unique(doses)) < 4) {
        dat <- matrix("Need at least 4 different dose levels to calculate Efficiency")
    } else if (model=="Sigmoidal Emax" & length(unique(doses))>=4) {
      cc <- calcCrit(pats/sum(pats), doses=doses, models=models, probs=1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow=T)
    }
  } else {
    dat <- matrix(NA, nrow = 3, byrow = TRUE)
  }
  return(dat)
}


ComputeEfficiency2 <- function(
  userVal,
  optVal,
  model,
  npats
  ) {
  doses <- userVal$dose
  pats <- userVal$patients
  models <- optVal$mods
  optD <- optVal$optD

  if (length(doses) > 0 & length(doses) == length(pats)) {

    if (model == "Linear" & length(unique(doses)) < 2) {
      dat <- matrix("Need at least 2 different dose levels to calculate Efficiency")
    } else if (model == "Linear" & length(unique(doses)) >= 2) {
      cc <- calcCrit(pats/sum(pats), doses=doses, models=models, probs=1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow=TRUE)
    }

    if (model == "Emax" & length(unique(doses)) < 3){
      dat <- matrix("Need at least 3 different dose levels to calculate Efficiency")
    } else if (model=="Emax" & length(unique(doses)) >=3 ){
      cc <- calcCrit(pats/sum(pats), doses=doses, models=models, probs=1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow=T)
    }
    if(model=="Sigmoidal Emax" & length(unique(doses))<4){
      dat <- matrix("Need at least 4 different dose levels to calculate Efficiency")
    }else if(model == "Sigmoidal Emax" & length(unique(doses)) >= 4){
      cc <- calcCrit(pats/sum(pats), doses=doses, models=models, probs=1)
      eff <- (exp(optD$crit)/npats) / (exp(cc)/sum(pats))
      patsAdd <- sum(pats)*1/eff-sum(pats)
      dat <- matrix(c(sum(pats),eff*100,patsAdd), nrow=3, byrow=T)
    }

  } else {
    dat <- matrix(NA, nrow = 3, byrow = TRUE)
  }
  return(dat)
}
