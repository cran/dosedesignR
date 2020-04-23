
#' Calculate dose-response curve for the linear model
#'
#' @param modelparam A named list of model parameters for the emax model:
#'                   list(intercept=, slope=)
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the dose levels, the second row the response
#'         for each dose level.
#' @export
#'
#' @examples FncLinear(list(intercept=3, slope=7), 0, 100)
FncLinear <- function(modelparam, minD=0, maxD)
{
  intercept <- modelparam$intercept
  slope <- modelparam$slope
  x <- 0:maxD
  y <- intercept + slope*x
  return(rbind(x,y))
}


#' Calculate dose-response curve for the emax model
#'
#' @param modelparam A named list of model parameters for the emax model:
#'                   list(E0=, ED50=, Emax=)
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the dose levels, the second row the response
#'         for each dose level.
#' @export
#'
#' @examples FncEmax(list(E0=3, ED50=20, Emax=100), 0, 100)
FncEmax <- function(modelparam, minD=0, maxD)
{
  E0 <- modelparam$E0
  ED50 <- modelparam$ED50
  Emax <- modelparam$Emax
  x <- 0:maxD
  y <- E0 + Emax*x/(ED50+x)
  return(rbind(x,y))
}

#' Calculate dose-response curve for the sigmoidal emax model
#'
#' @param modelparam A named list of model parameters for the emax model:
#'                   list(E0=, ED50=, Emax=, Hill=)
#' @param minD The smallest dose level
#' @param maxD The largest dose level
#'
#' @return Returns a 2 x D matrix, where D is the number of dose levels.
#'         The first contains the dose levels, the second row the response
#'         for each dose level.
#' @export
#'
#' @examples FncEmax(list(E0=3, ED50=20, Emax=100), 0, 100)
FncSigEmax <- function(modelparam, minD=0, maxD)
{
  E0 <- modelparam$E0
  ED50 <- modelparam$ED50
  Emax <- modelparam$Emax
  Hill <- modelparam$Hill
  x <- 0:maxD
  y <- E0 + Emax*x^Hill/(ED50^Hill+x^Hill)
  return(rbind(x,y))
}

ComputeModelFunction <- function(input, tab){
  
  model <- input[[paste("model",tab, sep="")]]
  x <- seq(input$dmin,input$dmax, length = 201) 
  
  if(model=="Linear") {
    Int <- input[[paste("linInt", tab, sep="")]]
    Slp <- input[[paste("linSlp", tab, sep="")]]
    y <- linear(x, Int, Slp)
  } else if(model=="Emax"){
    E0 <- input[[paste("emxE0", tab, sep="")]]
    ED50 <- input[[paste("emxED50", tab, sep="")]]
    Emax <- input[[paste("emxEmax", tab, sep="")]]
    y <- emax(x,e0=E0, ed50=ED50, eMax=Emax)
  } else if(model=="Sigmoidal Emax") {
    E0 <- input[[paste("semxE0", tab, sep="")]]
    ED50 <- input[[paste("semxED50", tab, sep="")]]
    Emax <- input[[paste("semxEmax", tab, sep="")]]
    Hill <- input[[paste("semxHill", tab, sep="")]]
    y <- sigEmax(x,e0=E0, ed50=ED50, eMax=Emax, h=Hill)
  }
  datplot <- data.frame(x,y)
  pl <- ggplot(data=datplot,aes(x=x,y=y))+geom_line()+labs(x="Dose", y="Response", title="Dose-response curve")+
    ylim(input$ymin,input$ymax)+xlim(input$dmin, input$dmax)+
    theme(axis.text = element_text(size=rel(1.1)),axis.title = element_text(size=rel(1.1)),plot.title = element_text(size=rel(1.25),hjust=0.5,face="bold"))
 return(pl)
}




