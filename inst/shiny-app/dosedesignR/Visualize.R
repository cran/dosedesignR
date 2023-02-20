dosedesignR_green <- "#5cb85c"
dosedesignR_blue <- "#3a6791"
dosedesignR_red <- "#c94053"

f_col <- colorRamp(c(dosedesignR_blue, "gray89", dosedesignR_green))
f_col_report <- colorRamp(c(dosedesignR_blue, "gray42", dosedesignR_green))

#' Calculate dose-response curve for the linear model
#'
#' @param modelparam A named list of model parameters for the linear model:
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

FncLinear <- function(
  modelparam,
  minD = 0,
  maxD
) {
  intercept <- modelparam$intercept
  slope <- modelparam$slope
  x <- 0:maxD
  y <- intercept + slope * x
  return(rbind(x, y))
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

FncEmax <- function(
  modelparam,
  minD = 0,
  maxD
){
  E0 <- modelparam$E0
  ED50 <- modelparam$ED50
  Emax <- modelparam$Emax
  x <- 0:maxD
  y <- E0 + Emax * x/(ED50 + x)
  return(rbind(x, y))
}

#' Calculate dose-response curve for the sigmoidal emax model
#'
#' @param modelparam A named list of model parameters for the sigmoidal emax model:
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
FncSigEmax <- function(
  modelparam,
  minD = 0,
  maxD
){
  E0 <- modelparam$E0
  ED50 <- modelparam$ED50
  Emax <- modelparam$Emax
  Hill <- modelparam$Hill
  x <- 0:maxD
  y <- E0 + Emax * x^Hill/(ED50^Hill + x^Hill)
  return(rbind(x, y))
}

#' Function to compute the model function
#'
#' @param input
#' @param tab
#' @param ymin
#' @param ymax
#' @param dmin
#' @param dmax
#' @param data
#' @param data_esti
#' @param plot_opt
#' @param selectnr
#'
#'

ComputeModelFunction <- function(
  input,
  tab,
  ymin,
  ymax,
  dmin,
  dmax,
  data = data,
  data_esti,
  plot_opt,
  selectnr
) {

  model <- input$model
  x <- seq(dmin,dmax, length = 201)
  if (model == "Linear") {
    Int <- input$parameter1
    Slp <- input$parameter2

    y <- linear(x, Int, Slp)

    if (!is.null(data_esti) & !is.null(selectnr)) {
      y_tmp <- lapply(data_esti[selectnr], function(x){ fitMod(x$df$dose, x$df$response, model = "linear")$coefs %>% as.numeric()} )
      y2 <- lapply(y_tmp, function(z){linear(x,z[1], z[2])})
    }

  } else if (model == "Emax") {
    E0 <- input$parameter1
    ED50 <- input$parameter2
    Emax <- input$parameter3
    y <- emax(x, e0 = E0, ed50 = ED50, eMax = Emax)
    if (!is.null(data_esti)) {
      y_tmp <- lapply(data_esti[selectnr], function(x){fitMod(x$df$dose, x$df$response, model="emax", bnds = defBnds(max(x$df$dose))$emax)$coefs  %>% as.numeric()})
      y2 <- lapply(y_tmp,function(z){ emax(x,e0=z[1], ed50=z[3], eMax=z[2])})
    }
  } else if(model == "Sigmoidal Emax") {
    E0 <- input$parameter1
    ED50 <- input$parameter2
    Emax <- input$parameter3
    Hill <- input$parameter4

    y <- sigEmax(x, e0 = E0, ed50 = ED50, eMax = Emax, h = Hill)
    if(!is.null(data_esti) & !is.null(selectnr)){
      y_tmp <-lapply(data_esti[selectnr], function(x){ fitMod(x$df$dose,x$df$response, model="sigEmax", bnds=defBnds(max(x$df$dose))$sigEmax)$coefs %>% as.numeric() } )
      y2 <-lapply(y_tmp,function(z){sigEmax(x,e0 = z[1], ed50 = z[3], eMax = z[2], h = z[4])})
    }
  }

  datplot <- data.frame(x,y)

  if (!is.null(selectnr)) {
    data_dummy <- data
    sn <- selectnr
    data_dummy2 <- data_dummy$data[sn]
    data_dummy3 <- do.call(rbind.data.frame,lapply(data_dummy2, function(x) data.frame(do.call(cbind.data.frame,x),check.names = FALSE)))

    for(i in 1:length(y2)) {
     data_cols <- rgb(f_col(seq(0, 1, length = length(data$data))), maxColorValue = 255)
     y2[[i]] <- cbind.data.frame(y2[[i]],'color' = data_cols[selectnr[i]])
     y2[[i]] <- cbind.data.frame(y2[[i]],'nr'= selectnr[i])
     y2[[i]] <- cbind.data.frame(y2[[i]],'x'= datplot$x)
   }
   y3 <- do.call(rbind.data.frame, lapply(y2, function(x) data.frame(do.call(cbind.data.frame, x), check.names = FALSE)))
   colnames(y3) <- c("y","color","nr","x")
  }

  pl <- ggplot(data = datplot, aes(x = x, y = y, group = 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#383838")) +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_text(colour = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#474747"),
      panel.grid.minor = ggplot2::element_line(colour = "#474747", size = 0.25),
      text = ggplot2::element_text(
        family = "",
        face = "plain",
        colour = "white",
        size = 12,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      )
    ) +
    ggplot2::geom_line(color = dosedesignR_red, lwd = 1.2) +
    ggplot2::labs(x = "Dose", y = "Response", title = "Dose-response curve") +
    ggplot2::ylim(ymin, ymax) +
    {if(!is.null(data_esti) & plot_opt == TRUE & !is.null(selectnr)) geom_line(data = y3, aes(x = y3$x, y = y3$y, group = y3$nr), colour = y3$color, lwd = 1.2)} +
    ggplot2::xlim(dmin, dmax) +
    {if(!is.null(data) & !is.null(selectnr)) geom_point(data = data_dummy3, aes(x = data_dummy3$dose, y = data_dummy3$response),color =data_dummy3$color)} +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size=rel(1.1)),
      axis.title = element_text(size=rel(1.1)),
      plot.title = element_text(
        size = rel(1.25),
        hjust = 0.5,
        face = "bold"
      )
    )
  return(pl)
}


#' Second function to compute the model function (for markdown report)
#'
#' @param model
#' @param tab
#' @param ymin
#' @param ymax
#' @param dmin
#' @param dmax
#' @param data
#' @param data_esti
#' @param plot_opt
#' @param selectnr
#' @param parameter1
#' @param parameter2
#' @param parameter3
#' @param parameter4
#'
ComputeModelFunction2 <- function(
  model,
  tab,
  ymin,
  ymax,
  dmin,
  dmax,
  data_esti,
  data,
  plot_opt,
  selectnr,
  parameter1,
  parameter2,
  parameter3,
  parameter4
) {

  x <- seq(dmin,dmax, length = 201)

  if(model == "Linear") {
    Int <- parameter1
    Slp <- parameter2

    y <- linear(x, Int, Slp)

    if(!is.null(data_esti) & !is.null(selectnr)){

      y_tmp <- lapply(data_esti[selectnr], function(x){ fitMod(x$df$dose, x$df$response, model = "linear")$coefs %>% as.numeric()} )

      y2 <- lapply(y_tmp,function(z){ linear(x,z[1], z[2])})
    }

  } else if(model == "Emax") {
    E0 <- parameter1
    ED50 <- parameter2
    Emax <- parameter3
    y <- emax(x,e0 = E0, ed50 = ED50, eMax = Emax)
    if (!is.null(data_esti)) {

      y_tmp <- lapply(data_esti[selectnr], function(x){ fitMod(x$df$dose, x$df$response, model="emax", bnds = defBnds(max(x$df$dose))$emax)$coefs  %>% as.numeric()})
      y2 <- lapply(y_tmp,function(z){ emax(x,e0=z[1], ed50=z[3], eMax=z[2])})
    }
  } else if(model == "Sigmoidal Emax") {
    E0 <- parameter1
    ED50 <- parameter2
    Emax <- parameter3
    Hill <- parameter4

    y <- sigEmax(x, e0 = E0, ed50 = ED50, eMax = Emax, h = Hill)
    if(!is.null(data_esti) & !is.null(selectnr)){
      y_tmp <-lapply(data_esti[selectnr], function(x){ fitMod(x$df$dose,x$df$response, model="sigEmax", bnds=defBnds(max(x$df$dose))$sigEmax)$coefs %>% as.numeric() } )
      y2 <-lapply(y_tmp,function(z){sigEmax(x,e0 = z[1], ed50 = z[3], eMax = z[2], h = z[4])})
    }
  }

   datplot <- data.frame(x,y)

   if (!is.null(selectnr)) {
     data_dummy <- data
     sn <- selectnr
     data_dummy2 <- data_dummy$data[sn]
     data_dummy3 <- do.call(rbind.data.frame,lapply(data_dummy2, function(x) data.frame(do.call(cbind.data.frame,x),check.names = FALSE)))

      for(i in 1:length(y2)){
        data_cols <- rgb(f_col_report(seq(0, 1, length = length(data$data))), maxColorValue = 255)
        y2[[i]] <- cbind.data.frame(y2[[i]],'color' = data_cols[selectnr[i]])
        y2[[i]] <- cbind.data.frame(y2[[i]],'nr'= selectnr[i])
        y2[[i]] <- cbind.data.frame(y2[[i]],'x'= datplot$x)
      }
      y3 <- do.call(rbind.data.frame, lapply(y2, function(x) data.frame(do.call(cbind.data.frame, x), check.names = FALSE)))
      colnames(y3) <- c("y","color","nr","x")
   }

  pl <- ggplot(data=datplot,aes(x=x,y=y, group = 1) )+
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = element_rect(fill = "#f0f0f0")) +
    ggplot2::theme(
      legend.position = "none",
      axis.text = element_text(colour = "black"),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "#fafafa"),
      panel.grid.minor = element_line(colour = "#fafafa", size = 0.25),
      text = element_text(
        family = "",
        face = "plain",
        colour = "black",
        size = 12,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      )
    ) +
    geom_line(color = dosedesignR_red, lwd = 1.2) +
    labs(x = "Dose", y = "Response", title = "Dose-response curve") +
    ylim(ymin,ymax) +

    {if(!is.null(data_esti)) geom_line(data = y3, aes(x = y3$x, y = y3$y, group = y3$nr), colour = y3$color, lwd = 1.2)} +

    xlim(dmin, dmax) +

    {if(!is.null(data) & !is.null(selectnr)) geom_point(data = data_dummy3, aes(x = data_dummy3$dose, y = data_dummy3$response),color =data_dummy3$color_report)} +
    theme(axis.text = element_text(size=rel(1.1)),axis.title = element_text(size=rel(1.1)),plot.title = element_text(size=rel(1.25),hjust=0.5,face="bold"))
   return(pl)
}

