
dosedesignR_green <- "#5cb85c"
dosedesignR_blue <- "#3a6791"
dosedesignR_red <- "#c94053"
dosedesignR_white <- "#ebebeb"

performSimulation <- function(
  input,
  tab,
  inp,
  UserValue,
  sdsim,
  dmin,
  dmax,
  clevel,
  sim = input$sim,
  model= input$model
) {

  for(i in 1:length(inp)) {
    assign(paste0("inp", i), inp[[i]])
    assign(paste0("o_doses", i), inp[[i]]$optValdoses())
    assign(paste0("o_pats", i), inp[[i]]$optValpats())
    assign(paste0("u_doses", i), inp[[i]]$UserValdoses())
    assign(paste0("u_pats", i), inp[[i]]$UserValpats())
    assign(paste0("model"), inp[[tab]]$model())
  }

  sim <- input$sim

  if (!is.null(sim)) {

    nam1 <- rlang::sym(paste0(str_sub(tolower(sim), 1, 1), "_doses", str_sub(sim, str_length(sim), str_length(sim))))
    nam2 <- rlang::sym(paste0(str_sub(tolower(sim), 1, 1), "_pats", str_sub(sim, str_length(sim), str_length(sim))))

    simdesign <- data.frame(doses = eval(nam1), pats = eval(nam2))

    if (length(simdesign$doses) == 0) {

      pmodel <- ggplot2::ggplot(data = data.frame()) +
        ggplot2::geom_point() +
        ggplot2::xlim(-1,1) +
        gplot2::ylim(-1,1)  +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "#383838")
        ) +
        ggplot2::geom_text() +
        ggplot2::annotate(
          "text",
          y = 0,
          x = 0,
          label = "No user defined model found.",
          angle = 0,
          colour = "white"
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          text = ggplot2::element_text(
            family = "",
            face = "plain",
            colour = "white",
            size = 12,
            lineheight = 0.9,
            hjust = 0.5,
            vjust = 0.5,
            angle = 0,
            margin = ggplot2::margin(),
            debug = FALSE
          )
        )
    } else {
      if (model == "Linear") {
        Int <- inp[[tab]]$parameter1()
        Slp <- inp[[tab]]$parameter2()
        doses <- simdesign$doses
        pats <- floor(simdesign$pats)
        x <- rep(simdesign$doses, pats)
        y <- DoseFinding::linear(x, Int, Slp) + rnorm(sum(pats), 0, sdsim)
        datasim <- data.frame(x, y)
        model <- DoseFinding::fitMod(x, y, model = "linear")
        level <- clevel/100
        doseSeq <- seq(0, dmax, length = 201)
        truem <- DoseFinding::linear(doseSeq, Int, Slp)

        pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
        quant <- qt(1 - (1 - level)/2, df = model$df)
        lbnd <- pred$fit - quant * pred$se.fit
        ubnd <- pred$fit + quant * pred$se.fit
        predval <- pred$fit
        plotdata <- data.frame(doseSeq, predval, lbnd, ubnd, truem)
        ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
        ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))

        p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(plot.background = element_rect(fill = "#383838")) +
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
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lbnd, ymax = ubnd), fill = "#212121") +
          ggplot2::geom_line(ggplot2::aes(y = predval), colour= dosedesignR_green, lwd = 1.1) +
          ggplot2::geom_line(ggplot2::aes(x  = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) +
          ggplot2::xlim(dmin, dmax) +
          ggplot2::ylim(ylimlow, ylimhigh) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(size = rel(1.1)),
            axis.title = ggplot2::element_text(size = rel(1.1))
          )

        tcec <- c("true curve", "estimated curve")
        datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

        pmodel  <- p + ggplot2::geom_point(data = datasim, aes(x,y), colour = "grey70") +
          ggplot2::labs(x = "Dose", y = "Response") +
          ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y, group = 1), alpha = 0) +
          ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = rel(1.1)),
            legend.key =  ggplot2::element_rect(fill = "#383838"),
            legend.margin = margin(l = -10),
            legend.position = "bottom",
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_legend(override.aes = list(size = 1.05, alpha = 1))
          )
      } else if (model == "Emax") {
        E0 <- inp[[tab]]$parameter1()
        ED50 <- inp[[tab]]$parameter2()
        Emax <- inp[[tab]]$parameter3()
        doses <- simdesign$doses
        pats <- floor(simdesign$pats)
        x <- rep(simdesign$doses, pats)
        y <- DoseFinding::emax(x, E0, Emax, ED50) + rnorm(sum(pats), 0, sdsim)
        datasim <- data.frame(x,y)
        model <- DoseFinding::fitMod(x,y, model="emax", bnds = defBnds(max(doses))$emax)
        level <- clevel/100
        model <- fitMod(x, y, model = "emax", bnds = defBnds(max(simdesign$doses))$emax)
        doseSeq <- seq(0, dmax, length = 201)
        truem <- emax(doseSeq, E0, Emax, ED50)
        pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
        quant <- qt(1 - (1 - level)/2, df = model$df)
        lbnd <- pred$fit - quant * pred$se.fit
        ubnd <- pred$fit + quant * pred$se.fit
        predval <- pred$fit
        plotdata <- data.frame(doseSeq, predval, lbnd,ubnd, truem)

        ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
        ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))

        p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "#383838")
          ) +
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
          ggplot2::geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "#212121") +
          ggplot2::geom_line(aes(y = predval), colour = dosedesignR_green, lwd = 1.1) +
          ggplot2::geom_line(aes(x = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) + xlim(dmin, dmax) +
          ggplot2::ylim(ylimlow, ylimhigh) +
          ggplot2::theme(
            axis.text = element_text(size = rel(1.1)),
            axis.title = element_text(size = rel(1.1))
          )
        tcec<- c("true curve", "estimated curve")
        datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

        pmodel  <- p +
          ggplot2::geom_point(data = datasim, aes(x, y), colour = "grey70") +
          ggplot2::labs(x = "Dose", y = "Response") +
          ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y,group = 1), alpha = 0) +
          ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = rel(1.1)), legend.key =  element_rect(fill = "#383838"),
            legend.margin = margin(l = -10), legend.position = "bottom", legend.title = element_blank()
          ) +
          ggplot2::guides(colour = guide_legend(override.aes = list(size = 1.05, alpha = 1)))

    } else if (model == "Sigmoidal Emax") {
      E0 <- inp[[tab]]$parameter1()
      ED50 <- inp[[tab]]$parameter2()
      Emax <- inp[[tab]]$parameter3()
      Hill <- inp[[tab]]$parameter4()
      doses <- simdesign$doses
      pats <- floor(simdesign$pats)
      x <- rep(simdesign$doses, pats)
      y <- DoseFinding::sigEmax(x, E0, Emax, ED50, Hill) + rnorm(sum(pats), 0, sdsim)
      datasim <- data.frame(x, y)
      model <- DoseFinding::fitMod(x, y, model = "sigEmax", bnds = defBnds(max(simdesign$doses))$sigEmax)
      level <- clevel/100
      doseSeq <- seq(0,  dmax, length = 201)
      truem <- DoseFinding::sigEmax(doseSeq, E0, Emax, ED50, Hill)
      pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
      quant <- qt(1 - (1 - level)/2, df = model$df)
      lbnd <- pred$fit - quant * pred$se.fit
      ubnd <- pred$fit + quant * pred$se.fit
      predval <- pred$fit
      plotdata <- data.frame(doseSeq, predval, lbnd, ubnd, truem)
      ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
      ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))

      p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.background = element_rect(fill = "#383838")) +
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
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lbnd, ymax = ubnd), fill = "#212121") +
        ggplot2::geom_line(ggplot2::aes(y = predval), colour = dosedesignR_green, lwd = 1.1) +
        ggplot2::geom_line(ggplot2::aes(x = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) + xlim(dmin, dmax) +
        ggplot2::ylim(ylimlow, ylimhigh) +
        ggplot2::theme(
          axis.text = element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1.1))
        )

      tcec <- c("true curve", "estimated curve")
      datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

      pmodel  <- p +
        ggplot2::geom_point(data = datasim, aes(x,y), colour = "grey70") +
        ggplot2::labs(x = "Dose", y = "Response") +
        ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y, group = 1), alpha = 0) +
        ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
        ggplot2::theme(
          legend.text = ggplot2::element_text(size = rel(1.1)),
          legend.key =  ggplot2::element_rect(fill = "#383838"),
          legend.margin = margin(l = -10),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::guides(colour = guide_legend(override.aes = list(size = 1.05, alpha = 1)))

      }
    }
    return(pmodel)
  }
}


#for rmarkdown report:
performSimulation2 <- function(
  tab,
  inp,
  sdsim,
  dmin,
  dmax,
  clevel,
  sim = input$sim,
  model = input$model
) {

  for(i in 1:length(inp)){
    assign(paste0("inp", i), inp[[i]])
    assign(paste0("o_doses", i), inp[[i]]$optValdoses())
    assign(paste0("o_pats", i), inp[[i]]$optValpats())
    assign(paste0("u_doses", i), inp[[i]]$UserValdoses())
    assign(paste0("u_pats", i), inp[[i]]$UserValpats())
    assign(paste0("model"), inp[[tab]]$model())
  }


  if (!is.null(sim)) {
    nam1 <- rlang::sym(paste0(str_sub(tolower(sim), 1, 1), "_doses", str_sub(sim, str_length(sim), str_length(sim))))
    nam2 <- rlang::sym(paste0(str_sub(tolower(sim), 1, 1), "_pats", str_sub(sim, str_length(sim), str_length(sim))))

    simdesign <- data.frame(doses = eval(nam1), pats = eval(nam2))

    if (length(simdesign$doses) == 0) {
      stop("No user defined model found.")
    }

    if (model == "Linear") {
      Int <- inp[[tab]]$parameter1()
      Slp <- inp[[tab]]$parameter2()
      doses <- simdesign$doses
      pats <- floor(simdesign$pats)
      x <- rep(simdesign$doses, pats)
      y <- DoseFinding::linear(x, Int, Slp) + rnorm(sum(pats), 0, sdsim)
      datasim <- data.frame(x, y)
      model <- DoseFinding::fitMod(x, y, model = "linear")
      level <- clevel/100
      doseSeq <- seq(0, dmax, length = 201)
      truem <- DoseFinding::linear(doseSeq, Int, Slp)

      pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
      quant <- qt(1 - (1 - level)/2, df = model$df)
      lbnd <- pred$fit - quant * pred$se.fit
      ubnd <- pred$fit + quant * pred$se.fit
      predval <- pred$fit
      plotdata <- data.frame(doseSeq, predval, lbnd, ubnd, truem)
      ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
      ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))

      p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.background = element_rect(fill = "#f0f0f0")) +
        ggplot2::theme(
          legend.position = "none",
          axis.text = ggplot2::element_text(colour = "black"),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = "#fafafa"),
          panel.grid.minor = ggplot2::element_line(colour = "#fafafa", size = 0.25),
          text = ggplot2::element_text(
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
        ggplot2::geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
        ggplot2::geom_line(aes(y = predval), colour= dosedesignR_green, lwd = 1.1) +
        ggplot2::geom_line(aes(x  = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) + xlim(dmin, dmax) +
        ggplot2::ylim(ylimlow, ylimhigh) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1.1))
        )

      tcec <- c("true curve", "estimated curve")
      datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

      pmodel  <- p +
        ggplot2::geom_point(data = datasim, aes(x,y), colour = "#212121") +
        ggplot2::labs(x = "Dose", y = "Response") +
        ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y, group = 1), alpha = 0) +
        ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
        ggplot2::theme(
          legend.text = ggplot2::element_text(size = rel(1.1)),
          legend.key =  ggplot2::element_rect(fill = "#f0f0f0"),
          legend.margin = margin(l = -10),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::guides(colour = guide_legend(override.aes = list(size = 1.05, alpha = 1)))
    } else if (model == "Emax") {
      E0 <- inp[[tab]]$parameter1()
      ED50 <- inp[[tab]]$parameter2()
      Emax <- inp[[tab]]$parameter3()
      doses <- simdesign$doses
      pats <- floor(simdesign$pats)
      x <- rep(simdesign$doses, pats)
      y <- DoseFinding::emax(x, E0, Emax, ED50) + rnorm(sum(pats), 0, sdsim)
      datasim <- data.frame(x,y)
      model <- DoseFinding::fitMod(x,y, model="emax", bnds = defBnds(max(doses))$emax)
      level <- clevel/100
      model <- fitMod(x, y, model = "emax", bnds = defBnds(max(simdesign$doses))$emax)
      doseSeq <- seq(0, dmax, length = 201)
      truem <- emax(doseSeq, E0, Emax, ED50)
      pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
      quant <- qt(1 - (1 - level)/2, df = model$df)
      lbnd <- pred$fit - quant * pred$se.fit
      ubnd <- pred$fit + quant * pred$se.fit
      predval <- pred$fit
      plotdata <- data.frame(doseSeq, predval, lbnd,ubnd, truem)

      ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
      ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))

      p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.background = element_rect(fill = "#f0f0f0")) +
        ggplot2::theme(
          legend.position = "none",
          axis.text = ggplot2::element_text(colour = "black"),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = "#fafafa"),
          panel.grid.minor = ggplot2::element_line(colour = "#fafafa", size = 0.25),
          text = ggplot2::element_text(
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
        ggplot2::geom_ribbon(aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
        ggplot2::geom_line(aes(y = predval), colour = dosedesignR_green, lwd = 1.1) +
        ggplot2::geom_line(aes(x = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) + xlim(dmin, dmax) +
        ggplot2::ylim(ylimlow, ylimhigh) +
        ggplot2::theme(
          axis.text = element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1.1))
        )

      tcec <- c("true curve", "estimated curve")
      datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

      pmodel  <- p +
        ggplot2::geom_point(data = datasim, aes(x, y), colour = "#212121") +
        ggplot2::labs(x = "Dose", y = "Response") +
        ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y,group = 1), alpha = 0) +
        ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
        ggplot2::theme(
          legend.text = ggplot2::element_text(size = rel(1.1)),
          legend.key =  ggplot2::element_rect(fill = "#f0f0f0"),
          legend.margin = margin(l = -10),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()) +
        ggplot2::guides(colour = guide_legend(override.aes = list(size = 1.05, alpha = 1)))

    } else if (model == "Sigmoidal Emax") {
      E0 <- inp[[tab]]$parameter1()
      ED50 <- inp[[tab]]$parameter2()
      Emax <- inp[[tab]]$parameter3()
      Hill <- inp[[tab]]$parameter4()
      doses <- simdesign$doses
      pats <- floor(simdesign$pats)
      x <- rep(simdesign$doses, pats)
      y <- DoseFinding::sigEmax(x, E0, Emax, ED50, Hill) + rnorm(sum(pats), 0, sdsim)
      datasim <- data.frame(x, y)
      model <- DoseFinding::fitMod(x, y, model = "sigEmax", bnds = defBnds(max(simdesign$doses))$sigEmax)
      level <- clevel/100
      doseSeq <- seq(0,  dmax, length = 201)
      truem <- DoseFinding::sigEmax(doseSeq, E0, Emax, ED50, Hill)
      pred <- predict(model, predType = "ls-means", doseSeq = doseSeq, se.fit = TRUE)
      quant <- qt(1 - (1 - level)/2, df = model$df)
      lbnd <- pred$fit - quant * pred$se.fit
      ubnd <- pred$fit + quant * pred$se.fit
      predval <- pred$fit
      plotdata <- data.frame(doseSeq, predval, lbnd, ubnd, truem)
      ylimlow <- min(c(truem[length(truem)] - 3 * sdsim, truem[1] - 3 * sdsim))
      ylimhigh <- max(c(truem[1] + 3 * sdsim, truem[length(truem)] + 3 * sdsim))
      p <- ggplot2::ggplot(plotdata, aes(x = doseSeq, group = 1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.background = element_rect(fill = "#f0f0f0")) +
        ggplot2::theme(
          legend.position = "none",
          axis.text = ggplot2::element_text(colour = "black"),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = "#fafafa"),
          panel.grid.minor = ggplot2::element_line(colour = "#fafafa", size = 0.25),
          text = ggplot2::element_text(
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
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lbnd, ymax = ubnd), fill = "grey70") +
        ggplot2::geom_line(ggplot2::aes(y = predval), colour = dosedesignR_green, lwd = 1.1) +
        ggplot2::geom_line(ggplot2::aes(x = doseSeq, y = truem), colour = dosedesignR_red, lwd = 1.1) + xlim(dmin, dmax) +
        ggplot2::ylim(ylimlow, ylimhigh) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = rel(1.1)),
          axis.title = ggplot2::element_text(size = rel(1.1))
        )

      tcec <- c("true curve", "estimated curve")
      datleg <-  data.frame(tcec, test = 1:2, y = 1:2)

      pmodel  <- p +
        ggplot2::geom_point(data = datasim, aes(x,y), colour = "#212121") +
        ggplot2::labs(x = "Dose", y = "Response") +
        ggplot2::geom_line(data = datleg, aes(colour = tcec, x = test, y = y, group = 1), alpha = 0) +
        ggplot2::scale_colour_manual(values = c(dosedesignR_red, dosedesignR_green), labels = c("true curve", "estimated curve")) +
        ggplot2::theme(
          legend.text = ggplot2::element_text(size = rel(1.1)),
          legend.key =  ggplot2::element_rect(fill = "#f0f0f0"),
          legend.margin = margin(l = -10), legend.position = "bottom", legend.title = ggplot2::element_blank()
        ) +
        ggplot2::guides(colour = guide_legend(override.aes = list(size = 1.05, alpha = 1)))

    }
    return(pmodel)
  }
}
