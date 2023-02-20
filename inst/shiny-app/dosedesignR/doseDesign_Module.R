#' doseDesign module ui function
#'
#' @description A shiny Module for the doseDesign-tab in dosedesignR.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

doseDesignUI <- function(id) {
  dosedesignR_green <- "#5cb85c"
  dosedesignR_blue <- "#3a6791"
  dosedesignR_red <- "#c94053"
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::column(4,
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(title = paste0("Candidate Model ", as.character(id)),
         shiny::selectInput(
           inputId = ns("model"),
           label = NULL,
           choices = c(
             "Linear", "Emax", "Sigmoidal Emax"
            ),
            selected = rep(
              c(
                "Linear", "Emax", "Sigmoidal Emax"
              ), as.numeric(id)
            )[as.numeric(id)]
          ),
          shiny::textOutput(ns("model")),
          shiny::column(4,
            shinyWidgets::materialSwitch(
              ns("parambox"),
              HTML('<p style="color:white"> Show Parameter </p>'),
              value = TRUE,
              status = "success"
            )
          ),
          shiny::column(4,
            shinyWidgets::materialSwitch(
              ns("paramesti"),
              HTML('<p style="color:white"> Show Estimates </p>'),
              value = TRUE,
              status = "success"
            )
          ),
          shiny::column(4,
            shinyWidgets::materialSwitch(
              ns("plot_opt"),
              HTML(
                '<p style="color:white"> Show Optimal Curve </p>'
              ),
              value = TRUE,
              status = "success"
            )
          ),
          shiny::column(10,
            shiny::tableOutput(ns("showEstimates")),
            shiny::tags$head(
              shiny::tags$style(
                "#ShowEstimates td{position:relative;};"
              )
            )
          ),
          shiny::column(12,
            shiny::column(6,
              shiny::numericInput(ns("parameter1"),
              label = "",
              value = rep(0,100)[as.numeric(id)])
            ),
            shiny::column(6,
              shiny::numericInput(ns("parameter2"),
              label = "",
              value = rep(1,100)[as.numeric(id)])
            )
          ),
          shiny::column(12,
            shiny::column(6,
              shiny::numericInput(ns("parameter3"),
              label = "",
              value = rep(0.5,100)[as.numeric(id)])
            ),
            shiny::column(6,
              shiny::numericInput(ns("parameter4"),
              label = "",
              value = rep(5,100)[as.numeric(id)])
            )
          ),
          shiny::column(12,
            shiny::actionButton(
              ns("go_stand"),
              "Standardize",
              icon = icon("chart-line"),
              style = paste0("color: #fff; background-color: ", dosedesignR_blue, "; border-color: #2e6da4")
            ),
            shiny::actionButton(
              ns("undoButton"),
              "Undo Standardize!",
              icon = icon("undo"),
              style = paste0("color: #fff; background-color: ", dosedesignR_blue, "; border-color: #2e6da4")
            )
          ),
          shiny::tabsetPanel(type = "tabs",
            shiny::tabPanel("Plot",
              shiny::column(12, style = "background-color:#383838;",
              shiny::plotOutput(ns("modelFunction"), width = "100%")
              )
            ),
            shiny::tabPanel("Simulation",
              simModule_UI(paste0("s",id))
            )
          ),
          shiny::column(12,
            style = "background-color:#383838;",
            shiny::plotOutput(ns("optDesign"))
          ),
          shiny::column(12, align = "center",
            shiny::wellPanel(style = "background: #333333; border: black",
              shiny::tableOutput(ns("viewOpt"))
            )
          ),
          shiny::column(12,
            # call module from file 'doseLevel_Module.R' to enter dose
            # groups and dose levels
            doseLevelUI(ns("oM1"))
          ),
          shiny::column(12, align = "center",
            shiny::wellPanel(style = "background: #333333; border: black",
            shiny::tableOutput(ns("viewEff")))
          )
        ), id = paste0("Tab", as.character(id)),
        # let only the first 2 tabs be open at the start
        open = paste0("Candidate Model ", as.character(id))
     )
    )
  )
}

#' doseDesign module server function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param nr panel number
#' @param levels level information (from app)
#' @param regimen regimen information (from app)
#' @param npats number of patients (from app)
#' @param dmin dose minimum (from app)
#' @param dmax dose maximum (from app)
#' @param ymin y-axis minimum (from app)
#' @param ymax y-axis maximum (from app)
#' @param sdsim simulation options (from app)
#' @param clevel level information (from app)
#' @param data data set (from app)
#' @param data2 second data set
#' @param selectnr Which data set (if multiple in .rds file) are selected
#' @param button save.button click
#' @param nrPanel total number of panels
#' @noRd
doseDesign <- function(
  input,
  output,
  session,
  nr,  # numeric value: panel number
  levels,
  regimen,
  npats,
  dmin,  # dose minimum
  dmax,  # dose maximum
  ymin,  # y axis minimum
  ymax,  # y axis maximum
  sdsim, # simulation options
  clevel,# level inforation
  data,  # df()
  data2, # da()
  selectnr, # which data sets (if multiple in .rds file) are selected
  button,   # save.button click
  nrPanel # total number of panels
  ){

  #condition that the switchInputs are only shown when a data set is loaded data2()
  shiny::observeEvent(c(data2(), input$paramesti), {
    if (!is.null(data2())) {
      shinyjs::show("paramesti")
      if (input$paramesti) {
      shinyjs::show("showEstimates")
      } else {
        shinyjs::hide("showEstimates")
      }
      shinyjs::show("plot_opt")
    } else {
      shinyjs::hide("paramesti")
      shinyjs::hide("plot_opt")
    }
  })

  # hide the parameters if switchInput parambox is false and
  # limit the number of parameter shown for the selected model if parambox is true
  shiny::observeEvent(c(input$model,input$parambox),  {
    if (input$parambox) {
      shinyjs::show("parameter1")
      shinyjs::show("parameter2")
      if (input$model %in% c('Linear', 'Linear in log')) {
        shinyjs::hide("parameter3")
        shinyjs::hide("parameter4")
      } else if (input$model %in% c('Emax')) {
        shinyjs::show("parameter3")
        shinyjs::hide("parameter4")
      } else {
        shinyjs::show("parameter3")
        shinyjs::show("parameter4")
      }
    } else {
      shinyjs::hide("parameter1")
      shinyjs::hide("parameter2")
      shinyjs::hide("parameter3")
      shinyjs::hide("parameter4")
    }
  })

  # call module from file 'doseLevel_Module.R' to enter dose
  # groups and dose levels
  oM <- shiny::callModule(doseLevel, "oM1")

  optValue <- shiny::reactiveValues()
  UserValue <- shiny::reactiveValues()

  # create reactive value start$dat which is set to TRUE if a data set was read in
  start <- shiny::reactiveValues(dat = FALSE)
    shiny::observeEvent(data2(), {
    if (!is.null(data2())) {
      start$dat <- TRUE
    }
  })

  output$flag <- shiny::reactive(start$dat)
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  output$optDesign <- shiny::renderPlot({
    # Make sure that enough dose levels are supplied
    tmp1 <- as.numeric(strsplit(levels(), ";")[[1]])

    if (regimen() == "Specify dose levels" & length(tmp1) > 0 & input$model == "Linear" )
      shiny::validate(need(length(tmp1) > 1, 'Need at least 2 dose levels'))
    if (regimen() == "Specify dose levels" & length(tmp1) > 0 & input$model == "Emax" ) {
      shiny::validate(need(length(tmp1) > 2, 'Need at least 3 dose levels'))
    }
    if (regimen() == "Specify dose levels" & length(tmp1) > 0 & input$model == "Sigmoidal Emax" )
      shiny::validate(need(length(tmp1) > 3, 'Need at least 4 dose levels'))

    # error messages for some parameters if parameters are negative
    if (input$model == "Emax" ) {
      shiny::validate(need(input$parameter2 > 0, 'For model Emax entries needs to be positive. Please select a value >0 for parameter ED50'))
    }
    if (input$model == "Sigmoidal Emax" ) {
      shiny::validate(need(input$parameter2 > 0, 'For model Emax Sigmoidal entries needs to be positive. Please select a value >0 for parameter ED50'))
      shiny::validate(need(input$parameter4 > 0, 'For model Emax Sigmoidal entries needs to be positive. Please select a value >0 for parameter Hill'))
    }

    # use function 'ComputeOptimal' from Optimization.R file
    x <- ComputeOptimal(input, levels = levels() , regimen = regimen(), dmin = dmin(), dmax = dmax())
    optValue$doses1 <- x$res[1,]
    optValue$distr1 <- x$res[2,]
    optValue$pats1  <- round(x$res[2, ] * npats(), 1)
    optValue$mods1 <- x$mods
    optValue$optD1 <- x$optD

    da <- data.frame(optValue$doses1, optValue$pats1)
    op <- c("optimal design")

    pl1 <- ggplot2::ggplot(
        data = da, aes(x = optValue$doses1, y = optValue$pats1, group = 1)
      ) +
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
      ggplot2::geom_linerange(
        ymin = 0,
        ymax = optValue$pats1,
        color = dosedesignR_red,
        lwd = 1.2
      ) +
      ggplot2::labs(x = "Dose", y = "Patients") +
      ggplot2::ylim(0, npats() * 0.7) +
      ggplot2::xlim(dmin(), dmax()) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = rel(1.1)),
        axis.title = ggplot2::element_text(size = rel(1.1))
      ) +
      ggplot2::scale_colour_manual(values = c("white")) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = rel(1.1)),
        legend.key = ggplot2::element_rect(size = 5),
        legend.margin = margin(l = -10),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::guides(
        colour = guide_legend(override.aes = list(size = 1, alpha = 1, linetype = 1))
      )

    UserValue$doses1 <- oM()$dose
    UserValue$pats1 <- oM()$patients

    if (length(UserValue$doses1) == length(UserValue$pats1) && length(UserValue$pats1) > 0) {
      op <- c("optimal design", "user defined design")
      plotdat2 <- data.frame(doses = UserValue$doses1, pats = UserValue$pats1)

      pl1 <- ggplot2::ggplot(
          data = da,
          ggplot2::aes(
            x = optValue$doses1,
            y = optValue$pats1,
            group = 1
          )
        ) +
        ggplot2::geom_linerange(
          ymin = 0,
          ymax = optValue$pats1,
          color = dosedesignR_red,
          lwd = 1.2
        ) +
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
        ggplot2::labs(x = "Dose", y = "Patients") +
        ggplot2::ylim(0, npats() * 0.7) +
        ggplot2::theme(
          axis.text = element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1.1))
        )

      pl1 <- pl1 +
        ggplot2::geom_point(
          data = plotdat2,
          ggplot2::aes(x = plotdat2$doses, y = plotdat2$pats, group = 1),
          colour = dosedesignR_green,
          size = rel(2)
        ) +
        ggplot2::xlim(
          dmin(), dmax()
        ) +
        ggplot2::scale_colour_manual(
          values = c("black", "#89D329")) +
          ggplot2::theme(
            legend.text = element_text(size = rel(1.1)),
            legend.key = element_rect(size = 5),
            legend.margin = margin(l = -10),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ggplot2::guides(
            colour = ggplot2::guide_legend(
              override.aes = list(
                size = c(1, 2.5),
                alpha = 1,
                shape = c(NA, 16),
                linetype = c(1, 0)
              )
            )
          )
    }
    suppressWarnings(print(pl1))
  })

  if (!is.null(data2())) {
    output$showEstimates <- shiny::renderTable({

      data2 <- data2()[selectnr()]
      if (input$model == "Linear") {
        tmp <- lapply(data2, function(x) {
          as.data.frame(t(DoseFinding::fitMod(x$df$dose, x$df$response, model = "linear")$coefs))
        })
        tmp2 <- lapply(tmp, function(x) {
          data.frame("Intercept" = c(input$parameter1, round(as.numeric(x[1]), 2)), "Slope" = c(input$parameter2, round(as.numeric(x[2]), 2)))})
      }
      if (input$model == "Emax") {
        tmp <- lapply(data2, function(x) {
          as.data.frame(t(DoseFinding::fitMod(x$df$dose, x$df$response, model = "emax", bnds = defBnds(max(x$df$dose))$emax)$coefs))
         })
        tmp2 <- lapply(tmp,function(x) {
          data.frame("E0" = c(input$parameter1, round(as.numeric(x[1]), 2)),
                     "ED50" = c(input$parameter2, round(as.numeric(x[3]), 2)),
                     "Emax" = c(input$parameter3, round(as.numeric(x[2]), 2)))
          })
      }
      if (input$model == "Sigmoidal Emax") {
        tmp <- lapply(data2, function(x){
          as.data.frame(t(DoseFinding::fitMod(x$df$dose, x$df$response, model = "sigEmax", bnds = defBnds(max(x$df$dose))$sigEmax)$coefs))
          })
        tmp2 <- lapply(tmp, function(x) {
          data.frame(
            "E0" = c(input$parameter1, round(as.numeric(x[1]), 2)),
            "ED50" = c(input$parameter2, round(as.numeric(x[3]), 2)),
            "Emax" = c(input$parameter3,round(as.numeric(x[2]), 2)),
            "Hill" = c(input$parameter4, round(as.numeric(x[4]), 2)))
        })
      }

      if (length(tmp2) > 0) {
        tmp2 <- rbind.data.frame(tmp2[[1]][1, ], do.call(rbind.data.frame, lapply(tmp2, function(x) data.frame(do.call(cbind.data.frame, x[2, ]), check.names = FALSE))))

        tmp2[1, ] <- paste0("<div style='width: 100%; height: 100%; z-index: 0;color: ", dosedesignR_red,";  '><span> ", tmp2[1, ], "</span></div>")
        for(i in 2:(dim(tmp2)[1])){
            tmp2[i, ] <- paste0("<div style='width: 100%; height: 100%; z-index: 0;color: ", data2()[[selectnr()[i - 1]]]$df$color[1], "; '><span> ", tmp2[i, ], "</span></div>")
        }

        tmp2

      }

    }, sanitize.text.function = function(x) x
  )}

  # Plot for the Plot Tab
  output$modelFunction <- shiny::renderPlot({
    shiny::validate(need(ymin() < ymax() , 'Maximum response needs to be greater than minimum response.'))

    # use the 'ComputeModelFunction' function from the Visualize.R file
    plotres <- ComputeModelFunction(
      input = input,
      tab = 1,
      ymin = ymin(),
      ymax = ymax(),
      dmin = dmin(),
      dmax = dmax(),
      data = data(),
      data_esti = data2(),
      plot_opt = input$plot_opt,
      selectnr = selectnr()
    )
    suppressWarnings(print(plotres))
  })

  # Table of dose levels
  output$viewOpt <- shiny::renderTable({
    dat <- matrix(c(optValue$doses1, round(optValue$pats1, 1)), nrow = 2, byrow = T)
    colnames(dat) <- NULL
    rownames(dat) <- c("Dose", "Patients")
    dat
  }, digits = 1, align = NULL, rownames = TRUE, colnames = FALSE)


  output$viewEff <- shiny::renderTable({
    # use the 'ComputeEfficiency' function from the Optimization.R file
    dat <- ComputeEfficiency(UserValue, optValue, input, 1, npats = npats())

    if (dim(dat)[1] == 3 & dim(dat)[2] == 1) {
    colnames(dat) <- NULL
    rownames(dat) <- c("Patients", "Efficiency [%]", "Additional patients needed")
    } else if (dim(dat)[1] == 1 & dim(dat)[2] == 1) {
      colnames(dat) <- NULL
      rownames(dat) <- c("Information:")
    }
    dat
  }, digits = 1, align = NULL, rownames = TRUE, colnames = FALSE)

  #  update the labels of the numeric input's depending on the selected model
  shiny::observeEvent(input$model, {
    if (input$model == "Linear") {
      shiny::updateNumericInput(session, "parameter1", label = "Intercept")
      shiny::updateNumericInput(session, "parameter2", label = "Slope")
    } else if (input$model == "Emax") {
      shiny::updateNumericInput(session, "parameter1", label = "E0")
      shiny::updateNumericInput(session, "parameter2", label = "ED50")
      shiny::updateNumericInput(session, "parameter3", label = "Emax")
    } else if (input$model == "Sigmoidal Emax") {
      shiny::updateNumericInput(session, "parameter1", label = "E0")
      shiny::updateNumericInput(session, "parameter2", label = "ED50")
      shiny::updateNumericInput(session, "parameter3", label = "Emax")
      shiny::updateNumericInput(session, "parameter4", label = "Hill")
    }
  })

  #### create undo button for the standardize button ####
  undo <- shiny::eventReactive(input$go_stand, {
    tmp <- c(
      shiny::isolate(input$parameter1),
      shiny::isolate(input$parameter2),
      shiny::isolate(input$parameter3),
      shiny::isolate(input$parameter4)
    )
    tmp
  })


  shiny::observeEvent(input$undoButton, {
    shiny::req(undo())
    tmp <- undo()
    shiny::updateNumericInput(session, "parameter1", value = tmp[1])
    shiny::updateNumericInput(session, "parameter2", value = tmp[2])
    shiny::updateNumericInput(session, "parameter3", value = tmp[3])
    shiny::updateNumericInput(session, "parameter4", value = tmp[4])
  })
  # Standardize Button
  shiny::observeEvent(input$go_stand, {
    req(undo())
    if (!is.null(input$model)) {

      if (input$model == "Linear") {
        if (!is.null(optValue$doses1)) {
          tmp <- DoseFinding::Mods(
            linear = NULL,
            maxEff = ymax(),
            doses = optValue$doses1,
            placEff = 0
          )
          shiny::updateNumericInput(session, "parameter2", value = as.numeric(tmp$linear[2]))
        }
      }

      if (input$model == "Emax") {
        if (!is.null(optValue$doses1)) {
          tmp <- DoseFinding::Mods(
            emax = input$parameter2,
            maxEff = ymax(),
            doses = optValue$doses1,
            placEff = 0
          )
          shiny::updateNumericInput(session,"parameter3", value = as.numeric(tmp$emax[2]))
        }
      }

      if (input$model == "Sigmoidal Emax") {
        if (!is.null(optValue$doses1)) {
          tmp <- DoseFinding::Mods(
            sigEmax = c(input$parameter2, input$parameter4),
            maxEff = ymax(),
            doses = optValue$doses1,
            placEff = 0
          )
          shiny::updateNumericInput(session, "parameter3", value = as.numeric(tmp$sigEmax[2]))
        }
      }
    }
  })

  # reactive object 'settings' to save all parameters
  # could be reduced without the model distinctive
  settings <- shiny::reactive({
    nrPanel()
    button()
    shiny::req(shiny::isolate(input$model), nrPanel())
    list(
      'model' = input$model,
      'parameter' = c(
        shiny::isolate(input$parameter1),
        shiny::isolate(input$parameter2),
        shiny::isolate(input$parameter3),
        shiny::isolate(input$parameter4)
      ),
      'levels' = oM(),
      'showParameter' = input$parambox
    )
  })

  return(
    list(
      parameter1 = shiny::reactive({input$parameter1}),
      parameter2 = shiny::reactive({input$parameter2}),
      parameter3 = shiny::reactive({input$parameter3}),
      parameter4 = shiny::reactive({input$parameter4}),
      send_design_button = shiny::reactive({input$send_design}),
      optValdoses = shiny::reactive({optValue$doses1}),
      optValpats = shiny::reactive({optValue$pats1}),
      UserValdoses = shiny::reactive({UserValue$doses1}),
      UserValpats = shiny::reactive({UserValue$pats1}),
      model  = shiny::reactive({input$model}),
      setting = shiny::reactive({settings()})
    )
  )
}
