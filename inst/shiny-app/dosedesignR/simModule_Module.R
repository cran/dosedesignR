#' Simulation module ui function
#'
#' @description A shiny Module for the simulation tab in dosedesignR.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

dosedesignR_green <- "#5cb85c"
dosedesignR_blue <- "#3a6791"
dosedesignR_red <- "#c94053"
dosedesignR_bright_grey <- "#424242"
dosedesignR_mid_grey <- "#383838"
dosedesignR_dark_grey <- "#333333"

simModule_UI <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabPanel(
      "Simulation",
      shiny::column(12,
        style = paste0("background-color:#383838;"),
        shiny::uiOutput(ns("sim")),
        shiny::actionButton(ns("gosim"), "Go"),
        shiny::plotOutput(ns("simDesign"))
      )
    )
  )
}

#' Simulation module server function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param number number of candidate model
#' @param no tab number
#' @param inp inputs from doseDesign module
#' @param levels level information (from app)
#' @param regimen regimen information (from app)
#' @param npats number of patients (from app)
#' @param dmin dose minimum (from app)
#' @param dmax dose maximum (from app)
#' @param ymin y-axis minimum (from app)
#' @param ymax y-axis maximum (from app)
#' @param sdsim simulation options (from app)
#' @param clevel level information (from app)
#' @noRd


simModule_Server <- function(
  input,
  output,
  session,
  number,
  no,
  inp,
  levels,
  regimen,
  npats,
  dmin,
  dmax,
  ymin,
  ymax,
  sdsim,
  clevel
) {
  output$sim <- shiny::renderUI({
    shiny::selectInput(
      session$ns("sim"),
      NULL,
      c(paste0("Optimal", 1:length(inp())), paste0("User Defined", 1:length(inp()))),
      selected = paste0("Optimal", no)
    )
  })

  output$simDesign <- shiny::renderPlot({
    shiny::validate(need(ymin() < ymax() , 'Maximum response needs to be greater than minimum response.'))
    input$gosim
    # use the function 'performSimulation' from the Simulation.R file
    plotres <- performSimulation(
      input,
      no,
      inp = inp(),
      sdsim = sdsim(),
      dmin = dmin(),
      dmax = dmax(),
      clevel = clevel(),
      sim = inp()$sim,
      model = inp()$model
    )
    if (!is.null(plotres)) {
      suppressWarnings(print(plotres))
    }
  })
}
