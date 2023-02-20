#' Inner doseLevel module ui function
#'
#' @description A shiny Module for the doseLevel in dosedesignR.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

doseLevel_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(3,
      shiny::textInput(
        inputId = ns("level"),
        label = paste0("Level ", strsplit(id,"tmp")[[1]][[2]])
      ),
      shiny::textInput(
        inputId = ns("patients"),
        label = paste0("Group ", strsplit(id,"tmp")[[1]][[2]])
      )
    )
  )
}

#' Inner doseLevel module server function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd

doseLevel_server <- function(input, output, session) {
  dosedesignR_green <- "#5cb85c"
  dosedesignR_blue <- "#3a6791"
  dosedesignR_red <- "#c94053"
  # observer to create 'green' or '", dosedesignR_red,"' borders around the textinputs for level and patient values
  shiny::observe({
    if (!is.null(input$level)) {
      if (input$level == "" & input$patients == "" ) {

      } else if (input$level != "" & input$patients =="" ) {
        shinyjs::runjs(paste0("document.getElementById('", session$ns("level"), "').style.border ='solid ", dosedesignR_green,"'"))
        shinyjs::runjs(paste0("document.getElementById('", session$ns("patients"), "').style.border ='solid ", dosedesignR_red,"'"))
      } else if (input$level =="" & input$patients !="" ) {
        shinyjs::runjs(paste0("document.getElementById('", session$ns("level"), "').style.border ='solid ", dosedesignR_red,"'"))
        shinyjs::runjs(paste0("document.getElementById('", session$ns("patients"), "').style.border ='solid ", dosedesignR_green,"'"))
      } else if (input$level !="" & input$patients !="" ) {
        shinyjs::runjs(paste0("document.getElementById('", session$ns("level"), "').style.border ='solid ", dosedesignR_green,"'"))
        shinyjs::runjs(paste0("document.getElementById('", session$ns("patients"), "').style.border ='solid ", dosedesignR_green,"'"))
      }
    }
  })

  return(
    list(
      level = input$level,
      patients = input$patients
    )
  )
}
