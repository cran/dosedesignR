#' doseLevel module ui function
#'
#' @description A shiny Module for the doseLevels in dosedesignR.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

doseLevelUI <- function(id){
  dosedesignR_green <- "#5cb85c"
  dosedesignR_blue <- "#3a6791"
  dosedesignR_red <- "#c94053"
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shinyjs::useShinyjs(),
      shiny::uiOutput(ns("doseUI")),     #group 1-4
      shiny::uiOutput(ns("doseUI2")),    #group 5-8
      shiny::uiOutput(ns("doseUI3")),    #group 9-12
      shiny::uiOutput(ns("doseUI4")),    #group 13-16
      shiny::uiOutput(ns("doseUI5")),    #group 17-20
      shiny::actionButton(
        inputId = ns("add"),
        label = "Add 4 groups",
        icon = icon("plus-circle"),
        style = paste0("color: #fff; background-color: ", dosedesignR_blue, "; border-color: #2e6da4")
      ),
      shiny::actionButton(
        inputId = ns("remove"),
        label = "Reduce 4 groups",
        icon = icon("minus-circle"),
        style = paste0("color: #fff; background-color: ", dosedesignR_blue, "; border-color: #2e6da4")
      )
    )
  )
}


#' doseLevel module server function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd

doseLevel <- function(input, output, session) {
  #hide all textinput for the level/dose 5-20
  shinyjs::hide("doseUI2")
  shinyjs::hide("doseUI3")
  shinyjs::hide("doseUI4")
  shinyjs::hide("doseUI5")
  #create reactive value to determine the number of 'add'/'remove' button
  #clicks. Start value is 1 which means only 4 Textinputs are shown.
  #Maximum value (restricted through observeEvent handler) is 5 which means
  #that all 20 Textinputs are shown.
  showhide <- shiny::reactiveValues(val = 1)

  shiny::observeEvent(input$add, {
    if (showhide$val < 5){
      showhide$val <- showhide$val + 1
    }
  })

  shiny::observeEvent(input$remove, {
    if (showhide$val > 1) {
      showhide$val <- showhide$val - 1
    }
  })

  shiny::observeEvent(showhide$val, {
    if (showhide$val == 1) {
      for (i in 2:5) {
        shinyjs::hide(paste0("doseUI", i))
      }
    } else if (showhide$val == 2) {
      for (i in 3:5) {
        shinyjs::hide(paste0("doseUI", i))
      }
      shinyjs::show("doseUI2")
    } else if (showhide$val == 3) {
      for (i in 4:5) {
        shinyjs::hide(paste0("doseUI", i))
      }
      for (i in 2:3) {
        shinyjs::show(paste0("doseUI", i))
      }
    } else if (showhide$val == 4) {
      shinyjs::hide("doseUI5")
      for (i in 2:4) {
        shinyjs::show(paste0("doseUI", i))
      }
    } else if (showhide$val == 5) {
      for (i in 2:5) {
        shinyjs::show(paste0("doseUI", i))
      }
    }
  })

  # call module doseLevel_UI from innerdoseLevel_Module.R
  shiny::observe({
    output$doseUI <- shiny::renderUI({
      purrr::map(paste0("tmp", 1:4), ~doseLevel_UI(id = session$ns(.x)))
    })
    output$doseUI2 <- shiny::renderUI({
      purrr::map(paste0("tmp", 5:8), ~doseLevel_UI(id = session$ns(.x)))
    })
    output$doseUI3 <- shiny::renderUI({
      purrr::map(paste0("tmp", 9:12), ~doseLevel_UI(id = session$ns(.x)))
    })
    output$doseUI4 <- shiny::renderUI({
      purrr::map(paste0("tmp", 13:16), ~doseLevel_UI(id = session$ns(.x)))
    })
    output$doseUI5 <- shiny::renderUI({
      purrr::map(paste0("tmp", 17:20), ~doseLevel_UI(id = session$ns(.x)))
    })
  })

  call_Mod <- shiny::reactive({
    val <- purrr::map(paste0("tmp", 1:20), ~shiny::callModule(doseLevel_server,.x))
    val
  })

  shiny::observe({(call_Mod())})

  values_level <- shiny::reactive({
    shiny::req(call_Mod())
    cM <- call_Mod()
    tmp <- do.call(rbind.data.frame, lapply(cM, function(x) data.frame(do.call(cbind.data.frame, x), check.names = FALSE)))
    if (dim(tmp)[1] > 0) {
      colnames(tmp) <- c("dose", "patients")
    }
    tmp <- as.data.frame(tmp)

    empty_as_na <- function(x) {
      if ("factor" %in% class(x)) {
        x <- as.character(x)
      }
      ifelse(as.character(x) != "", x, NA)
    }

    dose <- as.numeric(empty_as_na(tmp$dose))
    pat <- as.numeric(empty_as_na(tmp$patients))
    tmp <- data.frame(
      'dose' = dose,
      'patients' = pat,
      stringsAsFactors = FALSE
    )

    tmp_nr <- rowSums(!is.na(tmp))
    if (is.na(which(tmp_nr < 2)[1] - 1)) {
      tmp_nr <- length(tmp$dose)
    } else {
      tmp_nr <- which(tmp_nr < 2)[1] - 1
    }
    if (!is.na(tmp_nr) & tmp_nr > 0) {
      tmp[1:tmp_nr, ]
    }
  })

  return(values_level)
}
