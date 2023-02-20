#required R libraries
suppressMessages(library(shiny))
suppressMessages(library(DT))
suppressMessages(library(DoseFinding))
suppressMessages(library(latticeExtra))
suppressMessages(library(ggplot2))
suppressMessages(library(shinyjs))
suppressMessages(library(stringr))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinyBS))
suppressMessages(library(readxl))
suppressMessages(library(knitr))
suppressMessages(library(kableExtra))


# global options
colortagdosdesR <- shiny::getShinyOption(
  "colortagdosdesR", "body {background-color: #424242 ; color: #828282 }"
)

initial_value <- 4

numberCandidateModels <- shiny::getShinyOption("numberCandidateModels", initial_value)

dosedesignR_green <- "#5cb85c"
dosedesignR_blue <- "#3a6791"
dosedesignR_red <- "#c94053"
dosedesignR_bright_grey <- "#424242"
dosedesignR_mid_grey <- "#383838"
dosedesignR_dark_grey <- "#333333"
f_col <- colorRamp(c(dosedesignR_blue, "gray89", dosedesignR_green))
f_col_report <- colorRamp(c(dosedesignR_blue, "gray42", dosedesignR_green))


DFApp <- shiny::shinyApp(
  #### UI ####
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          ".shiny-notification {
          position:fixed;
          top: calc(57%);
          left: calc(3%);
          width: 350px;
          font-size: 30px;
          background-color: white;
          font-color: black;
          color: #424242;
          }"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(2,
        img(src = 'www/Iconx_dosedesignR.png', width = '100%'),
        shiny::div(
          shiny::tags$head(
            shiny::tags$style(
              HTML(
                "
                h1 {
                font-family: 'Arial';
                font-weight: 500;
                line-height: 1.5;
                color: #ffffff;
                }
                "
              )
            )
          )
        ),
        #### SIDEBAR PANEL ####
        #### ... 1. Data/Tab options ####
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel("Data/Tab options",
           shiny::wellPanel(style = "background: #424242; border-color:#383838",
              shinyWidgets::prettyRadioButtons(
                inputId = 'impswitch',
                label = HTML('<p style = "color:white"> Select file format </p>'),
                status = "primary",
                shape = 'round',
                animation = 'smooth',
                choiceNames = list(tags$span(style = "color:white", "*.RDS file"),
                tags$span(style = "color:white", "*.csv file"),
                tags$span(style = "color:white", "*.xls(x) file")),
                choiceValues = c('rds', 'csv','xls')
              ),
              shiny::uiOutput('impdata'),
              shiny::uiOutput("datasets"),
              shiny::uiOutput("update_datasets")
            ),
            shiny::tags$div(shiny::tags$br()),
            shiny::wellPanel(style = "background: #424242; border-color:#383838",
              shiny::downloadButton(
                outputId = "save_setting2",
                label = "Save Settings ",
                style = paste0("color:#FFFFFF ; background-color: ", dosedesignR_blue, ";")
              ),
              shiny::tags$div(shiny::tags$br()),
              shiny::uiOutput('load_data'),
              shiny::tags$div(shiny::tags$br())
            )
          ), id = "collapse1", open = "Data/Tab options"
        ),
        #### ... 2. Display Options ####
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(title = "Display Options",
            shiny::numericInput(
              inputId = "ymin",
              label = HTML('<p style="color:white"> Minimum response </p>'),
              value = 0
            ),
            shiny::numericInput(
              inputId = "ymax",
              label = HTML('<p style="color:white"> Maximum response </p>'),
              value = 100
            ),
            shinyjs::disabled(
              numericInput(
                inputId = "dmin",
                label = HTML('<p style="color:white"> Minimum dose </p>'),
                value = 0)
            ),
            shiny::numericInput(
              inputId = "dmax",
              label = HTML('<p style="color:white"> Maximum dose </p>'),
              value = 100
            )
          ), id = "collapse2", open = "Display Options"
        ),
        #### ... 3. Design Options ####
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(title = "Design Options",
            shiny::numericInput(
              inputId = "npats",
              label = HTML('<p style="color:white"> Number of patients </p>'),
              value = 100
            ),
            shiny::selectInput(
              inputId = 'regimen',
              label = HTML('<p style="color:white"> Dose restrictions </p>'),
              choices = c("Min and max dose only", "Specify dose levels")
            ),
            shiny::textInput(
              inputId = "levels",
              label = HTML('<p style="color:white"> Dose levels</p>'),
              placeholder = "0;10;50;100"
            ),
            shinyjs::disabled(
              selectInput(
                inputId = 'optimality',
                label = HTML('<p style="color:white"> Optimality criterion </p>'),
                choices = c("A-Optimality", "D-Optimality", "E-Optimality", "G-Optimality"),
                selected = "D-Optimality"
              )
            )
          ), id = "collapse3", open = "Design Options"
        ),
        #### ... 4. Simulation Options ####
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(
            title = "Simulation Options",
            shiny::numericInput(
              inputId = "sdsim",
              label = HTML('<p style="color:white"> SD for simulation </p>'),
              value = 10,
              min = 0
            ),
            shiny::numericInput(
              inputId = "clevel",
              label = HTML('<p style="color:white"> Confidence level [%] </p>'),
              value = 95,
              min = 0,
              max = 100
            )
          ), id = "collapse4", open = "Simulation Options"
        ),
        #### ... 5. Report ####
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(title = "Report",
            shinyWidgets::checkboxGroupButtons(
              inputId = "report_selection",
              label = HTML('<p style="color:white"> Include Option(s): </p>'),
              choices = c(
                "General Options",
                "Optimal Design",
                "Simulation",
                "Session Info"
              ),
              selected = c("General Options", "Optimal Design", "Simulation", "Session Info"),
              justified = FALSE,
              status = "primary",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            shinyWidgets::checkboxGroupButtons(
              inputId = "report_tabs",
              label = HTML('<p style="color:white"> Include Candidate Tab(s):</p>'),
              choices = paste0("Candidate Tab ", 1:numberCandidateModels),
              justified = FALSE,
              status = "primary",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            shiny::conditionalPanel('output.include_tab_flag',
              shiny::selectInput(
               inputId = "outFormat",
               label = "Report format",
               choices = list(
                 html = "html_document",
                 Word = "word_document"
                )
              ),
              shiny::downloadButton(
                outputId = "report",
                label = "Generate report",
                class = "button_class"),
            ),
            shiny::tags$head(tags$style(".button_class{color: white; background-color: #5cb85c}"))
          ), id = "collapse5"
        )
      ),
      #### MAIN PANEL ####
      shiny::column(10,
        shiny::tabsetPanel(id = "tabSet_Panel_id",
          #### ... 1. Dose Finding ####
          shiny::tabPanel("Dose Finding", value = 1,
            shinyjs::useShinyjs(),
            list(
              shiny::tags$head(shiny::tags$style(colortagdosdesR))
            ),
            shiny::tags$style(type = 'text/css',
              HTML(
                paste0(".panel-default > .panel-heading {color: #fff; background-color: ", dosedesignR_blue, ";}",
                ".panel-default {border-color: #383838}",
                ".panel-group .panel {background-color: #383838}",
                ".panel-default > .panel-heading + .panel-collapse > .panel-body {border-top-color: #424242}",
                ".panel-title {font-weight: bold}
                ")
              )
            ),
            shiny::mainPanel(width = 12,
              shiny::uiOutput('numModUI')
            )
          ),
          #### ... 3. Help ####
          shiny::navbarMenu("Help",
            #### ..... 3.1. Dose Design ####
            shiny::tabPanel("Dose Design", value = 3,
              shiny::uiOutput('manual_dosedesign')
            ),
            #### ..... 3.2. Data Structure ####
            shiny::tabPanel("Data Structure", value = 4,
             shiny::uiOutput('manual')
            )
          )
        )
      )
    )
  ),
  #### SERVER ####
  server <- function(input, output, session) {
    if (exists("apppars")) {
      numberCandidateModels <- apppars$numberCandidateModels
    } else {
      numberCandidateModels <- shiny::getShinyOption(
          "numberCandidateModels", numberCandidateModels
        )
    }
    #### SIDEBAR PANEL ####
    #### ... 1. Data/Tab Options ####
    output$load_data <- shiny::renderUI({
      shiny::fileInput(
        inputId = 'loadSettings',
        label = HTML('<p style="color:white"> Upload Settings (.rds) </p>'),
        multiple = FALSE,
        accept = '.RData'
      )
    })

    output$impdata <- shiny::renderUI({
      if (input$impswitch == 'rds') {
        shiny::fixedRow(
          shiny::column(12,
            shiny::fileInput(
              inputId = 'file',
              label = HTML('<p style="color:white"> Choose RDS file </p>'),
              multiple = FALSE,
              accept = '.RData'
            )
          )
        )
      } else if (input$impswitch == 'xls') {
        shiny::fixedRow(
          shiny::column(12,
            shiny::fileInput(
              inputId = 'xls',
              label = HTML('<p style="color:white"> Choose Excel file </p>'),
              multiple = FALSE,
              accept = '.xls,.xlsx'
            )
          )
        )
      } else if (input$impswitch == 'csv') {
        shiny::tagList(
          shiny::fixedRow(
            shiny::column(12,
              shiny::radioButtons(
                inputId = 'sep',
                label = HTML('<p style="color:white"> Select separator </p>'),
                inline = FALSE,
                choices = c(
                  'Comma' = ',',
                  'Semicolon' = ';',
                  'Tab' = '\t'
                ),
                selected = ','
              )
            ),
            shiny::column(12,
              shiny::radioButtons(
                inputId = 'quote',
                label = HTML('<p style="color:white"> Select quote </p>'),
                inline = FALSE,
                choices = c(
                  None = '',
                  'Double Quote (")' = '"',
                  "Single Quote (')" = "'"
                ),
                selected = '"'
              )
            ),
            shiny::column(12,
              shiny::radioButtons(
                inputId = 'dec',
                label = HTML('<p style="color:white"> Select decimal character </p>'),
                inline = FALSE,
                choices = c(
                  'Point (.)' = '.',
                  'Comma (,)' = ','
                ),
                selected = '.'
              )
            ),
            shiny::column(12,
              shiny::fileInput(
                inputId = 'csv',
                label = HTML('<p style="color:white"> Choose CSV file </p>'),
                multiple = TRUE,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv'
                )
              )
            )
          )
        )
      }
    })

    output$impdataMod <- shiny::renderUI({
      shiny::fileInput(
        inputId = 'file',
        label = 'Choose RDS file',
         multiple = FALSE,
        accept = '.RData'
      )
    })

    df <- shiny::reactive({
      if (is.null(input$file) & is.null(input$csv) & is.null(input$xls)) {
        data1 <- NULL
      }
      if (!is.null(input$file)) {
        data1 <- readRDS(input$file$datapath)
        if (is.data.frame(data1)) {
          data1 <- list('data' = list(data1))
        } else if (is.list(data1)) {
          data1 <- list('data' = data1[1:length(data1)])
        }
        for (i in 1:length(data1$data)) {
          data_cols <- rgb(f_col(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
          data_cols_report <- rgb(f_col_report(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
          data1$data[[i]] <- cbind(data1$data[[i]], 'color' = data_cols[i], 'color_report' = data_cols_report[i])
        }
      }
      if (!is.null(input$xls)) {
        data1 <- readxl::read_excel(input$xls$datapath)
        if ("Modell" %in% colnames(data1)) {
          split_data1 <- split(data1, data1$Modell)

          empty_list <- list('data' = NULL)

          for (i in 1:length(split_data1)) {
            data_cols <- rgb(f_col(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
            data_cols_report <- rgb(f_col_report(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
            empty_list$data[[i]] <- cbind(split_data1[[i]], 'color' = data_cols[i], 'color_report' = data_cols_report[i])
          }
          data1 <- empty_list
        } else if(!("Modell" %in% colnames(data1))) {

        }
      }
      if (!is.null(input$csv)) {
        data1 <- utils::read.csv(
          input$csv$datapath,
          header = TRUE,
          na.strings = c('NA','.',''),
          sep = input$sep,
          quote = input$quote,
          dec = input$dec,
          stringsAsFactors = FALSE
        )
        if ("Modell" %in% colnames(data1)) {
          split_data1 <- split(data1, data1$Modell)
          empty_list <- list('data' = NULL)
          for (i in 1:length(split_data1)) {
            data_cols <- rgb(f_col(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
            data_cols_report <- rgb(f_col_report(seq(0, 1, length = length(data1$data))), maxColorValue = 255)
            empty_list$data[[i]] <- cbind(split_data1[[i]], 'color' = data_cols[i], 'color_report' = data_cols_report[i])
          }
          data1 <- empty_list
        } else if (!("Modell" %in% colnames(data1))) {

        }
      }
      data1
    })


    output$update_datasets <- shiny::renderUI({
      shiny::req(df())
      shiny::actionButton(
        inputId = "update_datasets",
        label = "Update datasets",
        icon = icon("exchange-alt"),
        style = paste0("color:#FFFFFF ; background-color: #5cb85c;")
      )
    })

    output$datasets <- shiny::renderUI({
      shiny::req(df())
      dftmp <- df()
      choices <- paste0("Dataset ", 1:length(dftmp$data))

      choices.sym <- rep('glyphicon-plus-sign', length(dftmp$data))
      data_cols <- rgb(f_col(seq(0, 1, length = length(df()$data))), maxColorValue = 255)
      choices.col <- paste('color:', data_cols[1:length(dftmp$data)])

      uiElement <- shinyWidgets::pickerInput(
        'datasets',
        label = 'Datasets',
        choices = choices,
        multiple = TRUE,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = FALSE,
          `style` = 'background: btn-primary',
          `header` = 'Select multiple items',
          `none-selected-text` = 'Please select'
        ),
        choicesOpt = list(
          `icon` = choices.sym,
          `style` = choices.col
        )
      )
    })

    output$datasetsMod <- shiny::renderUI({
      shiny::req(df())
      dftmp <- df()
      choices <- paste0("Dataset ", 1:length(dftmp$data))

      choices.sym <- rep('glyphicon-plus-sign', length(dftmp$data))
      data_cols <- rgb(f_col(seq(0, 1, length = length(df()$data))), maxColorValue = 255)
      choices.col <- paste('color:', data_cols[1:length(dftmp$data)])

      uiElement <- shinyWidgets::pickerInput(
        'datasetsMod',
        label = 'Datasets',
        choices = choices,
        multiple = TRUE,
        selected = choices[1],
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = FALSE,
          `style` = 'background: btn-primary',
          `header` = 'Select multiple items',
          `none-selected-text` = 'Please select'
        ),
        choicesOpt = list(
          `icon` = choices.sym,
          `style` = choices.col
        )
      )
    })

    selectedData <- shiny::reactive({
      input$update_datasets
      if (!is.null(shiny::isolate(input$datasets))) {
        sD <- as.numeric(substr(shiny::isolate(input$datasets), 9, nchar(shiny::isolate(input$datasets))))
      } else {
        sD <- NULL
      }
      sD
    })

    da <- shiny::reactive({
      ds <- df()
      input$update_datasets
      if (!is.null(ds) & !is.null(shiny::isolate(input$datasets))) {
        da <- list()
        for(i in 1:length(ds$data)) {
          dt <- ds$data[[i]]
          dose_lev <- as.numeric(names(table(dt$dose)))
          # get the maximum dose
          dose_max <- max(as.numeric(names(table(dt$dose))))
          # get minimum and maximum response value
          response_min <- min(dt$response)
          response_max <- max(dt$response)

          da[[i]] <- list(
            'df' = dt,
            'dose_lev' = dose_lev,
            'dose_max' = dose_max,
            'response_min' = response_min,
            'response_max' = response_max
          )
        }
      } else {
        da <- NULL
      }
      da
    })

    shiny::observeEvent(selectedData(), {
      da <- da()
      sda <- selectedData()
      if (!is.null(da)) {
        if (!is.null(sda)) {
          shiny::updateNumericInput(
            session,
            "dmax",
            value = max(unlist(lapply(da[sda],function(x)(x$dose_max))))
          )
          shiny::updateNumericInput(
            session,
            "ymin",
            value = min(unlist(lapply(da[sda], function(x)(x$response_min))))
          )
          shiny::updateNumericInput(
            session,
            "ymax",
            value = max(unlist(lapply(da[sda], function(x)(x$response_max))))
          )
        }
      }
    })

    output$save_setting2 <- downloadHandler(
      filename = function() {
        paste("settings", ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(settings(), file)
      }
    )

    settings <- shiny::reactive({
      #save information inside the modules from reactive object call_Mod()
      param <- list()
      levelsettings <- list()
      for (i in 1:length(call_Mod())) {
        param[[i]] <- call_Mod()[[i]]$setting()

      }
      selected <- c()
      openclosed <- c()
      for (i in 1:numberCandidateModels) {
        openclosed[i] <- ifelse(!is.null(input[[paste0("Tab", i)]]), "open", "close")
        selected[i] <- ifelse(!is.null(input[[paste0("Tab", i)]]),paste0("Candidate Tab ", i),NA)
      }

      selected <- selected[!is.na(selected)]

      # updateCheckboxGroupButtons()
      #save global settings from sidepanel
      global_Variables <- c(
        input$ymin,
        input$ymax,
        input$dmin,
        input$dmax,
        input$npats,
        input$regimen,
        input$levels,
        input$sdsim,
        input$clevel
      )
      #save all objects as a list and store it as dosedesignR_setting.rds file
      save <- list(
        'Number of parameter' = length(call_Mod()),
        'Parameter' = param,
        'Open Tabs' = openclosed,
        'Global Variables' = global_Variables,
        'Selected Tabs' = selected
      )
      save
    })

    shiny::observeEvent(input$loadSettings, {
      tmp <- readRDS(input$loadSettings$datapath)

      for (i in 1:length(tmp$Parameter)) {
        if (tmp$'Open Tabs'[i] == "open") {
          shinyBS::updateCollapse(session, paste0("Tab", i), open = paste0("Candidate Model ", i))
        }
        if (tmp$'Open Tabs'[i] == "close") {
          shinyBS::updateCollapse(session, paste0("Tab", i), close = paste0("Candidate Model ", i))
        }
      }
      for (i in 1:length(tmp$Parameter)) {
        shinyWidgets::updateMaterialSwitch(session, paste0(i,"-parambox"), value = tmp$Parameter[[i]]$showParameter)
        shiny::updateSelectInput(session, paste0(i, "-model"), selected = tmp$Parameter[[i]]$model)

        for (j in 1:4) {
          shiny::updateNumericInput(session, paste0(i, "-parameter", j), value = tmp$Parameter[[i]]$parameter[j])
        }
        if (!is.null(tmp$Parameter[[i]]$levels)) {
          for (k in 1:dim(tmp$Parameter[[i]]$levels)[1]){
            shiny::req(call_Mod())
            shiny::updateTextInput(session, paste0(i, "-oM1-tmp", k, "-level"), value = tmp$Parameter[[i]]$levels$dose[k])
            shiny::updateTextInput(session, paste0(i, "-oM1-tmp", k, "-patients"), value = tmp$Parameter[[i]]$levels$patients[k])
          }
        }
      }
      shiny::updateNumericInput(session, "ymin", value = tmp$'Global Variables'[1])
      shiny::updateNumericInput(session, "ymax", value = tmp$'Global Variables'[2])
      shiny::updateNumericInput(session, "dmin", value = tmp$'Global Variables'[3])
      shiny::updateNumericInput(session, "dmax", value = tmp$'Global Variables'[4])
      shiny::updateNumericInput(session, "npats", value = tmp$'Global Variables'[5])
      shiny::updateSelectInput(session, "regimen", selected = tmp$'Global Variables'[6])
      shiny::updateTextInput(session, "levels", value = tmp$'Global Variables'[7])
      shiny::updateNumericInput(session, "sdsim", value = tmp$'Global Variables'[8])
      shiny::updateNumericInput(session, "clevel", value = tmp$'Global Variables'[9])
    })

    #### ... 2. Display Options ####
    shiny::observeEvent(input$ymin, {
      shiny::updateNumericInput(session, "ymin", value = input$ymin)
      shiny::updateNumericInput(session, "yminMod", value = input$ymin)
    })

    shiny::observeEvent(input$yminMod, {
      shiny::updateNumericInput(session, "ymin", value = input$yminMod)
      shiny::updateNumericInput(session, "yminMod", value = input$yminMod)
    })

    shiny::observeEvent(input$ymax, {
      shiny::updateNumericInput(session, "ymax", value = input$ymax)
      shiny::updateNumericInput(session, "ymaxMod", value = input$ymax)
    })

    shiny::observeEvent(input$ymaxMod, {
      shiny::updateNumericInput(session,"ymax", value = input$ymaxMod)
      shiny::updateNumericInput(session,"ymaxMod", value = input$ymaxMod)
    })

    shiny::observeEvent(input$dmin, {
      shiny::updateNumericInput(session, "dmin", value = input$dmin)
      shiny::updateNumericInput(session, "dminMod", value = input$dmin)
    })

    shiny::observeEvent(input$dminMod, {
      shiny::updateNumericInput(session ,"dmin", value = input$dminMod)
      shiny::updateNumericInput(session ,"dminMod", value = input$dminMod)
    })

    shiny::observeEvent(input$dmax, {
      shiny::updateNumericInput(session ,"dmax", value = input$dmax)
      shiny::updateNumericInput(session ,"dmaxMod", value = input$dmax)
    })

    shiny::observeEvent(input$dmaxMod, {
      shiny::updateNumericInput(session ,"dmax", value = input$dmaxMod)
      shiny::updateNumericInput(session ,"dmaxMod", value = input$dmaxMod)
    })

    #### ... 3. Design Options ####
    shiny::observeEvent(input$npats, {
      shiny::updateNumericInput(session, "npats", value = input$npats)
      shiny::updateNumericInput(session, "npatsMod", value = input$npats)
    })

    shiny::observeEvent(input$npatsMod, {
      shiny::updateNumericInput(session, "npats", value = input$npatsMod)
      shiny::updateNumericInput(session, "npatsMod", value = input$npatsMod)
    })

    shiny::observeEvent(input$regimen, {
      shiny::updateSelectInput(session, "regimen", selected = input$regimen)
      shiny::updateSelectInput(session, "regimenMod", selected = input$regimen)
    })

    shiny::observeEvent(input$regimenMod,{
      shiny::updateSelectInput(session, "regimen", selected = input$regimenMod)
      shiny::updateSelectInput(session, "regimenMod", selected = input$regimenMod)
    })
    #### ... 4. Simulation Options ####
    #call Module simModule_Server
    shiny::observe({
      purrr::map(
        number_Module$val2, ~ shiny::callModule(
          simModule_Server,
          id = .x,
          number = 1,
          no = as.numeric(substr(.x, 2, nchar(.x))),
          levels = lev,
          regimen = regi,
          npats = npat,
          dmax = dma,
          dmin = dmi,
          ymin = ymi,
          ymax = yma,
          sdsim = sds,
          clevel = clev,
          inp = call_Mod
        )
      )
    })

    #### ... 5. Report ####
    include_tab_flag <- shiny::reactiveValues(val = FALSE)

    shiny::observe({
      if (is.null(input$report_tabs)) {
        include_tab_flag$val <- FALSE
      } else {
        include_tab_flag$val <- TRUE
      }
    })

    output$include_tab_flag <- shiny::reactive(include_tab_flag$val)

    shiny::outputOptions(output,"include_tab_flag", suspendWhenHidden = FALSE)

    output$report <- shiny::downloadHandler(filename = function() {
      switch(
        input$outFormat,
        html_document = paste("report",gsub(":","-",Sys.time()), ".html", sep = ""),
        word_document = paste("report",gsub(":","-",Sys.time()), ".docx", sep = "")
      )
      },
      content = function(file) {
        withProgress(message = 'Generating Report, please wait...', {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "General_options.Rmd")
        file.copy("General_options.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Optimal_design_Intro.Rmd")
        file.copy("Optimal_design_Intro.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Optimal_design.Rmd")
        file.copy("Optimal_design.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Simulation_Intro.Rmd")
        file.copy("Simulation_Intro.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Simulation.Rmd")
        file.copy("Simulation.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Overview_models_Intro.Rmd")
        file.copy("Overview_models_Intro.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Overview_models_Intro2.Rmd")
        file.copy("Overview_models_Intro2.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Overview_models.Rmd")
        file.copy("Overview_models.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Session_info.Rmd")
        file.copy("Session_info.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "optimalContrast_power.Rmd")
        file.copy("optimalContrast_power.Rmd", tempReport_child, overwrite = TRUE)
        tempReport_child <- file.path(tempdir(), "Iconx_dosedesignR.png")
        file.copy("Iconx_dosedesignR.png", tempReport_child, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          numberCandidateModels = numberCandidateModels,
          minimumResponse = input$ymin,
          maximumResponse = input$ymax,
          minimumDose = input$dmin,
          maximumDose = input$dmax,
          data = df(),
          data_name = input$file$name,
          data2 = da(),
          plot_opt = input$plot_opt,
          sdsim = input$sdsim,
          clevel = input$clevel,
          selectnr = selectedData(),
          npats = input$npats,
          call_Mod = call_Mod(),
          regimen = input$regimen,
          regimen_text = ifelse(input$regimen == "Min and max dose only","Only restricted between min and max dose",paste0("Restricted to the following doses: ",input$levels)),
          levels = input$levels,
          report_selection = input$report_selection,
          report_tabs = as.numeric(substr(input$report_tabs,15,17)),
          data_cols =  rgb(f_col_report(seq(0, 1, length = length(df()$data))), maxColorValue = 255),
          rendered_by_shiny = TRUE
        )

        for(i in as.numeric(substr(input$report_tabs, 15, 17))) {
          params[[paste0("model", i)]] <- input[[paste0(i,"-model")]]
          params[[paste0("param", i, "_1")]] <- input[[paste0(i,"-parameter1")]]
          params[[paste0("param", i, "_2")]] <- input[[paste0(i,"-parameter2")]]
          params[[paste0("param", i, "_3")]] <- input[[paste0(i,"-parameter3")]]
          params[[paste0("param", i, "_4")]] <- input[[paste0(i,"-parameter4")]]
          params[[paste0("Mod_model", i)]] <- input[[paste0("Mod",i,"-model")]]
          params[[paste0("Mod_param", i, "_1")]] <- input[[paste0("Mod",i,"-parameter1")]]
          params[[paste0("Mod_param", i, "_2")]] <- input[[paste0("Mod",i,"-parameter2")]]
          params[[paste0("Mod_param", i, "_3")]] <- input[[paste0("Mod",i,"-parameter3")]]
          params[[paste0("Mod_param", i, "_4")]] <- input[[paste0("Mod",i,"-parameter4")]]
          params[[paste0("user_chosen",i)]] <- ifelse(is.null(call_Mod()[[i]]$setting()$levels),NA, call_Mod()[[i]]$setting()$levels)
        }

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          output_format = input$outFormat,
          envir = new.env(parent = globalenv())
        )
        })
      }
    )

    shiny::observe({shinyWidgets::updateCheckboxGroupButtons(session, inputId = "report_tabs", selected =settings()$'Selected Tabs')})

    #### MAIN PANEL ####
    #### ... 1. Dose Finding ####
    # Calculate and Update the number of module id s
    number_Module <- shiny::reactiveValues(
      val = paste0(1:numberCandidateModels),
      val2 = paste0("s",1:numberCandidateModels),
      val3 = paste0("inp", 1:numberCandidateModels, " =call_Mod()[[", 1:numberCandidateModels, "]]")
    )

    shiny::observeEvent(input$nrPanels, {
      number_Module$val <- as.character(1 : isolate(input$nrPanels))
      number_Module$val2 <- paste0("s", as.character(1 : shiny::isolate(input$nrPanels)))
      number_Module$val3 <- paste0("inp", as.character(1 : shiny::isolate(input$nrPanels)) ,
                                   "= call_Mod()[[", as.character(1 : input$nrPanels),"]]")
    })

    # Call Module
    output$numModUI <- shiny::renderUI ({
      shiny::fluidRow(width = 12,
        purrr::map(number_Module$val, ~ doseDesignUI(id = .x))
      )
    })

    regi <- shiny::reactive({input$regimen})
    lev <- shiny::reactive({input$levels})
    npat <- shiny::reactive({input$npats})
    dmi <- shiny::reactive({input$dmin})
    dma <- shiny::reactive({input$dmax})
    ymi <- shiny::reactive({input$ymin})
    yma <- shiny::reactive({input$ymax})
    sds <- shiny::reactive({input$sdsim})
    clev <- shiny::reactive({input$clevel})
    butt <- shiny::reactive({input$save.setting})
    nrPan <- shiny::reactive({numberCandidateModels})

    # save Output from doseDesign Modul as a reactive object call_Mod()

    reactive_values_send_design <- shiny::reactiveValues(
      doses = NULL,
      patients = NULL
    )

    shiny::observeEvent(call_Mod()[[1]]$send_design_button(), {
      for (i in 1:length(call_Mod()[[1]]$UserValdoses())) {
        shiny::updateNumericInput(session, paste0("dM1-tmp",i,"-level"), value = call_Mod()[[1]]$UserValdoses()[i])
      }
      for (i in 1:length(call_Mod()[[1]]$UserValpats())) {
        shiny::updateNumericInput(session, paste0("dM1-tmp", i, "-patients"), value = call_Mod()[[1]]$UserValpats()[i])
      }
      reactive_values_send_design$doses <- call_Mod()[[1]]$UserValdoses()
      reactive_values_send_design$patients <- call_Mod()[[1]]$UserValpats()
    })
    shiny::observeEvent(call_Mod()[[2]]$send_design_button(), {
      for (i in 1:length(call_Mod()[[2]]$UserValdoses())) {
        shiny::updateNumericInput(session, paste0("dM1-tmp",i,"-level"), value = call_Mod()[[2]]$UserValdoses()[i])
      }
      for (i in 1:length(call_Mod()[[2]]$UserValpats())) {
        shiny::updateNumericInput(session, paste0("dM1-tmp", i, "-patients"), value = call_Mod()[[2]]$UserValpats()[i])
      }
      reactive_values_send_design$doses <- call_Mod()[[2]]$UserValdoses()
      reactive_values_send_design$patients <- call_Mod()[[2]]$UserValpats()
    })
    shiny::observeEvent(call_Mod()[[3]]$send_design_button(), {
      for (i in 1:length(call_Mod()[[3]]$UserValdoses())) {
        updateNumericInput(session, paste0("dM1-tmp",i,"-level"), value = call_Mod()[[3]]$UserValdoses()[i])
      }
      for (i in 1:length(call_Mod()[[3]]$UserValpats())) {
        updateNumericInput(session, paste0("dM1-tmp", i, "-patients"), value = call_Mod()[[3]]$UserValpats()[i])
      }
      reactive_values_send_design$doses <- call_Mod()[[3]]$UserValdoses()
      reactive_values_send_design$patients <- call_Mod()[[3]]$UserValpats()
    })

    send_reac <- shiny::reactive({
      data.frame(doses = reactive_values_send_design$doses, patients = reactive_values_send_design$doses)
    })

    output$send_design_table <- shiny::renderTable({
      send_reac()
    },
    bordered = TRUE,
    spacing = 'xs',
    rownames = TRUE,
    digits = 2
    )

    call_Mod <- shiny::reactive({
      Val <- purrr::map(number_Module$val, ~ shiny::callModule(
        doseDesign,
        id = .x,
        nr = as.numeric(.x),
        levels = lev,
        regimen = regi,
        npats = npat,
        dmax = dma,
        dmin = dmi,
        ymin = ymi,
        ymax = yma,
        sdsim = sds,
        clevel = clev,
        data = df,
        data2 = da,
        selectnr = selectedData,
        button = butt,
        nrPanel = nrPan
      )
      )
      Val
    })

    doses <- shiny::reactive({
      shiny::req(dM())
      doses <- dM()$dose
      doses
    })

    index1 <- shiny::reactive({
      index <- c()
      for (i in 1:numberCandidateModels) {
        index[i] <- ifelse(!is.null(input[[paste0("TabMod", i)]]), TRUE, FALSE)
      }
      index
    })

    # wrap an observer around the callModule because of the reactive expression number_Module$val
    shiny::observe({(call_Mod())})

    #### ..... 2.2. Design ####
    dM <- shiny::callModule(doseLevel, "dM1")
    shiny::observe({dM()})

    #### ... 3. Help ####
    #### ..... 3.1. Dose Design ####
    output$manual_dosedesign <- shiny::renderUI({
      list(
        HTML(
          "
          <h1 style='color: white'> dosedesignR </h1>
          <p style='color: white'>

          </p>
          <h3 style='color: white'> Description </h3>
          <p style='color: white'>
          The Shiny application dosedesignR provides the user with an interactive application which can be used to facilitate the planning of dose finding studies by applying the theory of optimal experimental design. The application visualizes dose-response curves, optimal designs and corresponding simulated realizations of dose-response studies.
          Functionality
          After loading the package dosedesignR, the application can be started by calling the function launch_dosedesignR()upon which the user interface opens. The user interface is split into two parts: a sidebar panel on the left side for specifying general options and the main panel for investigating different dose-response relationships.
          Left sidebar panel
          In the Options panel it is possible to specify the minimum and maximum response as well as the minimum and maximum dose relevant for the dose-response relationship. The default setting for the minimum dose is 0, corresponding to placebo, which cannot be changed. The response and dose options are mainly used for plotting, even though the maximum dose is also a design parameter.
          For the Design Options the total number of patients that should be distributed across the different dose levels determined by the optimal design can be entered. Furthermore, it is possible to choose between two different dose restrictions for the optimal design: Min and max dose only and Specify dose levels. For Min and max dose only the optimal design is derived on the continuous design space between the minimum and the maximum dose defined above. These designs are theoretically optimal and calculated exactly. For Specify dose levels specific dose levels need to be entered in the Dose levels panel beneath (separated by semicolon) and the optimal design is derived restricted to these entered dose levels. To find the optimal designs an optimization algorithm selects among all possible combinations of patients and dose levels. Additionally, the underlying optimality criterion for the calculation of the optimal design can be selected (in the current version this is restricted to D-Optimality). Lastly, one can specify the population standard deviation of the response and a confidence level. Both will be used for the simulations in the main panel.
          Main panel
          The main panel is again split up into three vertical subpanels, labeled as Candidate Model 1, Candidate Model 2 and Candidate Model 3. Thus it allows to simultaneously compare three different dose-response models. So far the linear model, the emax model and the sigmoidal emax model are implemented. Let  denote the dose within the specified dose range. Then the models are defined as follows
          ,
          ,
          .
          Here, denotes the dose,  is the placebo response, is the maximal treatment effect,  is the half effect dose, i.e. the dose at which half of the maximal effect is reached, and h is the Hill parameter which determines the steepness of the response increase/decrease.

          Each of the three vertical candidate panels consists of three parts: a top part to choose the model parameters and plot the corresponding dose-response curve, a middle part to visualize the optimal design and a bottom part in which a user-defined design can be compared with the optimal design.
          In the top part, the values of the respective model parameters can be entered in the Parameters tab. The Plot tab next to the Parameters tab visualizes the dose-response curve based on the chosen model and the corresponding model parameters.
          In the middle part, the optimal design based on the chosen dose-response model and corresponding parameter values is visualized by black vertical lines, taking into account the number of patients, the dose restrictions and the optimality criterion selected in the left sidebar panel. Additionally, the optimal design is tabulated underneath the plots.
          In the bottom part, a user defined design can be entered by specifying dose levels in the Dose levels input box and the corresponding number of patients in the Patients input box (separated by semicolon, respectively). The user defined design is additionally visualized in the optimal design plot by green dots. The user defined design is not bound by the dose restrictions selected in the left sidebar panel and dose levels apart from these could be specified. Underneath the input boxes for the user defined design the properties of the design in tabular form are shown. In the first line, the total number of patients entered is shown. In the second line the efficiency of the user defined design compared to the optimal design (under the chosen dose restrictions) is shown in percentage. In the last line this efficiency is converted to the number of patients which would additionally be needed to achieve the same precision in the parameter estimation (O'Quigley et al., 2017).
          By choosing the Simulation tab next to the Parameters and Plot tab in the top part of the panel, another plot appears. Here it is possible to visualize simulated realizations of dose-response measurements and the corresponding effect on the estimation of the dose-response curve. The underlying design, i.e. the dose levels and the number of patients per dose level, can be selected to be any of the three optimal designs or any of the three user defined designs specified in the three candidate model panels. Response realizations are randomly drawn from a normal distribution with mean equal to the value of the true curve and variance based on the population standard deviation specified in the left sidebar panel. The true and the estimated curve together with the corresponding confidence interval are shown. True curve here refers to the specified candidate model in the corresponding panel.
          </p>
          <h3 style='color: white'> Additional information </h3>
          <p style='color: white'>
          The application was optimized for display on a screen/projector with HD resolution.
          </p>
          <h3 style='color: white'> Disclaimer </h3>
          <p style='color: white'>
          The authors do not guarantee the accuracy of any results.
          </p>
          <h3 style='color: white'> Acknowledgements </h3>
          <p style='color: white'>
          The authors would like to thank the dose finding group within Bayer's Biostatistics Innovation Center [1].
          Besides other packages, we mainly used the DoseFinding [2] and the shiny [3] package.
          </p>
          <h3 style='color: white'> References </h3>
          <ul>
          <li>
          <p style='color: white'>
          Kulmann H., Muysers C., Dmitrienko A. and Roehmel J. (2016). The Biostatistics Innovation Center at Bayer. Biopharmaceutical  Report 23, 1-5. </li>
          </p>
          <li>
          <p style='color: white'>
          Bornkamp B., Pinheiro J. and Bretz F. (2016). DoseFinding: Planning and Analyzing Dose Finding Experiments. R package version 0.9-15. https://CRAN.R-project.org/package=DoseFinding.
          </p>
          </li>
          <li>
          <p style='color: white'>
          Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2018). shiny: Web Application Framework for R. R package version 1.1.0. https://CRAN.R-project.org/package=shiny
          </p>
          </li>
          "
        )
      )
    })
    #### ..... 3.2. Data Structure ####
    output$manual <- shiny::renderUI({
      list(
        HTML(
          "
          <img src='www/Iconx_dosedesignR.png' alt='dosedesignR Logo' width='350' height='350' align ='right'>
          <h2 style = 'color: white'>Data Structure and Format </h2>

          <h3 style = 'color: white'>Data Structure</h3>

          <p style='color:white'>
          In order to use all features of the <b>dosedesignR</b> you can upload a .RDS file which has to include 1-9 data.frames. <br>
          The dataframes have to include the two columns dose and response.

          <br>
          If you have one data.frame in your .RDS file the structure of the data frame should look like this example:
          </p>
          <code style='color: #5cb85c; background-color: #424242'>
          'data.frame':	25 obs. of  2 variables: <br>
          &nbsp;$ dose    : num  0 0 0 0 0 0.05 0.05 0.05 0.05 0.05 ...<br>
          &nbsp;$ response: num  0.15 0.32 0.5 0.01 0.2 1 2.1 0.5 1.8 1.2 ...

          </code>
          <p style = 'color: white'>
          If you want to include multiple data frames in one .RDS file your file structure should look like this expample: <br>
          <code style='color: #5cb85c; background-color: #424242'>

          List of 3 <br>
          &nbsp; $ :'data.frame':	25 obs. of  2 variables: <br>
          &nbsp; &nbsp;..$ dose    : num [1:25] 0 0 0 0 0 0.05 0.05 0.05 0.05 0.05 ... <br>
          &nbsp;&nbsp;..$ response: num [1:25] 0.15 0.32 0.5 0.01 0.2 1 2.1 0.5 1.8 1.2 ... <br>
          &nbsp; $ :'data.frame':	30 obs. of  2 variables: <br>
          &nbsp;&nbsp;..$ dose    : num [1:30] 0 0 0 0 0 0 0.05 0.05 0.05 0.05 ... <br>
          &nbsp;&nbsp;..$ response: num [1:30] 0.25 0.22 0.4 0.03 0.4 0.4 1.2 2.2 0.5 1.4 ... <br>
          &nbsp; &nbsp;$ :'data.frame':	25 obs. of  2 variables: <br>
          &nbsp;&nbsp;..$ dose    : num [1:25] 0 0 0 0 0 1 1 1 1 1 ... <br>
          &nbsp;&nbsp;..$ response: num [1:25] 0.15 0.32 0.5 0.01 0.2 1 2.1 0.5 1.8 1.2 ... <br>
          </code>

          "
        )
      )
    })
  }
)
