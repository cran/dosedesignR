library(dosedesignR)
library(shiny)
library(DT)
library(DoseFinding)
library(latticeExtra)
library(ggplot2)
library(shinyjs)

source("Optimization.R")
source("Simulation.R")
source("Visualize.R")

colortagdosdesR <- shiny::getShinyOption("colortagdosdesR", "body {background-color: #424242 ; color: #828282 }")

DFApp <- shiny::shinyApp(
  ui=fluidPage(
    
    useShinyjs(),
    
    # Application title
    
    list(shiny::tags$head(shiny::tags$style(colortagdosdesR))),
    
    h2("dosedesignR"),
    sidebarPanel(width=2,
                 h4("Options"),
                 numericInput("ymin", "Minimum response", 0),
                 numericInput("ymax", "Maximum response", 100),
                 disabled(numericInput("dmin", "Minimum dose", 0)),
                 numericInput("dmax", "Maximum dose", 100),
                 br(),
                 h4("Design options"),
                 numericInput("npats", "Number of patients", 100),
                 selectInput('regimen', 'Dose restrictions', c("Min and max dose only", "Specify dose levels")),
                 textInput("levels", "Dose levels", placeholder = "0;10;50;100"),
                 disabled(selectInput('optimality', 'Optimality criterion', c("A-Optimality", "D-Optimality", "E-Optimality", "G-Optimality"), selected="D-Optimality")),
                 br(),
                 h4("Simulation options"),
                 numericInput("sdsim", "SD for simulation", 10, min=0),
                 numericInput("clevel", "Confidence level [%]", 95, min=0, max=100)
    ),
    mainPanel(width=10,
              tags$head(tags$style("#optDesign1{height:25vh !important;}")), # this line reduces the height of a specific plot "optDesign1"
              tags$head(tags$style("#optDesign2{height:25vh !important;}")),
              tags$head(tags$style("#optDesign3{height:25vh !important;}")),
              fluidRow(width=12,
                       column(4,
                              h4("Candidate Model 1"),
                              selectInput("model1", NULL , c("Linear", "Emax", "Sigmoidal Emax")),
                              textOutput("model1"),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Parameters", conditionalPanel(condition = "input.model1=='Linear'",
                                                                                  numericInput("linInt1", "Intercept", 0),
                                                                                  numericInput("linSlp1", "Slope", 0.8)),
                                                   conditionalPanel(condition = "input.model1=='Emax'",
                                                                    numericInput("emxE01", "E0", 1),
                                                                    numericInput("emxED501", "ED50", 25),
                                                                    numericInput("emxEmax1", "Emax", 100)),
                                                   conditionalPanel(condition = "input.model1=='Sigmoidal Emax'",
                                                                    numericInput("semxE01", "E0", 1),
                                                                    numericInput("semxED501", "ED50", 25),
                                                                    numericInput("semxEmax1", "Emax", 80),
                                                                    numericInput("semxHill1", "Hill", 5))
                                          ),
                                          tabPanel("Plot",
                                                   plotOutput("modelFunction1", width="100%")),
                                          tabPanel("Simulation",
                                                   selectInput("sim1", NULL, c("Optimal 1", "Optimal 2", "Optimal 3", 
                                                                               "User Defined 1", "User Defined 2", "User Defined 3"), selected="Optimal 1"),
                                                   actionButton("gosim1", "Go"),
                                                   plotOutput("simDesign1")))),
                       column(4,
                              h4("Candidate Model 2"),
                              selectInput("model2", NULL, c("Linear", "Emax", "Sigmoidal Emax"), selected="Emax"),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Parameters", conditionalPanel(condition = "input.model2=='Linear'",
                                                                                  numericInput("linInt2", "Intercept", 0),
                                                                                  numericInput("linSlp2", "Slope", 0.8)),
                                                   conditionalPanel(condition = "input.model2=='Emax'",
                                                                    numericInput("emxE02", "E0", 1),
                                                                    numericInput("emxED502", "ED50", 25),
                                                                    numericInput("emxEmax2", "Emax", 100)),
                                                   conditionalPanel(condition = "input.model2=='Sigmoidal Emax'",
                                                                    numericInput("semxE02", "E0", 1),
                                                                    numericInput("semxED502", "ED50", 25),
                                                                    numericInput("semxEmax2", "Emax", 80),
                                                                    numericInput("semxHill2", "Hill", 5))
                                          ),
                                          tabPanel("Plot", plotOutput("modelFunction2")),
                                          tabPanel("Simulation",
                                                   selectInput("sim2", NULL, c("Optimal 1", "Optimal 2", "Optimal 3", 
                                                                               "User Defined 1", "User Defined 2", "User Defined 3"), selected="Optimal 2"),
                                                   actionButton("gosim2", "Go"),
                                                   plotOutput("simDesign2"))
                              )),
                       column(4,
                              h4("Candidate Model 3"),
                              selectInput("model3", NULL, c("Linear", "Emax", "Sigmoidal Emax"), selected="Sigmoidal Emax"),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Parameters", conditionalPanel(condition = "input.model3=='Linear'",
                                                                                  numericInput("linInt3", "Intercept", 0),
                                                                                  numericInput("linSlp3", "Slope", 0.8)),
                                                   conditionalPanel(condition = "input.model3=='Emax'",
                                                                    numericInput("emxE03", "E0", 1),
                                                                    numericInput("emxED503", "ED50", 25),
                                                                    numericInput("emxEmax3", "Emax", 100)),
                                                   conditionalPanel(condition = "input.model3=='Sigmoidal Emax'",
                                                                    numericInput("semxE03", "E0", 1),
                                                                    numericInput("semxED503", "ED50", 25),
                                                                    numericInput("semxEmax3", "Emax", 80),
                                                                    numericInput("semxHill3", "Hill", 5))
                                          ),
                                          tabPanel("Plot", plotOutput("modelFunction3")),
                                          tabPanel("Simulation",
                                                   selectInput("sim3", NULL, c("Optimal 1", "Optimal 2", "Optimal 3", 
                                                                               "User Defined 1", "User Defined 2", "User Defined 3"), selected="Optimal 3"),
                                                   actionButton("gosim3", "Go"),
                                                   plotOutput("simDesign3"))))
                       
              ),
              fluidRow(width=12, align="center",
                       column(4,
                              plotOutput("optDesign1")), 
                       column(4,
                              plotOutput("optDesign2")),
                       column(4,
                              plotOutput("optDesign3"))
              ),
              fluidRow(width=12,
                       column(4, align="center",
                              tableOutput("viewOpt1")),
                       column(4, align="center",
                              tableOutput("viewOpt2")),
                       column(4, align="center",
                              tableOutput("viewOpt3"))
              ),
              fluidRow(width=12,
                       column(4, align="center",
                              textInput("UserDoses1", "Dose levels", placeholder = "0;10;50;100"),
                              textInput("UserPatients1", "Patients", placeholder = "25;20;50;5"),
                              tableOutput("viewEff1")),
                       column(4, align="center",
                              textInput("UserDoses2", "Dose levels", placeholder = "0;10;50;100"),
                              textInput("UserPatients2", "Patients", placeholder = "25;20;50;5"),
                              tableOutput("viewEff2")),
                       column(4, align="center",
                              textInput("UserDoses3", "Dose levels", placeholder = "0;10;50;100"),
                              textInput("UserPatients3", "Patients", placeholder = "25;20;50;5"),
                              tableOutput("viewEff3"))
              )
    )),
  
  server=function(input, output, session) {
    
    optValue <- reactiveValues()
    UserValue <- reactiveValues()
    
    ####
    # Computes and plots the optimal design for the choosen model for each tab
    ####
    output$optDesign1 <- renderPlot({
      
      # Make sure that enough dose levels are supplied
      tmp1 <- as.numeric(strsplit(input$levels, ";")[[1]])
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model1 == "Linear" )
        validate(need(length(tmp1) > 1, 'Need at least 2 dose levels'))
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model1 == "Emax" )
        validate(need(length(tmp1) > 2, 'Need at least 3 dose levels'))
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model1 == "Sigmoidal Emax" )
        validate(need(length(tmp1) > 3, 'Need at least 4 dose levels'))
      
      x <- ComputeOptimal(input, 1)
      optValue$doses1 <- x$res[1,]
      optValue$distr1 <- x$res[2,]
      optValue$pats1  <- round(x$res[2,]*input$npats,1)
      optValue$mods1 <- x$mods
      optValue$optD1 <- x$optD
      
      da <- data.frame(optValue$doses1,optValue$pats1)
      t<- c("optimal design")
      datleg <-  data.frame(t, test=1:2, y=1:2)
      
      pl1 <- ggplot(data=da,aes(x=optValue$doses1,y=optValue$pats1,group=1))+geom_linerange(ymin=0,ymax= optValue$pats1) +
        labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+xlim(input$dmin,input$dmax)+theme(axis.text = element_text(size=rel(1.1)),
                                                                                               axis.title = element_text(size=rel(1.1)))+
        geom_line(data = datleg, aes(colour = t, x = test, y = y,group=1), alpha = 0)+
        scale_colour_manual(values = c("black"))+    
        theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
              legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
        guides(colour=guide_legend(override.aes=list(size=1, alpha=1, linetype=1)))
      
      
      UserValue$doses1 <- as.numeric(strsplit(input$UserDoses1, ";")[[1]])
      UserValue$pats1 <- as.numeric(strsplit(input$UserPatients1, ";")[[1]])
      if(length(UserValue$doses1)==length(UserValue$pats1)&& length(UserValue$pats1)>0){
        t<- c("optimal design", "user defined design")
        datleg <-  data.frame(t, test=1:2, y=1:2)
        plotdat2 <- data.frame(doses=UserValue$doses1,pats= UserValue$pats1)
        pl1<- ggplot(data=da,aes(x=optValue$doses1,y=optValue$pats1))+geom_linerange(ymin=0,ymax= optValue$pats1) +
          labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+xlim(input$dmin,input$dmax)+theme(axis.text = element_text(size=rel(1.1)),
                                                                                                 axis.title = element_text(size=rel(1.1)))
        pl1<- pl1 + geom_point(data=plotdat2,aes(x=plotdat2$doses,y=plotdat2$pats,group=1),colour="#89D329",size=rel(2))    +
          xlim(input$dmin,input$dmax)+ geom_line(data = datleg, aes(colour = t, x = test, y = y,group=1), alpha = 0)+
          geom_point(data = datleg, aes(colour=t,group=1), alpha=0, x=1, y=1) +
          scale_colour_manual(values = c("black","#89D329"))+    
          theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
                legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
          guides(colour=guide_legend(override.aes=list(size=c(1,2.5), alpha=1, shape=c(NA,16),linetype=c(1,0))))
      }
      suppressWarnings(print(pl1))
    } )
    output$optDesign2 <- renderPlot({
      
      # Make sure that enough dose levels are supplied
      tmp1 <- as.numeric(strsplit(input$levels, ";")[[1]])
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model2 == "Emax" )
        validate(need(length(tmp1) > 2, 'Need at least 3 dose levels'))
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model2 == "Sigmoidal Emax" )
        validate(need(length(tmp1) > 3, 'Need at least 4 dose levels'))
      
      x <- ComputeOptimal(input, 2)
      optValue$doses2 <- x$res[1,]
      optValue$distr2 <- x$res[2,]
      optValue$pats2  <- round(x$res[2,]*input$npats,1)
      optValue$mods2 <- x$mods
      optValue$optD2 <- x$optD
      da <- data.frame(optValue$doses2,optValue$pats2)
      
      t<- c("optimal design")
      datleg <- data.frame(t, test=1:3, y=1:3)
      
      pl2 <- ggplot(data=da,aes(x=optValue$doses2,y=optValue$pats2))+geom_linerange(ymin=0,ymax= optValue$pats2) +
        labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+xlim(input$dmin,input$dmax)+theme(axis.text = element_text(size=rel(1.1)),
                                                                                               axis.title = element_text(size=rel(1.1)))+
        geom_line(data = datleg, aes(colour = t, x = test, y = y, group=1), alpha = 0)+
        scale_colour_manual(values = c("black"))+    
        theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
              legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
        guides(colour=guide_legend(override.aes=list(size=1, alpha=1, linetype=1)))
      
      UserValue$doses2 <- as.numeric(strsplit(input$UserDoses2, ";")[[1]])
      UserValue$pats2 <- as.numeric(strsplit(input$UserPatients2, ";")[[1]])
      if(length(UserValue$doses2)==length(UserValue$pats2)&& length(UserValue$pats2)>0){
        t<- c("optimal design", "user defined design")
        datleg <-  data.frame(t, test=1, y=1)
        plotdat2 <- data.frame(doses=UserValue$doses2,pats= UserValue$pats2)
        pl2<- ggplot(data=da,aes(x=optValue$doses2,y=optValue$pats2))+geom_linerange(ymin=0,ymax= optValue$pats2) +
          labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+theme(axis.text = element_text(size=rel(1.1)),
                                                                     axis.title = element_text(size=rel(1.1)))
        pl2<- pl2 + geom_point(data=plotdat2,aes(x=plotdat2$doses,y=plotdat2$pats),colour="#89D329",size=rel(2))    +
          xlim(input$dmin,input$dmax)+
          geom_line(data = datleg, aes(colour = t, x = test, y = y, group=1), alpha = 0)+
          geom_point(data = datleg, aes(colour=t), alpha=0, x=1, y=1) +
          scale_colour_manual(values = c("black","#89D329"))+    
          theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
                legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
          guides(colour=guide_legend(override.aes=list(size=c(1,2.5), alpha=1, shape=c(NA,16),linetype=c(1,0))))
      }
      suppressWarnings(print(pl2))})
    output$optDesign3 <- renderPlot({
      
      # Make sure that enough dose levels are supplied
      tmp1 <- as.numeric(strsplit(input$levels, ";")[[1]])
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model3 == "Emax" )
        validate(need(length(tmp1) > 2, 'Need at least 3 dose levels'))
      if( input$regimen=="Specify dose levels" & length(tmp1) > 0 & input$model3 == "Sigmoidal Emax" )
        validate(need(length(tmp1) > 3, 'Need at least 4 dose levels'))
      
      x <- ComputeOptimal(input, 3)
      optValue$doses3 <- x$res[1,]
      optValue$distr3 <- x$res[2,]
      optValue$pats3  <- round(x$res[2,]*input$npats,1)
      optValue$mods3 <- x$mods
      optValue$optD3 <- x$optD
      da <- data.frame(optValue$doses3,optValue$pats3)
      
      t<- c("optimal design")
      datleg <-  data.frame(t, test=1:3, y=1:3)
      pl3 <- ggplot(data=da,aes(x=optValue$doses3,y=optValue$pats3))+geom_linerange(ymin=0,ymax= optValue$pats3) +
        labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+xlim(input$dmin,input$dmax)+theme(axis.text = element_text(size=rel(1.1)),
                                                                                               axis.title = element_text(size=rel(1.1)))+
        geom_line(data = datleg, aes(colour = t, x = test, y = y, group=1), alpha = 0)+
        scale_colour_manual(values = c("black"))+    
        theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
              legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
        guides(colour=guide_legend(override.aes=list(size=1, alpha=1, linetype=1)))
      
      UserValue$doses3 <- as.numeric(strsplit(input$UserDoses3, ";")[[1]])
      UserValue$pats3 <- as.numeric(strsplit(input$UserPatients3, ";")[[1]])
      if(length(UserValue$doses3)==length(UserValue$pats3)&& length(UserValue$pats3)>0){
        t<- c("optimal design", "user defined design")
        datleg <-  data.frame(t, test=1, y=1)
        plotdat2 <- data.frame(doses=UserValue$doses3,pats= UserValue$pats3)
        pl3<- ggplot(data=da,aes(x=optValue$doses3,y=optValue$pats3))+geom_linerange(ymin=0,ymax= optValue$pats3) +
          labs(x="Dose", y="Patients")+ylim(0,input$npats*0.7)+theme(axis.text = element_text(size=rel(1.1)),
                                                                     axis.title = element_text(size=rel(1.1)))
        pl3<- pl3 + geom_point(data=plotdat2,aes(x=plotdat2$doses,y=plotdat2$pats),colour="#89D329",size=rel(2))    +
          xlim(input$dmin,input$dmax)+
          geom_line(data = datleg, aes(colour = t, x = test, y = y, group=1), alpha = 0)+
          geom_point(data = datleg, aes(colour=t, group=1), alpha=0, x=1, y=1) +
          scale_colour_manual(values = c("black","#89D329"))+    
          theme(legend.text=element_text(size=rel(1.1)), legend.key = element_rect(size=5),
                legend.margin=margin(l=-10),legend.position = "bottom",legend.title =element_blank())    +   
          guides(colour=guide_legend(override.aes=list(size=c(1,2.5), alpha=1, shape=c(NA,16),linetype=c(1,0))))
      }
      suppressWarnings(print(pl3))})
    
    ####
    # Plot theoretical curve of the chosen model
    ####
    output$modelFunction1 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      plotres <- ComputeModelFunction(input,1)
      suppressWarnings(print(plotres))
    })
    output$modelFunction2 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      plotres <- ComputeModelFunction(input,2)
      suppressWarnings(print(plotres))
    })
    output$modelFunction3 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      plotres <- ComputeModelFunction(input,3)
      suppressWarnings(print(plotres))
    })
    
    ####
    # Simulate new data, fit model and plot
    ####
    output$simDesign1 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      input$gosim1
      plotres <- performSimulation(input, 1, optValue, UserValue)
      suppressWarnings(print(plotres))
    })
    
    output$simDesign2 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      input$gosim2
      plotres <- performSimulation(input, 2, optValue, UserValue)
      suppressWarnings(print(plotres))
    })
    
    output$simDesign3 <- renderPlot({
      validate(need(input$ymin < input$ymax , 'Maximum response needs to be greater than minimum response.'))
      input$gosim3
      plotres <- performSimulation(input, 3, optValue, UserValue)
      suppressWarnings(print(plotres))
    })
    
    
    # Creates a table with the optimal design
    output$viewOpt1 <- renderTable({
      
      dat <- matrix(c(optValue$doses1,round(optValue$pats1,1)), nrow=2, byrow=T)
      colnames(dat) <- NULL
      rownames(dat) <- c("Dose", "Patients")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    output$viewOpt2 <- renderTable({
      
      dat <- matrix(c(optValue$doses2,round(optValue$pats2,1)), nrow=2, byrow=T)
      colnames(dat) <- NULL
      rownames(dat) <- c("Dose", "Patients")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    output$viewOpt3 <- renderTable({
      
      dat <- matrix(c(optValue$doses3,round(optValue$pats3,1)), nrow=2, byrow=T)
      colnames(dat) <- NULL
      rownames(dat) <- c("Dose", "Patients")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    
    
    # Computes Efficiency
    output$viewEff1 <- renderTable({
      dat <- ComputeEfficiency(UserValue, optValue,input, 1)
      colnames(dat) <- NULL
      rownames(dat) <- c("Patients","Efficiency [%]", "Additional patients needed")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    
    output$viewEff2 <- renderTable({
      dat <- ComputeEfficiency(UserValue,optValue, input, 2)
      colnames(dat) <- NULL
      rownames(dat) <- c("Patients","Efficiency [%]", "Additional patients needed")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    
    output$viewEff3 <- renderTable({
      dat <- ComputeEfficiency(UserValue,optValue, input, 3)
      colnames(dat) <- NULL
      rownames(dat) <- c("Patients","Efficiency [%]", "Additional patients needed")
      dat
    }, digits=1, align=NULL, rownames=T, colnames=F)
    
  }
)
