#' run_dosedesignR - Launches the Shiny application
#'
#' @export run_dosedesignR
#'
#' @description
#' Starts the dosedesignR application in the client's browser.
#'
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#' @param ColorBG background color (defaults to "#424242")
#' @param ColorText text color (defaults to "#828282")
#' @param numberCandidateModels number of panels(default: 6)
#'
#' @keywords dosedesignR
#'
#' @details Further information on how to use this application can be found in the vignette of this package.
#'
#'
#' @examples
#' if(interactive()){
#' ## Launch application on localhost (127.0.0.1)
#' ## -------------------------------------------
#' ## By default run_dosedesignR starts the application on localhost
#' ## and a randomly selected port (e.g. 9876), in which case you can connect
#' ## to the running application by navigating your browser to
#' ## http://localhost:9876.
#' run_dosedesignR()
#'
#' ## Launch application on a different host
#' ## --------------------------------------
#' ## You can also run the application on a different host
#' ## by specifying a hostname and port. Just make sure to
#' ## use an open port on your machine. Here "open" means
#' ## that the port should not be used by another service
#' ## and the port is opened by your firewall.
#' run_dosedesignR(host="your-hostname", port=8888)
#'
#'
#' ## Make the application available to your coworkers
#' ## ------------------------------------------------
#' ## within your local area network even without a
#' ## dedicated Shiny server. The value set through the
#' ## host argument says to accept any connection (not just from localhost).
#' ## Then take note of your local IP (if you are under linux,
#' ## you can see it through ifconfig). Say your IP is 192.168.1.70.
#' ## Your colleagues can use your app by inserting in the address
#' ## bar of their browser 192.168.1.70:8888, i.e. your IP followed
#' ## by : and the port number you selected.
#' run_dosedesignR(host="0.0.0.0", port=8888)
#'
#' ## Launch application on a different browser
#' ## ----------------------------------------
#' ## To run the shiny app on a different browser than your standard browser
#' ## use the "browser" argument to set the path to the respective .exe file.
#' runDoseDesignR(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#' }
#'
#'
#'



run_dosedesignR <-  function(
  ColorBG = "#424242",
  ColorText = "#828282",
  browser = getOption("shiny.launch.browser", interactive()),
  host = NULL,
  port = NULL,
  numberCandidateModels = 4
) {

    appDir <- system.file("shiny-app", "dosedesignR", "app.R",
                          package = "dosedesignR")
    if (appDir == "") {
      stop("Could not find Shiny app directory. Try re-installing `dosedesignR`.", call. = FALSE)
    }

    colortagdosdesR <- paste("body {background-color: ", ColorBG, " ; color: ", ColorText, " }", sep = "")

    shiny::shinyOptions(colortagdosdesR = colortagdosdesR)
    shiny::shinyOptions(numberCandidateModels = numberCandidateModels)

     ui <- server <- NULL

    source(appDir, local=TRUE)

    source(system.file("shiny-app", "dosedesignR", "doseDesign_Module.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "simModule_Module.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "doseLevel_Module.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "innerdoseLevel_Module.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "Optimization.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "Simulation.R",
                          package = "dosedesignR"))
    source(system.file("shiny-app", "dosedesignR", "Visualize.R",
                          package = "dosedesignR"))

    shiny::getShinyOption("numberCandidateModels", numberCandidateModels)

    app <- shiny::shinyApp(ui, server)

    shiny::runApp(appDir, display.mode = "normal", host = host, port = port, launch.browser = browser)
  }
