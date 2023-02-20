.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the dosedesignR app! Please use function run_dosedesignR() to start the application.")
  shiny::addResourcePath('www', system.file("www", package = "dosedesignR"))
}
