
#' Profiler QA3
#'
#' @description Starts the shiny profiler QA3 app.
#' This application helps generate QA3 regressions using niskin samples from profiles.
#' Have niskin sample data to hand!.
#'
#' @return starts the profiler QA3 app
#' @export
profiler.QA3app <- function(){
  shiny::runApp(system.file('profiler_QA3', package='cefasMOS'))
}

