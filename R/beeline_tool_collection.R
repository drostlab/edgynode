#' @title Names of all individual BEELINE tools
#' @description This is a list of all individual BEELINE tools for which output folders can be created.
#' @export
beeline_tool_collection <- function() {
  beeline_tool_collection <-
    c(
      "GENIE3",
      "GRISLI",
      "GRNBOOST2",
      "GRNVBEM",
      "LEAP",
      "PIDC",
      "PPCOR",
      "SCINGE",
      "SCODE",
      "SCRIBE",
      "SINCERITIES"
    )
  return(beeline_tool_collection)
}

