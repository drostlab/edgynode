#' @title General BEELINE Parser
#' @description This function provides users with a wrapper function to parse the output files
#' of any BEELINE tool.
#' @param data_folder a file path to the BEELINE output folder storing all individual BEELINE tool runs (output folders).
#' @param name_tool name of the individual BEELINE tool for which output shall be imported (see tool collection with \code{\link{beeline_tool_collection}}).
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @seealso \code{\link{network_import_genie}}, \code{\link{network_import_grisli}}
#' @examples
#' # import BEELINE output file
#' beeline_out_folder <- system.file('beeline_examples', package = 'edgynode')
#' beeline_list <- beeline_parser(beeline_out_folder, beeline_tool_collection())
#' # look at example
#' beeline_list
#' @export

beeline_parser <- function(data_folder, name_tool) {

  beeline_tools <- list.files(data_folder)
  if (!all(is.element(beeline_tools, beeline_tool_collection())))
    warning("The input folder '", data_folder, "' misses the following folders: ", paste0(setdiff(beeline_tools, beeline_tool_collection()), collapse = ", "))

   res <- 0
  # res0 <- 1
  # res1 <- 2



  if (!file.exists(data_folder))
    stop("Please provide a valid folder path.", call. = FALSE)

  if (name_tool == "GENIE3") {
    res <- genie(file.path(data_folder,
                            "GENIE3",
                            "outFile.csv",
                            fsep = "/"))
  }

   if (name_tool == "GRISLI") {
     res <- grisli(file.path(data_folder,
                              "GRISLI",
                              "0",
                              "outFile.txt",
                              fsep = "/"))
   }
  #   res1 <- GRISLI(file.path(data_folder,
  #                            "GRISLI",
  #                            "1",
  #                            "outFile.txt",
  #                            fsep = "/"))
  # }

  # if (name_tool == "GRNBOOST2"){
  #   res <- GRNBOOST2(file.path(data_folder,
  #                           "GRNBOOST2",
  #                           "outFile.txt",
  #                           fsep = "/"))
  # }

  # if (name_tool == "GRNVBEM"){
  #   res0 <- GRNVBEM(file.path(data_folder,
  #                            "GRNVBEM",
  #                            "outFile0.txt",
  #                            fsep = "/"))
  #   res1 <- GRNVBEM(file.path(data_folder,
  #                            "GRISLI",
  #                            "outFile1.txt",
  #                            fsep = "/"))
  # }

  # if (name_tool == "LEAP"){
  #   res0 <- LEAP(file.path(data_folder,
  #                            "LEAP",
  #                            "outFile0.txt",
  #                            fsep = "/"))
  #   res1 <- LEAP(file.path(data_folder,
  #                            "LEAP",
  #                            "outFile1.txt",
  #                            fsep = "/"))
  # }

  if (name_tool == "PIDC"){
    res <- pidc(file.path(data_folder,
                          "PIDC",
                          "outFile.txt",
                          fsep = "/"))
  }

  if (name_tool == "PPCOR"){
    res <- ppcor(file.path(data_folder,
                           "PPCOR",
                           "outFile.txt",
                           fsep = "/"))
  }

  # if (name_tool == "SCINGE"){
  #   res0 <- SCINGE(file.path(data_folder,
  #                            "SCINGE",
  #                            "0",
  #                            "SCINGE_Ranked_Edge_List.txt",
  #                            fsep = "/"))
  #   res1 <- SCINGE(file.path(data_folder,
  #                            "SCINGE",
  #                            "1",
  #                            "SCINGE_Ranked_Edge_List.txt",
  #                            fsep = "/"))
  # }

  # if (name_tool == "SCODE"){
  #   res0 <- SCODE(file.path(data_folder,
  #                            "SCODE",
  #                            "0",
  #                            "meanA.txt",
  #                            fsep = "/"))
  #   res1 <- SCODE(file.path(data_folder,
  #                            "SCODE",
  #                            "1",
  #                            "meanA.txt",
  #                            fsep = "/"))
  # }

  # if (name_tool == "SCRIBE"){
  #   res0 <- SCRIBE(file.path(data_folder,
  #                            "SCRIBE",
  #                            "outFile0.csv",
  #                            fsep = "/"))
  #   res1 <- SCRIBE(file.path(data_folder,
  #                            "SCRIBE",
  #                            "outFile1.csv",
  #                            fsep = "/"))
  # }

  # if (name_tool == "SINCERITIES"){
  #   res0 <- SINCERITIES(file.path(data_folder,
  #                            "SINCERITIES",
  #                            "outFile0.txt",
  #                            fsep = "/"))
  #   res1 <- SINCERITIES(file.path(data_folder,
  #                            "SINCERITIES",
  #                            "outFile1.txt",
  #                            fsep = "/"))
  # }

  return(res)

}
