#' Get the path of the POF toy example files
#' @description This function provides the path of the microdata from year 2017-2018 of the POF toy example files, loaded with this package.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param path Name of file. If \code{NULL}, the POF toy example files names will be listed.
#' @return A vector with names of all the available POF toy example files or the path for specific requested POF toy example file.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{get_pof} for downloading, labeling, deflating and creating survey design object for POF microdata.\cr \link[POFIBGE]{read_pof} for reading POF microdata.\cr \link[POFIBGE]{pof_labeller} for labeling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_deflator} for adding deflator variables to POF microdata.\cr \link[POFIBGE]{pof_design} for creating POF survey design object.
#' @examples
#' \donttest{
#' pof_example()
#' pof_example(path="exampledata.txt")
#' pof_example(path="input_example.txt")
#' pof_example(path="dictionaryexample.xls")
#' pof_example(path="deflatorexample.xls")}
#' @export

pof_example <- function(path = NULL) {
  message("The pof_example function is under development and will be available soon in package POFIBGE.")
  return(NULL)
  if (is.null(path)) {
    dir(system.file("extdata", package="POFIBGE"))
  }
  else {
    system.file("extdata", path, package="POFIBGE", mustWork=TRUE)
  }
}
