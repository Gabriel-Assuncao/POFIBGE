#' Create POF survey object with its sample design
#' @description This function creates POF survey object with its sample design for analysis using \code{survey} package functions.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param data_pof A tibble of POF microdata read with \code{read_pof} function.
#' @return An object of class \code{survey.design} with the data from POF and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{get_pof} for downloading, labelling, deflating and creating survey design object for POF microdata.\cr \link[POFIBGE]{read_pof} for reading POF microdata.\cr \link[POFIBGE]{pof_labeller} for labelling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_deflator} for adding deflator variable to POF microdata.\cr \link[POFIBGE]{pof_example} for getting the path of the POF example files.
#' @examples
#' \donttest{
#' # Using data read from disk
#' input_path <- pof_example(path="input_example.txt")
#' data_path <- pof_example(path="exampledata.txt")
#' dictionary.path <- pof_example(path="dictionaryexample.xls")
#' deflator.path <- pof_example(path="deflatorexample.xls")
#' pof.df <- read_pof(microdata=data_path, input_txt=input_path, vars="V0408")
#' pof.df <- pof_labeller(data_pof=pof.df, dictionary.file=dictionary.path)
#' pof.df <- pof_deflator(data_pof=pof.df, deflator.file=deflator.path)}
#' \donttest{
#' pof.svy <- pof_design(data_pof=pof.df)
#' # Calculating expenses or acquisitions rate
#' if (!is.null(pof.svy)) survey::svymean(x=~V0408, design=pof.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' pof.df2 <- get_pof(year=2017, selected=FALSE, anthropometry=FALSE, vars="V0408",
#'                        labels=TRUE, deflator=TRUE, design=FALSE, savedir=tempdir())
#' pof.svy2 <- pof_design(data_pof=pof.df2)
#' # Calculating expenses or acquisitions rate
#' if (!is.null(pof.svy2)) survey::svymean(x=~V0408, design=pof.svy2, na.rm=TRUE)}
#' @export

pof_design <- function(data_pof) {
  message("The pof_design function is under development and will be available soon in package POFIBGE.")
  return(NULL)
  if (sum(class(data_pof) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA_POF", "V0024", "V0028", "V00282", "V00283") %in% names(data_pof))) |
        !(FALSE %in% (c("UPA_POF", "V0024", "V0029", "V00292", "V00293") %in% names(data_pof))) |
        !(FALSE %in% (c("UPA_POF", "V0024", "V0030", "V00302", "V00303") %in% names(data_pof)))) {
      options(survey.lonely.psu="adjust")
      if (!(FALSE %in% (c("UPA_POF", "V0024", "V0028", "V00282", "V00283") %in% names(data_pof)))) {
        data_prior <- survey::svydesign(ids=~UPA_POF, strata=~V0024, data=data_pof, weights=~V0028, nest=TRUE)
        popc.types <- data.frame(V00283=as.character(unique(data_pof$V00283)), Freq=as.numeric(unique(data_pof$V00282)))
        popc.types <- (popc.types[order(popc.types$V00283),])
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00283, population=popc.types)
      }
      else if (!(FALSE %in% (c("UPA_POF", "V0024", "V0029", "V00292", "V00293") %in% names(data_pof)))) {
        data_prior <- survey::svydesign(ids=~UPA_POF, strata=~V0024, data=data_pof, weights=~V0029, nest=TRUE)
        popc.types <- data.frame(V00293=as.character(unique(data_pof$V00293)), Freq=as.numeric(unique(data_pof$V00292)))
        popc.types <- (popc.types[order(popc.types$V00293),])
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00293, population=popc.types)
      }
      else {
        data_prior <- survey::svydesign(ids=~UPA_POF, strata=~V0024, data=data_pof, weights=~V0030, nest=TRUE)
        popc.types <- data.frame(V00303=as.character(unique(data_pof$V00303)), Freq=as.numeric(unique(data_pof$V00302)))
        popc.types <- (popc.types[order(popc.types$V00303),])
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00303, population=popc.types)
      }
    }
    else {
      message("Weight variables required for sample design are missing.")
      data_posterior <- data_pof
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so applying another design is not possible.")
    data_posterior <- data_pof
  }
  return(data_posterior)
}
