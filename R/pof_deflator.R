#' Add deflator variables to POF microdata
#' @description This function adds deflator variables to POF microdata. For deflation of income variables, the documentation provided through the following address must be used: (see the file 'POFIBGE_Deflator.pdf' at the general documentation folder) - \url{https://ftp.ibge.gov.br/Orcamentos_Familiares/}.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pof A tibble of POF microdata read with \code{read_pof} function.
#' @param deflator.file The deflator file for selected survey available on official website: (select the deflator zip file at the general documentation folder) - \url{https://ftp.ibge.gov.br/Orcamentos_Familiares/}.
#' @return A tibble with the data provided from POF survey and the deflator variables added for use.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{get_pof} for downloading, labeling, deflating and creating survey design object for POF microdata.\cr \link[POFIBGE]{read_pof} for reading POF microdata.\cr \link[POFIBGE]{pof_labeller} for labeling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_design} for creating POF survey design object.\cr \link[POFIBGE]{pof_example} for getting the path of the POF toy example files.
#' @examples
#' \donttest{
#' # Using data read from disk
#' input_path <- pof_example(path="input_example.txt")
#' data_path <- pof_example(path="exampledata.txt")
#' dictionary.path <- pof_example(path="dictionaryexample.xls")
#' deflator.path <- pof_example(path="deflatorexample.xls")
#' pof.df <- read_pof(microdata=data_path, input_txt=input_path, vars=c("V0407","V0408"))
#' pof.df <- pof_labeller(data_pof=pof.df, dictionary.file=dictionary.path)
#' pof.df <- pof_deflator(data_pof=pof.df, deflator.file=deflator.path)}
#' \donttest{
#' # Downloading data
#' pof.df2 <- get_pof(year=2017, selected=FALSE, anthropometry=FALSE, vars=c("V0407","V0408"),
#'                        labels=TRUE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' deflator.path2 <- pof_example(path="deflatorexample.xls")
#' pof.df2 <- pof_deflator(data_pof=pof.df2, deflator.file=deflator.path2)}
#' @export

pof_deflator <- function(data_pof, deflator.file) {
  message("The pof_deflator function is under development and will be available soon in package POFIBGE.")
  return(NULL)
  if (sum(class(data_pof) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("V0020", "V0001") %in% names(data_pof)))) {
      data_pof <- data_pof[, !names(data_pof) %in% c("Deflator"), drop=FALSE]
      deflator <- suppressMessages(readxl::read_excel(deflator.file))
      colnames(deflator)[c(1:2)] <- c("V0020", "V0001")
      deflator$V0001 <- as.factor(deflator$V0001)
      if (identical(intersect(levels(deflator$V0001), levels(as.factor(data_pof$V0001))), character(0)) & length(levels(deflator$V0001)) == length(levels(as.factor(data_pof$V0001)))) {
        levels(deflator$V0001) <- levels(as.factor(data_pof$V0001))
      }
      data_pof <- merge(x=data_pof, y=deflator, by.x=c("V0020", "V0001"), by.y=c("V0020", "V0001"), all.x=TRUE, all.y=FALSE)
      if (!(FALSE %in% (c("ID_DOMICILIO") %in% names(data_pof)))) {
        data_pof <- data_pof[order(data_pof$V0024, data_pof$ID_DOMICILIO, data_pof$C00301),]
      }
      else {
        data_pof <- data_pof[order(data_pof$V0024, data_pof$UPA_POF, data_pof$V0006_POF, data_pof$C00301),]
      }
      data_pof <- tibble::as_tibble(data_pof)
    }
    else {
      message("Merge variables required for adding deflator variables are missing.")
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so adding deflator variables is not possible.")
  }
  return(data_pof)
}
