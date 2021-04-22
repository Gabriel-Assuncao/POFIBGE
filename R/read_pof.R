#' Read POF microdata
#' @description This function reads POF microdata.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param microdata A text file containing microdata from POF survey, available on official website: (select a microdata file, according to the appropriated select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, data) - \url{https://ftp.ibge.gov.br/Orcamentos_Familiares/}.
#' @param input_txt A text file, related to the microdata, containing the input script for SAS, available on official website: (select the dictionary and input zip file, according to the appropriated select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, documentation) - \url{https://ftp.ibge.gov.br/Orcamentos_Familiares/}.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{get_pof} for downloading, labelling, deflating and creating survey design object for POF microdata.\cr \link[POFIBGE]{pof_labeller} for labelling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_deflator} for adding deflator variable to POF microdata.\cr \link[POFIBGE]{pof_design} for creating POF survey design object.\cr \link[POFIBGE]{pof_example} for getting the path of the POF example files.
#' @examples
#' \donttest{
#' input_path <- pof_example(path="input_example.txt")
#' data_path <- pof_example(path="exampledata.txt")
#' pof.df <- read_pof(microdata=data_path, input_txt=input_path, vars="V0408")}
#' @export

read_pof <- function(microdata, input_txt, vars = NULL) {
  message("The read_pof function is under development and will be available soon in package POFIBGE.")
  return(NULL)
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(suppressMessages({readr::read_table2(input_txt, col_names=FALSE) %>% subset(substr(X1, 1, 1) == "@") %>%
      dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"), start=as.numeric(gsub("@", "", X1)), X3=as.integer(chartr("$", " ", X3)), end=start+X3-1)}))
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c("V0001", "UPA_POF", "V0006_POF", "V0015", "V0020", "V0024", "V0028", "V00282", "V00283", "V0029", "V00292", "V00293", "V0030", "V00302", "V00303", "C00301", "M001", "W001", vars))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pof <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  data_pof <- data_pof[(data_pof$V0015 == "01" & !is.na(data_pof$V0015)),]
  return(data_pof)
}
