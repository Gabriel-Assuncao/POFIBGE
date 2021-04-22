#' Download, label, deflate and create survey design object for POF microdata
#' @description Core function of package. With this function only, the user can download a POF microdata from a year and get a sample design object ready to use with \code{survey} package functions.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param year The year of the data to be downloaded. Must be a number equal to 2008 or 2017. Vector not accepted.
#' @param selected Logical value. If \code{TRUE}, the specific questionnaire for selected resident will be used. If \code{FALSE}, the basic questionnaire for household and residents will be used.
#' @param anthropometry Logical value. If \code{TRUE}, the specific questionnaire for the anthropometry module of the selected resident will be used. If \code{FALSE}, the questionnaire defined by the argument \code{selected} of this function will be used. This argument will be used only if \code{year} is equal to 2017.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variable will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} with the data from POF and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[POFIBGE]{read_pof} for reading POF microdata.\cr \link[POFIBGE]{pof_labeller} for labelling categorical variables from POF microdata.\cr \link[POFIBGE]{pof_deflator} for adding deflator variable to POF microdata.\cr \link[POFIBGE]{pof_design} for creating POF survey design object.\cr \link[POFIBGE]{pof_example} for getting the path of the POF example files.
#' @examples
#' \donttest{
#' pof.svy <- get_pof(year=2017, selected=FALSE, anthropometry=FALSE, vars=c("V0406","V0407","V0408"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' if (!is.null(pof.svy)) survey::svymean(x=~V0408, design=pof.svy, na.rm=TRUE)
#' pof.svy2 <- get_pof(year=2017, selected=TRUE, anthropometry=FALSE, vars=c("V4104","V4105"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' if (!is.null(pof.svy2)) survey::svymean(x=~V4104, design=pof.svy2, na.rm=TRUE)
#' pof.svy3 <- get_pof(year=2017, selected=FALSE, anthropometry=TRUE, vars=c("V4104","V4105"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' if (!is.null(pof.svy3)) survey::svymean(x=~V4104, design=pof.svy3, na.rm=TRUE)}
#' @export

get_pof <- function(year, selected = FALSE, anthropometry = FALSE, vars = NULL,
                     labels = TRUE, deflator = TRUE, design = TRUE, savedir = tempdir())
{
  message("The get_pof function is under development and will be available soon in package POFIBGE.")
  return(NULL)
  if (year != 2008 & year != 2017) {
    message("Year must be equal to 2008 or 2017.")
    return(NULL)
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", tempdir()), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  if (year == 2008) {
    pofyear = "2008_2009"
  }
  else if (year == 2017) {
    pofyear = "2017_2018"
  }
  else {
    pofyear = ""
  }
  ftpdir <- paste0("ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_", pofyear, "/Microdados/")
  if (!projmgr::check_internet()) {
    message("The internet connection is unavailable.")
    return(NULL)
  }
  if (httr::http_error(GET(ftpdir, timeout(60)))) {
    message("The microdata server is unavailable.")
    return(NULL)
  }
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n"))
  dataname <- datayear[which(startsWith(datayear, paste0("POF_", year)))]
  if (length(dataname) == 0) {
    message("Data unavailable for selected year.")
    return(NULL)
  }
  docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao/"), dirlistonly=TRUE)), "\n"))
  inputzip <- docfiles[which(startsWith(docfiles, "Dicionario_e_input"))]
  utils::download.file(url=paste0(ftpdir, "Documentacao/", inputzip), destfile=paste0(savedir, "/Dicionario_e_input.zip"), mode="wb")
  utils::unzip(zipfile=paste0(savedir, "/Dicionario_e_input.zip"), exdir=savedir)
  utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  microdataname <- dir(savedir, pattern=paste0("^POF_", year, ".*\\.txt$"), ignore.case=FALSE)
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
  inputname <- dir(savedir, pattern=paste0("^input_POF_", year, ".*\\.txt$"), ignore.case=FALSE)
  inputfile <- paste0(savedir, "/", inputname)
  inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),])[length(inputfile)]
  data_pof <- POFIBGE::read_pof(microdata=microdatafile, input_txt=inputfile, vars=vars)
  if (anthropometry == TRUE & year == 2017) {
    data_pof <- data_pof[(data_pof$W001 == "1" & !is.na(data_pof$W001)),]
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0028", "V00281", "V00282", "V00283", "V0029", "V00291", "V00292", "V00293"))]
    if (selected == TRUE) {
      message("The definition of TRUE for the selected argument will be ignored, since the anthropometry argument was also defined as TRUE.")
    }
  }
  else if (selected == TRUE | (selected == FALSE & anthropometry == TRUE)) {
    data_pof <- data_pof[(data_pof$M001 == "1" & !is.na(data_pof$M001)),]
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0028", "V00281", "V00282", "V00283", "V0030", "V00301", "V00302", "V00303"))]
    if (selected == FALSE) {
      message("The selected argument was defined as true for the use of the anthropometry module, since the year is different from 2017.")
    }
  }
  else {
    data_pof <- data_pof[,!(names(data_pof) %in% c("V0029", "V00291", "V00292", "V00293", "V0030", "V00301", "V00302", "V00303"))]
  }
  if (labels == TRUE) {
    if (exists("pof_labeller", where="package:POFIBGE", mode="function")) {
      dicname <- dir(savedir, pattern=paste0("^dicionario_POF_microdados_", year, ".*\\.xls$"), ignore.case=FALSE)
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
      data_pof <- POFIBGE::pof_labeller(data_pof=data_pof, dictionary.file=dicfile)
    }
    else {
      message("Labeller function is unavailable in package POFIBGE.")
    }
  }
  if (deflator == TRUE) {
    if (exists("pof_deflator", where="package:POFIBGE", mode="function")) {
      ftpdef <- ("ftp://ftp.ibge.gov.br/Orcamentos_Familiares/")
      deffiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdef, "Documentacao_Geral/"), dirlistonly=TRUE)), "\n"))
      defzip <- deffiles[which(startsWith(deffiles, "Deflatores"))]
      utils::download.file(url=paste0(ftpdef, "Documentacao_Geral/", defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^deflator_POF.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
      data_pof <- POFIBGE::pof_deflator(data_pof=data_pof, deflator.file=deffile)
    }
    else {
      message("Deflator function is unavailable in package POFIBGE.")
    }
  }
  if (design == TRUE) {
    if (exists("pof_design", where="package:POFIBGE", mode="function")) {
      data_pof <- POFIBGE::pof_design(data_pof=data_pof)
    }
    else {
      message("Sample design function is unavailable in package POFIBGE.")
    }
  }
  return(data_pof)
}
