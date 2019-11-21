#' External File import evaluator for unique file types including - 'xls', 'xlsx', 'csv', 'mtb', 'txt'
#'
#' @param fileInput
#'
#' @return fOut a data object containing standard parameter, state, time series data
#' @export
#' @importFrom xfun file_ext
#'
#' @examples
#' varName <- mendRead(fileInput)
#'
mendRead <- function(fileInput) {
  fIn <- fileInput
  fType <- file_ext(fIn)
  fOut <-   switch(fType,
              "xls" = ,
              "xlsx" = {
                  fIn %>%
                  excel_sheets() %>%
                  set_names() %>%
                  map(read_excel, path = fIn)
                },
              "csv" = {read.csv(fIn)},
              "mtp" = {read.mtp(fIn)},
              "txt" = {read.table(fIn)},
              stop("Incorrect file format. Please use excel, csv, txt, or mtp files. Exiting now.")
         )
  return(fOut)
}
