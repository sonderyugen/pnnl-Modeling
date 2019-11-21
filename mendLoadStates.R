#' External data name accomodation // can be modified to accomodate different naming protocols regarding initial carbon pool state values
#'
#' @param st  // raw external data
#' @param sts // placeholder state values via analytical steady state approximation
#'
#' @return // state matrix
#' @export
#'
#' @examples
#' varName <- mendLoadStates(st, sts)
#'
mendLoadStates <- function(st, sts) {
## can call mendNameStates() once issue is resolved
  nameList <- names(st)
  if(length(st) != length(sts)) {
    tSts <- sts[["T"]]; tSt <- st[["T"]]

    for (i in 1:length(nameList)) {
      sts[[nameList[[i]]]] <- st[[nameList[[i]]]]
    }

    X <- sts[!names(sts) %in% nameList]; names(X) <- names(sts[!names(sts) %in% nameList])
    X <- tSt * (X / tSts)

    name <- names(X)
    for (i in 1:length(name)) {
      sts[[name[[i]]]] <- X[[name[[i]]]]
    }
    return(sts)
  } else {
    return(st)
  }
}
