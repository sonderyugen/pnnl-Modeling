mendNameState <- function(nameList) {
  charList <- list(Q = c("q", "Q"), D = c("d", "D"), B = c("b", "B"), M = c("m", "M"), P = c("p", "P"), C = c("c", "C"), T = c("t", "T", "s", "S"))
  for (i in 1:length(charList)) {
    if (!grep(paste(charList[[i]], collapse = "|"), nameList, fixed = TRUE)) {
      nameList[[grep(paste(charList[[i]], collapse = "|"), nameList, fixed = TRUE)]] <- names(charList[[i]])
    }
  }

  strList <- list(Q = c("qoc", "qpool", "qocpool", "slow", "slowpool"),
                  D = c("doc", "dis", "diss", "disolved", "dpool", "docpool"),
                  B = c("mbc", "bpool", "mbcpool", "mic", "micpool", "microbe", "microbepool"),
                  M = c("moc", "mineral", "minpool", "mineral-associated"),
                  P.lignin = c("p.lignin", "plignin", "pl", "p.l", "plig", "p.lig"),
                  P.cellulose = c("p.cellulose", "pecellulose", "pc", "p.c", "pcel", "p.cel", "pcell", "p.cell"),
                  P = c("poc", "part", "partpool", "particulate","ppool", "particulatepool"),
                  EM = c("em"), EP = ("ep"),
                  C = c("respiration", "resp", "co2"),
                  T = c("total", "soc", "tpool", "spool"))

  for (i in 1:length(strList)) {
    if (exists(grep(paste(strList[[i]], collapse = "|"), nameList, ignore.case = TRUE))) {
      nameList[[grep(paste(strList[[i]], collapse = "|"), nameList, ignore.case = TRUE)]] <- names(strList[[i]])
    }
  }
  return(nameList)
}

#  nameList[[grep(paste(c("qoc", "qpool", "qocpool", "slow", "slowpool", "q"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("qoc", "qpool", "qocpool", "slow", "slowpool", "q"), "Q", ignore.case = TRUE)
#  nameList[[grep(paste(c("doc", "dis", "diss", "disolved", "dpool", "docpool", "d"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("doc", "dis", "diss", "disolved", "dpool", "docpool", "d"), "D", ignore.case = TRUE)
#  nameList[[grep(paste(c("mbc", "bpool", "mbcpool", "mic", "micpool", "microbe", "microbepool", "b"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("mbc", "bpool", "mbcpool", "mic", "micpool", "microbe", "microbepool", "b"), "B", ignore.case = TRUE)
#  nameList[[grep(paste(c("p.lignin", "plignin", "pl", "p.l", "plig", "p.lig"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("p.lignin", "plignin", "pl", "p.l", "plig", "p.lig"), "P.lignin", ignore.case = TRUE)
#  nameList[[grep(paste(c("p.cellulose", "pecellulose", "pc", "p.c", "pcel", "p.cel", "pcell", "p.cell"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("p.cellulose", "pecellulose", "pc", "p.c", "pcel", "p.cel", "pcell", "p.cell"), "P.cellulose", ignore.case = TRUE)
#  nameList[[grep(paste(c("poc", "part", "partpool", "particulate","ppool", "particulatepool", "p"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("poc", "part", "partpool", "particulate","ppool", "particulatepool", "p"), "P", ignore.case = TRUE)
#  nameList[[grep(paste(c("ep"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("ep"), "EP", nameList, ignore.case = TRUE)
#  nameList[[grep(paste(c("em"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("em"), "EM", nameList, ignore.case = TRUE)
#  nameList[[grep(paste(c("respiration", "resp"), collapse = "|"), nameList, ignore.case = TRUE), "c"]] <- mgsub(nameList, c("respiration", "resp", "co2", "c"), "C", nameList, ignore.case = TRUE)
#  nameList[[grep(paste(c("total", "soc", "tpool", "spool", "t", "s"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c( "total", "soc", "tpool", "spool", "s", "t"), "T", nameList, ignore.case = TRUE)
#  nameList[[grep(paste(c("moc", "mineral", "minpool", "mineral-associated"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("moc", "mineral", "minpool", "mineral-associated", "m"), "M", ignore.case = TRUE)
#  nameList[[grep(paste(c("M", "m"), collapse = "|"), nameList, ignore.case = TRUE)]] <- mgsub(nameList, c("moc", "mineral", "minpool", "mineral-associated", "m"), "M", fixed = TRUE)

##  #' Variable input naming conversion approach
##  #'
##  #' @param nameList // user specified list of state names
##  #'
##  #' @return nameList // renamed list of state names
##  #' @export
##  #'
##  #' @examples
##  #' varName <- mendNameState(nameList)
##  #'
