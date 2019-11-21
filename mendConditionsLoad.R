#' Model environment conditions: pH, Temp - can be loaded manually or curated from data
#'
#' @param temp  // initial temperature value(s)
#' @param pH    // initial pH values(s)
#' @param kin   // user specified reference temperature value for kinetics equations
#'
#' @return conditionsOut
#' @export
#' @importFrom weathermetrics celsius.to.kelvin
#'
#' @examples
#' conditionsOut <- mendConditionsLoad(temp, pH, kin)
#'
mendConditionsLoad <- function(temp, pH, kin) {
  conditionsOut <- list()

  Temp.ref.C = 25; Temp.init.C = kin; Temp.cur.C = temp                      # degrees celsius
  Temp.init.K = weathermetrics::celsius.to.kelvin(Temp.init.C, round = 0)
  Temp.ref.K = weathermetrics::celsius.to.kelvin(Temp.ref.C, round = 0)
  Temp.cur.K = weathermetrics::celsius.to.kelvin(Temp.cur.C, round = 0)

  pH.ref = 6; pH.init = pH                                                    # pH values

  for (i in 1:length(temp)) {
    conditionsOut[[i]] <- c(Temp.ref.C = Temp.ref.C, Temp.init.C = Temp.init.C, Temp.cur.C = Temp.cur.C[[i]],
                     Temp.ref.K = Temp.ref.K, Temp.init.K = Temp.init.K, Temp.cur.K = Temp.cur.K[[i]],
                     pH.ref = pH.ref, pH.init = pH.init)
  }
  return(conditionsOut)
}
