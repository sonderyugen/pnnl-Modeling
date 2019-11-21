#' Initial pool values calculated anlytically for standard steady-state values
#'
#' @param temp        // inital temperature value
#' @param parameters  // inital parameter values
#'
#' @return statesOut // matrix of initial state values for use in solving, optimization, and calibration
#' @export
#'
#' @examples
#' varName <- mendSteadyState(parameters, temp)
#'
mendSteadyState <- function (parameters, temp) {
## steady state calculations ##########################################################################
with(as.list(c(parameters)), {
  A = 1 - (Ec.init - (Ec.Kc * temp)) + (1 - Frac.P.ep - Frac.P.em) * (Ec.init - (Ec.Kc * temp)) * (1 - Frac.gd) * (I.f + 1)

  P.lignin = K.p.init / ((V.p.cellulose.init * ((Frac.P.ep / R.ep) * ((Ec.init - Ec.Kc * temp)) / A) * (I.f + 1)) - 1)
  P.cellulose = K.p.init / ((V.p.lignin.init * ((Frac.P.ep / R.ep) * ((Ec.init - Ec.Kc * temp)) / A) * (I.f + 1)) - 1)
  P = P.lignin + P.cellulose
  M = K.m.init / ((V.m.init * (Frac.P.em / R.em) * ((Ec.init - (Ec.Kc * temp)) / ((1 - Frac.fd) * A)) * (1 + I.f)) - 1)
  Q = Q.max / (1 + (V.d.init / (Mr * K.d.init * K.ba)))
  B = (I.d + I.p) / (((1 / (Ec.init - (Ec.Kc * temp))) - 1) * Mr)
  D = K.d.init / (V.d.init / Mr)
  EP = B * ((Mr * Frac.P.ep) / R.ep)
  EM = B * ((Mr * Frac.P.em) / R.em)

  T = Q + D + M + P + B + EP + EM
  statesOut <- c(Q = Q, D = D, M = M, P.lignin = P.lignin, P.cellulose = P.cellulose, P = P, B = B, EP = EP, EM = EM, C = 0, T = T)
  return(statesOut)
})
}
