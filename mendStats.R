#' Standard MEND range values per original authors // for use as standard placeholders when external data is limited
#'
#' @param parameters // standard parameter values for use in solving, optimization, and calibration
#'
#' @return ranges // standard range values for use in optimization
#' @export
#'
#' @examples
#' varName <- mendStats(parameters)
#'
mendStats <- function(parameters) {
## continous means and standard deviations for implementation is optimx optimization package
with(as.list(parameters), {
  means = c(Ea.p.lignin = Ea.p.lignin, Ea.p.cellulose = Ea.p.cellulose,                              ## activation energies
            Mr = Mr,                                                                                      ## maintenance factor
            Frac.fd = Frac.fd, Frac.gd = Frac.gd,                                                         ## distribution fractions
            Q.max = Q.max,                                                                                ## mineral capacity
            R.ep = R.ep, R.em = R.em,                                                                     ## extracellular enzyme turnover
            Frac.P.ep = Frac.P.ep, Frac.P.em = Frac.P.ep,                                                 ## fraction carbon decay to DOC
            V.m.init = V.m.init, V.d.init = V.d.init,                                ## Vmax decomposition P by EP/M by EM/D uptake by MCB
            K.des = K.des, K.ads = K.ads,                                                    ## binding affinity, desportion rate, specific adsorption rate
            K.p.init = K.p.init, K.m.init = K.m.init, K.d.init = K.d.init,                                ## half-saturation constants decomposition P by EP/M by EM/D uptake by MCB
            I.d = I.d, I.p = I.p                                                                          ## POC/DOC Inputs
  )


  ea.p.lignin = 17; ea.p.cellulose = 15;                                          ## lower case for standard deviation
  mr = ((10^(-3.6+0.7)) - (10^(-3.6)));
  f.d = ((0.8 - 0.2) / 6); g.d = ((0.8 - 0.2) / 6);
  qmax = 1.1;
  r.ep = ((10^(-3.0 + 0.5)) - (10^(-3.0))); r.em = ((10^(-3.0 + 0.5)) - (10^(-3.0)));
  p.ep = ((10^(-2.0 + 0.5)) - (10^(-2.0))); p.em = ((10^(-2.0 + 0.5)) - (10^(-2.0)));
  v.p = ((33.0 - 0.2) / 6); v.m = ((22.0 - 0.05) / 6); v.d = ((10^(-3.3 + 0.7)) - (10^(-3.3)));
  k.ba = 5; k.des = ((10^(-2)) - (10^(-4))); k.ads = sqrt(((k.ba^2) * (k.des^2)) + ((k.ba^2) * (K.des^2)) + ((k.des^2) * (K.ba^2)));
  k.p = sqrt(((((11 - 3) / 6)^2) * ((v.p^2))) + ((((11 - 3) / 6)^2) * (V.p.init^2)) + ((v.p^2) * (6^2))); k.m = sqrt(((((51 - 4) / 14)^2) * (v.m^2)) + ((((51 - 4) / 14)^2) * (V.m.init)) + ((v.m^2) * (14^2))); k.d = 0.12;
  i.d = sqrt(((10^(-3.8 + 0.3) - 10^-3.8)^2) * 0.5); i.p = sqrt(((10^(-3.8 + 0.3) - 10^-3.8)^2) * 0.5)

  sds = c(Ea.p.lignin = ea.p.lignin, Ea.p.cellulose = ea.p.cellulose,
          Mr = mr,
          Frac.fd = f.d, Frac.gd = g.d,
          Q.max = qmax,
          R.ep = r.ep, R.em = r.em,
          Frac.P.ep = p.ep, Frac.P.em = p.em,
          V.m.init = v.m, V.d.init = v.d,
          K.des = k.des, K.ads = k.ads,
          K.p.init = k.p, K.m.init = k.m, K.d.init = k.d,
          I.d = i.d, I.p = i.p
  )
  maximum = means + 3 * sds
  minimum = means - 3 * sds
  ranges <- list(mean = means, sd = sds, maximum = maximum, minimum = minimum)
})
}
