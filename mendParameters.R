#' Standard MEND parameter values per original authors // for use as standard placeholders when external data is limited
#'
#' @return parameters // standard parameter values for use in solving, optimization, and calibration
#' @export
#'
#' @examples
#' varName <- mendParameters()
#'
mendParameters <- function() {
  V.p.init = 2.5; V.m.init = 1; V.d.init = 5 * 10^(-4)                 ## Vmax reaction rate constants # POC by EP, MOC by EM, MBC by DOC
  V.p.cellulose.init = 5.02; V.p.lignin.init = 0.96;                    # POC by EP cellulose, POC by EP lignin
                                                                        # (mg C * mg C^-1 *h^-1)


  K.p.init = 50; K.m.init = 250; K.d.init = 0.26                       ## Michaelis-Menten Half-Saturation constants
                                                                        # POC decomposition, MOC decomposition, DOC decomposition
                                                                        # (mg C / g soil)

  K.ba = 6                                                             ## binding affinity
  K.ads = 6 * 10^(-3); K.des = 1 * 10^(-3)                             ## specific adsorption and desportion rates for DOC


  Mr = 2.8 * 10^(-4)                                                   ## Specific Maintenance Factor
                                                                        # (mg C * mg C^-1 *h^-1)

  r = 8.314                                                            ## rydberg constant
                                                                        # J mol^-1 K^-1

  Ec.init = 0.609; Ec.Kc = 0.012                                        ## CUE constant, CUE slope (degrees Celsius^-1)


  Frac.fd = 0.5; Frac.gd = 0.5                                         ## Fraction decomposed POC allocated to DOC, # Fraction dead MBC allocated to DOC
  Frac.P.ep = 1 * 10^(-2); Frac.P.em = 1 * 10^(-2)                     ## Fraction of Mr allocated to production of Extracellular Organic Carbon
                                                                        # fraction Mr for production of EP, fraction of Mr for production of EM


  R.ep = 1 * 10^(-3); R.em = 1 * 10^(-3)                               ## Turnover rate of extracellular Enzyme Organic Carbon
                                                                        # turnover rate for EP, turnover rate for EM
                                                                        # (mg C * mg C^-1 *h^-1)

  I.d = ((1.6 * 10^(-4)) / 2); I.p = ((1.6 * 10^(-4)) / 2)             ## POC inputs, DOC inputs
  I.p.cellulose = I.p/2; I.p.lignin = I.p/2                            ## Assume 50/50 for now
  I.f = (I.d / I.p); I.i = (I.p + I.d)                                 ## input ratio
                                                                        # (mg C * g soil)^-1

  Q.max = 1.7;                                                         ## maximum mineral adsorption capacity
                                                                        # mg C/g

  Ea.p.lignin = 53; Ea.p.cellulose = 37                                ## Activation Energy Particulate
  Ea.d = 47; Ea.m = 47                                                 ## Activation Energy Dissolved, Mineral Associated
  Ea.K = 30; Ea.mr = 20                                                ## Activation Energy Michaelis-Menten, Maintenance
  Ea.ads = 5; Ea.des = 20                                              ## Activation Energy Adsorption, Desorption


  parameters <- c(V.p.init = V.p.init, V.m.init = V.m.init, V.d.init = V.d.init,
                  V.p.cellulose.init = V.p.cellulose.init, V.p.lignin.init = V.p.lignin.init,
                  K.p.init = K.p.init, K.m.init = K.m.init, K.d.init = K.d.init,
                  K.ba = K.ba, K.ads = K.ads, K.des = K.des,
                  Mr = Mr, r = r,
                  Ec.init = Ec.init, Ec.Kc = Ec.Kc,
                  Frac.fd = Frac.fd, Frac.gd = Frac.gd, Frac.P.ep = Frac.P.ep, Frac.P.em = Frac.P.em,
                  R.ep = R.ep, R.em = R.em,
                  I.d = I.d, I.p = I.p, I.p.cellulose = I.p.cellulose, I.p.lignin = I.p.lignin, I.f = I.f, I.i = I.i,
                  Q.max = Q.max,
                  Ea.p.lignin = Ea.p.lignin, Ea.p.cellulose = Ea.p.cellulose,
                  Ea.d = Ea.d, Ea.m = Ea.m, Ea.K = Ea.K, Ea.mr = Ea.mr, Ea.ads = Ea.ads, Ea.des = Ea.des)


  return(parameters)
  # POC(P) - Particulate Organic Carbon
  # MOC(M) - Mineral-Associated Organic Carbon
  # DOC(D) - Dissolved Organic Carbon
  # MBC(B) - Microbial Biomass Organic Carbon
  # EP - Particulate Enzymes
  # EM - Mineral-Associated Enzymes
  # CUE - Carbon Use Efficiency
}
