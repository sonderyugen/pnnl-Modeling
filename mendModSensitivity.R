# #' Function for deSolve systems solver 'func' argument as utilized in 'mendSensitivity()'//sensitivity::desla() for parameteric sensitivity analysis
# #'
# #' @param vars         // range of selected parameter values for use in 'mendSensitivity()'//sensitivity::desla()
# #' @param times        // time sequence
# #' @param states       // inital pool state values
# #' @param parameters   // intial parameter values sans values assigned to 'vars' by 'mendSensitivity()'
# #'
# #' @return firstOrder   // list of equations for integration by solver
# #' @export
# #'
# #' @examples
# #' varName <-  mendMod(vars, times, states, parameters)
# #'
#mendModSensitivity <- function(times, states, parameters, vars) {
#  with(as.list(c(vars, states, parameters)), {
#    ## functions ##############################################################################################################################
#    ## enzyme kinetics functions
#    Temp = 285
#    F.v.p.lignin <- V.p.lignin.init * exp(-(Ea.p.lignin / r) * ((1 / Temp.cur.K) - (1 / Temp)))               ## Vmax Particulate Organic Carbon lignin
#    F.v.p.cellulose <-  V.p.cellulose.init * exp(-(Ea.p.cellulose / r) * ((1 / Temp.cur.K) - (1 / Temp)))     ## Vmax Particulate Organic Carbon cellulose
#    F.v.d <- V.d.init * exp(-(Ea.d / r) * ((1 / Temp.cur.K) - (1 / Temp)))                                    ## Vmax Dissolved Organic Carbon
#    F.v.m <- V.m.init * exp(-(Ea.m / r) * ((1 / Temp.cur.K) - (1 / Temp)))                                    ## Vmax Mineral-Associated Organic Carbon

#    F.k.p <- K.p.init * exp(-(Ea.K / r) * ((1 / Temp.cur.K) - (1 / Temp)))                          ## Km Particulate Organic Carbon
#    F.k.d <- K.d.init * exp(-(Ea.K / r) * ((1 / Temp.cur.K) - (1 / Temp)))                          ## Km Dissolved Organic Carbon
#    F.k.m <- K.m.init * exp(-(Ea.K / r) * ((1 / Temp.cur.K) - (1 / Temp)))                          ## Km Mineral-Associated Organic Carbon

#    F.k.ads <- K.ads *  exp(-(Ea.ads / r) * ((1 / Temp.cur.K) - (1 / Temp)))                          ##Km MOC adsorption
#    F.k.des <- K.des *  exp(-(Ea.des / r) * ((1 / Temp.cur.K) - (1 / Temp)))                          ##Km MOC desportion

#    F.mr <- Mr * exp(-(Ea.mr / r) * ((1 / Temp.cur.K) - (1 / Temp)))                              ##Maintenance rate


    ## carbon efficiency functions
#    if (CUE) {
#      F.ec <- Ec.init - (Ec.Kc * Temp.cur.C) # - Temp.ref.C)                              ## Carbon Use Efficiency (CUE) value       //Temp.cur.C ~  state pool 'K'
#    } else {
#      F.ec <- Ec.init - (Ec.Kc * Temp.init.C) # - Temp.ref.C)                             ## Carbon Use Efficiency (CUE) value       //Temp.cur.C ~  state pool 'K'
#    }


#    ## differential equation functions
#    F.f1 <- (1 / F.ec) * (F.v.d + F.mr) * ((D * B) / (F.k.d +  D))                                                  ## Dissovled Organic Carbon uptake by Microbial Biomass Organic
#    F.f2.lignin <- ((F.v.p.lignin * EP * P.lignin) / (F.k.p + P))                                                   ## Particulate Organic Carbon Decomposition Lignin
#    F.f2.cellulose <- ((F.v.p.cellulose * EP * P.cellulose) / (F.k.p + P))                                          ## Particulate Organic Carbon Decomposition Cellulose
#    F.f2 <- F.f2.lignin + F.f2.cellulose                                                                            ## Particulate Organic Carbon Decomposition
#    F.f3 <- (F.v.m * EM * M) / (F.k.m + M)                                                                          ## Mineral-associatied Organic Carbon Decomposition
#    F.f4 <- ((1 / F.ec) - 1) * ((F.v.d * B * D) / (F.k.d + D))                                                      ## Microbial Growth Respiration from Dissolved Organic Carbon for Growth
#    F.f5 <- ((1 / F.ec) - 1) * ((F.mr * B * D) / (F.k.d + D))                                                       ## Microbial Growth Respiration for Maintenance
#    F.f6 <- F.k.ads * (1 - (Q / Q.max)) * D                                                                         ## Adsorption of Dissolved Organic Carbon to Mineral-Associated Organic Carbon
#    F.f7 <- F.k.des * (Q / Q.max)                                                                                   ## Desorption of Mineral-Associated Organic Carbon to Dissolved Organic Carbon
#    F.f8 <- ((1 - Frac.P.ep - Frac.P.em) * F.mr) * B                                                                ## Microbial Biomass Organic Carbon Mortality
#    F.f9.M <- Frac.P.em * F.mr * B                                                                                  ## Enzyme Production for Mineral-Associated Organic Carbon Enzymes
#    F.f9.P <- Frac.P.ep * F.mr * B                                                                                  ## Enzyme Production for Particulate Organic Carbon Enzymes
#    F.f10.M <- R.em * EM                                                                                            ## Enzyme Turnover for Mineral-Associated Organic Carbon Enzymes
#    F.f10.P <- R.ep * EP                                                                                            ## Enzyme Turnover for Particulate Organic Carbon Enzymes

    ############################################################################################################################################

    ## Differential equations ##################################################################################################################
#    dP.lignin <- I.p.lignin - F.f2.lignin                                                                     ## POC Lignin
#    dP.cellulose <- ((1 - Frac.gd) * F.f8) + I.p.cellulose - F.f2.cellulose                                   ## POC Cellulose
#    dP <- dP.lignin + dP.cellulose                                                                            ## Particulate Organic Carbon (POC)
#    dM <- ((1 - Frac.fd) * F.f2) - F.f3                                                                       ## Mineral Organic Carbon
#    dQ <- F.f6 - F.f7                                                                                         ## Adsorption/Desorption Flux Pool
#    dB <- F.f1 - (F.f4 + F.f5) - F.f8 - (F.f9.P + F.f9.M)                                                     ## Microbial Biomass Organic Carbon
#    dD <- I.d + (Frac.fd * F.f2) + (Frac.gd * F.f8) + F.f3 + (F.f10.P + F.f10.M) - F.f1 - (F.f6 - F.f7)       ## Dissolved Organic Carbon
#    dEP <- F.f9.P - F.f10.P                                                                                   ## Particulate Organic Carbon Enzyme Carbon
#    dEM <- F.f9.M - F.f10.M                                                                                   ## Mineral-Associated Organic Carbon Enzyme Carbon
#    dC <-  D(F.f4 + F.f5)                                                                                     ## Carbon Dioxide Flux
#    dT <- I.p + I.d - (F.f4 + F.f5)                                                                           ## Total Organic Carbon
#    #    dK <-                                                                                                    ## Temperature Cycle
#    #############################################################################################################################################
#    firstOrder <- list(c(dQ, dD, dM, dP.lignin, dP.cellulose, dP, dB, dEP, dEM, dC, dT)) ## dK, DpH
#    return(firstOrder)
#  })
#}
