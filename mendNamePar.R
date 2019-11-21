mendNamePar <- function(nameList) {
  charList <- list()
  for (i in 1:length(charList)) {
    if (!grep(paste(charList[[i]], collapse = "|"), nameList, fixed = TRUE)) {
      nameList[[grep(paste(charList[[i]], collapse = "|"), nameList, fixed = TRUE)]] <- names(charList[[i]])
    }
  }

  strList <- list()

  for (i in 1:length(strList)) {
    if (exists(grep(paste(strList[[i]], collapse = "|"), nameList, ignore.case = TRUE))) {
      nameList[[grep(paste(strList[[i]], collapse = "|"), nameList, ignore.case = TRUE)]] <- names(strList[[i]])
    }
  }
  return(nameList)
}

#  gsub(grep(c("v.p.init", "vp", "vp.init", "vpinit", "v.pinit"), "V.p.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("v.m.init", "vm", "vm.init", "vminit", "v.minit"), "V.m.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("v.d.init", "vd", "vd.init", "vdinit", "v.dinit"), "V.d.init", nameList, ignore.case = TRUE))

#  gsub(grep(c("v.p.cellulose.init", "vcellulose", "vcel", "vcell", "vpcellulose","vpcell", "vpcel", "v.cellulose", "v.cel", "v.cell",
#            "vcelluloseinit", "vcelinit", "vcellinit", "vpcelluloseinit","vpcellinit", "vpcelinit", "v.celluloseinit", "v.celinit", "v.cellinit",
#            "vcellulose.init", "vcel.init", "vcell.init", "vpcellulose.init","vpcell.init", "vpcel.init", "v.cellulose.init", "v.cel.init", "v.cell.init",
#            "v.pcellulose", "v.pcell", "v.pcel", "vp.cellulose", "vp.cell", "vp.cel", "vp.celinit", "v.pcelluloseinit", "v.pcellinit", "v.pcelinit", "vp.celluloseinit", "vp.cellinit", "vp.celinit",
#            "v.pcellulose.init", "v.pcell.init", "v.pcel.init", "vp.cellulose.init", "vp.cell.init", "vp.cel.init"),
#          "V.p.cellulose.init", nameList, ignore.case = TRUE))

#  gsub(grep(c("v.p.lignin.init", "vlignin", "vlig", "vlignin","vplig", "v.lignin", "v.lig",
#            "vlignininit", "vliginit","vpliginit","vpliginit", "v.liginit", "v.liginit",
#            "vlignin.init", "vlig.init","vplignin.init","vplig.init","v.lignin.init", "v.lig.init",
#            "v.plignin", "v.lig", "vp.lignin", "vp.lig", "vp.liginit", "vp.lignininit", "v.plignininit", "v.pliginit",
#            "v.lignin.init", "v.plig.init", "vp.lignin.init", "vp.lig.init"),
#          "V.p.lignin.init", nameList, ignore.case = TRUE))

#  gsub(grep(c("k.p.init", "kp", "kp.init", "kpinit", "k.pinit"), "K.p.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("k.m.init", "km", "km.init", "kminit", "k.minit"), "K.m.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("k.d.init", "kd", "kd.init", "kdinit", "k.dinit"), "K.d.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("k.ba", "k.b.a", "kb.a", "kba"), "K.ba", nameList, ignore.case = TRUE))
#  gsub(grep(c("k.ads", "kads"), "K.ads", nameList, ignore.case = TRUE))
#  gsub(grep(c("k.des", "kdes"), "K.des", nameList, ignore.case = TRUE))
#  gsub(grep(c("mr", "m.r"), "Mr", nameList, ignore.case = TRUE))
#  gsub(grep(c("r"), "r", nameList, ignore.case = TRUE))
#  gsub(grep(c("ec.init", "ecinit"), "Ec.init", nameList, ignore.case = TRUE))
#  gsub(grep(c("ec.kc", "eckc"), "Ec.Kc", nameList, ignore.case = TRUE))
#  gsub(grep(c("frac.fd", "fd", "fracfd", "ffd", "f.fd"), "Frac.fd", nameList, ignore.case = TRUE))
#  gsub(grep(c("frac.gd", "gd", "fracgd", "fgd", "f.gd"), "Frac.gd", nameList, ignore.case = TRUE))
#  gsub(grep(c("frac.p.ep", "ep", "fracep", "fep", "f.ep", "pep", "fracpep", "fpep", "f.pep", "p.ep", "fracp.ep", "fp.ep", "f.p.ep"), "Frac.P.ep", nameList, ignore.case = TRUE))
#  gsub(grep(c("frac.p.em", "em", "fracem", "fem", "f.em", "pem", "fracpem", "fpem", "f.pem", "p.em", "fracp.em", "fp.em", "f.p.em"), "Frac.p.em", nameList, ignore.case = TRUE))
#  gsub(grep(c("r.ep", "rep"), "R.ep", nameList, ignore.case = TRUE))
#  gsub(grep(c("r.em", "rem"), "R.em", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.d", "id"), "I.d", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.p", "ip"), "I.p", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.p.cellulose", "ipcellulose", "i.pcellulose", "ip.cellulose", "i.p.cell", "ipcell", "i.pcell", "ip.cell", "i.p.cel", "ipcel", "i.pcel", "ip.cel"), "I.p.cellulose", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.p.lignin", "iplignin", "i.lignin", "ip.lignin", "i.p.lig", "lig", "i.lig", "ip.lig"), "I.p.lignin", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.f", "if"), "I.f", nameList, ignore.case = TRUE))
#  gsub(grep(c("i.i", "ii"), "I.i", nameList, ignore.case = TRUE))
#  gsub(grep(c("q.max", "qmax", "q", "qval"), "Q.max", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.p.lignin", "eaplignin", "ea.plignin", "eap.lignin", "ea.p.lig", "eaplig", "ea.plig", "eap.lig"), "Ea.p.lignin", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.p.cellulose", "eapcellulose", "ea.cellulose", "eap.cellulose", "ea.p.cell", "eapcell", "ea.cell", "eap.cell","ea.p.cel", "eapcel", "ea.cel", "eap.cel"), "Ea.p.cellulose", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.d", "ead"), "Ea.d", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.m", "eam"), "Ea.m", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.k", "eak"), "Ea.K", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.mr", "eam.r", "eamr", "ea.m.r"), "Ea.mr", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.ads", "eaads"), "Ea.ads", nameList, ignore.case = TRUE))
#  gsub(grep(c("ea.des", "eades"), "Ea.des", nameList, ignore.case = TRUE))


##  #' Variable input naming conversion approach
##  #'
##  #' @param nameList // user specified list of state names
##  #'
##  #' @return nameList // renamed list of state names
##  #' @export
##  #'
##  #' @examples
##  #' varName <- mendNamePar(nameList)
##  #'
