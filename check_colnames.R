# ----------------------------------------------------------------------------------------
# R Code for MA2 of "How biased is the literature in Psychological Science?"     
# File content: CHECK OF COLUMN NAMES                     
# Last update: 26 05 2020                                                         
# Authors: Magdalena Siegel & Jakob Pietschnig                                  
# Contact: magdalena.siegel@univie.ac.at                                         
# ----------------------------------------------------------------------------------------


if(type_ES == "z"){
  es <- sprintf("z: %s", "z" %in% colnames(data))
  es.SE <- sprintf("z.SE: %s", "z.SE" %in% colnames(data))
} else if (type_ES == "r") {
  es <- sprintf("r: %s", "r" %in% colnames(data))
  es.SE <- sprintf("r.SE: %s", "r.SE" %in% colnames(data))
} else if (type_ES == "d") {
  es <- sprintf("d: %s", "d" %in% colnames(data))
  es.SE <- sprintf("d.SE: %s", "d.SE" %in% colnames(data))
} else if (type_ES == "g") {
  es <- sprintf("g: %s", "g" %in% colnames(data))
  es.SE <- sprintf("g.SE: %s", "g.SE" %in% colnames(data))
} else if (type_ES == "OR") {
  es <- sprintf("ai, bi: %s %s", "ai" %in% colnames(data), "bi" %in% colnames(data))
  es.SE <- sprintf("ci, di: %s %s", "ci" %in% colnames(data), "di" %in% colnames(data))
} else if (type_ES == "logOR") {
  es <- sprintf("logOR: %s", "logOR" %in% colnames(data))
  es.SE <- sprintf("logOR.SE: %s", "logOR.SE" %in% colnames(data))
}

report.colnames <- c(
  sprintf("studyname: %s", "studyname" %in% colnames(data)),
  sprintf("year: %s", "year" %in% colnames(data)),
  sprintf("n: %s", "n" %in% colnames(data)),
  sprintf("n1: %s n2: %s", "n1" %in% colnames(data), "n2" %in% colnames(data)),
  es,
  es.SE)

rm(es)
rm(es.SE)

cat(report.colnames, sep = "\n")