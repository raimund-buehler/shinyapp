# ----------------------------------------------------------------------------------------
# R Code for MA2 of "How biased is the literature in Psychological Science?"     
# File content: CHECK OF COLUMN NAMES                     
# Last update: 26 05 2020                                                         
# Authors: Magdalena Siegel & Jakob Pietschnig                                  
# Contact: magdalena.siegel@univie.ac.at                                         
# ----------------------------------------------------------------------------------------

####CHECK FOR ES
ESvec <- c("z", "r", "d", "g", "OR", "logOR")
es <- ESvec[which(ESvec %in% colnames(data))]

####CHECK FOR SE
SEvec <- paste(ESvec, ".SE", sep = "")
se <- SEvec[which(SEvec %in% colnames(data))]

####CHECK FOR YEAR
YEARvec <- c("year", "study year", "study_year", "pulication year", "publication_year", "pubyear", "pub_year") 
year <- YEARvec[which(YEARvec %in% colnames(data))]

####CHECK FOR SAMPLE SIZE
SAMPvec <- c("n", "n1", "n2", "n3", "n4", "sample size", "Sample size")
n <- SAMPvec[which(SAMPvec %in% colnames(data))]

####CHECK FOR STUDY ID
IDvec <- c("name", "article", "Article", "studyname", "Studyname", "author", "Author", "study_name")
id <- IDvec[which(IDvec %in% colnames(data))]

####CHECK FOR PUBLICATION STATUS
PUBvec <- c("published", "pub", "publ","publication status", "publication_status", "pub status", "pub_status")
pub <- PUBvec[which(PUBvec %in% colnames(data))]

#xpub <- stringdistmatrix(c("name", "article", "Article", "studyname", "Studyname", "author", "Author", "study_name"), colnames(data), "lv")
#minipub <- which(xpub == min(xpub), arr.ind = TRUE)
#data[[minipub[2]]]

##Fuzzy matching idea with stringdistmatrix
# x <- stringdistmatrix(c("published", "pub", "publ","publication status", "publication_status", "pub status", "pub_status"), colnames(data), "lv")
# mini <- which(x == min(x), arr.ind = TRUE)
# data[[mini[2]]]

report.colnames <- data.table("es" = es, "se" = se, "year" = year, "n" = n, "id" = id, "pub" = pub)


#check if multiple were found:
#length(unique(report.colnames$es))
#replace $es with $se, $year etc.

