vers <- getRversion()
if (vers < "2.13") {
    stop("Your R version is ", vers, ". The flowWorkspace library requires R version 2.13 or greater")
}

source("../../tools/Rpackages/install-util.R")
install.dependencies("flowWorkspace",
    c("rrcov", "feature"),
    c("RBGL", "graph", "XML", "flowCore", "flowViz", "Rgraphviz", "Biobase", "IDPmisc", "flowStats"))
