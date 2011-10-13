# get the library name from the command line arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
    stop("expected library name after --args command line switch")
}
packageName = args[1]


source("../../tools/Rpackages/install-util.R")

# check if library is installed: exit code 0 is installed, 1 is not installed
if (is.installed(packageName)) {
    cat("library", packageName, "is installed in", getwd(), "\n")
    q(save="no", status=0)
} else {
    cat("library", packageName, "is not yet installed in", getwd(), "\n")
    q(save="no", status=1)
}


