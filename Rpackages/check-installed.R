# get the library name from the command line arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
    stop("expected library name after --args command line switch")
}
lib_to_check = args[1]


source("../../tools/Rpackages/install-util.R")

# check if library is installed: exit code 0 is installed, 1 is not installed
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if (is.installed(lib_to_check)) {
    print(paste("library", lib_to_check, "is installed in", getwd()))
    q(save="no", status=0)
} else {
    print(paste("library", lib_to_check, "is not installed in", getwd()))
    q(save="no", status=1)
}


