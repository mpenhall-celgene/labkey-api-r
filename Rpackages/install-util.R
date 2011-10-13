# ensure we are running from the sampledata/rlabkey directory
libdir = getwd()
if (length(grep("sampledata.rlabkey$", libdir)) == 0) {
    stop("Please run this script from the sampledata/rlabkey directory")
}


# set the library search path to the current executing directory (sampledata/rlabkey)
.libPaths(libdir)
cat("library search path:\n  ", paste(.libPaths(), collapse="\n   "), "\n")


is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

install.dependencies <- function (packageName, cran_deps=NULL, bioc_deps=NULL)
{
    if (!is.installed(packageName)) {
        cat("library", packageName, "is not yet installed in", libdir, "\n")

        # install CRAN dependencies
        if (length(cran_deps) > 0) {
            cat("installing CRAN dependencies...\n")
            install.packages(pkgs=cran_deps, lib=libdir, destdir=".", repos="http://cran.fhcrc.org/")
            cat("installed CRAN dependencies.\n")
        }

        # install BioC dependencies
        if (length(bioc_deps) > 0) {
            cat("installing BioConductor dependencies...\n")
            source("http://bioconductor.org/biocLite.R")
            biocLite(bioc_deps, lib=libdir, destdir=".")
            cat("installed BioConductor dependencies.\n")
        }

    } else {
        cat("library", packageName, "is already installed in", libdir, "\n")
    }
}
