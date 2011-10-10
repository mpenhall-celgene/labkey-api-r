# ensure we are running from the sampledata/rlabkey directory
libdir = getwd()
if (length(grep("sampledata.rlabkey$", libdir)) == 0) {
    stop("Please run this script from the sampledata/rlabkey directory")
}


# add the current executing directory (sampledata/rlabkey) to the library search path
.libPaths(c(libdir, .libPaths()))


is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

install.dependencies <- function (packageName, cran_deps, bioc_deps)
{
    if (!is.installed(packageName)) {

        # install CRAN dependencies
        if (length(cran_deps) > 0) {
            print("installing CRAN dependencies...")
            install.packages(pkgs=cran_deps, lib=libdir, repos="http://cran.fhcrc.org/")
        }

        # install BioC dependencies
        if (length(bioc_deps) > 0) {
            print("installing BioConductor dependencies...")
            source("http://bioconductor.org/biocLite.R")
            biocLite(c, lib=libdir, destdir=".")
        }

    } else {
        print(paste(packageName, " is already installed"))
    }
}
