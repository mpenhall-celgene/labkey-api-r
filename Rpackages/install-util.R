##
#  Copyright (c) 2010 LabKey Corporation
# 
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

# ensure that R_LIBS_USER is defined
libdir = Sys.getenv("R_LIBS_USER")
if (length(args)==0) {
    stop("R_LIBS_USER is not defined.\n", call.=FALSE)
}

# set the library search path to R_LIBS_USER
.libPaths(libdir)
cat("library search path:\n  ", paste(.libPaths(), collapse="\n   "), "\n")

is.installed <- function(mypkg) length(find.package(mypkg, quiet=TRUE)) > 0

check.installed <- function(mypkg)
{
    isInstalled <- is.installed(mypkg)

    if (isInstalled) {
        cat("library", packageName, "is installed in", libdir, "\n")
    } else {
        cat("library", packageName, "is not yet installed in", libdir, "\n")
    }

    isInstalled
}

install.dependencies <- function (packageName, cran_deps=NULL, bioc_deps=NULL, rwin_deps=NULL)
{
    options(repos=structure(c(CRAN="http://cran.fhcrc.org/")))

        # install any missing CRAN dependencies
        cran_deps <- cran_deps[!sapply(cran_deps, is.installed)]
        if (length(cran_deps) > 0) {
            cat("installing CRAN dependencies:", cran_deps, "\n")
            install.packages(pkgs=cran_deps, lib=libdir, destdir=libdir, repos="http://cran.fhcrc.org/", INSTALL_opts=c("--no-multiarch"))
            cat("installed CRAN dependencies.\n")
        }

        # install any missing BioC dependencies
        bioc_deps <- bioc_deps[!sapply(bioc_deps, is.installed)]
        if (length(bioc_deps) > 0) {
            cat("installing BioConductor dependencies:", bioc_deps, "\n")
            source("http://bioconductor.org/biocLite.R")
            biocLite(bioc_deps, lib=libdir, destdir=libdir, INSTALL_opts=c("--no-multiarch"))
            cat("installed BioConductor dependencies.\n")
        }

        # install any missing Windows binary dependencies (for RCurl)
        rwin_deps <- rwin_deps[!sapply(rwin_deps, is.installed)]
        if (length(rwin_deps) > 0) {
            cat("installing RWin dependencies:", rwin_deps, "\n")
            install.packages(pkgs=rwin_deps, lib=libdir, destdir=libdir, repos=c("http://cran.fhcrc.org/", "http://www.stats.ox.ac.uk/pub/RWin"), INSTALL_opts=c("--no-multiarch"))
            cat("installed RWin dependencies.\n")
        }
}
