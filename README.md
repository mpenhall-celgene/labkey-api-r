Rlabkey is an interface between the R language and LabKey Server that has been designed to combine the strengths of LabKey Server and the R language platform.

## Contents
### `/labkey-api-r`
- **`build/`** - Directory where packages are built, created by 'build' task.
  - **`Rlabkey.Rcheck/`** - Folder created by "check" task, contains intermediate files for validation checks
- **`test/`** - Scripts for testing Rlabkey and for installing test dependencies.
  - **`vignette.R`** - R test script designed to connect to a LabKey instance on localhost with a project named "apisamples"
  - **`listArchive.zip`** - List archive used by `vignette.R` and Selenium tests
- **`docs/`** - The source files for the Users Guide, plus a latex doc and corresponding Pdf describing the package build environment.

### `/labkey-api-r/Rlabkey`
Contains all source code for Rlabkey and its documentation.
- **`DESCRIPTION`** - Text manifest file specifying version number, dependencies, and other properties.  Developer maintained.
- **`NAMESPACE`** - Specifies the public function names.  Developer maintained.
- **`NEWS`** - Document of change history for this project, developer maintaind.
- **`inst/doc/`** - Folder for documents
  - **`usersguide.pdf`** Documentation from `/docs/UserGuide` printed to PDF
- **`man/`** - Source files for function-by-function reference documentation.  After a successful build, a final pdf of the function reference documentation can be found in Rlabkey.Rcheck/Rlabkey-manual.pdf.
- **`R/`** - R Source files for package functions
- **`src/`** - Source files for a faster implementation of Json parsing for retrieved data.  Written in C, built at package install time.
- **`src-*/`** - generated by C build

---

## Building

Install tools needed for building R packages. [Build tools](https://cran.r-project.org/bin/) are available on [CRAN](https://cran.r-project.org/).  
####OS specific instructions:
- [Windows](docs/build_windows.md)
- Others TBD

After all tools are installed, run `./gradlew build`.

## Installing

`./gradlew installRLabkey` will install Rlabkey and its dependencies into your user's package library (`R_LIBS_USER`)

R package validation (`./gradlew check`) also requires these dependencies.

