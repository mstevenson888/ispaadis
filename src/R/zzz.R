.onAttach <- function(libname, pkgname)
{
  ver <- as.character(read.dcf(file.path(libname, pkgname, "DESCRIPTION"), "Version"))
  packageStartupMessage("Package ispaadis ", ver, " is loaded", appendLF = TRUE)
  packageStartupMessage("Type help(isa.about) for summary information")
  packageStartupMessage("\n")
}
