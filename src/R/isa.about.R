isa.about <- function () 
{
    cat("\n")
    cat("-----------------------------------------------------------\n")
    ver <- packageDescription("ispaadis", lib.loc = NULL, 
        fields = "Version")
    cat(paste("ispaadis version", ver, "is now loaded"))
    cat("\n")
    cat("See See http://fvas.unimelb.edu.au/veam for details.")
    cat("\n")
    cat("-----------------------------------------------------------\n")
    invisible()
}
