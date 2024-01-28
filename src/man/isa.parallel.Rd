\name{isa.parallel}

\alias{isa.parallel}

\title{
Run InterSpread Plus simulations using multiple cores
}

\description{
Run InterSpread Plus simulations using multiple cores.
}

\usage{
isa.parallel(ini_file_path, ISP_folder_root = "C:/ISP/",
   total_iterations, ncores = NA, 
   output_filepaths = c("general" = "general", 
      "general_structure" =  "results/SC01/results_\%\%type\%\%_SC01.txt"))
}

\arguments{
  \item{ini_file_path}{character, the length of the observation period (usually days).}
  \item{ISP_folder_root}{character, the size of the population at individuals risk.}
  \item{total_iterations}{scalar numeric, the number of model iterations to be run.}
  \item{ncores}{scalar numeric, the number of cores to use. If \code{code} the function uses the total number of cores available minus 2.}
  \item{output_filepaths}{character, paths to the InterSpread Plus output file.}
}

\details{
By default, InterSpread Plus uses a single processor core. This function allows a model simulation to be run on multiple computer cores, reducing simulation run times. For this function to operate the location of the InterSpread executable must be added to your system path.
}

\examples{
\dontrun{
## For the InterSpread Plus model parameterised in file SC01.ini, run twenty 
## iterations using 16 cores.

isa.parallel(ini_file_path = "C:/ISP/ini/SC01.ini", 
   ISP_folder_root = "C:/ISP/",
   total_iterations = 20, 
   ncores = 16, 
   output_filepaths = c("general" = "general", 
      "general_structure" =  "results/SC01/results_\%\%type\%\%.txt"))
}
}
