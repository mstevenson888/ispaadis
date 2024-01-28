
rem ===================================================================
rem STEP 1: COPY PACKAGE TO C:\TEMP
rem ===================================================================

rem Copy the contents of C:\Data\Software\Statistics\R\dev\ispaadis\src to C:\TEMP\ispaadis


rem ===================================================================
rem STEP 3: BUILD AND CHECK THE PACKAGE
rem ===================================================================

# Need program QPDF to be installed and on the computer path. QPDF is used to reduce the size of PDF files.
# Download qpdf-10.0.1-bin-mingw64.zip from https://sourceforge.net/projects/qpdf/files/qpdf/10.0.1/

# Unzip and copy to C:\Program Files\QPDF. Add C:\Program Files\QPDF\qpdf-10.0.1\bin to the path. 

# Build Linux version of package (from the DOS prompt):

C:
CD C:\TEMP
R CMD build --resave-data ispaadis

rem Check the package:
C:
CD C:\TEMP
R CMD check --as-cran ispaadis_0.1-5.tar.gz

rem If non-ASCII characters found in Rd files, run the following code to find the problem:

library(tools)
showNonASCIIfile("C:\\Temp\\epiR\\man\\rsu.pfree.rs.Rd")

# Build Windows version of package (from the DOS prompt):

C:
CD C:\TEMP
R CMD INSTALL --build --resave-data ispaadis_0.1-5.tar.gz 

rem Good idea to run an additional Windows check using: 

https://win-builder.r-project.org/
https://win-builder.r-project.org/upload.aspx

rem Upload package as release. System will send an email detailing results of check.


rem ===================================================================
rem STEP 4: WINDOWS
rem ===================================================================

rem Install program into Windows then copy the package manual into the docs folder. From the DOS prompt:

CD C:\Program Files\R\R-4.2.2\library
zip -r9X ispaadis.zip ispaadis

rem rename ispaadis.zip ispaadis_0.1-4.zip


rem ===================================================================
rem STEP 6: SUBMIT TO CRAN
rem ===================================================================

rem Via Web page:
rem See: http://cran.r-project.org/submit.html
