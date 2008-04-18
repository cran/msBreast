%%  WARNING: This file was automatically generated from the associated 
%%  Breast2003.mid file. Do NOT edit this Rd file to make a change. Instead, 
%%  edit the Breast2003.mid file in the project MID directory. Once the 
%%  Breast2003.mid file has been updated, this Rd file, and all other 
%%  documentation (such as corresponding LaTeX, SGML and HTML documentation) 
%%  should be regenerated using the mid.pl Perl script. 

%% R documentation for the Breast2003QCList function

\name{Breast2003QCList}
\alias{Breast2003QCList}
\docType{data}
\title{Mass Spectra from a Breast Cancer Quality Control Sample}
\concept{breast cancer}
\concept{quality control}
\description{A data object of class \code{msList},
consisting of 96 mass spectra
generated from a pooled sample of nipple aspirate fluid (NAF)
from healthy breasts and breasts with cancer.
(see references for details).}
\usage{Breast2003QCList}
\format{
  An object of \code{msList}. See \code{\link[msProcess]{msList}} 
  in package \code{\link[msProcess:msProcess-package]{msProcess}} for details.
  }
\source{\url{http://bioinformatics.mdanderson.org/pubdata.html}}
\references{
Coombes, K.R., Tsavachidis, S., Morris, J.S.,
Baggerly, K.A., and Kuerer, H.M.,
"Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, \bold{5}:4107--17, 2005.

Pawlik, T.M., Fritsche, H., Coombes, K.R., Xiao, L., Krishnamurthy, S., Hunt, K.K., 
Pusztai, L., Chen, J.N., Clarke, C.H., Arun, B., Hung, M.C., and Kuerer, H.M.,
"Significant differences in nipple aspirate fluid protein expression between 
healthy women and those with breast cancer demonstrated by time-of-flight mass 
spectrometry,"
\emph{Breast Cancer Research and Treatment}, \bold{89}(2):149--57, 2005.

Kuerer, H.M., Coombes, K.R., Chen, J.N., Xiao, L., Clarke, C., Fritsche, H., 
Krishnamurthy, S., Marcy, S., Hung, M.C., and Hunt, K.K.,
"Association between ductal fluid proteomic expression profiles and 
the presence of lymph node metastases in women with breast cancer,"
\emph{Surgery}, \bold{136}(5):1061--9, 2004.

Coombes, K.R., Fritsche, Jr., H.A., Clarke, C., Chen, J.N.,
Baggerly, K.A., Morris, J.S., Xiao, L.C., Hung, M.C., and Kuerer, H.M.,
"Quality control and peak finding for proteomics data collected from
nipple aspirate fluid by surface-enhanced laser desorption and ionization,"
\emph{Clinical Chemistry}, \bold{49}(10):1615--23, 2003.
}

\seealso{
\code{\link{Breast2003QC}}.}

\examples{
if (require("msProcess")) {
if (!exists("Breast2003QCList")) data("Breast2003QCList", package="msBreast")

## print an \code{msList} object 
Breast2003QCList

## print the synopsis of an \code{msList} object 
summary(Breast2003QCList)

## plot the 10th spectrum from an \code{msList} 
## object 
plot(Breast2003QCList, index=10)
}
}
\keyword{datasets}
