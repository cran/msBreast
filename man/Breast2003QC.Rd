\name{Breast2003QC}
\alias{Breast2003QC}
\docType{data}
\title{Mass Spectra from a Breast Cancer Quality Control Sample}
\concept{breast cancer}
\concept{quality control}
\description{A data object of class \code{msSet}, consisting  of 96 mass spectra
generated from a pooled sample of nipple aspirate fluid (NAF)
from healthy breasts and breasts with cancer.
The data set was derived from \code{Breast2003QCList} by eliminating the mass region
below 950 Da/charge. Please see the references for more details.}
\usage{Breast2003QC}
\format{
  An object of \code{msSet}. See \code{\link[msProcess]{msSet}} 
  in package \code{\link[msProcess:msProcess-package]{msProcess}} for details.}
\source{\url{http://bioinformatics.mdanderson.org/pubdata.html}}
\references{
Coombes, K.R., Tsavachidis, S., Morris, J.S.,
Baggerly, K.A., and Kuerer, H.M.,
"Improved peak detection and quantification of mass spectrometry data
acquired from surface-enhanced laser desorption and ionization
by denoising spectra with the undecimated discrete wavelet transform,"
\emph{Proteomics}, 5:4107--17, 2005.

Pawlik, T.M., Fritsche, H., Coombes, K.R., Xiao, L., Krishnamurthy, S., Hunt, K.K., 
Pusztai, L., Chen, J.N., Clarke, C.H., Arun, B., Hung, M.C., and Kuerer, H.M.,
"Significant differences in nipple aspirate fluid protein expression between 
healthy women and those with breast cancer demonstrated by time-of-flight mass 
spectrometry,"
\emph{Breast Cancer Research and Treatment}, 89(2):149--57, 2005.

Kuerer, H.M., Coombes, K.R., Chen, J.N., Xiao, L., Clarke, C., Fritsche, H., 
Krishnamurthy, S., Marcy, S., Hung, M.C., and Hunt, K.K.,
"Association between ductal fluid proteomic expression profiles and 
the presence of lymph node metastases in women with breast cancer,"
\emph{Surgery}, 136(5):1061--9, 2004.

Coombes, K.R., Fritsche, Jr., H.A., Clarke, C., Chen, J.N.,
Baggerly, K.A., Morris, J.S., Xiao, L.C., Hung, M.C., and Kuerer, H.M.,
"Quality control and peak finding for proteomics data collected from
nipple aspirate fluid by surface-enhanced laser desorption and ionization,"
\emph{Clinical Chemistry}, 49(10):1615--23, 2003.
}

\seealso{
\code{\link{Breast2003QCList}}.}

\examples{
if (require("msProcess")) {
if (!exists("Breast2003QC")) data("Breast2003QC", package="msBreast")

## plot a few spectra 
plot(Breast2003QC, subset=1:5)

## image of all spectra 
image(Breast2003QC)
}
}
\keyword{datasets}
