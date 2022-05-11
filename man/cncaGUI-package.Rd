\name{cncaGUI-package}
\alias{cncaGUI-package}
\docType{package}
\title{Canonical Non-Symmetrical Correspondence Analysis package}
\description{ GUI with which users can construct and interact
        with Canonical Correspondence Analysis and Canonical Non-Symmetrical Correspondence Analysis and provides inferential results by using Bootstrap Methods.}
\details{
\tabular{ll}{
Package: \tab cncaGUI\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2022-05-10\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es},
Priscila Willems \email{pwillems@bariloche.inta.gov.ar},
Purificacion Galindo Villardon \email{pgalindo@usal.es}}
\references{
\itemize{
\item Ter-Braak CJF (1986). Canonical Correspondence Analysis: a New Eigenvector Technique
for Multivariate Direct Gradient Analysis. \emph{Ecology}, 67(5), 1167-1179.
\item Willems P, Galindo P (2008).Canonical non-symmetrical correspondence
 analysis: an alternative in constrained ordination. \emph{SORT}, 32(1), 93--112.
\item Doledec S, Chessel D (1994). Co-inertia analysis: an alternative method for studying
species environment relationships. \emph{Freshwater Biology}, 31, 277--294.
\item Efron B (1979). Bootstrap methods: Another look at the jackknife. Annals of Statistics, 7, 1-26.
\item Efron B (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82, 171-185.
\item Efron B, Tibshirani RJ (1993). An introduction into the bootstrap. New York: Chapman and Hall.
 }
}
\keyword{ package }
\examples{
data(especies)
data(variables)
cnca(especies, variables)
}
