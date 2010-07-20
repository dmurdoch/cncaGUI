\name{cnca}
\alias{cnca}
\title{Canonical Non-symmetrical Correspondence Analysis in R}
\description{Provides a GUI with which users can construct and interact
        with Canonical Non-symmetrical Correspondence Analysis}
\usage{
cnca()
}
\details{This function does not need any arguments. When the function is launched, 
firstly, it is necessary to enter the search path for the file containing sites to 
species data and secondly,the search path for the file containing sites to 
environmental variables data, both files must have Excel format. Once you have 
entered this files an options window is displayed where you can change the color, 
the label and/or the symbol of an element or of a set of elements; to select the 
transformation data; to tick the checkbox to show the axes in the graph; to tick 
the checkbox to show the points representing the sites and to tick the checkbox 
to show the sites labels.
Press the Graph button and when the graph will be shown the function will allow you
 to change characteristics of the points with the mouse.
Press the right mouse button and a window will be displayed to change the color, the
 label and/or the symbol of the nearest point of position clicked.
Press the left mouse button and a window will be displayed to select one option: 
Change the position label, Remove label or Do nothing.
Press the 3D button and a window will be displayed with the 3D-graph.

}
\value{A graph showing the data representation and an output file containing the 
contributions, qualities of representation, coordinates and eigen values}
\references{Willems P, Galindo P (2008).\dQuote{Canonical non-symmetrical correspondence
 analysis: an alternative in constrained ordination.} \emph{SORT}, \bold{32(1)}, 93--112.
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Priscila Willems \email{pwillems@bariloche.inta.gov.ar},
Purificacion Galindo Villardon \email{pgalindo@usal.es}}
\examples{
cnca()
}
\keyword{multivariate}
