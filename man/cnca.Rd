\name{cnca}
\alias{cnca}
\title{Canonical Non-Symmetrical Correspondence Analysis in R}
\description{ GUI with which users can construct and interact
        with Canonical Correspondence Analysis and Canonical Non-Symmetrical Correspondence Analysis and provides inferential results by using Bootstrap Methods. The Coinertia Analysis is also available in its classical version.}
\usage{
cnca(fespecies, fvambientales)
}

\arguments{
\item{fespecies}{a data frame with information about species}
\item{fvambientales}{a data frame with information about enviromental variables}
}

\details{When the function is launched, the user have to select the analysis to be applied to its datasets.
Then, the user can change the names of the sets of species, variables and sites and if the selected analysis is Canonical Correspondence Analysis or Canonical Non-Symmetrical Correspondence Analysis is necessary to enter the number of quantitative and qualitative variables presented in the information about variables. Once the user press the OK button,  an option window is displayed where the user can change the color,
the size, the label and/or the symbol of an element or of a set of elements; to select the 
transformation data; to tick the checkbox to show the axes in the graph; to tick 
the checkbox to show the points representing the sites and to tick the checkbox 
to show the sites labels and to change the window size containing the graphs. Press the Graph button and choose the number of axes to be retained. When the graph 
will be shown the function will allow you to change characteristics of the points with the mouse.
Press the right mouse button and a window will be displayed to change the color, the size,
 the label and/or the symbol of the nearest point of position clicked.
Press the left mouse button and a window will be displayed to select one option: 
Change the position label, Remove label or Do nothing.
It is also possible to select the dimensions shown in the graph and to change the limits of the axes.
In the window there are different menus depending on the selected analysis. If the user selected Canonical Correspondence Analysis or Canonical Non-Symmetrical Correspondence Analysis, the menus are:


\itemize{
        \item File
	\itemize{
		\item Copy image
		\item Save image
		\itemize{
			\item PDF file
			\item Eps file
			\item Png file
			\item Jpg/Jpeg file
		}
		\item Exit
	}
	\item 3D
	\itemize{
		\item 3D
	}
	\item Projections
	\itemize{
		\item Species
		\item Sites
		\item Back to original data
	}
	\item Bootstrap
	\itemize{
		\item Bootstrap
	}
	\item Options
	\itemize{
		\item Change title
		\item Show/Hide axes
		\item Show/Hide Sites
		\item Show/Hide labels for Sites
	}	
}

The File menu provides different options to save the graph and permits to exit the program. The second menu shows the graph in 3 dimensions. The third menu allows the user to project the sites or the species onto the direction representing one variable selected from a listbox. This menu permits to go back to original graph. The fourth menu provides inferential results usin Boostrap methods. It it possible to select the number of resamples and the confidence level to calculate the intervals that show the inferential results. These results have been saved in a file and together with it, the program saves graphs with histograms and QQ-plots generated with the bootstrap replications. The last menu provides options to change the title and to show/hide the axes, the sites and the label for sites.

If the selected analysis is Coinertia Analysis, the menus are:

\itemize{
        \item File
        \itemize{
		\item Copy image
		\item Save image
		\itemize{
			\item PDF file
			\item Eps file
			\item Png file
			\item Jpg/Jpeg file
		}
		\item Exit
	}
	\item 3D
	\itemize{
		\item 3D
	}
	\item COIA
	\itemize{
		\item Show all graphs
        	\item Sites graph
		\item Species graph
		\item Environmental variables graph
		\item Species axes
		\item Enviromental variables axes
                }
	\item Options
	\itemize{
		\item Change title
		\item Show/Hide axes
	}	
}

The two fisrt menus are the same. The third menu permits the user to change the graph shown in the window. It is possible to change the title and to show/hide the axes in the graph through the last menu.
}
\value{A graph showing the data representation and two output files containing the 
contributions, qualities of representation, coordinates and eigen values as classical and inferential results.}
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
\item Efron B,  Tibshirani RJ (1993). An introduction into the bootstrap. New York: Chapman and Hall.
 }
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Priscila Willems \email{pwillems@bariloche.inta.gov.ar},
Purificacion Galindo Villardon \email{pgalindo@usal.es}}
\examples{
data(especies)
data(variables)
cnca(especies, variables)
}
\keyword{multivariate}






