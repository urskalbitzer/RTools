% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plotting.R
\name{plot_2vars}
\alias{plot_2vars}
\title{Function to plot three variables in 3d plot}
\usage{
plot_2vars(x1, x1_label, x2, x2_label, y, y_label, intercept = NULL,
  x1_coef = NULL, x2_coef = NULL, grid.lines = 20, theta = 30,
  phi = 30)
}
\arguments{
\item{x1}{Values for first independent variable (x-parameter)}

\item{x1_label}{Label for first independent variable}

\item{x2}{Values for second independent variable (y-parameter)}

\item{x2_label}{Label for second independent variable}

\item{y}{Values for dependent variable (z-paramter)}

\item{y_label}{Label for dependent variable}

\item{intercept}{Coefficient for the intercept of the linear model}

\item{x1_coef}{Coefficient for x1 of the linear model}

\item{grid.lines}{Number of gridlines for the prediction plane}

\item{theta}{Angle 1 (default 30)}

\item{phi}{Angle 2 (default 30)}

\item{x1_coef}{Coefficient for x2 of the linear model}
}
\description{
Creates a 3d plot using the scatter3D function from the plot3D package
Predictions from model can be provided as well to plot the prediction plane
}
\examples{

}
