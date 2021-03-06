% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_recipe.R
\name{read_recipe}
\alias{read_recipe}
\title{Read a Recipe in .xml Format}
\usage{
read_recipe(x)
}
\arguments{
\item{x}{File}
}
\value{
a list with a tibble for $pipetting and a tibble for recipe components.
}
\description{
Read an \code{.xml} file that is read by the Minstrel liquid handling robot. Used
to tell the robot what recipe to conduct. Individual components of the actual
pipetting are calculated by the robot once recipe is given.
}
\examples{
library(minstrel)

fl <- system.file("extdata", "test_plate.xml", package = "minstrel")

read_recipe(fl)
}
