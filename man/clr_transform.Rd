% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clr.R
\name{clr_transform}
\alias{clr_transform}
\title{Centered log-ratio (CLR) transform community data}
\usage{
clr_transform(x, base = 2, ignore.zeroes = TRUE)
}
\arguments{
\item{x}{A vegan-format matrix of samples by taxa}

\item{base}{Base for logarithm (default=2)}

\item{ignore.zeroes}{Should zeros be included when calculating geometric mean (default=TRUE)}
}
\value{
CLR-transformed matrix
}
\description{
Centered log-ratio (CLR) transform community data
}
