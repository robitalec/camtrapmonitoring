% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-ct.R
\name{sample_ct}
\alias{sample_ct}
\title{Camera trap sampling}
\usage{
sample_ct(region, n, type, strata = NULL)
}
\arguments{
\item{region}{spatial feature object across which points will be sampled}

\item{n}{number of random points. If \code{strata} is provided,
n represents the number of random points per strata}

\item{type}{type of sampling, see \code{\link[sf:st_sample]{sf::st_sample()}}}

\item{strata}{column name in region indicating strata}
}
\value{
\code{sf} (see \code{\link[sf:sf]{sf::st_sf()}}) object with POINT geometry
}
\description{
Sample potential camera trap locations. For stratified sampling, provide a
suitable column to stratify region by. Alternatively, \code{\link[sf:st_sample]{sf::st_sample()}}
is used directly to sample points across all features.
}
\examples{
# Example grid with density levels (High, Medium, Low)
data(clearwater_lake_density)

# Stratified random points for each density level
pts_random <- sample_ct(
  region = clearwater_lake_density, n = 20,
  type = 'random', strata = 'density')

# Plot density grid and sampled points
plot(clearwater_lake_density, reset = FALSE)
plot(pts_random, add = TRUE, pch = 1, strata = 1)

# Regular sampled points across all features
pts_regular <- sample_ct(
  region = clearwater_lake_density, n = 20, type = 'regular')

# Plot density grid and sampled points
plot(clearwater_lake_density, reset = FALSE)
plot(pts_regular, add = TRUE, pch = 2, strata = 1)
}
