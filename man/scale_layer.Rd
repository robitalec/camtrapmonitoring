% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-layer.R
\name{scale_layer}
\alias{scale_layer}
\title{Scale in region of interest}
\usage{
scale_layer(target, region, center = TRUE, scale = TRUE)
}
\arguments{
\item{target}{SpatRaster target (see \code{\link[terra:rast]{terra::rast()}})}

\item{region}{object which can be passed to \code{\link[terra:ext]{terra::ext()}} including \code{sf},
\code{SpatRaster}, and 2x2 matrices.}

\item{center}{see \code{\link[terra:scale]{terra::scale()}}}

\item{scale}{see \code{\link[terra:scale]{terra::scale()}}}
}
\value{
\code{SpatRaster} layer, cropped to extent of provided region, and scaled.
}
\description{
Helper function to scale a target layer in a region of interest.
Can be used to compare locations with \code{\link[=eval_pt]{eval_pt()}} and \code{\link[=eval_buffer]{eval_buffer()}},
and select locations based off of relative values instead of absolute values.
}
\examples{
# Load packages
library(terra)

# Load data
data("clearwater_lake_hydro")
clearwater_elev_path <- system.file(
  "extdata", "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
clearwater_lake_elevation <- rast(clearwater_elev_path)

# Region of interest around Clearwater lake
roi <- clearwater_lake_hydro[4,]

# Scale elevation in extent of density grid
elev_scaled <- scale_layer(target = clearwater_lake_elevation, region = roi)
}
\seealso{
\code{\link[terra:scale]{terra::scale()}}
}
