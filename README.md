
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

# camtrapmonitoring

## Installation

You can install the development version of camtrapmonitoring with:

``` r
remotes::install_github("robitalec/camtrapmonitoring")
```

This package depends on `sf`, `terra`, `distanceto` and `units`. System
dependencies include `udunits` ([see here for
installation](https://github.com/r-quantities/units#installation)), and
GDAL, GEOS and PROJ4 ([see here for
installation](https://github.com/r-spatial/sf/#installing)). Windows
users, make sure to use
[RTools](https://cran.r-project.org/bin/windows/Rtools/).

## Name

This package was previously named {wildcam} but renamed to
{camtrapmonitoring} to reflect broadened scope of package from planning
camera trap surveys to community based monitoring of wildlife and
density estimation, and to avoid confusion with other projects using
camera traps named wildcam or similar.
