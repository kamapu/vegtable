
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Use snippet 'render_markdown' for it -->

# vegtable

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vegtable)](https://cran.r-project.org/package=vegtable)
[![DOI](https://zenodo.org/badge/55006983.svg)](https://zenodo.org/badge/latestdoi/55006983)
<br>
[![R-CMD-check](https://github.com/kamapu/vegtable/workflows/R-CMD-check/badge.svg)](https://github.com/kamapu/vegtable/actions)
[![codecov](https://codecov.io/gh/kamapu/vegtable/branch/master/graph/badge.svg)](https://codecov.io/gh/kamapu/vegtable)
<br>
[![CRAN_downloads](http://cranlogs.r-pkg.org/badges/vegtable)](https://cran.r-project.org/package=vegtable)
[![total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/vegtable)](https://cran.r-project.org/package=vegtable)
<!-- badges: end -->

The aim of `vegtable` is to provide a way for handling databases stored
in [Turboveg](http://www.synbiosys.alterra.nl/turboveg). This package
incorporates many concepts and some functions included in the package
[vegdata](https://cran.r-project.org/package=vegdata) but defining an
homonymous `S4` class containing all elements of a database in just one
object. The package `vegtable` also contains several methods for this
object class.

Species lists in `vegtable` objects are handled by the package
[taxlist](https://github.com/kamapu/taxlist), thus I will recommend to
take a look on it.

This package has been developed as a tool handling data stored in
[SWEA-Dataveg](http://www.givd.info/ID/AF-00-006). Further development
is running in the context of the project
[GlobE-wetlands](https://www.wetlands-africa.de/).

An important source of inspiration for `vegtable` have been the
enthusiastic discussions during several versions of the [Meetings on
Vegetation
Databases](http://www.hswt.de/person/joerg-ewald/vegetationsdatenbanken.html).

## Updating to the last version of vegtable

The very first step is to install the package
[devtools](https://github.com/hadley/devtools) and dependencies. Then
you just need to execute following commands in your R-session:

``` r
library(devtools)
install_github("kamapu/vegtable")
```

## Some examples

The current version of `vegtable` includes an example data, which
corresponds to a subset from
[SWEA-Dataveg](http://www.givd.info/ID/AF-00-006). This data set
contains plot observations done in Kenya imported from 5 sources.

``` r
library(vegtable)
#> Loading required package: taxlist
#> 
#> Attaching package: 'taxlist'
#> The following objects are masked from 'package:base':
#> 
#>     levels, print
#> 
#> Attaching package: 'vegtable'
#> The following object is masked from 'package:base':
#> 
#>     transform
data(Kenya_veg)

# validate and explore
validObject(Kenya_veg)
#> [1] TRUE
summary(Kenya_veg)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 9501 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1946 
#>    plots with records: 1946 
#>    variables in header: 34 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE
```

Among others, the object contains plot observations done in the Aberdare
National Park (Kenya) by **Schmitt (1991)**. We can make a subset
including the plots classified by the mentioned author into the
*Juniperus procera*-*Podocarpus latifolius* community (IDs 780 to 798).

``` r
JPcomm <- subset(Kenya_veg, ReleveID %in% c(780:798))
summary(JPcomm)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 717.4 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 19 
#>    plots with records: 19 
#>    variables in header: 17 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE
```

If you have geo-referenced plot observations, you can use the
coordinates to produce a map of the distribution of your plots by using
the package `leaflet`.

``` r
library(leaflet)
leaflet(JPcomm@header) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, color = "red",
                opacity = 0.3, radius = 1)
```

## Further readings

-   [Basics on the work with vegetation-plots in
    vegtable](https://kamapu.github.io/posts/vegtable-press-2/)
-   [Introduction to the package
    vegtable](https://kamapu.github.io/posts/vegtable-intro/)
-   [vegtable: An R object for vegetation-plot data
    sets](https://kamapu.github.io/posts/2020-10-29-vegtablepress/)
