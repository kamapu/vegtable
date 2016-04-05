<!-- README.md is generated from README.Rmd. Please edit that file -->



# vegtables

The aim of `vegtables` is to provide a way for handling databases stored in
[Turboveg](http://www.synbiosys.alterra.nl/turboveg).
This package incorporates many concepts and some functions included in the
package [vegdata](https://cran.r-project.org/web/packages/vegdata/index.html)
but the main difference is that `vegtables` implements an `S4` class
(`'vegtable'`) containing all elements of a database in just one object.
`vegtables` also implement some methods for this kind of objects.

You may also be aware, that species lists in `vegtables` are handled by the
package [taxlist](https://github.com/kamapu/taxlist), thus it will be
recommendable to start looking at the `taxlist-manual`.

This package has been developed as a tool handling data stored in
[SWEA-Dataveg](http://www.givd.info/ID/AF-00-006).
Further development is running in the context of the project
[GlobE-wetlands](https://www.wetlands-africa.de/).

An important source of inspiration for `vegtables` have been the enthusiastic
discussions during several versions of the
[Meetings on Vegetation Databases](http://www.hswt.de/person/joerg-ewald/vegetationsdatenbanken.html).

## Updating to the last version of vegtables
The very first step is to install the package
[devtools](https://github.com/hadley/devtools) and dependencies.
Then you just need to execute following commands in your R-session:


```r
library(devtools)
install_github("kamapu/vegtables")
```
