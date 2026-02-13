# Convert a data frame into a vegtable object.

Conversion of a data frame containing a cross table of abundance or
cover of species in single plots.

This function coerces a data frame containing a vegetation cross table
into a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object. The input data frame `x` may include information on the layers
or not.

## Usage

``` r
df2vegtable(x, species, layer, ...)

# S4 method for class 'data.frame,numeric,numeric'
df2vegtable(x, species, layer, ...)

# S4 method for class 'data.frame,numeric,missing'
df2vegtable(x, species, layer, ...)
```

## Arguments

- x:

  A data frame formatted for a taxlist object.

- species:

  Numeric or integer indicating the position of the column with species
  names.

- layer:

  Numeric or integer indicating the position of the column with layers.

- ...:

  Further arguments passed from or to other methods.

## Value

A
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Creating data set 'dune_veg'
library(vegan)
#> Loading required package: permute

## Load data from vegan
data(dune)
data(dune.env)

## Conversion to vegtable
dune_veg <- data.frame(
  species = colnames(dune), t(dune),
  stringsAsFactors = FALSE, check.names = FALSE
)
dune_veg <- df2vegtable(dune_veg, species = 1)

summary(dune_veg)
#> ## Metadata 
#>    object size: 24.9 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 20 
#>    plots with records: 20 
#>    variables in header: 1 
#>    number of relations: 0 
#> 
#> ## Taxonomic List 
#>    taxon names: 30 
#>    taxon concepts: 30 
#>    validity: TRUE 
#> 

## Adding environmental variables
dune.env$ReleveID <- as.integer(rownames(dune.env))
header(dune_veg) <- dune.env

summary(dune_veg)
#> ## Metadata 
#>    object size: 28.9 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 20 
#>    plots with records: 20 
#>    variables in header: 6 
#>    number of relations: 0 
#> 
#> ## Taxonomic List 
#>    taxon names: 30 
#>    taxon concepts: 30 
#>    validity: TRUE 
#> 
```
