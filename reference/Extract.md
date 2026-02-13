# Select or replace elements in objects

Methods for quick access to slot `header` of
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects or for access to single cover scales in
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
objects. Also replacement methods are implemented.

## Usage

``` r
# S4 method for class 'vegtable'
x$name

# S4 method for class 'vegtable,ANY'
x$name <- value

# S4 method for class 'coverconvert'
x$name

# S4 method for class 'coverconvert'
x[i]

# S4 method for class 'coverconvert,coverconvert'
x$name <- value

# S4 method for class 'vegtable'
x[i, j, ..., drop = FALSE]

# S4 method for class 'vegtable'
x[i, j] <- value
```

## Arguments

- x:

  Object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- name:

  A name to access.

- value:

  Either a vectors or a list, used as replacement.

- i, j:

  Indices for access.

- ...:

  Further arguments passed to or from other methods.

- drop:

  A logical value passed to Extract.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Range of latitude values in database
range(Kenya_veg$LATITUDE)
#> [1] -1.880917  2.668861

## Summary of countries
summary(Kenya_veg$COUNTRY)
#>      KE      AD      AE      AF      AG      AI      AL      AM      AN      AO 
#>    1946       0       0       0       0       0       0       0       0       0 
#>      AQ      AR      AS      AT      AU      AW      AZ      BA      BB      BD 
#>       0       0       0       0       0       0       0       0       0       0 
#>      BE      BF      BG      BH      BI      BJ      BM      BN      BO      BR 
#>       0       0       0       0       0       0       0       0       0       0 
#>      BS      BT      BV      BW      BY      BZ      CA      CC      CF      CD 
#>       0       0       0       0       0       0       0       0       0       0 
#>      CG      CH      CI      CK      CL      CM      CN      CO      CR      CS 
#>       0       0       0       0       0       0       0       0       0       0 
#>      CU      CV      CX      CY      CZ      DE      DJ      DK      DM      DO 
#>       0       0       0       0       0       0       0       0       0       0 
#>      DZ      EC      EE      EG      EH      ER      ES      ET      FI      FJ 
#>       0       0       0       0       0       0       0       0       0       0 
#>      FK      FM      FO      FR      FX      GA      GB      GD      GE      GF 
#>       0       0       0       0       0       0       0       0       0       0 
#>      GH      GI      GL      GM      GN      GP      GQ      GR      GS      GT 
#>       0       0       0       0       0       0       0       0       0       0 
#>      GU      GW      GY      HK      HM      HN      HR      HT      HU (Other) 
#>       0       0       0       0       0       0       0       0       0       0 
summary(droplevels(Kenya_veg$COUNTRY))
#>   KE 
#> 1946 

## First 5 samples
summary(Kenya_veg[1:5, ])
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 683.5 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 5 
#>    plots with records: 5 
#>    variables in header: 11 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE 
#> 
```
