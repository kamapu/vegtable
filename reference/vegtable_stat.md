# General statistics from vegtable objects

This function calculates general statistics of local **Turboveg**
databases as required by GIVD (Global Index of Vegetation-Plot
Databases, <https://www.givd.info>).

This function is based on a script delivered by GIVD for summarising
statistics required in the descriptions of databases (see meta data in
the page of the Global Index for Vegetation-Plot Databases).

## Usage

``` r
vegtable_stat(vegtable, ...)

# S3 method for class 'vegtable'
vegtable_stat(vegtable, ...)
```

## Arguments

- vegtable:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- ...:

  Further arguments passed among methods.

## Author

GIVD. Adapted by Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Statistics for GIVD
vegtable_stat(Kenya_veg)
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
#> 
#> REFERENCES 
#> Primary references: 5
#> 
#> ## AREA 
#> Area range (m^2): 150 - 1750
#> <1 m^2: 0%
#> 1-<10 m^2: 0%
#> 10-<100 m^2: 0%
#> 100-<1000 m^2: 2%
#> 1000-<10000 m^2: 1%
#> >=10000 m^2: 0%
#> unknow: 97%
#> 
#> ## TIME 
#> oldest: 1983 - youngest: 2014
#> <=1919: 0%
#> 1920-1929: 0%
#> 1930-1939: 0%
#> 1940-1949: 0%
#> 1950-1959: 0%
#> 1960-1969: 0%
#> 1970-1979: 0%
#> 1980-1989: 36%
#> 1990-1999: 31%
#> 2000-2009: 2%
#> 2010-2019: 1%
#> unknow: 30%
#> 
#> ## DISTRIBUTION 
#> KE: 100%
#> 
#> ## PERFORMANCE 
#> 01: 83%
#> 02: 17%
#> 03: 0%
#> 04: 0%
#> 05: 0%
#> 06: 0%
#> 07: 0%
#> 08: 0%
#> 09: 0%
#> 10: 0%
#> 11: 0%
#> 12: 0%
#> 
```
