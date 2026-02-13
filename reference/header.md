# Retrieve or replace slot header in vegtable objects

Retrieve or replace the content of slot `header` in
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects.

## Usage

``` r
header(x, ...)

# S4 method for class 'vegtable'
header(x, ...)

header(x) <- value

# S4 method for class 'vegtable,data.frame'
header(x) <- value
```

## Arguments

- x:

  Object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- ...:

  Further arguments passed to or from other methods.

- value:

  Data frame to be set as slot `header`.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
head(header(Kenya_veg))
#>     ReleveID COUNTRY REFERENCE TABLE_NR NR_IN_TAB COVERSCALE DATE SURF_AREA
#> 358      358      KE      2974        1         1         01 <NA>        NA
#> 359      359      KE      2974        1         2         01 <NA>        NA
#> 360      360      KE      2974        1         3         01 <NA>        NA
#> 361      361      KE      2974        1         4         01 <NA>        NA
#> 362      362      KE      2974        1         5         01 <NA>        NA
#> 363      363      KE      2974        1         6         01 <NA>        NA
#>     ALTITUDE EXPOSITION INCLINATIO COV_TOTAL TREE_HIGH     REMARKS LONGITUDE
#> 358       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#> 359       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#> 360       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#> 361       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#> 362       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#> 363       NA       <NA>         NA        NA        NA Mount Nyiro   36.8167
#>     LATITUDE PH_H2O                               COMM_TYPE COORDVAL SOIL_TEXT
#> 358   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#> 359   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#> 360   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#> 361   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#> 362   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#> 363   2.1833     NA Faureo-Ilicetum xymaletosum wet variant       NA      <NA>
#>     HEIGHT_T1 HEIGHT_T2 HEIGHT_S1 HEIGHT_S2 HEIGHT_HL COVER_T1 COVER_T2
#> 358        NA        NA        NA        NA        NA       NA       NA
#> 359        NA        NA        NA        NA        NA       NA       NA
#> 360        NA        NA        NA        NA        NA       NA       NA
#> 361        NA        NA        NA        NA        NA       NA       NA
#> 362        NA        NA        NA        NA        NA       NA       NA
#> 363        NA        NA        NA        NA        NA       NA       NA
#>     COVER_S1 COVER_S2 COVER_HL ORIG_CODE RECORD DIM_1 DIM_2
#> 358       NA       NA       NA      <NA>     vp    NA    NA
#> 359       NA       NA       NA      <NA>     vp    NA    NA
#> 360       NA       NA       NA      <NA>     vp    NA    NA
#> 361       NA       NA       NA      <NA>     vp    NA    NA
#> 362       NA       NA       NA      <NA>     vp    NA    NA
#> 363       NA       NA       NA      <NA>     vp    NA    NA
```
