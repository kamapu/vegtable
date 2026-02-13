# Import of vegetation data from Turboveg databases

Import function for **Turboveg** databases into an object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).
Most of the contents of **Turboveg** databases are included in DBF files
and therefore imported by the function
[`foreign::read.dbf()`](https://rdrr.io/pkg/foreign/man/read.dbf.html).
The automatic setting of database path will be done by the function
[`vegdata::tv.home()`](https://rdrr.io/pkg/vegdata/man/tv.home.html) but
it can be customised by the argument `tv_home`.

The species list will be imported by using the function
[`taxlist::tv2taxlist()`](https://docs.ropensci.org/taxlist/reference/tv2taxlist.html)
and therefore formatted as a
[taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
object. Similarly, conversion tables will be handled as
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
objects.

Empty columns in the header will be deleted in the imported object.

The function `tv2coverconvert()` reads the content of cover conversion
tables stored in **Turboveg** and attempts to reformat them in a more
comprehensive structure.

This function is used by `tv2vegtable()` to import the respective
conversion table from **Turboveg** databases. Note that conversion
tables in **Turboveg** have only stored the middle point for each cover
class in a scale, thus it will be recommended to rebuild the
`coverconvert` slot or use
[braun_blanquet](http://kamapu.github.io/vegtable/reference/braun_blanquet-data.md).

## Usage

``` r
tv2vegtable(
  db,
  tv_home = tv.home(),
  skip_empty_relations = TRUE,
  skip_scale,
  clean = TRUE
)

tv2coverconvert(file, as.is = TRUE)
```

## Arguments

- db:

  Name of **Turboveg** data base as character value.

- tv_home:

  **Turboveg** installation path as character value.

- skip_empty_relations:

  Logical value indicating whether empty relations may be excluded from
  imported database or not.

- skip_scale:

  Character value indicating scales to be excluded in slot
  `coverconvert`.

- clean:

  Logical value indicating whether output object should be cleaned or
  not.

- file:

  A connection to a DBF file containing conversion table in
  **Turboveg**.

- as.is:

  A logical value passed to
  [`foreign::read.dbf()`](https://rdrr.io/pkg/foreign/man/read.dbf.html).

## Value

A
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object in the case of `tv2vegtable()`. A
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
object in the case of `tv2coverconvert()`.

## See also

[`taxlist::tv2taxlist()`](https://docs.ropensci.org/taxlist/reference/tv2taxlist.html)
[`foreign::read.dbf()`](https://rdrr.io/pkg/foreign/man/read.dbf.html)
[`vegdata::tv.home()`](https://rdrr.io/pkg/vegdata/man/tv.home.html)

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Installed 'Turboveg' version of 'Fujiwara et al. (2014)'
# TV_Home <- file.path(path.package("vegtable"), "tv_data")
# Veg <- tv2vegtable("Fujiwara_2014", TV_Home)
# summary(Veg)
## Installed 'Turboveg' version of "Fujiwara et al. (2014)"
TV_Home <- file.path(path.package("vegtable"), "tv_data", "popup", "Swea")
Table <- tv2coverconvert(file.path(TV_Home, "tvscale.dbf"))

## First scale have to be deleted from conversion table
Table@value <- Table@value[-1]
Table@conversion <- Table@conversion[-1]
summary(Table)
#> ## Number of cover scales: 12 
#> 
#> * scale 'br_bl': 
#>   Levels   Range
#> 1      r   0 - 1
#> 2      x   0 - 1
#> 3      +   1 - 2
#> 4      1   2 - 3
#> 5      2  3 - 13
#> 6      3 13 - 38
#> 7      4 38 - 68
#> 8      5 68 - 88
#> 
#> * scale 'b_bbds': 
#>   Levels   Range
#> 1      r   0 - 1
#> 2      +   1 - 2
#> 3      1   2 - 3
#> 4     2m   3 - 4
#> 5     2a   4 - 8
#> 6     2b  8 - 18
#> 7      3 18 - 38
#> 8      4 38 - 68
#> 9      5 68 - 88
#> 
#> * scale 'londo': 
#>    Levels   Range
#> 1      r1   0 - 1
#> 2      p1   0 - 1
#> 3      a1   0 - 1
#> 4      m1   0 - 1
#> 5      r2   1 - 2
#> 6      p2   1 - 2
#> 7      a2   1 - 2
#> 8      m2   1 - 2
#> 9      r4   2 - 4
#> 10     p4   2 - 4
#> 11     a4   2 - 4
#> 12     m4   2 - 4
#> 13     1-   4 - 7
#> 14      1  7 - 10
#> 15     1+ 10 - 12
#> 16     2- 12 - 17
#> 17      2 17 - 20
#> 18     2+ 20 - 22
#> 19     3- 22 - 27
#> 20      3 27 - 30
#> 21     3+ 30 - 32
#> 22     4- 32 - 37
#> 23      4 37 - 40
#> 24     4+ 40 - 42
#> 25     5- 42 - 47
#> 26      5 47 - 50
#> 27     5+ 50 - 52
#> 28     6- 52 - 57
#> 29      6 57 - 60
#> 30     6+ 60 - 62
#> 31     7- 62 - 67
#> 32      7 67 - 70
#> 33     7+ 70 - 72
#> 34     8- 72 - 77
#> 35      8 77 - 80
#> 36     8+ 80 - 82
#> 37     9- 82 - 87
#> 38      9 87 - 90
#> 39     9+ 90 - 92
#> 40     10 92 - 97
#> 
#> * scale 'pr_ab': 
#>   Levels  Range
#> 1      x  0 - 1
#> 2      1 1 - 50
#> 
#> * scale 'ordin.': 
#>   Levels   Range
#> 1      1   0 - 1
#> 2      2   1 - 2
#> 3      3   2 - 3
#> 4      4   3 - 4
#> 5      5   4 - 8
#> 6      6  8 - 18
#> 7      7 18 - 38
#> 8      8 38 - 68
#> 9      9 68 - 88
#> 
#> * scale 'bds': 
#>    Levels   Range
#> 1       r   0 - 1
#> 2      +r   0 - 1
#> 3      +p   0 - 1
#> 4      +a   0 - 1
#> 5      1p   0 - 1
#> 6      +b   1 - 2
#> 7      1a   1 - 2
#> 8      1b   2 - 3
#> 9      2m   3 - 4
#> 10     2a   4 - 8
#> 11     2b  8 - 18
#> 12     3a 18 - 31
#> 13     3b 31 - 43
#> 14     4a 43 - 56
#> 15     4b 56 - 68
#> 16     5a 68 - 81
#> 17     5b 81 - 93
#> 
#> * scale 'doing': 
#>    Levels   Range
#> 1       r   0 - 1
#> 2       p   0 - 1
#> 3       a   1 - 2
#> 4       m   2 - 4
#> 5      01  4 - 10
#> 6      02 10 - 20
#> 7      03 20 - 30
#> 8      04 30 - 40
#> 9      05 40 - 50
#> 10     06 50 - 60
#> 11     07 60 - 70
#> 12     08 70 - 80
#> 13     09 80 - 90
#> 14     10 90 - 97
#> 
#> * scale 'const': 
#>   Levels   Range
#> 1      r   0 - 2
#> 2      +   2 - 7
#> 3      1  7 - 10
#> 4      2 10 - 30
#> 5      3 30 - 50
#> 6      4 50 - 70
#> 7      5 70 - 90
#> 
#> * scale 'domin': 
#>    Levels   Range
#> 1       + 0 - 0.1
#> 2      11 0 - 0.1
#> 3       1 0.1 - 2
#> 4       2   2 - 3
#> 5       3   3 - 4
#> 6       4  4 - 13
#> 7       5 13 - 23
#> 8       6 23 - 29
#> 9       7 29 - 42
#> 10      8 42 - 63
#> 11      9 63 - 85
#> 12     10 85 - 99
#> 
#> * scale 'colin': 
#>   Levels   Range
#> 1      1   0 - 2
#> 2      2   2 - 4
#> 3      3  4 - 15
#> 4      4 15 - 38
#> 5      5 38 - 63
#> 6      6 63 - 88
#> 7      0 88 - NA
#> 
#> * scale 'tansly': 
#>    Levels   Range
#> 1       - 0 - 0.1
#> 2      vr 0 - 0.1
#> 3       r 0.1 - 1
#> 4       s 0.1 - 1
#> 5      ro 0.1 - 1
#> 6      lo 0.1 - 1
#> 7       + 0.1 - 1
#> 8       o   1 - 3
#> 9      lf   1 - 3
#> 10     of   1 - 3
#> 11     oa   3 - 4
#> 12     oc   4 - 5
#> 13     od   5 - 6
#> 14      f   6 - 8
#> 15     la   8 - 9
#> 16     fa   8 - 9
#> 17     fc  9 - 10
#> 18     fd 10 - 12
#> 19      a 12 - 15
#> 20     lc 12 - 15
#> 21     ac 15 - 18
#> 22     ld 18 - 22
#> 23     ad 18 - 22
#> 24      c 22 - 30
#> 25     cd 30 - 40
#> 26     va 30 - 40
#> 27      d 40 - 60
#> 
#> * scale 'schltk': 
#>   Levels       Range
#> 1     .1     0 - 0.1
#> 2      1   0.1 - 1.8
#> 3      5   1.8 - 5.1
#> 4     10  5.1 - 11.3
#> 5     20 11.3 - 20.5
#> 6     30 20.5 - 30.5
#> 7     60 30.5 - 50.5
#> 8     70 50.5 - 70.5
#> 

## Compare the 'Turboveg' version with a vegtable version
data(braun_blanquet)
summary(Table$br_bl)
#> ## Number of cover scales: 1 
#> 
#> * scale 'br_bl': 
#>   Levels   Range
#> 1      r   0 - 1
#> 2      x   0 - 1
#> 3      +   1 - 2
#> 4      1   2 - 3
#> 5      2  3 - 13
#> 6      3 13 - 38
#> 7      4 38 - 68
#> 8      5 68 - 88
#> 
summary(braun_blanquet$br_bl)
#> ## Number of cover scales: 1 
#> 
#> * scale 'br_bl': 
#>   Levels    Range
#> 1      r    0 - 1
#> 2      +    0 - 1
#> 3      1    1 - 5
#> 4      2   5 - 25
#> 5      3  25 - 50
#> 6      4  50 - 75
#> 7      5 75 - 100
#> 
```
