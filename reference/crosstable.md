# Generating cross tables from database lists

cross table is the most common format required by statistical packages
used to analyse vegetation data (e.g.
[vegan](https://CRAN.R-project.org/package=vegan)).

You may use for convenience a formula as
`'abundance ~ plot + species + ...'`. Additional variables used for rows
(`...`) can be for instance the layers. For objects of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md),
the formula can also include variables from the species list (for
example `AcceptedName`, `AuthorName`) or even taxon traits.

If required, tables already formatted as cross tables can be converted
into column-oriented tables by using the function `cross2db()`.

## Usage

``` r
crosstable(formula, data, ...)

# S4 method for class 'formula,data.frame'
crosstable(
  formula,
  data,
  FUN,
  na_to_zero = FALSE,
  use_nas = TRUE,
  as_matrix = FALSE,
  ...
)

# S4 method for class 'formula,vegtable'
crosstable(
  formula,
  data,
  FUN,
  level,
  include_lower = FALSE,
  na_to_zero = FALSE,
  use_nas = TRUE,
  ...
)

cross2db(object, ...)

# S3 method for class 'data.frame'
cross2db(
  object,
  layers = FALSE,
  na_strings,
  terms,
  na.rm = TRUE,
  split_cover,
  ...
)

# S3 method for class 'formula'
cross2db(object, data, ...)

# S3 method for class 'matrix'
cross2db(object, ...)
```

## Arguments

- formula:

  A formula indicating the variables used in the cross table. This
  formula can be represented as `'abundance ~ cols + rows'`, where
  `'abundance'` is the numeric variable quantified for a row in a
  column, for instance the abundance of a species in a plot. Further
  variables can be set as additional rows indices in a cross table.

- data:

  Either a data frame or an object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- ...:

  Further arguments passed to the function
  [`stats::aggregate()`](https://rdrr.io/r/stats/aggregate.html).

- FUN:

  Function used to aggregate values in the case of a multiple occurrence
  of a species in a plot, for instance.

- na_to_zero:

  A logical value indicating whether zeros should be inserted into empty
  cells or not.

- use_nas:

  Logical value indicating whether NAs should be considered as levels
  for categorical variables or not.

- as_matrix:

  A logical value, whether output should be done as matrix or data
  frame.

- level:

  A character vector with taxonomic ranks (levels) requested in the
  cross table.

- include_lower:

  A logical value indicating wether lower value to the requested levels
  should be merged or not. It works only if `'level'` is not missing.
  Note that if you like to include higher ranks or rankless taxa in the
  cross table, you will rahter need to run
  [`taxlist::merge_taxa()`](https://docs.ropensci.org/taxlist/reference/merge_taxa.html)
  on slot **species**.

- object:

  A data frame or a matrix including a cross table. Note that
  `cross2db()` assumes observations as columns and species (and layers)
  as rows in the `data.frame-method` but species as columns and
  observations as rows in the `matrix-method`.

- layers:

  Logical value, whether the cross table includes a layer column besides
  the species column or not. This apply for `cross2db()` and will be
  ignored if 'terms' are provided.

- na_strings:

  Character vector indicating no records in the cross table.

- terms:

  A character vector with the names of columns used by `cross2db()` as
  species and layers.

- na.rm:

  A logical value indicating to `cross2db()` whether empty cells should
  be included in the database list or not.

- split_cover:

  A character value showing a symbol used to split the column cover by
  `cross2db()`. This is used in the case that a vegetation table
  includes abundance and sociability in a same value.

## Value

An object of class [data.frame](https://rdrr.io/r/base/data.frame.html).

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
# Produce a subset
veg <- subset(Kenya_veg, REFERENCE == 2331, slot = "header")

## transform cover to percentage
veg <- cover_trans(veg, to = "cover_perc", rule = "middle")

## cross table of the first 5 plots
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
  data = veg[1:5, ], FUN = mean, na_to_zero = TRUE)
head(Cross)
#>               AcceptedName                AuthorName 1940 1937 1938 1939 1941
#> 1      Commiphora africana          (A. Rich.) Engl.  0.5    0    0    0  0.0
#> 2     Eragrostis macilenta         (A. Rich.) Steud.  3.0    3    3    3  0.0
#> 3       Helinus mystacinus (Aiton) E. Mey. ex Steud.  0.0    0    0    0  3.0
#> 4   Eragrostis cilianensis        (All.) F. T. Hubb.  3.0    3    3    3  3.0
#> 5 Astripomoea lachnosperma        (Choisy) A. Meeuse  3.0    3    0    3  3.0
#> 6       Coccinia trilobata        (Cogn.) C. Jeffrey  3.0    0    3    3  0.5

## cross table of recorded genera
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = mean, level = "genus")
head(Cross[ , 1:7])
#>   AcceptedName       AuthorName 2005 2059 2061 2080 2102
#> 1  Cyphostemma (Planch.) Alston  0.5  0.5  0.5  0.5  0.5
#> 2  Cassipourea            Aubl.   NA   NA   NA   NA   NA
#> 3       Dregea          E. Mey.   NA   NA   NA   NA   NA
#> 4    Digitaria           Haller   NA   NA   NA   NA   NA
#> 5 Chlorophytum        Ker Gawl.   NA   NA   NA   NA   NA
#> 6 Plectranthus          L'HÃ©r.   NA   NA   NA   NA   NA

## cross table of data merged to genus
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = sum, level = "genus", include_lower = TRUE)
head(Cross[ , 1:7])
#>      AcceptedName                                 AuthorName 2008 2102 2112
#> 1       Bullockia      (Bridson) Razafim., Lantz & B. Bremer  0.5   NA   NA
#> 2          Scutia                     (Comm. ex DC.) Brongn.   NA  0.5  0.5
#> 3   Dichrostachys                         (DC.) Wight & Arn.   NA   NA   NA
#> 4 Lepidotrichilia (Harms) J.-F. Leroy ex T.D. Penn. & Styles   NA   NA   NA
#> 5      Afrocarpus        (J. Buchholz & E.G. Gray) C.N. Page   NA   NA   NA
#> 6        Leonotis                             (Pers.) R. Br.   NA   NA   NA
#>   2121 2165
#> 1   NA   NA
#> 2  0.5    1
#> 3   NA   NA
#> 4 10.0   NA
#> 5   NA   NA
#> 6   NA   NA

## the same for families
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = sum, level = "family", include_lower = TRUE)
head(Cross[ , 1:7])
#>     AcceptedName       AuthorName 1966 1980 1988 2005 2010
#> 1 Passifloraceae Juss. ex Roussel  0.5  0.5  0.5  0.5  0.5
#> 2    Acanthaceae               NA   NA 22.0 10.0  0.5 19.0
#> 3    Achariaceae               NA   NA   NA   NA   NA   NA
#> 4      Aizoaceae               NA   NA   NA   NA  3.0   NA
#> 5  Amaranthaceae               NA  3.0   NA 19.0 36.0  3.0
#> 6 Amaryllidaceae               NA   NA   NA   NA   NA   NA
```
