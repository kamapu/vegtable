# Create coverconvert objects

The class
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
contains tables for transforming cover values to percentage using the
function
[`cover_trans()`](http://kamapu.github.io/vegtable/reference/cover_trans.md).
These objects can be created from conversion tables imported as data
frames.

## Usage

``` r
df2coverconvert(x, ...)

# S3 method for class 'list'
df2coverconvert(x, ...)

# S3 method for class 'data.frame'
df2coverconvert(x, name, ...)
```

## Arguments

- x:

  Either a data frame or a list of data frames containing the conversion
  table. Three columns are mandatory in such data frames, namely
  **value** (factor with the symbols for each class in the cover scale,
  sorted from the lowest to the highest value), **bottom** (numeric
  value with the bottom values of each class), and **top** (numeric
  value with the top values of each class). The values **bottom** and
  **top** are usually as cover percentage but they may refer to any
  other numeric abundance.

- ...:

  Further arguments passed among methods.

- name:

  A character value used as name of the cover scale in the data frame
  method. In the list method, this name will be extracted from the names
  of the elements in the list.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Convert object into list
cov <- as(Kenya_veg@coverconvert, "list")

## Convert back to coverconvert
cov <- df2coverconvert(cov)
```
