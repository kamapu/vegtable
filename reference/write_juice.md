# Exporting tables for Juice

This function produce txt files as inport formats for **Juice**
(<https://www.sci.muni.cz/botany/juice/>).

This function produces two output files to be imported into a **Juice**
file: A vegetation table produced by
[`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
and a header table. Both tables share the file name plus a suffix
(`table` for the vegetation table and `header` for the header).

For the import in **Juice**, you go to the menu
`File -> Import -> Table -> from Spreadsheet File (e.g. EXCEL Table)`
and then follow the wizard. Do not forget to select the proper settings
in the wizard: 1) 'Character delimiting columns: Comma' (for default
argument values). 2) 'Use the second column as layer information:
Unchecked'. 3) 'Cover values: Percentage Values'.

To further import the header table you need to go to the menu
`File -> Import -> Header Data -> From Comma Delimited File`.

In the `header` (see **Value**), the first column (`Table number`)
corresponds to the plot number assigned by **Juice** at import, while
the column (`Releve number`) is the number originally assigned to the
plot (e.g. **Turboveg** ID).

## Usage

``` r
write_juice(data, file, formula, ...)

# S4 method for class 'vegtable,character,formula'
write_juice(
  data,
  file,
  formula,
  FUN,
  db_name = "Plot Observations",
  header,
  coords,
  sep = ",",
  ...
)

read_juice(file, encoding = "LATIN-1", sep = ";", na = "", ...)
```

## Arguments

- data:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- file:

  Character value indicating the name of output files (without file
  extension).

- formula:

  A formula passed to
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md).

- ...:

  Further arguments. While `write_juice()` passes them to the function
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md),
  `read_juice()` passes those arguments to
  [`readLines()`](https://rdrr.io/r/base/readLines.html).

- FUN:

  Funtion passed to
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md).

- db_name:

  Name for data set displayed in inport wizard.

- header:

  Variables of header to be exported.

- coords:

  Names of coordinate variables in header of `data`.

- sep:

  Separator used to split rows into columns.

- encoding:

  Argument passed to
  [`readLines`](https://rdrr.io/r/base/readLines.html).

- na:

  Character used as not available values.

## Value

For `read_juice()`, a list with two elements: A data frame of species by
plot (`cross_table`), and a data frame with header data (`header`).

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Subset and transform cover values to percentage
vegetation <- Kenya_veg[1:20, ]
vegetation <- cover_trans(x = vegetation, to = "cover_percent", rule = "middle")

## Write in tempdir
write_juice(data = vegetation, file = file.path(tempdir(), "SWEA"),
    formula = cover_percent ~ ReleveID + AcceptedName, FUN = mean,
    header = c("ReleveID", "COMM_TYPE"))
#> Processing header data...
#> Processing vegetation table...
#> DONE!
#> 
#> Data set name: Plot Observations
#> Number of observations: 20
#> Recorded species: 104
## Installed 'Juice' version of 'Wetlands_veg'
Veg <- file.path(path.package("vegtable"), "juice", "Wetlands_juice.txt")
Veg <- read_juice(Veg)

summary(Veg)
#>             Length Class      Mode
#> cross_table 88     data.frame list
#> header      24     data.frame list
```
