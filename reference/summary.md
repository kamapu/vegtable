# Summary method for vegtable objects

Display summaries for
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects.

Those methods are implemented for objects of the classes
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md),
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
and
[shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md).

The method for class `vegtable` retrieves the metadata, the size of the
object, its validity and additional statistics on the content of input
object.

For objects of class
[shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md),
the function `summary()` will either retrieve general statistics when
`companion` is missing, or a more detailed display when accompained by a
[taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
or
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object.

## Usage

``` r
# S4 method for class 'vegtable'
summary(object, units = "Kb", ...)

# S4 method for class 'coverconvert'
summary(object, ...)

# S4 method for class 'shaker'
summary(object, companion, authority = FALSE, ...)

# S4 method for class 'vegtable'
show(object)

# S4 method for class 'vegtable'
print(x, ...)

# S4 method for class 'coverconvert'
show(object)

# S4 method for class 'coverconvert'
print(x, ...)

# S4 method for class 'shaker'
show(object)

# S4 method for class 'shaker'
print(x, ...)
```

## Arguments

- object, x:

  Object to be summarized.

- units:

  Units used for object size (passed to
  [`format()`](https://rdrr.io/r/base/format.html)).

- ...:

  further arguments to be passed to or from other methods.

- companion:

  Companion object (either a
  [taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
  or a
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- authority:

  Logical value indicating whether authors should be displayed or not.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Summary for 'vegtable' objects
summary(Wetlands_veg)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 350.6 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 100 
#>    plots with records: 100 
#>    variables in header: 24 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 417 
#>    taxon concepts: 96 
#>    validity: TRUE 
#> 
## Summary for 'coverconvert' objects
summary(braun_blanquet)
#> ## Number of cover scales: 3 
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
#> * scale 'b_bbds': 
#>   Levels    Range
#> 1      r    0 - 1
#> 2      +    0 - 1
#> 3      1    1 - 5
#> 4     2m    1 - 5
#> 5     2a   5 - 15
#> 6     2b  15 - 25
#> 7      3  25 - 50
#> 8      4  50 - 75
#> 9      5 75 - 100
#> 
#> * scale 'ordin.': 
#>   Levels    Range
#> 1      1    0 - 1
#> 2      2    0 - 1
#> 3      3    1 - 5
#> 4      4    1 - 5
#> 5      5   5 - 15
#> 6      6  15 - 25
#> 7      7  25 - 50
#> 8      8  50 - 75
#> 9      9 75 - 100
#> 
## Summary for 'shaker' objects (alone and with companion)
summary(Wetlands, Wetlands_veg)
#> ## Species groups: 
#> * 'Cyperus papyrus' group: 
#>      Cyperus papyrus 
#>      Cyclosorus interruptus 
#>      Lepistemon owariense 
#> * 'Cyperus latifolius' group: 
#>      Cyperus latifolius 
#>      Ludwigia abyssinica 
#>      Lythrum rotundifolium 
#> * 'Centrostachys aquatica' group: 
#>      Centrostachys aquatica 
#>      Coldenia procumbens 
#>      Heliotropium indicum 
#>      Persicaria senegalensis 
#> * 'Pistia stratiotes' group: 
#>      Pistia stratiotes 
#>      Eichhornia crassipes 
#>      Azolla pinnata ssp. africana 
#> * 'Azolla nilotica' group: 
#>      Azolla nilotica 
#>      Lemna aequinoctialis 
#> * 'Ammannia prieuriana' group: 
#>      Ammannia prieuriana 
#>      Ethulia conyzoides 
#>      Leersia hexandra 
#> 
#> ## Formulas: 
#> * HY1: groups:'Centrostachys aquatica' 
#> * HY2: species:'Nymphaea lotus > 25' 
#> * HE1: groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50' 
#> * HE2: species:'Typha domingensis > 25' 
#> * HE3: species:'Cyperus exaltatus > 50' 
#> * HE4: groups:'Cyperus latifolius' 
#> * HE5: species:'Phragmites australis > 50' 
#> * HE6: species:'Panicum subalbidum > 50' 
#> * HE7: groups:'Ammannia prieuriana' 
#> 
```
