# Class containing Cocktail algorithms.

Objects used for collecting Cocktail definitions.

These objects work as **expert systems** for recognition of defined
vegetation units among plots of a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object. A `shaker` object will be always dependent on a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object, which is called `companion`. Since modifications in the
`companion` may affect the functionality of the `shaker` object, it will
be recommended to create the last during a session by a source script
instead of recycling them from old R images.

## Slots

- `pseudos`:

  List containing IDs of taxa that will be merged into pseudo-species.

- `groups`:

  List containing IDs of taxa belonging to the same Cocktail group.

- `dominants`:

  A data frame including lists of species used as dominant species in
  Cocktail algorithms, as well as operators and cover values used in the
  formulas.

- `formulas`:

  List with formulas that will be used as definitions for vegetation
  units.

## See also

[`make_cocktail()`](http://kamapu.github.io/vegtable/reference/make_cocktail.md)
[`set_pseudo()`](http://kamapu.github.io/vegtable/reference/make_cocktail.md)
[`set_group()`](http://kamapu.github.io/vegtable/reference/make_cocktail.md)
[`set_formula()`](http://kamapu.github.io/vegtable/reference/make_cocktail.md)

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
showClass("shaker")
#> Class "shaker" [package "vegtable"]
#> 
#> Slots:
#>                                                   
#> Name:     pseudos     groups  dominants   formulas
#> Class:       list       list data.frame       list
```
