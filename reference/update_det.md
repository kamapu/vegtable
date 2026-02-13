# Update by determined specimens

Reference specimens can be integrated in slot **layers** within a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object. Updated entries in the specimens can be updated in slot
**samples** by using this function. Alternatively expert opinions can be
inserted and applied in case of disagreement with the original records.

## Usage

``` r
update_det(x, specimens, ...)

# S4 method for class 'vegtable,character'
update_det(x, specimens, ...)
```

## Arguments

- x:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object to be updated.

- specimens:

  A character vector indicating the names of tables included in slot
  **layers** with updates to be applied. Note that they will be applied
  in the same order of the vector in the case of multiple updates.

- ...:

  Further arguments (not yet in use).
