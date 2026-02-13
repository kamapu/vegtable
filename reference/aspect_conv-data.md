# Conversion of aspect classes to azimuth

Conversion table required to transform values of aspect to azimuth in
degrees.

## Usage

``` r
aspect_conv
```

## Format

A numeric vector of values in degrees for the symbols used as names.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
aspect_conv[c("N", "S", "ENE", "SSW")]
#>     N     S   ENE   SSW 
#>   0.0 180.0  67.5 202.5 
```
