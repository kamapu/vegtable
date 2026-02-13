# Retrieve or replace relations in vegtable objects

Tables providing information about levels of categorical variables in
the header are called `popups` in **Turboveg** databases but `relations`
in
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects. Such variables will be converted into factors in the slot
`header` according to the levels and their sorting in the respective
relation.

## Usage

``` r
veg_relation(vegtable, relation, ...)

# S4 method for class 'vegtable,character'
veg_relation(vegtable, relation, match_header = FALSE, ...)

veg_relation(vegtable) <- value

# S4 method for class 'vegtable,data.frame'
veg_relation(vegtable) <- value
```

## Arguments

- vegtable:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- relation:

  A character value indicating the relation table to be retrieved or
  replaced.

- ...:

  Further arguments to be passed among methods.

- match_header:

  A logical vector, whether only levels occurring in slot `header`
  should be considered or all.

- value:

  A data frame containing the new veg_relation.

## Value

This function retrieves and object of class `data.frame`. In the
replacement method, an object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md),
including `value` in the slot `relations`.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## overview of references
veg_relation(Kenya_veg, "REFERENCE")
#>    REFERENCE                     AUTHOR YEAR
#> 1       2789           Alvarez M et al. 2012
#> 2       3782                  Alvarez M 2016
#> 3       3489         Cahngwony K et al. 2015
#> 4       3783                  Alvarez M 2013
#> 5       2974                Bussmann RW 2002
#> 6       3011                  Schmitt K 1991
#> 7       3012                Bussmann RW 1994
#> 8       3784                  Alvarez M 2011
#> 9       2331                  Bronner G 1990
#> 10      3785                  Alvarez M 2014
#> 11      3786                     Behn K 2013
#> 12      3506          Fujiwara K et al. 2014
#> 13      3545                 Eisering M 2012
#> 14      3388                    Taton A 1949
#> 15      2797               Ngome AF al. 2012
#> 16      2396 Schultka W and Cornelius R 1997
#> 17      3352 Ayichedehou M and Lejoly J 2000
#> 18      3460             de Bock et al. 2009
#> 19      3239               Guyot et al. 1994
#> 20      3203                    Taton A 1948
#> 21      3211    Szafranski F et Apema K 1983
#> 22      3212        Szafranski F et al. 1983
#> 23      3158               Mullenders W 1953
#> 24      3350               Masens D-MYB 2000
#> 25      3347    Lejoly J and Lisowski S 2000
#> 26      3245         de Foucault et al. 1999
#> 27      3379           Mosango M et al. 2001
#> 28      3562        Abyot Dibaba et al. 2014
#> 29      3589              Zerihun Woldu 1986
#> 30      3464    Furness HD and Breen CM 1980
#>                                                                                                                                                                                                     TITLE
#> 1                                                                                                                      Floristic classification of the vegetation in small wetlands of Kenya and Tanzania
#> 2                                                          Syntaxonomic classification of aquatic and semi-aquatic vegetation in two East African sites: preliminary results based on Cocktail algorithms
#> 3                                                                             Biomass and quality changes of forages along land use and soil type gradients in the riparian zone of Lake Naivasha, Kenya.
#> 4                                                                                                                                                 Relv\u0082s collected in Kihoto (Lake Naivasha, Kenya).
#> 5                                                                                                              Islands in the desert - forest vegetation of Kenya's smaller mountains and highland areas.
#> 6                                                                                                                                                     The vegetation of the Aberdare National Park Kenya.
#> 7                                                                                   The forests of Mount Kenya - vegetation, ecology, destruction and management of a tropical mountain forest ecosystem.
#> 8                                                                                                                                          Relev\u0082s collected in the Churo-Laikipia transect (Kenya).
#> 9                                                                                                                              Vegetation and land use in the Mathews Range area, Samburu-District, Kenya
#> 10                                                                                                                                                   Relev\u0082s collected in Marigat and Churo (Kenya).
#> 11                                                                                                                                                                                        GlobE Typology.
#> 12                                                                                                                                   Forest types and biodiversity around the Great Rift Valley in Kenya.
#> 13                                                                          Forest age, habitat disturbance and vegetation composition as potential determinants of Afrotropical geometrid moth diversity
#> 14                                                                                                        Les principales associations herbeuses de la R\u0082gion de Nioka et leur valeur agrostologique
#> 15                                                                                              Management options and soil types differentially affect weeds in maize fields of Kakamega, Western Kenya.
#> 16                                                                                                               Vegetation structure of a heavily grazed range in northern Kenya: tree and shrub canopy.
#> 17                                                                                                        \u0090tude phytosociologique des friches sur vertisols dans les environs d'Abomey (B\u0082nin).
#> 18                                                                                                      Ecohydrology of a seasonal wetland in the Rift Valley: ecological characterisation of Lake Solai.
#> 19                                                                                         La v\u0082g\u0082tation des zones inond\u0082es du Sud du Togo et son \u0082tat actuel sous l'emprise humaine.
#> 20                                                                                                                                          La colonisation des roches granitiques de la Region de Nioka.
#> 21                                              Contribution \u0085 la connaissance des groupements v\u0082g\u0082taux aquatiques et semi-aquatiques dans les environs de Kisangani (Haut-Za\u008bre). I.
#> 22 Contribution \u0085 la connaissance des groupemente v\u0082g\u0082taux aquatiques et semi-aquatiques dans les environs de Kisangani (Haut-Za\u008bre). II. L'association \u0085 Eleocharis acutangula.
#> 23                                                                                       Contribution a l'\u0082tude des groupements v\u0082g\u0082taux de la contr\u0082e de Goma-Kisenyi (Kivu-Ruanda).
#> 24                                                   La prairie aquatique \u0085 Salvinia nymphellula et Limnophila ceratophylloides dans la r\u0082gion de Kikwit (Bandundu, R\u0082p. D\u0082m. Congo).
#> 25                                                                                    La v\u0082g\u0082tation des clairi\u008ares sur sol hydromorphe dans le Parc National d'Odzala (Congo-Brazzaville).
#> 26                                                                                                   Contribution \u0085 l'\u0082tude phytosociologique des v\u0082g\u0082tations inondables du Sud Togo.
#> 27                                                                                                                              Talinetum paniculatum, a new synanthropic association of tropical Africa.
#> 28                                                             Diversity, structure and regeneration status of the woodland and riverine vegetation of Sire Beggo in Gololcha District, Eastern Ethiopia.
#> 29                                                                                                                                       Grassland communities on the central plateau of Shewa, Ethiopia.
#> 30                                                                                                                            The vegetation of seasonally flooded areas of the Pongolo River floodplain.
#>                                                                         PUBLISHED
#> 1                                              Biodiversity and Ecology 4: 63-76.
#> 2                                                     Phytocoenologia (in press).
#> 3                                              Ecological Indicators 49: 169-177.
#> 4                                                                    Unpublished.
#> 5                              Journal of East African Natural History 91: 27-79.
#> 6                                                  Hochgebigsforschung 8: 259 pp.
#> 7                                                                            <NA>
#> 8                                                                    Unpublished.
#> 9                                                    Dissertationes Botanicae 160
#> 10                                                                   Unpublished.
#> 11                                                                   Unpublished.
#> 12                                              Contributii Botanice 49: 143-178.
#> 13                                                            ETH (Master Thesis)
#> 14             Bulletin Agricole du Congo Belge et du Ruanda-Urundi 40: 1884-1900
#> 15                    Journal of Agricultural Science and Technology A 2: 104-114
#> 16                                       Journal of Arid Environments 36: 291-306
#> 17                                      Colloques Phytosociologiques 27: 465-477.
#> 18                                        African Journal of Ecology 47: 289-298.
#> 19                                                    Biogeographica 70: 161-182.
#> 20                                                          Vegetatio 1: 317-332.
#> 21  Bulletin de la Soci\u0082t\u0082 Royale de Botanique de Belgique 116: 93-106.
#> 22 Bulletin de la Soci\u0082t\u0082 Royale de Botanique de Belgique 116: 189-194.
#> 23                                                            Vegetatio 4: 73-83.
#> 24                                      Colloques Phytosociologiques 27: 429-442.
#> 25                                      Colloques Phytosociologiques 27: 371-382.
#> 26                                        Belgian Journal of Botany 132: 141-152.
#> 27                                           Polish Botanical Journal 46: 99-107.
#> 28                                  Momona Ethiopian Journal of Science 6: 70-96.
#> 29                                                            Vegetatio 67: 3-16.
#> 30                                                          Bothalia 13: 217-231.
```
