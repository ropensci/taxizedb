taxizedb
========



[![cran checks](https://cranchecks.info/badges/worst/taxizedb)](https://cranchecks.info/pkgs/taxizedb)
[![Build Status](https://travis-ci.org/ropensci/taxizedb.svg?branch=master)](https://travis-ci.org/ropensci/taxizedb)
[![codecov](https://codecov.io/gh/ropensci/taxizedb/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/taxizedb)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/taxizedb)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/taxizedb)](https://cran.r-project.org/package=taxizedb)
[![DOI](https://zenodo.org/badge/53961466.svg)](https://zenodo.org/badge/latestdoi/53961466)

`taxizedb` - Tools for Working with Taxonomic Databases on your Machine

[taxize](https://github.com/ropensci/taxize) is a heavily used taxonomic toolbelt
package in R - However, it makes web requests for nearly all methods. That is fine
for most cases, but when the user has many, many names it is much more efficient
to do requests to a local SQL database.

Not all taxonomic databases are publicly available, or possible to mash into a SQLized
version. Taxonomic DB's supported thus far:

* NCBI - they provide a SQL dump
* ITIS - they provide a SQL dump
* COL - we make a SQL database from darwin core archive
* Theplantlist - we make a SQL database from CSV files they provide
* GBIF taxonomic backbone - we make a SQL database from darwin core archive
* Wikidata taxonomic data - from <https://zenodo.org/record/1213477>

Get in touch [in the issues](https://github.com/ropensci/taxizedb/issues) with
any ideas on new data sources.

This package for each data sources performs the following tasks:

* Download database - `db_download_*`
* Create `dplyr` SQL backend - `src_*`

All databases are SQLite.

Using the src connection, use `dplyr`, etc. to do operations downstream. Or create
your own database connection to the sqlite file.

## install

cran version


```r
install.packages("taxizedb")
```

dev version


```r
devtools::install_github("ropensci/taxizedb")
```


```r
library("taxizedb")
library("dplyr")
```

## Download DBs

ITIS


```r
db_download_itis()
```

The Plant List (TPL)


```r
db_download_tpl()
```

Catalogue of Life (COL)


```r
db_download_col()
```

## connect to the DBs

By default `src_*` functions use a path to the cached database file.
You can alternatively pass in your own path if you've put it 
somewhere else.

ITIS


```r
src_itis <- src_itis()
```

TPL


```r
src_tpl <- src_tpl()
```

COL


```r
src_col <- src_col()
```

## query with SQL syntax


```r
sql_collect(src_itis, "select * from hierarchy limit 5")
#> # A tibble: 5 x 5
#>                     hierarchy_string    tsn parent_tsn level childrencount
#> *                              <chr>  <int>      <int> <int>         <int>
#> 1                             202422 202422          0     0        154282
#> 2                      202422-846491 846491     202422     1          2666
#> 3               202422-846491-660046 660046     846491     2          2654
#> 4        202422-846491-660046-846497 846497     660046     3             7
#> 5 202422-846491-660046-846497-846508 846508     846497     4             6
```


```r
# or pipe the src to sql_collect
src_itis %>% sql_collect("select * from hierarchy limit 5")
```

## use dplyr verbs

get a `tbl`


```r
hiers <- src_itis %>% tbl("hierarchy")
#> # Source:   table<hierarchy> [?? x 5]
#> # Database: postgres 9.6.0 [sacmac@localhost:5432/ITIS]
#>                                              hierarchy_string    tsn parent_tsn level childrencount
#>                                                         <chr>  <int>      <int> <int>         <int>
#>  1                                                     202422 202422          0     0        154282
#>  2                                              202422-846491 846491     202422     1          2666
#>  3                                       202422-846491-660046 660046     846491     2          2654
#>  4                                202422-846491-660046-846497 846497     660046     3             7
#>  5                         202422-846491-660046-846497-846508 846508     846497     4             6
#>  6                  202422-846491-660046-846497-846508-846553 846553     846508     5             5
#>  7           202422-846491-660046-846497-846508-846553-954935 954935     846553     6             3
#>  8      202422-846491-660046-846497-846508-846553-954935-5549   5549     954935     7             2
#>  9 202422-846491-660046-846497-846508-846553-954935-5549-5550   5550       5549     8             0
#> 10           202422-846491-660046-846497-846508-846553-954936 954936     846553     6             0
#> # ... with more rows
```

select certain fields


```r
hiers %>% select(TSN, level)
#> # Source:   lazy query [?? x 2]
#> # Database: postgres 9.6.0 [sacmac@localhost:5432/ITIS]
#>       tsn level
#>     <int> <int>
#>  1 202422     0
#>  2 846491     1
#>  3 660046     2
#>  4 846497     3
#>  5 846508     4
#>  6 846553     5
#>  7 954935     6
#>  8   5549     7
#>  9   5550     8
#> 10 954936     6
#> # ... with more rows
```

## Local versions of `taxize` functions

A few of the key functions from `taxize` have been ported to `taxizedb`.
Support is currently limited to the NCBI taxonomy database.

`children` accesses the nodes immediately descending from a given taxon


```r
children(3701, db='ncbi')
#> $`3701`
#>    childtaxa_id                                                     childtaxa_name childtaxa_rank
#> 1       1837063                         Arabidopsis thaliana x Arabidopsis halleri        species
#> 2       1547872                                              Arabidopsis umezawana        species
#> 3       1328956 (Arabidopsis thaliana x Arabidopsis arenosa) x Arabidopsis suecica        species
#> 4       1240361                         Arabidopsis thaliana x Arabidopsis arenosa        species
#> 5        869750                          Arabidopsis thaliana x Arabidopsis lyrata        species
#> 6        412662                                            Arabidopsis pedemontana        species
#> 7        378006                         Arabidopsis arenosa x Arabidopsis thaliana        species
#> 8        347883                                              Arabidopsis arenicola        species
#> 9        302551                                              Arabidopsis petrogena        species
#> 10        97980                                               Arabidopsis croatica        species
#> 11        97979                                            Arabidopsis cebennensis        species
#> 12        81970                                                Arabidopsis halleri        species
#> 13        59690                                             Arabidopsis kamchatica        species
#> 14        59689                                                 Arabidopsis lyrata        species
#> 15        45251                                               Arabidopsis neglecta        species
#> 16        45249                                                Arabidopsis suecica        species
#> 17        38785                                                Arabidopsis arenosa        species
#> 18         3702                                               Arabidopsis thaliana        species
#> 
#> attr(,"class")
#> [1] "children"
#> attr(,"db")
#> [1] "ncbi"
```

`classification` finds the lineage of a taxon


```r
classification(3702, db='ncbi')
#> $`3702`
#>                    name         rank      id
#> 1    cellular organisms      no rank  131567
#> 2             Eukaryota superkingdom    2759
#> 3         Viridiplantae      kingdom   33090
#> 4          Streptophyta       phylum   35493
#> 5        Streptophytina    subphylum  131221
#> 6           Embryophyta      no rank    3193
#> 7          Tracheophyta      no rank   58023
#> 8         Euphyllophyta      no rank   78536
#> 9         Spermatophyta      no rank   58024
#> 10        Magnoliophyta      no rank    3398
#> 11      Mesangiospermae      no rank 1437183
#> 12       eudicotyledons      no rank   71240
#> 13           Gunneridae      no rank   91827
#> 14         Pentapetalae      no rank 1437201
#> 15               rosids     subclass   71275
#> 16              malvids      no rank   91836
#> 17          Brassicales        order    3699
#> 18         Brassicaceae       family    3700
#> 19           Camelineae        tribe  980083
#> 20          Arabidopsis        genus    3701
#> 21 Arabidopsis thaliana      species    3702
#> 
#> attr(,"class")
#> [1] "classification"
#> attr(,"db")
#> [1] "ncbi"
```

`downstream` finds all taxa descending from a taxon


```r
downstream(3700, db='ncbi')
#> $`3700`
#>      childtaxa_id                                                     childtaxa_name     rank
#> 1         2071891                                                     Draba taylorii  species
#> 2         2071524                                                  Rorippa tenerrima  species
#> 3         2071523                                                Rorippa crystallina  species
#> 4         2071509                                                   Physaria calderi  species
#> 5         2071468                                                 Erysimum arenicola  species
#> 6         2071452                                                   Draba yukonensis  species
#> 7         2071451                                                   Draba thompsonii  species
#> ...
#> 326       1492251                                                 Erysimum lilacinum  species
#> 327       1492250                                              Erysimum leucanthemum  species
#> 328       1492249                                               Erysimum leptostylum  species
#> 329       1492248                                              Erysimum leptophyllum  species
#> 330       1492247                                               Erysimum leptocarpum  species
#> 331       1492246                                                Erysimum ledebourii  species
#> 332       1492245                                                Erysimum laxiflorum  species
#> 333       1492244                                                  Erysimum kurdicum  species
#>  [ reached getOption("max.print") -- omitted 2880 rows ]
#>
#> attr(,"class")
#> [1] "downstream"
#> attr(,"db")
#> [1] "ncbi"
```

All of these functions run very fast. It only takes a few seconds to find all
bacterial taxa and count them: 


```r
downstream(2, db='ncbi')[[1]] %>%
    dplyr::group_by(rank) %>%
    dplyr::count()
#> #> [1] 138695
#> # A tibble: 18 x 2
#> # Groups:   rank [18]
#> rank                 n
#> <chr>            <int>
#>  1 class               83
#>  2 family             483
#>  3 forma                4
#>  4 genus             3497
#>  5 no rank          37140
#>  6 order              198
#>  7 phylum             134
#>  8 species          97031
#>  9 species group       68
#> 10 species subgroup    10
#> 11 subclass             3
#> 12 subfamily            1
#> 13 subgenus             1
#> 14 suborder             8
#> 15 subphylum            1
#> 16 subspecies          10
#> 17 tribe                2
#> 18 varietas            21
```

## Mapping functions

Several mapping functions are available for the NCBI taxonomy database:


```r
# Map scientific or common names to taxonomy IDs
name2taxid("pig")
#> [1] "9823"

# Map taxonomy IDs to scientific names
taxid2name(9823)
#> [1] "Sus scrofa"

# Map taxonomy IDs to rank
taxid2rank(2)
#> [1] "superkingdom"
```

## Contributors

* [Scott Chamberlain](https://github.com/sckott)
* [Zebulun Arendsee](https://github.com/arendsee)
* [Tamora James](https://github.com/tdjames1)


## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxizedb/issues).
* License: MIT
* Get citation information for `taxizedb` in R doing `citation(package = 'taxizedb')`
* Please note that this project is released with a [Contributor Code of Conduct][coc]. By participating in this project you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[coc]: https://github.com/ropensci/taxizedb/blob/master/CODE_OF_CONDUCT.md
