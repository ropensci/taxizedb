taxizedb
========



[![Build Status](https://api.travis-ci.org/ropenscilabs/taxizedb.png?branch=master)](https://travis-ci.org/ropenscilabs/taxizedb)

`taxizedb` - Tools for Working with Taxonomic Databases

## install


```r
devtools::install_github("ropenscilabs/taxizedb")
```


```r
library("taxizedb")
library("dplyr")
```

## start your SQL DBs

Remember to start your PostgreSQL database for ITIS and ThePlantList and your MySQL database for COL

## Download and load DBs

ITIS


```r
x <- db_download_itis()
db_load_itis(x)
```

The Plant List (TPL)


```r
x <- db_download_tpl()
db_load_tpl(x)
```

Cataloge of Life (COL)


```r
x <- db_download_itis()
db_load_itis(x)
```

## connect to the DBs

ITIS 


```r
src <- src_itis()
```

TPL


```r
src <- src_tpl()
```

COL 


```r
src <- src_col()
```

## query with SQL syntax 


```r
sql_collect(src, "select * from hierarchy limit 5")
#> Source: local data frame [5 x 5]
#> 
#>                     hierarchy_string    tsn parent_tsn level childrencount
#>                                (chr)  (int)      (int) (int)         (int)
#> 1                             202422 202422          0     0        145306
#> 2                      202422-846491 846491     202422     1          2666
#> 3               202422-846491-660046 660046     846491     2          2654
#> 4        202422-846491-660046-846497 846497     660046     3             7
#> 5 202422-846491-660046-846497-846508 846508     846497     4             6
```


```r
# or pipe the src to sql_collect
src %>% sql_collect("select * from hierarchy limit 5")
#> Source: local data frame [5 x 5]
#> 
#>                     hierarchy_string    tsn parent_tsn level childrencount
#>                                (chr)  (int)      (int) (int)         (int)
#> 1                             202422 202422          0     0        145306
#> 2                      202422-846491 846491     202422     1          2666
#> 3               202422-846491-660046 660046     846491     2          2654
#> 4        202422-846491-660046-846497 846497     660046     3             7
#> 5 202422-846491-660046-846497-846508 846508     846497     4             6
```

## use dplyr verbs

get a `tbl`


```r
hiers <- src %>% tbl("hierarchy")
```

limit 10


```r
hiers %>% top_n(10)
#> Source: postgres 9.4.5 [sacmac@localhost:5432/ITIS]
#> From: hierarchy [10 x 5]
#> Filter: min_rank(desc(childrencount)) <= 10 
#> 
#>                                                       hierarchy_string
#>                                                                  (chr)
#> 1                                                               202423
#> 2                                                        202423-914154
#> 3                                                 202423-914154-914155
#> 4                                          202423-914154-914155-914158
#> 5                                    202423-914154-914155-914158-82696
#> 6                             202423-914154-914155-914158-82696-563886
#> 7                       202423-914154-914155-914158-82696-563886-99208
#> 8                202423-914154-914155-914158-82696-563886-99208-100500
#> 9         202423-914154-914155-914158-82696-563886-99208-100500-563890
#> 10 202423-914154-914155-914158-82696-563886-99208-100500-563890-914213
#> Variables not shown: tsn (int), parent_tsn (int), level (int),
#>   childrencount (int)
```

select certain fields


```r
hiers %>% select(tsn, level)
#> Source: postgres 9.4.5 [sacmac@localhost:5432/ITIS]
#> From: hierarchy [528,960 x 2]
#> 
#>       tsn level
#>     (int) (int)
#> 1  202422     0
#> 2  846491     1
#> 3  660046     2
#> 4  846497     3
#> 5  846508     4
#> 6  846553     5
#> 7  954935     6
#> 8    5549     7
#> 9    5550     8
#> 10 954936     6
#> ..    ...   ...
```

## Meta

* Please [report any issues or bugs](https://github.com/ropenscilabs/taxizedb/issues).
* License: MIT
* Get citation information for `taxizedb` in R doing `citation(package = 'taxizedb')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
