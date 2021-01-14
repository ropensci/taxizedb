taxizedb
========



[![cran checks](https://cranchecks.info/badges/worst/taxizedb)](https://cranchecks.info/pkgs/taxizedb)
[![R-check](https://github.com/ropensci/taxizedb/workflows/R-check/badge.svg)](https://github.com/ropensci/taxizedb/actions?query=workflow%3AR-check)
[![CircleCI](https://circleci.com/gh/ropensci/taxizedb.svg?style=svg)](https://circleci.com/gh/ropensci/taxizedb)
[![codecov](https://codecov.io/gh/ropensci/taxizedb/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/taxizedb)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/taxizedb)](https://github.com/r-hub/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/taxizedb)](https://cran.r-project.org/package=taxizedb)
[![DOI](https://zenodo.org/badge/53961466.svg)](https://zenodo.org/badge/latestdoi/53961466)

`taxizedb` - Tools for Working with Taxonomic Databases on your machine

Docs: <https://ropensci.github.io/taxizedb/>

[taxize](https://github.com/ropensci/taxize) is a heavily used taxonomic toolbelt
package in R - However, it makes web requests for nearly all methods. That is fine
for most cases, but when the user has many, many names it is much more efficient
to do requests to a local SQL database.

## Data sources

Not all taxonomic databases are publicly available, or possible to mash into a SQLized
version. Taxonomic DB's supported:

- NCBI: text files are provided by NCBI, which we stitch into a sqlite db
- ITIS: they provide a sqlite dump, which we use here
- The PlantList: created from stitching together csv files. this
 source is no longer updated as far as we can tell. they say they've
 moved focus to the World Flora Online
- Catalogue of Life: created from Darwin Core Archive dump.
- GBIF: created from Darwin Core Archive dump. right now we only have
 the taxonomy table (called gbif), but will add the other tables in the
 darwin core archive later
- Wikidata: aggregated taxonomy of Open Tree of Life, GLoBI and Wikidata. 
 On Zenodo, created by Joritt Poelen of GLOBI.
- World Flora Online: http://www.worldfloraonline.org/

Update schedule for databases:

- NCBI: since `db_download_ncbi` creates the database when the function
is called, it's updated whenever you run the function
- ITIS: since ITIS provides the sqlite database as a download, you can
delete the old file and run `db_download_itis` to get a new dump;
they I think update the dumps every month or so
- The PlantList: no longer updated, so you shouldn't need to download
this after the first download. hosted on Amazon S3
- Catalogue of Life: a GitHub Actions job runs once a day at 00:00 UTC,
building the lastest COL data into a SQLite database thats hosted on
Amazon S3
- GBIF: a GitHub Actions job runs once a day at 00:00 UTC,
building the lastest GBIF data into a SQLite database thats hosted on
Amazon S3
- Wikidata: last updated April 6, 2018. Scripts are available to 
update the data if you prefer to do it yourself.
- World Flora Online: since `db_download_wfo` creates the database when
the function is called, it's updated whenever you run the function

 Links:

- NCBI: ftp://ftp.ncbi.nih.gov/pub/taxonomy/
- ITIS: https://www.itis.gov/downloads/index.html
- The PlantList - http://www.theplantlist.org/
- Catalogue of Life:
  - latest monthly edition via http://www.catalogueoflife.org/DCA_Export/archive.php
- GBIF: http://rs.gbif.org/datasets/backbone/
- Wikidata: https://zenodo.org/record/1213477
- World Flora Online: http://www.worldfloraonline.org/

Get in touch [in the issues](https://github.com/ropensci/taxizedb/issues) with
any ideas on new data sources.

All databases are SQLite.

## Package API

This package for each data sources performs the following tasks:

* Downloaded taxonomic databases `db_download_*`
* Create `dplyr` SQL backend via `dbplyr::src_dbi` - `src_*` 
* Query and get data back into a data.frame - `sql_collect`
* Manage cached database files - `tdb_cache`
* Retrieve immediate descendents of a taxon - `children`
* Retrieve the taxonomic hierarchies from local database - `classification`
* Retrieve all taxa descending from a vector of taxa - `downstream`
* Convert species names to taxon IDs - `name2taxid`
* Convert taxon IDs to species names - `taxid2name`
* Convert taxon IDs to ranks - `taxid2rank`

You can use the `src` connections with `dplyr`, etc. to do operations downstream. Or use the database connection to do raw SQL queries.

## install

cran version


```r
install.packages("taxizedb")
```

dev version


```r
remotes::install_github("ropensci/taxizedb")
```

## Contributors

* [Scott Chamberlain](https://github.com/sckott)
* [Zebulun Arendsee](https://github.com/arendsee)
* [Tamora James](https://github.com/tdjames1)


## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxizedb/issues).
* License: MIT
* Get citation information for `taxizedb` in R doing `citation(package = 'taxizedb')`
* Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
