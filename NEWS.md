taxizedb 0.3.1
==============

* New maintainer (#65).

taxizedb 0.3.0
==============

## NEW FEATURES

* `db_download()` gains new parameter `overwrite` (logical): used to state that you want to overwrite an existing database on disk. before this you would have to manually delete an older database file (#34)
* new function added `taxa_at()` for getting taxa at specific scientific ranks. For example, your known taxon is the family Lachnospiraceae with NCBI identifier of 186803. You want information on the phylum which the Lachnospiraceae family is in. This function can do that for you.  (#51)

## BUG FIXES

* fixed problem in internal function `txdb_rr()`: in older verions of R (e.g., 3.6) we were creating a data.frame in this function without settings `stringsAsFactors=FALSE`, resulting in different behavior in R v3 vs. R v4 given the change in `stringsAsFactors` behavior in R v4 onward (#54)


taxizedb 0.2.2
==============

## BUG FIXES

* fix failing tests (#50)


taxizedb 0.2.0
==============

## NEW FEATURES

* gains 3 new data sources: NCBI taxonomy, World Flora Online, Wikidata (#18) (#49) (#37)
* gains ports of `taxize` functions to `taxizedb` (NCBI & ITIS supported): `children`, `classification`, `downstream`. beware when both `taxize` and `taxizedb` loaded in the same R session to namespace calls to these three functions (#19) (#25) (#44) (#48)
* gains mapping functions: `name2taxid` (scientific or common name to taxonomy ID); `taxid2name` (taxonomy ID to scientific name); `taxid2rank` (taxonomy ID to rank) (#41) (#42)
* intro vignette added (#17)
* GBIF and COL data sources are not updated daily in the repos https://github.com/ropenscilabs/gbif-backbone-sql and https://github.com/ropenscilabs/col-sql/ via GithHub Actions. See those repos for details (#26)
* update package level manual file (`?taxizedb-package`) with details on each data source, their update schedules, and examples
* all data sources now use SQLite as the database storage engine. passwords/ports/usernames/etc are no longer needed! note that some `db_download*` functions download already created SQLite databases, whereas for other data sources the database is built locally on your machine from other data formats downloaded (see also #36, #46)
* a copy of the taxonomic ranks information from taxize package was ported over for internal use to be able to make `downstream()` work for most data sources

## MINOR IMPROVEMENTS

* remove check for whether SQLite is installed (#5 #29)
* all `src_*` functions now only have two paramters: `path` and `...`. where path by default figures out the path for you using the function `db_path()`, and `...` allows the user to pass on parameters to `DBI::dbConnect`

## DEFUNCT

* `db_load()` is now defunct. Now just use `db_download*` then `src*` for your data source (see also #43)


taxizedb 0.1.4
==============

## BUG FIXES

* Fixes to SQL database connection functions for changes in `dplyr`, 
which now requires `dbplyr` package - also `DBI` now imported (#16)


taxizedb 0.1.0
==============

## NEW FEATURES

* Released to CRAN
