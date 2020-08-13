taxizedb 0.2.0
==============

## NEW FEATURES

 * support for the NCBI taxonomy database added
 * gains ports of `taxize` functions to `taxizedb` (NCBI & ITIS supported): `children`, `classification`, `downstream`. beware when both `taxize` and `taxizedb` loaded in the same R session to namespace calls to these three functions.
 * gains mapping functions: `name2taxid` (scientific or common name to taxonomy ID); `taxid2name` (taxonomy ID to scientific name); `taxid2rank` (taxonomy ID to rank)
 * 

taxizedb 0.1.4
==============

## BUG FIXES

* Fixes to SQL database connection functions for changes in `dplyr`, 
which now requires `dbplyr` package - also `DBI` now imported (#16)


taxizedb 0.1.0
==============

## NEW FEATURES

* Released to CRAN
