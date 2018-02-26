taxizedb 0.2.0
==============

## NEW FEATURES

 * Support for the NCBI taxonomy database

 * Ports of `taxize` functions to `taxizedb` (only NCBI supported)  
   - `children'
   - `classification'
   - `downstream'

 * Mapping functions
   - `name2taxid` - scientific or common name to taxonomy ID
   - `taxid2name` - taxonomy ID to scientific name
   - `taxid2rank` - taxonomy ID to rank

taxizedb 0.1.4
==============

## BUG FIXES

* Fixes to SQL database connection functions for changes in `dplyr`, 
which now requires `dbplyr` package - also `DBI` now imported (#16)



taxizedb 0.1.0
==============

## NEW FEATURES

* Released to CRAN
