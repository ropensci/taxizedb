% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxizedb-package.R
\docType{package}
\name{taxizedb-package}
\alias{taxizedb-package}
\alias{taxizedb}
\title{taxizedb}
\description{
Taxonomic databases interface
}
\section{Supported data sources and database structure}{

All are using SQLite as the database
\itemize{
\item NCBI: text files are provided by NCBI, which we stitch into a sqlite db
\item ITIS: they provide a sqlite dump, which we use here
\item The PlantList: created from stitching together csv files. this
source is no longer updated as far as we can tell. they say they've
moved focus to the World Flora Online
\item Catalogue of Life: created from Darwin Core Archive dump. Using the
latest monthly edition via
http://www.catalogueoflife.org/DCA_Export/archive.php
\item GBIF: created from Darwin Core Archive dump. right now we only have
the taxonomy table (called gbif), but will add the other tables in the
darwin core archive later
\item Wikidata: aggregated taxonomy of Open Tree of Life, GLoBI and Wikidata.
On Zenodo, created by Joritt Poelen of GLOBI.
\item World Flora Online: https://www.worldfloraonline.org/
}
}

\section{Update schedule for databases}{

\itemize{
\item NCBI: since \code{db_download_ncbi} creates the database when the function
is called, it's updated whenever you run the function
\item ITIS: since ITIS provides the sqlite database as a download, you can
delete the old file and run \code{db_download_itis} to get a new dump;
they I think update the dumps every month or so
\item The PlantList: no longer updated, so you shouldn't need to download
this after the first download
\item Catalogue of Life: a GitHub Actions job runs once a day at 00:00 UTC,
building the lastest COL data into a SQLite database thats hosted on
Amazon S3
\item GBIF: a GitHub Actions job runs once a day at 00:00 UTC,
building the lastest COL data into a SQLite database thats hosted on
Amazon S3
\item Wikidata: last updated April 6, 2018. Scripts are available to
update the data if you prefer to do it yourself.
\item World Flora Online: since \code{db_download_wfo} creates the database when
the function is called, it's updated whenever you run the function
}
}

\section{Links}{

\itemize{
\item NCBI: ftp://ftp.ncbi.nih.gov/pub/taxonomy/
\item ITIS: https://www.itis.gov/downloads/index.html
\item The PlantList - http://www.theplantlist.org/
\item Catalogue of Life:
via http://www.catalogueoflife.org/content/annual-checklist-archive
\item GBIF: http://rs.gbif.org/datasets/backbone/
\item Wikidata: https://zenodo.org/record/1213477
\item World Flora Online: https://www.worldfloraonline.org/
}
}

\examples{
\dontrun{
library(dplyr)

# data source: NCBI
db_download_ncbi()
src <- src_ncbi()
df <- tbl(src, "names")
filter(df, name_class == "scientific name")

# data source: ITIS
## download ITIS database
db_download_itis()
## connect to the ITIS database
src <- src_itis()
## use SQL syntax
sql_collect(src, "select * from hierarchy limit 5")
### or pipe the src to sql_collect
src \%>\% sql_collect("select * from hierarchy limit 5")
## use dplyr verbs
src \%>\%
  tbl("hierarchy") \%>\%
  filter(ChildrenCount > 1000)
## or create tbl object for repeated use
hiers <- src \%>\% tbl("hierarchy")
hiers \%>\% select(TSN, level)

# data source: The PlantList

"The Plant List (TPL) is no longer accessible. If you have a copy of the
sqlite database you can still use the rest of the TPL functions with it.
We suggest using the World Flora Online (WFO) database as a replacement." 

## connecto the tpl database
src <- src_tpl()
## do queries
tpl <- tbl(src, "tpl")
filter(tpl, Family == "Pinaceae")

# data source: Catalogue of Life
## download col datababase
db_download_col()
## connec to the col database
src <- src_col()
## do queries
names <- tbl(src, "taxa")
select(names, taxonID, scientificName)

# data source: GBIF
## download gbif datababase
db_download_gbif()
## connecto the gbif database
src <- src_gbif()
## do queries
df <- tbl(src, "gbif")
select(df, taxonID, scientificName)

# data source: Wikidata
db_download_wikidata()
src <- src_wikidata()
df <- tbl(src, "wikidata")
filter(df, rank_id == "Q7432")

# data source: World Flora Online
db_download_wfo()
src <- src_wfo()
df <- tbl(src, "wfo")
filter(df, taxonID == "wfo-0000000010")
}
}
\keyword{package}
