% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_load.R
\name{db_load-defunct}
\alias{db_load-defunct}
\alias{db_load_itis}
\alias{db_load_tpl}
\alias{db_load_col}
\alias{db_load_gbif}
\alias{db_load_ncbi}
\alias{db_load_wikidata}
\title{Load taxonomic databases}
\usage{
db_load_itis(...)

db_load_tpl(...)

db_load_col(...)

db_load_gbif(...)

db_load_ncbi(...)

db_load_wikidata(...)
}
\arguments{
\item{...}{ignored}
}
\value{
An error message.
}
\description{
This family of functions has been removed. Use \link{db_download} then
\link{src_taxizedb}.
}
