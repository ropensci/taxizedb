% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ap_downstream.R
\name{downstream}
\alias{downstream}
\title{Retrieve all taxa descending from a vector of taxa}
\usage{
downstream(x, db = "ncbi", verbose = TRUE, ...)
}
\arguments{
\item{x}{(character) Vector of taxon keys for the given database}

\item{db}{(character) The database to search, one of ncbi, itis,
gbif, col, or wfo}

\item{verbose}{(logical) Print verbose messages}

\item{...}{Additional arguments passed to database specific downstream
functions}
}
\value{
list of data.frames with the columns: childtaxa_id, childtaxa_name,
and rank. This is exactly equivalent to the output of \code{taxize::downstream()}
}
\description{
This function is nearly equivalent to the \code{taxize::downstream()} function
}
\examples{
\dontrun{
# get descendents from all ranks
# downstream(c(3700, 9605)) # takes a while

# limit results to species
downstream(c(3700, 9605), downto='species')

# allow ambiguous nodes but no ambiguous species
downstream(
  c(3700, 9605),
  downto='species',
  ambiguous_nodes=FALSE,
  ambiguous_species=TRUE
)

# ITIS
id <- name2taxid('Aves', db = "itis")
downstream(id, db = "itis", downto = "family")
downstream(id, db = "itis", downto = "genus")
id <- name2taxid('Bombus', db = "itis")
downstream(id, db = "itis", downto = "species")

# COL
id <- name2taxid('Chordata', db = "col")
downstream(id, db = "col", downto = "family")

# GBIF
id <- name2taxid('Pinaceae', db = "gbif")
downstream(id, db = "gbif", downto = "genus")

# World Flora Online
id <- name2taxid('Pinaceae', db = "wfo")
downstream(id, db = "wfo", downto = "species")
}
}
