txdb_ids <- c(
  '01', # domain
  '05','10','20','25', # kingdom
  '30','40','45', # phylum/division
  '50','60','70','80','81','82', # class
  '83','84','85','86','87', # cohort
  '90','100','110','120','125', # order
  '130','140','150', # family
  '155','160','170', # tribe
  '180','190','200','210','215','217', # genus
  '220','225','230', # species/etc.
  '240','250','255','260','265', # variety/sub/form/etc.
  '280', # genetic variants
  '300' # unspecified
)
txdb_ranks <- c(
  'domain', # domain
  'superkingdom','kingdom','subkingdom','infrakingdom,superphylum', # kingdom
  'phylum,division','subphylum,subdivision','infradivision', # phylum/division
  'superclass','class','subclass','infraclass','subterclass','parvclass', # class
  'megacohort','supercohort','cohort','subcohort','infracohort', # cohort
  'superorder','order','suborder','infraorder','parvorder', # order
  'superfamily','family','subfamily', # family
  'supertribe','tribe','subtribe', # tribe
  'genus','subgenus','section','subsection','species group,series','species subgroup', # genus
  'species','infraspecies','subspecies,forma specialis', # species/etc.
  'variety,varietas','subvariety,race','stirp','form,forma,morph','subform', # variety/sub/form/etc.
  'biotype,isolate,pathogroup,serogroup,serotype,strain,aberration', # genetic variants
  'unspecified,no rank,unranked,clade' # unspecified
)
txdb_rr <- data.frame(
  rankid = txdb_ids,
  ranks = txdb_ranks
)

txdb_which_rank_v <- function(x) {
  vapply(x, txdb_which_rank, 1L)
}
txdb_which_rank <- function(x) {
  which(sapply(txdb_rr$ranks, function(z) {
    any(unlist(strsplit(z, split = ",")) == x)
  }, USE.NAMES = FALSE))
}
txdb_prune_too_low <- function(x, rank, ignore_no_rank = FALSE) {
  rank_target_no <- as.numeric(txdb_rr[txdb_which_rank(rank), "rankid"])
  rank_nos <- as.numeric(
    txdb_rr[vapply(x$rank, function(z) txdb_which_rank(z), 1),
    "rankid"])
  if (ignore_no_rank) rank_nos[rank_nos %in% c(300, 400)] <- 0
  x[!rank_nos > rank_target_no, ]
}
