#tsn <- 173420
make_sql <- function(x) {
  sprintf("select t.tsn, t.parent_tsn, t.complete_name as combinedName,
                           r.rank_name, r.rank_id, a.taxon_author as author
      from taxonomic_units t
      left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
      inner join taxon_unit_types r on r.rank_id = t.rank_id and r.kingdom_id = t.kingdom_id
      where (%s or %s)
      and (t.name_usage='valid' or t.name_usage='accepted')",
      paste(sprintf("t.parent_tsn='%s'", x), collapse = " or "),
      paste(sprintf("t.tsn='%s'", x), collapse = " or ")
      )
}



#tsn2 <- 173420 # amphibia TSN
tsn2 <- 179913 # mammalia TSN
out <- list()
# pb <- txtProgressBar(min = 0, max = 10, initial = 0,
#                      style = 3)
for (i in 1:10) {
  #setTxtProgressBar(pb, i)
  # tmp <- lapply(tsn2, function(z) {
  #   df <- sql_collect(src, make_sql(z))
  #   filter(df, tsn != z)
  # })
  i <- 11
  df <- sql_collect(src, make_sql(tsn2))
  df$rank_name <- rcrossref:::strtrim(df$rank_name)

  keep <- df %>% filter(rank_name == "Species")

  df <- filter(df, !tsn %in% tsn2)

  #df <- bind_rows(tmp)
  tsn2 <- df %>% filter(rank_name != "Species") %>% .$tsn
  #out[[i]] <- filter(df, rank_name == "Species")
  out[[i]] <- bind_rows(df, keep)
}
close(pb)
out
all <- bind_rows(out)
all$rank_name <- rcrossref:::strtrim(all$rank_name)
all %>%
  filter(rank_name == "Species") %>%
  distinct() %>%
  readr::write_csv(path = "~/Dropbox/sAPROPOS project/DemogData/mammals.csv")
