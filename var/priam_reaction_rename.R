priam_reaction_rename <- function(priam_table){
head(priam_table)
priam_table <-rename(priam_table,c("<http://www.biopax.org/release/bp-level3.owl#xref" = "ECurl"))
priam_table[, evalue := as.numeric(evalue)]
setkey(priam_table, evalue)
# rename the headers in the priam table
priam_table <-rename(priam_table,c("align_length" = "aln","bit_score" = "bs","evalue" = "e","is_best_overlap" = "isb","positive_hit_probability" = "php",
                                   "profile_from" = "pf","profile_length" = "pl","profile_proportion" = "pp","profile_to" = "pt","query_from" = "qf","query_length" = "ql","query_strand" = "qs","query_to" = "qt"))
priam_table <- priam_table[,.(ncbiprotein,ECurl,aln,bs,e,isb,php,profile_ID,pf,pl,pp,pt,qf,ql,qs,qt)]
return(priam_table)
}