priam_protein_rename <- function(priam_table_prot){
  priam_table_prot <-rename(priam_table_prot,c("<http://www.biopax.org/release/bp-level3.owl#xref" = "xref"))
  # rename the headers in the priam table
  priam_table_prot <-rename(priam_table_prot,c("align_length" = "aln","bit_score" = "bs","evalue" = "e","is_best_overlap" = "isb","positive_hit_probability" = "php",
                                               "profile_from" = "pf","profile_length" = "pl","profile_proportion" = "pp","profile_to" = "pt","query_from" = "qf","query_length" = "ql","query_strand" = "qs","query_to" = "qt"))
  priam_table_prot <- priam_table_prot[,.(ncbiprotein,xref,aln,bs,e,isb,php,profile_ID,pf,pl,pp,pt,qf,ql,qs,qt)]
  
  return (priam_table_prot)
  
}