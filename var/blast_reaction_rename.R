blast_reaction_rename <- function(swiss_table){
  
  #swiss_table <- dcast(results[tool == 'Blast'], ncbiprotein + feature ~ colname,fun.aggregate = paste,collapse = "__")
  swiss_table[, uniprot := o(paste('uniprot', substr(subjectid, 4, 9), sep ='/'))]
  # Feature aln bitscore eval gaps mm %ident qend qstart send sstart subjectid subjectname tool version
  
  # Grab names of tool and version to display in header
  swiss_tool <- toupper(swiss_table$tool[1])
  swiss_version <- swiss_table$version[1]
  # Re-order and rename the datatable
  swiss_table <- swiss_table[,.(ncbiprotein,uniprot,alignment_length,bitscore,evalue,gaps,mismatches,percidentity,qend,qstart,send,sstart,subjectname)]
  swiss_table <- rename(swiss_table,c("alignment_length" = "aln","evalue" = "e","mismatches" = "mm","percidentity" = "pi","bitscore" = "bs",
                                      "qend" = "qe","send" = "se","qstart" = "qs","sstart" = "ss"))
  return (swiss_table)
}