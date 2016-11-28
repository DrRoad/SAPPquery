blast_protein_rename <- function(result){
result <- blastresults[tool=='cog']
result <- result[,.(ncbiprotein,alignment_length,bitscore,evalue,gaps,mismatches,percidentity,qend,qstart,send,sstart,subjectname)]
result <- rename(result,c("alignment_length" = "aln","evalue" = "e","mismatches" = "mm","percidentity" = "pi","bitscore" = "bs",
                          "qend" = "qe","send" = "se","qstart" = "qs","sstart" = "ss"))
return(result)
}