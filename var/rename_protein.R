library(data.table)

# Create a function to clean up the protein header
rename_protein <- function(x){
x[,ncbiprotein:=str_match(header,'[N,X]P_[[:digit:]]+.[[:digit:]]+')]
x[,colname:=sub('>','',sub('<http://csb.wur.nl/genome/','',colname))]
x[,tool:=sub('>','',sub('<http://csb.wur.nl/genome/','',tool))]
x[,feature:=sub('>','',sub('<http://csb.wur.nl/genome/protein/[[:alnum:]]+/','',feature))]
#simple report per protein and tool, some hickups where duplicate values per feature trigger aggregation dcast
x <- x[!grepl('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',colname)] #remove, duplicate values triggers aggregation in dcast
x[, ncbiprotein := o(paste('ncbiprotein', ncbiprotein, sep ='/'))] #TEST
return (x)
}