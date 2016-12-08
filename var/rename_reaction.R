library(data.table)

#rename_reaction(results)
# Create a function to clean up the protein header
rename_reaction <- function(x){
  x[, ncbiprotein := str_match(header, '[N,X]P_[[:digit:]]+.[[:digit:]]+')]
  x[, colname := sub('>', '', sub('<http://csb.wur.nl/genome/', '', colname))]
  x[, tool := sub('>', '', sub('<http://csb.wur.nl/genome/', '', tool))]
  x[, feature := sub('>','',sub('<http://csb.wur.nl/genome/protein/[[:alnum:]]+/','',feature))]
  x <-x[!grepl('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',colname)]
  x[,Ncbiprotein:=ncbiprotein]
  x[, ncbiprotein := o(paste('ncbiprotein', ncbiprotein, sep ='/'))]
  return (x)
}