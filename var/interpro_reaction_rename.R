interpro_reaction_rename <- function(interpro_table){
# Change the analysis column slightly so the identifiers mathces the terms in identifiers.org, then link signature column
interpro_table[analysis == 'prints', analysis := 'sprint']
interpro_table[analysis == 'prositepatterns', analysis :='prosite']
interpro_table[, signature := o(paste(analysis, sub('>','',sub('<http://csb.wur.nl/genome/', '', signature)), sep = '/'))]
# Drop the columns that are not needed
interpro_table <- interpro_table[,.(ncbiprotein,signature,analysis,begin,end,score)]
return (interpro_table)
}