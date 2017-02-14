manual_annotation <- function(creator,geneid, genecard,description,organization){

### START THE RGBOL API
domain <- Domain$new("")
date <- Sys.Date()
gene_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", geneid ,"",sep ="")
try(aGene <- domain$life.gbol$createGene(gene_link))
aGene$setGene(genecard)
aGene$setNumber(geneid)
# Here is the comment from the author
### Does the link need to be unique? ###
try(note <- domain$life.gbol$createNote("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/anote"))
note$setText(description)
aGene$addNote(note)

# Organization Here is give the namespace /org isntead of /wur like the origina
try(organisation <- domain$life.gbol$createOrganisation("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/org"))
organisation$setLegalName(organization)

# Author
auth_link_name <- substr(creator,1,6)
auth_link_clean <- sub(" ","_", auth_link_name)
auth_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/" ,auth_link_clean, "" ,sep=""  )

try(author <- domain$life.gbol$createCurator(auth_link))
author$setName(creator)
author$setOrcid("http://orcid.org/123123-1123")
author$setActedOnBehalfOf(organisation)

# Description and date
manualAnotResult <- domain$life.gbol$createAnnotationResult("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/manualAnnotationResult")
manualAnotResult$setWasAttributedTo(author)
manualAnnotationActivity <- domain$life.gbol$createAnnotationActivity("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/manualAnnotationActivity")
# This will just be the same date.
manualAnnotationActivity$setStartedAtTime(date)
manualAnnotationActivity$setEndedAtTime(date)

set.seed(1)
rand <- stringi::stri_rand_strings(1, 5)
filename <- paste(auth_link_clean,"-",rand, ".ttl",sep="")
domain$save(filename)

#curlup <- paste("fread(\'curl -X POST -H Content-Type:text/turtle -G http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql -T Robert-GNZuC.ttl\') ")
curlup <- paste("fread(\'curl -X POST -H Content-Type:text/turtle -G http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql -T ",filename,"\') ")
try(eval(parse(text=curlup)))

domain$close()
}