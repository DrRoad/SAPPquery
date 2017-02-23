library(RGBOLApi)
manual_annnotation <- function(creator, geneid, genecard, description, org){
  ### START THE RGBOL API
  domain <- Domain$new("")
  date <- Sys.Date()
  set.seed(as.integer(Sys.time()))
  rand <- stringi::stri_rand_strings(1, 5)

  gene_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", geneid ,"",sep ="")
  aGene <- domain$life.gbol$createGene(gene_link)
  aGene$setGene(genecard)
  aGene$setNumber(geneid)

  # Here is the comment from the author
  note_link <- paste ("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/",geneid,"/comment",sep="")
  note <- domain$life.gbol$createNote(note_link)
  note$setText(description)
  aGene$addNote(note)
  
  geneprov_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/",geneid,"/genProv",sep="")
  geneProv <- domain$life.gbol$createGeneProvenance(geneprov_link)
  
  aGene$setProvenance(geneProv)
  
  # Organization Here is give the namespace /org isntead of /wur like the origina
  if (org == "uos"){
    org_name <- "University of Sterling"
  }
  if (org == "NMBU"){
    org_name <- "NMBU"
  }
  if (org == "wur"){
    org_name <- "Wageningen University"
  }
  org_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/",geneid,"/",org,"",sep="")
  organisation <- domain$life.gbol$createOrganisation(org_link)
  organisation$setLegalName(org_name)
  #organisation
  
  # Author
  auth_link_name <- substr(creator,1,6)
  auth_link_clean <- sub(" ","_", auth_link_name)
  auth_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/" ,auth_link_clean, "" ,sep=""  )
  author <- domain$life.gbol$createCurator(auth_link)
  author$setName(creator)
  author$setOrcid("http://orcid.org/123123-1123")
  author$setActedOnBehalfOf(organisation)
  aGene$addXref(author)
  
  # Description and date
  manualAnotResult <- domain$life.gbol$createAnnotationResult("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/manualAnnotationResult")
  manualAnotResult$setWasAttributedTo(author)
  manualAnnotationActivity <- domain$life.gbol$createAnnotationActivity("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/manualAnnotationActivity")
  
  # This will just be the same date.
  manualAnnotationActivity$setStartedAtTime(date)
  manualAnnotationActivity$setEndedAtTime(date)
  manualAnotResult$setWasGeneratedBy(manualAnnotationActivity)
  #geneProv$setOrigin(manualAnotResult)
  
  #xrefprov_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/",geneid,"/xrefprov", sep ="")
  #xrefProvenance <- domain$life.gbol$createXRefProvenance(xrefprov_link)
  #xrefProvenance$setNote(shQuote(date))
  
  filename <- paste(auth_link_clean,"-",rand, ".ttl",sep="")
  
  ### This needs to bechanged if running locally.
  file_link <- paste("/mnt/users/rohaftor/ShinyApps/SAPP/datafiles/",filename,"",sep="")
  domain$save(file_link)
  
  #curlup <- paste("fread(\'curl -X POST -H Content-Type:text/turtle -G http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql -T Robert-GNZuC.ttl\') ")
  #curlup <- paste("fread(\'curl -X POST -H Content-Type:text/turtle -G http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql -T ",file_link,"\') ")
  #try(eval(parse(text=curlup)))
  domain$close()
}
