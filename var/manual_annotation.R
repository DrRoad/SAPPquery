
manual_annnotation <- function(creator, geneid, genecard, description, org, prot, ecnumber){
  domain <- Domain$new("")
  date <- Sys.Date()
  # Create a hash using time t o create unique URI's
  set.seed(as.integer(Sys.time()))
  rand <- stringi::stri_rand_strings(1, 7)
  
  # Create a mainDnaObjct
  main_dna <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/mainDnaObject",sep="")
  mainDnaObject <- domain$life.gbol$createDNAObject(main_dna)
  
  # Create the Gene, which is the central node
  gene_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/Gene",sep ="")
  aGene <- domain$life.gbol$createGene(gene_link)
  aGene$setGene(genecard)
  aGene$setNumber(geneid)
  # A comment from the author 
  note_link <- paste ("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/Comment",sep="")
  note <- domain$life.gbol$createNote(note_link)
  note$setText(description)
  aGene$addNote(note)
  
  mainDnaObject$addFeature(aGene)
  geneprov_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/genProv",sep="")
  geneProv <- domain$life.gbol$createGeneProvenance(geneprov_link)
  aGene$setProvenance(geneProv)
  
  # Create the organization
  org_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/Organization",sep="")
  organisation <- domain$life.gbol$createOrganisation(org_link)
  organisation$setLegalName(org)
  
  # Create an author
  auth_link_name <- substr(creator,1,6)
  auth_link_clean <- sub(" ","_", auth_link_name)
  auth_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/Author" ,sep="")
  author <- domain$life.gbol$createCurator(auth_link)
  author$setName(creator)
  author$setOrcid("http://orcid.org/123123-1123")
  author$setActedOnBehalfOf(organisation)
  
  aGene$addXref(author)
  
  # Create a database to allow searcing for a gene/protein name in genecards
  database_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/Database" ,sep="")
  database <- domain$life.gbol$createDatabase(database_link)
  db_link <- paste("http://www.genecards.org/cgi-bin/carddisp.pl?", genecard ,"",sep="")
  database$setBase(db_link)
  database$setShortName("genecards")
  
  # Create a protein object
  protein_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/" ,rand ,"/Protein" ,sep="")
  protein_xref <- domain$life.gbol$createCDS(protein_link)
  protein_xref$setProtein_id(prot)
  protein_xref$setProvenance(geneProv)
  aGene$addXref(protein_xref)
  
  mainDnaObject$addFeature(protein_xref)
  
  # Create an EC number object
  ec_number <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/" ,rand ,"/ECnumber" ,sep="")
  ec_xref <- domain$life.gbol$createXRef(ec_number)
  ec_xref$setAccession(ecnumber)
  ec_xref$setProvenance(geneProv)
  ec_xref$setDb(database)
  aGene$addXref(ec_xref)
  
  mainDnaObject$addFeature(ec_xref)
  
  # Create an aonnotation refrence in this case a manual annotation
  anotr_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/annotationResult",sep="")
  anotResult <- domain$life.gbol$createAnnotationResult(anotr_link)
  annosoft_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/" ,rand ,"/annotationSoftware" ,sep="")
  annotationSoftware <- domain$life.gbol$createAnnotationSoftware(annosoft_link)
  annotationSoftware$setName("Manual annotation")
  annotationSoftware$setVersion("1.0")
  anotResult$setWasAttributedTo(annotationSoftware)
  
  manual_annolink <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/manualAnnotationResult",sep="")
  manualAnotResult <- domain$life.gbol$createAnnotationResult(manual_annolink)
  manualAnotResult$setWasAttributedTo(author)
  manulactivity_link <- paste("http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/", rand ,"/manualAnnotationActivity",sep="")
  
  manualAnnotationActivity <- domain$life.gbol$createAnnotationActivity(manulactivity_link)

  manualAnnotationActivity$setStartedAtTime(as.Date(date))
  manualAnnotationActivity$setEndedAtTime(as.Date(date))
  manualAnotResult$setWasGeneratedBy(manualAnnotationActivity)
  anotResult$setWasGeneratedBy(manualAnnotationActivity)

  geneProv$setOrigin(manualAnotResult)
  
  filename <- paste(auth_link_clean,"-",rand, ".ttl",sep="")
  
  ### This needs to bechanged if running locally.
  file_link <- paste("/mnt/users/rohaftor/ShinyApps/SAPP/datafiles/",filename,"",sep="")
  domain$save(file_link)
  
  curlup <- paste("fread(\'curl -X POST -H Content-Type:text/turtle -G http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql -T ",file_link,"\') ")
  try(eval(parse(text=curlup)))
  domain$close()
}
