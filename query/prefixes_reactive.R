# Base query
reactive_queries =c( 
"prefix ssb: <http://csb.wur.nl/genome/>
prefix uni: <http://purl.uniprot.org/core/>
prefix uniec: <http://purl.uniprot.org/enzyme/>
prefix idec: <http://identifiers.org/ec-code/>
prefix biopax: <http://www.biopax.org/release/bp-level3.owl#>"
,
# Interpro query
"select ?header ?colname ?value ?feature
where { #Interpro features : ec-codes mapped via ssb:SignatureAccession) 
?signature biopax:xref idec:4.2.1.11.
?signature a ssb:SignatureAccession. 
?feature ssb:signature ?signature.             
?protein ssb:feature ?feature. 
?cds ssb:header ?header.
?cds ssb:protein ?protein.
?feature ?colname ?value.  }"
,
# PRIAM Query
"select ?header ?colname ?value ?feature
where { #Priam features : ec-codes directly linked to feature
?feature biopax:xref idec:4.2.1.11.
?feature a ssb:Priam.
?protein ssb:feature ?feature. 
?cds ssb:header ?header.
?cds ssb:protein ?protein.
?feature ?colname ?value. }"
,
# Uniprot Query
"select ?header ?colname ?value ?feature
where { #UNiprot features : ec-codes mapped via uniprot entry
?uniprot uni:enzyme uniec:4.2.1.11.
BIND (replace(str(?uniprot),'http://purl.uniprot.org/uniprot/','') AS ?id)
?uniprot uni:reviewed ?reviewed.
#?uniprot uni:organism ?organism.
?uniprot ssb:subjectid ?subjectid
VALUES ?reviewed { true }  
?feature a ssb:Blast.
FILTER(contains(str(?feature),?id))
?feature ssb:subjectid ?subjectid.
?protein ssb:feature ?feature. 
?cds ssb:header ?header.
?cds ssb:protein ?protein.
?feature ?colname ?value.}"
,
# Enzdp query<-
"select ?header ?colname ?value ?feature
where { #EnzDP features : ec-codes directly linked to feature
?feature biopax:xref idec:4.2.1.11.
?feature a ssb:EnzDP.
?protein ssb:feature ?feature. 
?cds ssb:header ?header.
?cds ssb:protein ?protein.
?feature ?colname ?value. }"
)