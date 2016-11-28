ipr_query <-"
prefix ssb: <http://csb.wur.nl/genome/>
  prefix biopax3: <http://www.biopax.org/release/bp-level3.owl>
  select *
  where
{
  ?signature a ssb:SignatureAccession.
  VALUES ?signature { domains }
  ?signature ssb:interpro_description ?interpro_description;
  #ssb:signature_description ?signature_description;
  #ssb:unipathway ?unipathway;
  <http://www.biopax.org/release/bp-level3.owl#xref> ?xref
}"