protein_query<- "
prefix ssb: <http://csb.wur.nl/genome/>
select ?header ?tool ?feature ?colname ?value
where
  {
  ?cds ssb:header ?header.
  FILTER(contains(?header,'changeme'))
  ?cds ssb:protein ?protein.
  ?protein ssb:feature ?feature.
  ?feature a ?tool.
  ?feature ?colname ?value
}"