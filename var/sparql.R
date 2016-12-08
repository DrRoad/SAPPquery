library(XML)
library(RCurl)
library(bitops)
library(data.table)

sparql <- function(endpoint,query)
{
  require('data.table')
  cat(query,file='query.tmp') #write qyery to tempr
  #curlcall <- paste("results <- fread(\'curl -X POST",endpoint,"--data-urlencode query@query.tmp -H Accept:text/tab-separated-values  -s\')")
  curlcall <- paste("results <- fread(\'curl -X POST",endpoint,"--data-urlencode query@query.tmp -H Accept:text/tab-separated-values  -s\')")
  eval(parse(text=curlcall))
  return(results)
}