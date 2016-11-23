library(data.table)
# Create a empty datafram, need for the rendering part in the shiny server
noframe <- function(){
  ncbiprotein <-c("N/A")
  uniprot <-c("N/A")
  aln <-c("N/A")	
  bs <-c("N/A")	
  e <-c("N/A")	
  gaps<-c("N/A")	
  mm<-c("N/A")	
  pi<-c("N/A")	
  qe<-c("N/A")	
  qs<-c("N/A")	
  se<-c("N/A")	
  ss<-c("N/A")
  subjectname<-c("N/A")
  new_frame <- data.frame(ncbiprotein,uniprot,aln,bs,e,gaps,mm,pi,qe,qs,se,ss)
  return (new_frame)
}