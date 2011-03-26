pubmed.search <- function(query,limits,save=TRUE,destfile,format="bibtex",show=TRUE,encoding="UTF-8",...)

{

   e.query <- esearch.url(query,limits)
   temp = paste("temporary",round(runif(1)*1000000,0),".txt",sep="_",collapse="")
 
   download.file(e.query,dest=temp,quiet=TRUE)

   #PARSE AND DELTE
   ids <- tryCatch(parse.xml(temp),error=function(e){NA})

   file.remove(temp)
   
   if(any(is.na(ids))){
      stop("Error in search. No records returned.")
   }
       
   #SEND TO HUBMED TO GET DESIRED FORMAT
   hubmed.search.url <- hubmed.url(ids,format)

   if(save){
   #FILE EXTENSION
   if(format=="ris"){
     destfile <- paste(destfile,".ris",sep="",collapse="")
    }
   else if(format=="bibtex"){
      destfile <- paste(destfile,".bib",sep="",collapse="")
    }
   else if(format=="rdf"){
      destfile <- paste(destfile,".rdf.cgi",sep="",collapse="")
    }
   else{
     destfile <- paste(destfile,".xml",sep="",collapse="")
    }
   
   download.file(url=hubmed.search.url,dest=destfile,quiet=TRUE,...)

   if(show) file.show(destfile)
   }

    #TEMPORARY STORAGE FOR REFERENCE CREATION
   
    temp = paste("temporary",round(runif(1)*1000000,0),".ris",sep="_",collapse="")
   
    hubmed.search.url <- hubmed.url(ids,"ris")
    download.file(url=hubmed.search.url,dest=temp,quiet=TRUE)
   
    Reference <- tryCatch(read.ris(temp,id="pubmed",encoding=encoding),
                          error=function(e){NA})

    file.remove(temp)
   
    if(class(Reference)!="Reference"){
       stop("Error in read.ris.")
    }
   else{
      return(Reference)
  }
 }


esearch.url <- function(query,limits)
  {
    if(missing(limits)) limits <- list()
      
    trail.url <- limits.url(limits)
    base.url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?tool=R"
    query <- paste("term=",query,sep="",collapse="")
    esearch.url = paste(c(base.url,query,trail.url),collapse="&")

    esearch.url
  }


limits.url <- function(limits)
{

  limit.names <- names(limits)
  fields <- c("reldate","mindate","maxdate","datetype","retstart","retmax")
   
  if(!all(limit.names %in% fields)) stop("Unrecognized limits")

  if(("mindate"%in%limit.names)&&!("maxdate"%in%limit.names)) warning("mindate supplied but maxdate missing")

  if(("maxdate"%in%limit.names)&&!("mindate"%in%limit.names)) warning("maxdate supplied but mindate missing")

  if(!("retstart"%in%limit.names)) limits$retstart <- 0
  if(!("retmax"%in%limit.names)) limits$retmax <- 1000
  if(("reldate"%in%limit.names)&&!("datetype"%in%limit.names)){
    warning("reldate supplied but datetype not given; edat assumed")
    limits$datetype = "edat"
  }

  limits.url <- paste(names(limits),as.character(limits),sep="=")
  limits.url <- paste(c(limits.url,"usehistory=y"),collapse="&")

  limits.url
}


parse.xml <- function(file)
{
  pids <- readLines(con=file)
  i <- grep("<Id>",pids)

  if(length(i)==0){
    stop("Search returned no records.")
  }
  else{   #ISOLATE PMIDS FROM XML RETURNED BY ESEARCH
   sub(".*<Id>([0-9]{1,8})</Id>.*","\\1",pids[i]) 
  }
}

hubmed.url <- function(ids,format)
{
  
  hubmed.url <- "http://www.hubmed.org/export/format.cgi?uids="
  hubmed.url <- sub("format",format,hubmed.url)
  hubmed.url <- paste(c(hubmed.url,paste(ids,collapse=",")),sep="",collapse="")

  if(!(format%in%c("ris","bibtex","rdf","mods"))) stop("Unrecognized file format.")
     
  hubmed.url                     
}
