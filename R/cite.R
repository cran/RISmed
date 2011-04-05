cite <- function(object,author.count=3) UseMethod("cite")

setMethod("cite",signature(object="Reference"),

          function(object,author.count=3){

             mapply(paste.citation,
               au=object@au,
               title=object@ti,
               volume=object@vl,
               journal=object@jo,
               issue=object@is,
               s=object@sp,
               e=object@ep,MoreArgs=list(author.count=author.count))
})


strip.space <- function(x){  #REMOVING BEGINNING AND ENDING SPACE
  x <- strsplit(x," ")[[1]]
  paste(x[x!=""],collapse=" ")
}

get.author.str <- function(au=c("Foo,B","Doe,John F")){

  #GIVEN VECTOR OF AUTHORS RETURN COMMA-SEPARATED LIST
  
  get.middle.name <- function(au){
    
    first.name <- (strsplit(au,",")[[1]])[2]

    #REMOVE PUNCTUATION
    first.name <- gsub("[. ]","",first.name)
    first.name <- strsplit(first.name,sp="")[[1]]
    number.of.characters <- length(first.name)

    if(number.of.characters<=3){
      #RETURN COLLAPSED UPPER CASE
       first.name <- first.name[grep("[A-Z]",first.name)]
      }
    else{
      first.name <- (strsplit(au,",")[[1]])[2]
      first.name <- strsplit(first.name," ")[[1]]
 
      #TAKE FIRST LETTER OF EACH PART OF FIRST.NAME
      first.name <- sub("([A-Z]).*","\\1",first.name)
    }

    paste(first.name,sep="",collapse="")
  }

  first.name <- sapply(au,get.middle.name)
  last.name <- sapply(au,function(a){sub("([A-Z].*),.*","\\1",a)})

  paste(paste(last.name,first.name),collapse=", ")
 
}

get.title.str <- function(ti=" Hello World "){
  ti <- strip.space(ti)
  if(length(grep("\\.",ti))==0) ti = paste(ti,".",sep="",collapse="")
  ti
}

#INPUT ARE PROCESSED FIELD COMPONENTS

paste.citation <- function(
                     au=c("Foo,B","Taylor,John L"),
                     year="2001",
                     title="Title.",
                     journal="Journal",
                     volume="1",
                     issue="",
                     start.page="1",
                     end.page="2",
                     author.count
                     )
  {

    missing.check <- function(x,f=function(x){x}){
          ifelse(is.na(x)|x=="","",f(x))
    }

    if(length(au)>author.count){
      au.str <- get.author.str(au[1:author.count])
      au.str <- paste(au.str,", et al",sep="",coll="")
    }
    else{
    au.str <- get.author.str(au)
    }
    ti.str <- missing.check(title,get.title.str)
    jr.str <- missing.check(journal)
    volume <- missing.check(volume)
    issue <- missing.check(issue)
    year <- missing.check(year)
    start.page <- missing.check(start.page)
    end.page <- missing.check(end.page)
    
    if(issue!=""){
      volume.issue.str <- paste(volume,"(",issue,")",sep="",coll="")
    }
    else{
      volume.issue.str <- as.character(volume)
    }

    if(start.page==""){
      volume.pages.str <- volume.issue.str
    }
    else if(end.page==""){
      volume.pages.str <- paste(volume.issue.str,":",start.page,sep="",collapse="")
    }
    else{
       volume.pages.str <- paste(volume.issue.str,":",start.page,"-",end.page,
                                 sep="",collapse="")
    }
                         
    paste(paste(au.str,".",sep="",coll=""),
          ti.str,
          jr.str,
          paste(year,";",sep="",coll=""),
          volume.pages.str,
          collapse=""
          )
          
  }

