setClass(
         "Reference",
         representation(
                        id = "character",
                        au = "list",
                        jo = "character",
                        pb = "character",
                        ti = "character",
                        vl = "character",
                        is = "character",
                        year = "numeric",
                        month = "numeric",
                        day = "numeric",
                        sp = "character",
                        ep = "character",
                        ab = "character",
                        kw = "list"
                        )
         )

Reference <- function(
                        id,
                        au,
                        jo,
                        pb,
                        ti,
                        vl,
                        is,
                        year,
                        month,
                        day,
                        sp,
                        ep,
                        ab,
                        kw
                      ){

  new("Reference",
                        id = id,
                        au = au,
                        jo = jo,
                        pb = pb,
                        ti = ti,
                        vl = vl,
                        is = is,
                        year = year,
                        month = month,
                        day = day,
                        sp = sp,
                        ep = ep,
                        ab = ab,
                        kw = kw
      )

  
}

### CREATE GENERIC FOR DATA FRAME OF META REFERENCE DATA

as.data.frame <- function(x,au.depth=1,title.length=30) UseMethod("as.data.frame")

setMethod("as.data.frame",signature(x="Reference"),function(x,au.depth=1,title.length=30){

                 unfactor <- function(x){
                   if(is.factor(x)) x = as.character(levels(x))[x]
                   x
                 }
                 
                 unlist.au = unlist(x@au)
                 au.count = sapply(x@au,length)
     
                 au.order =  unlist(sapply(au.count,function(y){1:y}))
                 drop = au.order>au.depth
                 rep = ifelse(au.count>au.depth,au.depth,au.count)

                 if(title.length==Inf){
                   title = x@ti
                 }
                 else{
                   title = substr(x@ti,1,title.length)
                 }
                 
                 df <- data.frame(
                 id = rep(x@id,rep),
                 author = unlist.au[!drop],
                 order.author = au.order[!drop],
                 journal = rep(x@jo,rep),
                 publisher = rep(x@pb,rep),
                 title = rep(title,rep),
                 volume = rep(x@vl,rep),
                 issue = rep(x@is,rep),
                 year = rep(x@year,rep),
                 month = rep(x@month,rep),
                 day = rep(x@day,rep),
                 first.page = rep(x@sp,rep),
                 last.page = rep(x@ep,rep)
                           )

                 #REMOVE FACTORS FOR NUMERIC-TYPE VARIABLES
                 df$volume = unfactor(df$volume)
                 df$issue = unfactor(df$issue)
                 df$first.page = unfactor(df$first.page)
                 df$last.page = unfactor(df$last.page)
                 
                 return(df)
               })


### CREATE GENERIC FOR RETURNING LIST OF ABSTRACTS

abstract <- function(object) UseMethod("abstract")
           
setMethod("abstract",signature(object="Reference"),
          function(object){
            abstracts <- object@ab
            names(abstracts) <- object@id
            abstracts
             }
          )

keyword <- function(object) UseMethod("keyword")
           
setMethod("keyword",signature(object="Reference"),
          function(object){
            keywords <- object@kw
            names(keywords) <- object@id
            keywords
             }
          )


setMethod("show","Reference",function(object){
            
            df <- as.data.frame(object)
            
            df.brief <- subset(df,select=c(
                                         "id",
                                         "author",
                                         "title",
                                         "year"
                                         ))
             print(df.brief)
          })

summary.Reference <- function(object,...){

            n = length(object@id)

            record.string <- ifelse(n==1,"record.","records.")

            record.string <- paste("A Reference object with",n,record.string,collapse="")

            names(record.string) <- ""

            print(record.string,quote=FALSE)
            
            print(table(object@year))
                }

setMethod("summary","Reference",summary.Reference)
