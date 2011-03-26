read.ris <- function(file,id.type="author.year",encoding="UTF-8"){
  
temp <- scan(file,what="character",encoding=encoding,quiet=TRUE)

## IDENTIFY END OF RECORD (EOR)

eor.index <- grep("^ER$",temp) #ANY TERMS MATCHING ER

## GET TAG ENDING

tag.ending <- temp[eor.index[1]+1]

## TAG IDENTIFIED EOR

temp[eor.index] <- "ER!"

## CREATE STRING

temp.string <- paste(temp,collapse=" ")

## CREATES VECTOR OF EACH RECORD, AVOIDING SPLITTING ON ER- TYPES IN RECORD CONTENT
er.tag <- paste("ER!",tag.ending,collapse="")
temp.list <- strsplit(temp.string,er.tag)[[1]]

### MARK THE START OF FIELDS AND SEPARATE
field.tag <- paste("([A-Z][A-Z0-9]",tag.ending,")",collapse="")
temp.list <- gsub(field.tag,"!!\\1",temp.list)

temp.list <- lapply(temp.list,function(record){
             strsplit(record,"!!")[[1]]
})


sapply.field.value.gettor <- function(field.pattern,x,ending,as.list=FALSE){
  
### GENERAL FUNCTION FOR GETTING THE VALUE FOR A SPECIFIC FIELD
### RETURNS NA FOR NO MATCH
### RETURNS MULTIPLE MATCHES AS VECTOR IN ORDER OF OCCURRENCES

field.value.gettor <- function(field.pattern,x){

  replace.pattern <- paste(c(field.pattern,ending,"(.*)"),collapse=" ")
  field.pattern <- paste(c(field.pattern,ending),collapse=" ")

  # INDEX MATCHING FIELDS
  where.are.fields <- grep(field.pattern,x)

  if(length(where.are.fields)==0){
    return(NA)
  }
  else{

  # GET STRING PORTION OF MATCHES

    value <- sapply(x[where.are.fields],function(value){
      sub("( )+$","",sub(replace.pattern,"\\1",value))
    })

    return(as.character(value))
  }
}

### BEGIN APPLY

   if(as.list){
   lapply(temp.list,function(z){field.value.gettor(field.pattern,z)})
   }
   else{
   sapply(temp.list,function(z){field.value.gettor(field.pattern,z)})
   }
}

### GET FIELDS

au = sapply.field.value.gettor("A[U1]",temp.list,end=tag.ending,as.list=TRUE)
jo = sapply.field.value.gettor("JO",temp.list,end=tag.ending)
pb = sapply.field.value.gettor("PB",temp.list,end=tag.ending)
ti = sapply.field.value.gettor("T[I1]",temp.list,end=tag.ending)
vl = sapply.field.value.gettor("VL",temp.list,end=tag.ending)
is = sapply.field.value.gettor("IS",temp.list,end=tag.ending)
dp = sapply.field.value.gettor("PY",temp.list,end=tag.ending)
if(all(is.na(dp))) dp = sapply.field.value.gettor("Y1",temp.list,end=tag.ending)
sp = sapply.field.value.gettor("SP",temp.list,end=tag.ending)
ep = sapply.field.value.gettor("EP",temp.list,end=tag.ending)
ab = sapply.field.value.gettor("AB",temp.list,end=tag.ending)

### SETTING REPRESENTATION TYPE

jo = as.character(jo)
pb = as.character(pb)
ti = as.character(ti)
vl = as.character(vl)
is = as.character(is)
sp = as.character(sp)
ep = as.character(ep)
ab = as.character(ab)


### CHECK WHICH ITEMS CONTAIN A MONTH OR DATE INFO

 if(any(grep("/",dp))){
       
dp.split <- strsplit(dp,"/")

### ANY OTHER DATE FORMAT WILL BE TREATED AS ONLY CONTAINING INFORMATION ABOUT THE YEAR
is.full.date <- sapply(dp.split,length)==3

### CREATE YEAR, MONTH AND DAY
year <- numeric(length(dp))
year[is.full.date] <- as.numeric(format(as.Date(dp[is.full.date],"%Y/%m/%d"),"%Y"))
year.list <- sapply(dp.split[!is.full.date],function(x){as.numeric(x[1])})
if(any(!is.full.date)) year[!is.full.date] <- as.numeric(unlist(year.list))

month <- numeric(length(dp))
month[is.full.date] <- as.numeric(format(as.Date(dp[is.full.date],"%Y/%m/%d"),"%m"))
month[!is.full.date] <- NA

day <- numeric(length(dp))
day[is.full.date] <- as.numeric(format(as.Date(dp[is.full.date],"%Y/%m/%d"),"%d"))
day[!is.full.date] <- NA
 }
else{
  year <- sub(".*([0-9]{4}).*","\\1",dp)
  year <- as.numeric(year)
  month <- as.numeric(rep(NA,length(year)))
  day <- month
}

### CHECK FOR USER-DEFINED ID; SET ID

if(all(id.type=="author.year")){

  last <- sapply(au,function(x){sub("([a-zA-Z]*),.*","\\1",x[1])})
  last <- unlist(last)

  id <- paste(last,year,sep="")

  if(length(unique(id))!=length(id)){

    freq.id <- table(id)
    index <- which(freq.id>1)
    non.unique.id <- names(freq.id)[index]

    for(key in non.unique.id){
      key.index <- which(id==key)
      order <- letters[1:length(key.index)] #ADD LETTER
      id[key.index] <- paste(id[key.index],order,sep="")
        }
    }
}
else if(all(id.type=="pubmed")){
  id = sapply.field.value.gettor("M1",temp.list,end=tag.ending)
  id = as.character(id)
  if(any(nchar(id)>8)) id = as.character(1:length(temp.list))
}
else{
  if(all(id.type=="numeric")|length(id.type)!=length(temp.list)){
    id = as.character(1:length(temp.list))
    }
  else{
    id = as.character(id.type)
  }
}


             Reference( id,
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
                        ab
                      )

}
