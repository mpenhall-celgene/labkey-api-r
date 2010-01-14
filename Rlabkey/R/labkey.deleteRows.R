labkey.deleteRows <- function(baseUrl, folderPath, schemaName, queryName, toDelete) 
{  
## Default showAllRows=TRUE
showAllRows=TRUE

## Error if any of baseUrl, folderPath, schemName or toDelete are missing
if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("toDelete")==FALSE)
stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and toDelete."))

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## URL encode folder path, JSON encode post body
if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
nrows <- nrow(toDelete)
ncols <- ncol(toDelete)
p1 <- toJSON(list(schemaName=schemaName, queryName=queryName, apiVersion=8.3))
cnames <- colnames(toDelete)
p3 <- NULL
for(j in 1:nrows)
    {cvalues <- as.list(toDelete[j,])
    names(cvalues) <- cnames
    p2 <- toJSON(cvalues)
    p3 <- paste(p3,",",p2,sep="")}
pbody <- paste(substr(p1,1,nchar(p1)-1),', \"rows\":[',substr(p3,2,nchar(p3)),"] }",sep="")


## Set options
reader <- basicTextGatherer()
header <- basicTextGatherer()
handle <- getCurlHandle()
headerFields <- c('Content-Type'="application/json;charset=utf-8")
clist <- ifcookie()
if(clist$Cvalue==1) {myopts <- curlOptions(cookie=paste(clist$Cname,"=",clist$Ccont, sep=""),
                        writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE,
                        ssl.verifypeer=FALSE, followlocation=TRUE)} else
{myopts <- curlOptions(netrc=1, writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE,
                        ssl.verifypeer=FALSE, followlocation=TRUE)}


## Post form
myurl <- paste(baseUrl,"query",folderPath,"deleteRows.api",sep="")
curlPerform(url=myurl, postFields=pbody, httpheader=headerFields, .opts=myopts, curl=handle)

 
## Error checking, decode data and return in data frame
h <- parseHeader(header$value())
status <- getCurlInfo(handle)$response.code
message <- h$statusMessage
if(status==500) {decode <- fromJSON(reader$value()); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}

if(status>=400)
    {contTypes <- which(names(h)=='Content-Type')
    if(length(contTypes)>1 & h[contTypes[2]]=="application/json;charset=utf-8")
        {decode <- fromJSON(reader$value()); message<-decode$exception; stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))} else
    {stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}}

newdata <- fromJSON(reader$value())


return(newdata)
}
                                                              