library(RCurl)
library(XML)

getUrl <- function() {
    today <- as.POSIXlt(Sys.Date())$mday
    link <- "www.beautifulbritain.co.uk/htm/onthisday/current/"
    link <- paste(paste(link, today, sep = ""), ".htm", sep = "")
    return(link)
}

strip <- function(node) {
    str <- xmlValue(node)
    prfx <- substr(str,1,4)
    str <- substr(str,5,nchar(str))
    str <- paste(prfx, str)
    return(str)
}

getFact <- function() {
    data <- getURL(getUrl(), ssl.verifypeer=0L, followlocation=1L)
    doc <- htmlParse(data)
    ps <- getNodeSet(doc, "//p")
    ps <- lapply(ps, strip)
    return(ps[round(runif(1)*length(ps),0)])
}

print(getFact())