readinputfile1<-function(filepath,sepchar){
  browser()
  result = tryCatch({
    filedata<-read.table(filepath,sep=sepchar,header=T,stringsAsFactors=T,quote="",comment.char="")
  }, warning = function(w) {
    cat("warning read.table()\n") #warning-handler-code
    filedata<-""
  }, error = function(e) {
    cat("error read.table()\n") #error-handler-code
    output$warning<-renderText("ERROR - Could not read input file")
    filedata<-""
  }, finally = {
    cat("cleanup  read.table()\n") #cleanup-code
    return(filedata)
  })
  
}


readinputfile<-function(filepath,sepchar,decchar){
  #browser()
  res <- try(read.table(filepath,sep=sepchar,header=T,stringsAsFactors=T,quote="",comment.char="",dec=decchar))
  if(inherits(res, "try-error")){
    # error - try reading ANSI encoding
    res <- try(read.table(filepath,sep=sepchar,header=T,stringsAsFactors=T,quote="",comment.char="", fileEncoding="Windows-1252",dec=decchar))
    if(inherits(res, "try-error")){
      res<-"Error reading file"
    }
  } 
    return(res)
  }