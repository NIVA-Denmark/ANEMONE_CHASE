

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

readinputfilexl<-function(filepath,sepchar,decchar){
  require(readxl)
  
  
  res <- try(read_excel(filepath, 1))
  if(inherits(res, "try-error")){
    # error - try reading ANSI encoding
      res<-"Error reading Excel file"
    }
  return(res)
}



file_ext <- function(f_name) {
  n<-gregexpr("\\.",f_name)[[1]]
  pos<-n[length(n)]
  tolower(substr(f_name,1+pos,nchar(f_name)))
  }
