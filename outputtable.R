

IndicatorTableDT<-function(df,roundlist=NULL,valuecols="",cols=""){
  

  if(is.data.frame(df)){
    
    dt<-datatable(df)
    if(!is.null(roundlist)){
      dt<-dt %>% formatRound(columns=roundlist, digits=3)
    }
    
    if(valuecols!=""){
      if(cols==""){
        cols<-valuecols
      }
    dt<-dt %>% # Style cells status
      formatStyle(
        valueColumns = valuecols, #"QEStatus",
        columns = cols, #c("QEStatus","ConSum"),
        backgroundColor = styleEqual(c("High","Good","Moderate","Poor","Bad"), 
                                     c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000"))
      )
    }
    
  
    return(DT::renderDataTable({dt}))
  }else{
    return(DT::renderDataTable({
      dt<-data.frame() %>% 
        datatable()
    }))
  }
}


