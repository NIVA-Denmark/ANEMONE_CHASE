library("dplyr")
library("tidyr")

#===============================================================================
# function Assessment
Assessment<- function(assessmentdata,summarylevel=0){
  datain<-assessmentdata
  



   requiredcols <- c("Matrix","Substance","Threshold","Status")
   extracols <- c("Waterbody","Response")
   
  
  #Check column names in the imported data
  cnames<-names(assessmentdata)
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse=FALSE
  
  doConfidence<-FALSE
  if("Confidence" %in% cnames){
    doConfidence<-TRUE
  }
  
  for (i in 1:nimp){
    for (j in 1:nreq){
      if(toupper(requiredcols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j]=1
      }
    }
    for (j in 1:nextra){
      if(toupper(extracols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- extracols[j]
        okextra[j]=1
      }
    }
  }
  
  for(j in 1:nextra){
    if(okextra[j]==0){
      if(extracols[j]=="Waterbody"){
        assessmentdata$Waterbody<-"1"
        assessmentdata$Waterbody<-factor(assessmentdata$Waterbody,levels=c("1"))
        assessmentdata <- assessmentdata %>% 
          select(Waterbody, everything())
        
      }else{
        assessmentdata[[extracols[j]]]<-1
      }
    }
  }

  n<-sum(ok, na.rm = TRUE)
  
  if(n<nreq){
    # The required columns were not found in the input data
    message("Error in CHASE Assessment. Required column(s) were not found in the input data:")
    for (j in 1:nreq){
      if(ok[j]!=1){
        message(paste("    ",requiredcols[j]))
      }
    }
    
    missingcols<-requiredcols[ok!=1]
    return(paste0("Missing columns: '",paste(missingcols,collapse="','"),"'"))
  }else{
    # The required columns are present - do the assessment
    
    #browser()
    # for data having identical values in all columns except Status, 
    # then the average status is used
    groupcols<-names(assessmentdata)
    groupcols<- groupcols[!groupcols=="Status"]
    
    assessmentdata <- assessmentdata %>%
      mutate(id=row_number()) %>%
      group_by(.dots=groupcols) %>%
      summarise(Status=mean(Status,na.rm=T),n=n(),id=min(id,na.rm=T)) %>%
      arrange(id) %>%
      select(-id) %>%
      ungroup()    
    
    
    
    # Change order of matrices factors
    mat1<-data.frame(unique(assessmentdata$Matrix))
    names(mat1)[1] <- 'Matrix'
    mat1$char<-as.character(mat1$Matrix)
    mat1$len<-nchar(mat1$char)
    mat1<-arrange(mat1,len)
    
    assessmentdata$Matrix <- factor(assessmentdata$Matrix, levels = mat1$char)

    # All combinations of matrices and waterbodies
    # This is used to ensure that a NA is returned where the combinations are missing
    waterbodies<-unique(assessmentdata$Waterbody)
    matrices<-unique(assessmentdata$Matrix)
    matrices<-expand.grid(waterbodies, matrices)
    names(matrices)[1] <- 'Waterbody'
    names(matrices)[2] <- 'Matrix'
    
    
    
    assessmentdata$CR<-ContaminationRatio(assessmentdata$Threshold,assessmentdata$Status,assessmentdata$Response)
    
    if(doConfidence){
      QEdata<-assessmentdata
      QEdata$ConfScore<-toupper(substr(QEdata$Confidence,1,1))
      QEdata$ConfScore<-ifelse(QEdata$ConfScore=="H",1,
                               ifelse(QEdata$ConfScore=="M",0.5,0))
      QEdata<-QEdata %>%
        group_by(Waterbody,Matrix) %>%
        summarise(sumCR=sum(CR,na.rm=T), Count=n(),avgCR=mean(CR,na.rm=T))
      
    }
    
    QEdata<-assessmentdata %>%
      group_by(Waterbody,Matrix) %>%
      summarise(sumCR=sum(CR,na.rm=T), Count=n(),avgCR=mean(CR,na.rm=T)) %>%
      mutate(ConSum=sumCR/sqrt(Count),
             matrixmatch=tolower(Matrix))
    
   BioEffectsList <- c("biological effects",
                       "biological.effects",
                       "bio effects",
                       "bio.effects")
   
    # for biological effects, use the average of the contamination ratio
    QEdata<-QEdata %>%
      mutate(ConSum=ifelse(matrixmatch %in% BioEffectsList,avgCR,ConSum)) %>%
      select(-c(sumCR,avgCR,Count,matrixmatch))
    

    QEspr<-spread(QEdata,Matrix,ConSum)
    
    QEdata$QEStatus<-CHASEStatus(QEdata$ConSum)
    QEdata<-left_join(matrices,QEdata,c('Waterbody','Matrix'))
    QEdata<-arrange(QEdata,Waterbody,Matrix)
    
    CHASE<-summarise(group_by(QEdata,Waterbody), ConSum=max(ConSum, na.rm = TRUE))
    CHASEQE<-inner_join(QEdata, CHASE, by=c('Waterbody','ConSum'))
    CHASEQE<-rename(CHASEQE,Status=QEStatus,Worst=Matrix)
    assessmentdata<-left_join(assessmentdata,QEdata,c('Waterbody','Matrix'))
    QEspr<-inner_join(QEspr, CHASEQE, 'Waterbody')
    
    
    for(j in 1:nextra){
      if(extracols[j]=='Waterbody' & okextra[j]==0){
        #assessmentdata[[extracols[j]]]<-NULL
        #QEdata[[extracols[j]]]<-NULL
      }
    }
    
    #return(n)
    if(summarylevel==1){
      return(assessmentdata)
    }else if(summarylevel==2){
      return(QEspr)
    }else if(summarylevel==3){
      return(QEdata)
    }else if(summarylevel==4){
      return(CHASEQE)
    }else{
      return(datain)
    }
    #
  }
}

#===============================================================================
# function ContaminationRatio
ContaminationRatio<- function(threshold, status, response=1){
  # If response is not specified, it will be assumed to be positive
  # i.e. ContaminationRatio increases (worsens) with increasing status value
  if (missing(response)){
    response=1
  }
  response<-ifelse(is.na(response), 1, response)
  
  # ContaminationRatio calculated depending on Response direction
  cr<-ifelse(response>0, status/threshold, threshold/status)
    return(cr)
}

#===============================================================================
#Function CHASEStatus
CHASEStatus<-function(CRsum){
  status<-ifelse(CRsum>0.5, "Good", "High")
  status<-ifelse(CRsum>1, "Moderate", status)
  status<-ifelse(CRsum>5, "Poor", status)
  status<-ifelse(CRsum>10, "Bad",status )
  return(status)
}

AddColours<-function(CRsum){
  co<-ifelse(CRsum>0.5, '#66FF66', '#3399FF')
  co<-ifelse(CRsum>1, '#FFFF66', co)
  co<-ifelse(CRsum>5, '#FF9933', co)
  co<-ifelse(CRsum>10, '#FF6600',co)
  return(co)
}



#===============================================================================
#Function Contamination sum
ContaminationSum <- function(ratiolist){
  
}




