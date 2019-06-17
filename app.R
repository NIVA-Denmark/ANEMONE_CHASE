rm(list = ls())
library(shiny)
library(ggplot2)
library(DT)
library(data.table)
source('CHASE.R')
source('outputtable.R')


ui <- fluidPage(
  
  a(target="blank",href="http://anemoneproject.eu/",img(src="anemone_logo.jpg")),
  titlePanel("CHASE Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose input file'),
      withTags({
        div(class="header", checked=NA,
            h4("Instructions"),
            p("Select the file containing input data for the CHASE assessment.
              The file must be in text format with columns separated by semi-colons.
              Column headers must be included. The required columns are:"),
            ul(
              li("Matrix"),
              li("Substance"),
              li("Threshold"),
              li("Status")
            ),
            p("The following columns are optional:"),
            ul(
              li("Waterbody"),
              li("Response (1 or -1)")
            ),
            p("The assesssment is made per waterbody. If no waterbody is specified, all indicators are combined in a single assessment."),
            p("Response=1 (default): status worsens with increasing indicator value. Response=-1: status improves with increasing indicator value"),
            p("Example data can be found here:", HTML("<a href='data/CHASE_test.csv' target='_blank'>CHASE_test.csv</a>"))
        )
        
      }),
      

      withTags({
        div(class="header", checked=NA,
            h4("More information"),
            p("To find out more, contact ",
            a(href="mailto:cjm@niva-dk.dk", "NIVA Denmark")),
            a(href="mailto:cjm@niva-dk.dk",img(src="NIVA-Denmark-150.png"))
        )
      })
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Data", 
                 h3(tags$style(type='text/css', '#warning {color: red;}'), 
                    textOutput("warning")),
                 p(DT::dataTableOutput("InDatatable"))
                 
                 ),
        tabPanel("Indicators", h3(""),
                 p(DT::dataTableOutput("IndicatorsTable"))),
        tabPanel("Results by Matrix", h3(""),
                 p(DT::dataTableOutput("QEResultsTable"))),   
        tabPanel("Overall Results", h3(""),
                 p(DT::dataTableOutput("ResultsTable"))),   
        tabPanel("Plot", h3(""),
                 p(plotOutput("plot",inline=TRUE)) )  
      ) # tabset panel
    )
  )  
  
) #fluid page

server <- function(input, output, session) {

  output$caption <- renderText(input$num)
  
  addResourcePath("data","./data/")
 
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    output$warning<-NULL
    
    result = tryCatch({
      filedata<-read.table(infile$datapath,sep=";",header=T,stringsAsFactors=T,quote="",comment.char="")
    }, warning = function(w) {
      cat("warning read.table()\n") #warning-handler-code
      filedata<-NULL
    }, error = function(e) {
      cat("error read.table()\n") #error-handler-code
      output$warning<-renderText("ERROR - Could not read input file")
      filedata<-NULL
    }, finally = {
      cat("cleanup  read.table()\n") #cleanup-code
      return(filedata)
    })
    
    
  })
  
  InData <- reactive({
    df<-filedata()
    if (is.null(df)){
      return(NULL)
    }else{
      if(length(df)>0){
      out<-Assessment(df,0)     #Individual indicator results
      if(is.data.frame(out)){
        return(out)
      }else{
        output$warning<-renderText(out)
        return(data.frame())
      }
      }else{
        return(data.frame())
        }
    }
  })
  
  IndicatorsData<- reactive({
    df<-filedata()
    if (is.null(df)){
      return(NULL)
      }else{
        if(length(df)>0){  
          out<-Assessment(df,1)     #Individual indicator results
          if(is.data.frame(out)){
            return(out)
          }else{
            return(data.frame())
          }
        }else{
          return(data.frame())
        }    
    } 
  })
  
  QEdata <- reactive({
    df<-filedata()
    if (is.null(df)){
      return(NULL)
    }else{
      if(length(df)>0){  
        out<-Assessment(df,3)     #Quality Element results
        if(is.data.frame(out)){
          return(out)
        }else{
          return(data.frame())
        }
      }else{
        return(data.frame())
      } 
    }
  })
  QEspr <- reactive({
    df<-filedata()
    if (is.null(df)){
      return(NULL)
      }else{
        if(length(df)>0){
          out<-Assessment(df,2)     #QE Results transposed
          if(is.data.frame(out)){
            return(out)
          }else{
            return(data.frame())
          }         
          
          }else{
          return(data.frame())
        } 
        
 
    } 
  })
  
  CHASEplot<- reactive({
    QE<-QEdata()
    
    ymax=max(QE$ConSum,na.rm=TRUE)
    ymax=ceiling(ymax)
    if(ymax>5 & ymax<10){ymax=10}
    if(ymax>1 & ymax<5){ymax=5}
    
    if (is.null(QE)){return(NULL)}
    
    levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),
                              levels=c("High","Good","Moderate","Poor","Bad")),
                       c(0.0,0.5,1,5,10),
                       c(0.5,1,5,10,ymax))
    names(levels)[1] <- 'Status'
    names(levels)[2] <- 'ymin'
    names(levels)[3] <- 'ymax'
    
    levels2<-levels
    levels$x<-0.5
    levels2$x<-0.5+max(as.numeric(QE$Waterbody))
    
    levels<-rbind(levels, levels2)
    
    levels<-levels[levels$ymin<=ymax,]
    ymax2=max(levels$ymax,na.rm=TRUE)
    levels[levels$ymax==ymax2,]$ymax<-ymax    
    Palette1=c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
    
    shapelist<-c(0,1,2,4)
    #browser()
    df<-QE 
    df$Waterbody<-factor(df$Waterbody,levels=rev(levels(df$Waterbody)))

    p<-ggplot(data=df,x=Waterbody,y=ConSum) + theme_bw(base_size=14) +
      theme(legend.position="top",legend.box="horizontal") +
      geom_point(size=5,data=df, aes(x=factor(Waterbody), y=ConSum,shape=Matrix)) +
      geom_ribbon(data=levels,aes(x=x,ymin=ymin,ymax=ymax,fill=Status),alpha=1) +
      geom_point(size=5,data=df, aes(x=factor(Waterbody), y=ConSum,shape=Matrix)) +
      scale_fill_manual(name="Status", values=Palette1)+
      scale_shape_manual(values=shapelist) +
      ylab('Contamination Sum') + xlab('Waterbody') +
      coord_flip()
      return(p)
  })
  
  figh<-reactive({
    QE<-QEspr()
    n<-nrow(QE)
    return(100+n*60)
  })
  
  
  observe({
    df<-InData()
    if(!is.null(df)){
      if(ncol(df)>1){
        output$InDatatable <- IndicatorTableDT(df)
      }else{
        output$InDatatable <- NULL
      }
    }
    
    })
  #output$InDatatable <- renderTable({return(InData())})
  
  observe({
    df<-IndicatorsData()
    if(!is.null(df)){
      if(ncol(df)>1){
        output$IndicatorsTable <- IndicatorTableDT(df,roundlist=c("CR","ConSum"),valuecols="QEStatus",cols=c("QEStatus","ConSum"))
      }else{
        output$IndicatorsTable <- NULL
      }
    }
  })
  
  observe({
    QEnames<-QEdata()
    df<-QEdata()
    if(!is.null(df)){
      if(ncol(df)>1){
        output$QEResultsTable <- IndicatorTableDT(df,roundlist=c("ConSum"),valuecols="QEStatus",cols=c("QEStatus","ConSum"))
      }else{
        output$QEResultsTable <- NULL
        }
      }
    
  })
  
  
  
  observe({
    QEnames<-QEdata()
    if(!is.null(QEnames)){

      QEnames<-distinct(QEnames,Matrix)
      QEnames<-as.character(QEnames$Matrix)
      roundcols<-c("ConSum",QEnames)
    }else{
      roundcols<-c("ConSum")
    }
    df<-QEspr()
    if(!is.null(df)){
      if(ncol(df)>1){
        output$ResultsTable <- IndicatorTableDT(QEspr(),roundlist=roundcols,valuecols="Status",cols=c("Status","ConSum"))
      }else{
        output$ResultsTable <- NULL
      }
    }
  })
  
  
  
  
  output$QEtable <- renderTable({return(QEspr())})

  observe({
    if (is.null(QEdata())){
      output$plot <- NULL
    }else{
      output$plot <- renderPlot({return(CHASEplot())},
                                width=800,height=figh(),res=72) 
    }
  })

  
}

shinyApp(ui=ui, server=server)
