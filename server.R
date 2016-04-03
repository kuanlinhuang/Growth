#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
   datasetInput<-reactive({
    
    if(is.null(input$file1)) return(NULL) 
    inFile <- input$file1
    
    dataframe<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
            quote=input$quote,dec=input$dec)
    })
    
    
   #names of variables
   output$Names<-renderText({
     as.vector(c(colnames(datasetInput())))
   })
   
   output$contents<- renderTable({
    
     if(input$obs==0){
       n<-nrow(datasetInput())
       datasetInput()
     }
     datasetInput()[1:input$obs,]
     
   })
   
   #P2
  inputVariable <- reactive({
     colnames(datasetInput())
   })
  

   
   output$Dep<- renderUI({
    if(is.null(inputVariable()))
    return()
      values <- as.factor(inputVariable())
      rounds_levels <- c(as.character(values))
      selectInput("Variable", "Dependents Variables:", rounds_levels)
   })
   
   
   output$VarI <- renderUI({
     values <- as.factor(inputVariable())
     rounds_levels <- c(as.character(values))
     checkboxGroupInput("VarInd", 
                        "Independent Variables:",
              rounds_levels [!rounds_levels  %in% input$dependent])
   })
   
   output$Interaction <- renderUI({
     checkboxInput("Interaction1", "Interaction", value=FALSE)
   })
   
   runRegression <- reactive({
   
      #predictors<-paste(input$VarInd)
     A<-input$VarInd
     A1<-as.data.frame(datasetInput()[,A])
     colnames(A1)<-A
     if(length(colnames(A1)>1)){
       if(input$Interaction1==TRUE){
     predictors<-paste(colnames(A1),collapse="*")
       }else{
     predictors<-paste(colnames(A1),collapse="+")
       }
     }else{
       predictors<-colnames(A1)
     }
      Dep<-input$Variable

      For<-as.formula(paste(Dep," ~ ",predictors))
     
     #For<-as.formula(paste("Tumor.volume..mm3."," ~ ",paste(c("Days","Treatment"),collapse= "*")))
     
     
      A1<-as.data.frame(datasetInput())
      for(i in 1: ncol(A1)){
      if(is.factor(A1[,i])){
      A1[,i]<-relevel(A1[,i], ref = 4)
      }
      }
       lm(For,data=A1)
  
   })
   

 
  
   output$regTab <- renderTable({
     if(!is.null(input$VarInd)){
       summary(runRegression())$coefficients
         } else {
       print(data.frame(Warning="Please select Model Parameters."))
     }
   },digits=-5)
   
   output$Anov <- renderTable({
     if(!is.null(input$VarInd)){
       anova(runRegression())
        } else {
       print(data.frame(Warning="Please select Model Parameters."))
     }
   },digits=-5)

   output$Yvar<- renderUI({
     if(is.null(inputVariable()))
       return()
     values <- as.factor(inputVariable())
     rounds_levels <- c(as.character(values))
     selectInput("YVari", "Y-axis", rounds_levels)
   })
   
   output$Xvar<- renderUI({
     if(is.null(inputVariable()))
       return()
     values <- as.factor(inputVariable())
     rounds_levels <- c(as.character(values))
     selectInput("XVari", "X-axis:", rounds_levels)
   })
   
   output$Factor<- renderUI({
     if(is.null(inputVariable()))
       return()
     values <- as.factor(inputVariable())
     rounds_levels <- c(as.character(values))
     selectInput("Facto", "Factor:", rounds_levels)
   })
   
   
   
GrowPlot<- reactive({ 
  
  Data<-datasetInput()
  if(is.null(Data)){
    return()
  }else{
  if(is.numeric(Data[,input$Facto])) return(NULL) 
  if(is.character(Data[,input$YVari])) return(NULL)
  
   p<-ggplot(Data, aes(as.numeric(Data[,input$XVari]),as.numeric(Data[,input$YVari]), color=as.factor(Data[,input$Facto]))) +
    stat_summary(fun.data=mean_se, geom="pointrange")+
    aes(colour = as.factor(Data[,input$Facto])) + stat_summary(fun.y = mean, geom="line") +
    labs(y=as.character(input$YVari), x=as.character(input$XVari))+
    theme(legend.title=element_blank())+ 
    theme(axis.title.y = element_text(size = rel(input$cexLab), angle = 90))+
    theme(axis.title.x = element_text(size = rel(input$cexLab), angle = 00))
  p}
   })

output$GrPlot<- renderPlot({ 
  if(is.null(GrowPlot())){
    return("warning: select any var to plot")
  }
GrowPlot()
  
})


output$VarInt<- renderUI({
  if(is.null(inputVariable()))
    return()
  values <- as.factor(inputVariable())
  rounds_levels <- c(as.character(values))
  selectInput("VarTDe", "Var to plot:", rounds_levels)
})

output$FactoD<- renderUI({
  if(is.null(inputVariable()))
    return()
  values <- as.factor(inputVariable())
  rounds_levels <- c(as.character(values))
  selectInput("FactoDD", "Factor:", rounds_levels)
})

DensPlot<- reactive({ 
  
  Data<-datasetInput()
  
  if(is.numeric(Data[,input$FactoDD])) return(NULL) 
  if(is.character(Data[,input$VarTDe])) return(NULL) 
  qplot(as.numeric(Data[,input$VarTDe]), geom="density", fill=as.factor(Data[,input$FactoDD]), alpha=I(.5),
        main="", xlab=as.character(input$VarTDe),
        ylab="Density")+theme(legend.title=element_blank())
  
 })

output$DensityPlot<- renderPlot({ 
  if(is.null(DensPlot())){
    return("warning: select any var to plot")
  }
  DensPlot()
  
})
   

output$DownloadGr <- downloadHandler(
  filename = function() { 
    paste("GrowPlot", '.jpg', sep='') 
  },
  content = function(FILE=NULL) {
    jpeg(filename=FILE)
    print(GrowPlot())
    dev.off()
  }
) 

output$DensityPlot1 <- downloadHandler(
  filename = function() { 
    paste("DensityPlot", '.jpg', sep='') 
  },
  content = function(FILE=NULL) {
    jpeg(filename=FILE)
    print(DensPlot())
    dev.off()
  }
) 
   
   
   
  }
  )
