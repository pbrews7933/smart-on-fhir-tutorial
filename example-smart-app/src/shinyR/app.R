#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# options(shiny.port = 4314)
library(shiny)
library(shinyjs)
library(lubridate)
#library(dplyr)
library(ggplot2)
library(reshape2)
library(dbConnect)
library(ggpubr)
library(aws.s3)
library(RCurl)
library(readr)
library(reshape)  # to get at cast
library(dplyr)
library(gridExtra)
library(cowplot)
library(jsonlite)
library(httr)
library(rlist)
library(gsubfn)

#library(scales)
# reset value of action button to prevent further un-requested recomputation of the expensive process


# Define UI for application that draws the graphics
ui <- fluidPage(
  # Application title
  titlePanel(textOutput("patientBanner"), windowTitle="Ellumen Wearable"),
  useShinyjs(),
  # Date ranges for the graph
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "DateRangeOptions",
        label = h3("Date Range"),
        choices = list(
          "Today" = 1,
          "One Week" = 2,
          "One Month" = 3,
          "Two Months" = 4,
          "Six Months" = 5,
          "One Year" = 6,
          "Two Years" = 7,
          "Custom Date Range" = 8
        ),
        selected = 3
      ),
      dateRangeInput("Dates", start = strftime(Sys.Date(), "%Y-%m-%d"), end= strftime(Sys.Date(), "%Y-%m-%d"), label = h3(""), max = strftime(Sys.Date(), "%Y-%m-%d")),
      radioButtons(
        "MeasurementOptions",
        label = h3("Settings"),
        choices = list(
          "lbs" = 1,
          "kg" = 2
        ),
        selected = 1
      ),
      verbatimTextOutput('v1'), 
      verbatimTextOutput('v2')
    ),
    # Tabset Panel with graphs specific to the device in use
    mainPanel(htmlOutput("Main"),
              tabsetPanel(
                #              tabPanel("Omron:Omron Heartvue", plotOutput("TSOMRON_OMRON_HEARTVUE")),
                #              tabPanel("FitBit:FitBit Wearable", plotOutput("TSFITBIT_FITBIT"))
                id="TS")
    )
  )
)

server <- function(input, output, session) {
  URLString <- parseQueryString(isolate(session$clientData$url_search))
  if (length(URLString)>0) {
    qID <- ""
    defaultSDate <- strftime(Sys.Date() - 31, "%Y-%m-%d")
    defaultEDate <- strftime(Sys.Date(), "%Y-%m-%d")
    if (checkURLString(URLString,'patient') != "") {
      qType <- "Patient"
      qID <- URLString['patient']
    } else if (checkURLString(URLString,'unique') != "") {
      qType <- "Unique"
      qID <- URLString['unique']
    }
    config <- fromJSON("config/settings.json")
    EHRConnect <- FALSE
    if (!grepl("^[a-zA-Z0-9_]*$", qID)) {qID <- ""}
    if (qID != ""){
      if (checkURLString(URLString,'access_token') != "" && checkURLString(URLString,'srvrURL') != "" ) {
        EHRConnect <- TRUE
        accessToken <- URLString['access_token']
        serverURL <- toString(URLString['srvrURL'])
        patientID <- URLString['patient']
        encounter <- URLString['encounter']
        httpPatient <- httpGET(paste(serverURL,
                                     "/Patient/"
                                     ,patientID, sep="")
                               ,accessToken)
        
        jsonPatient <- content(httpPatient, as="parsed", type="application/json")
        height <- ""
        effDateTime <- "0000-00-00T00:00:00.000Z"
        httpObservation <- httpGET(paste(serverURL,
                                         "/Observation?patient="
                                         , patientID
                                         ,"&code=http://loinc.org|8302-2"
                                         , sep="")
                                   , accessToken)
        repeat  {
          nextURL <- ""
          jsonObservation <- content(httpObservation, as="parsed", type="application/json")
          if (jsonObservation$resourceType !="Bundle") {
            jsonObservations <- jsonObservation
          } else {     
            jsonObservations <- jsonObservation$entry
            for (i in 1:length(jsonObservation$link)) {
              if (jsonObservation$link[[i]]$relation == "next"){
                nextURL <- jsonObservation$link[[i]]$url
              }
            }
          }
          for (i in 1:length(jsonObservations)) {
            if (!is.null(jsonObservations[[i]]$resource$code$coding[[1]]$code)) {
              if (jsonObservations[[i]]$resource$code$coding[[1]]$code == '8302-2') {
                if (!is.null(jsonObservations[[i]]$resource$effectiveDateTime)) {
                  if (jsonObservations[[i]]$resource$effectiveDateTime > effDateTime){
                    effDateTime <- jsonObservations[[i]]$resource$effectiveDateTime
                    height <- paste(jsonObservations[[i]]$resource$valueQuantity$value
                                    , jsonObservations[[i]]$resource$valueQuantity$unit)
                  }
                }
              }
            }
          }
          if (nextURL == "") break
          httpObservation <- httpGET(nextURL, accessToken)
        }
       # getDocumentReference(serverURL, patientID, accessToken)
        idAndFullname <- paste(checkJsonString(jsonPatient$id)
          ,prefixText(" ",checkJsonString(jsonPatient$name[[1]]$family[[1]]))
          ,prefixText(", ",checkJsonString(jsonPatient$name[[1]]$given[[1]])),sep="")
        banner <- paste(idAndFullname,"-"
                                  ,prefixText(" Gender:", checkJsonString(jsonPatient$gender))
                                  ,prefixText(" DoB:", checkJsonString(jsonPatient$birthDate))
                                  ,prefixText(" Height:", checkJsonString(height))
                                  , sep="")
        if(URLString['need_patient_banner'] =="true") {
          output$patientBanner <- renderText(banner)
        } else {
          output$patientBanner <- renderText("")
        }
      }
      fetchedRows<-retrieveRow(config$AWS_RDS,qType, qID,defaultSDate, defaultEDate)
      ds <- unique(fetchedRows$DeviceString)
      if (length(ds) > 0) {
        by(ds, 1:length(ds), function(row){
          c <- gsub(":","_",gsub(" ","_",row))
          if(EHRConnect){
            appendTab(inputId = "TS",
                      tabPanel(title = row,  plotOutput(paste("TS", c, sep="")),
                               textInput(paste("TI", c, sep=""), "Comments", "", width = "100%"),
                               htmlOutput(paste("GL", c, sep="")), 
                               htmlOutput(paste("JL", c, sep="")),
                               actionButton(paste("AB", c, sep=""), paste("Save ", c, " to EHR",sep="")),
                               htmlOutput(paste("DR", c, sep="")),
                               value = paste("TS", c, sep="")))
            
          } else {   # No Action Button
            appendTab(inputId = "TS",
                      tabPanel(title = row,  plotOutput(paste("TS", c, sep="")),
                               htmlOutput(paste("GL", c, sep="")), 
                               htmlOutput(paste("JL", c, sep="")),
                               value = paste("TS", c, sep="")))
            
          }
        })
        updateTabsetPanel(session, "TS",
                          selected = "TSOMRON_OMRON_HEARTVUE"
        )
      }
  }
}
# Show a plot of the device in question
output$TSOMRON_OMRON_HEARTVUE <- renderPlot({
  enable("ABOMRON_OMRON_HEARTVUE")
  filePrefix <- "OM"
  queryString <- ""
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
  if (dateList[1] < defaultSDate | dateList[2] > defaultEDate) {
    defaultSDate <- dateList[1]
    defaultEDate <- dateList[2]
    fetchedRows<-retrieveRow(config$AWS_RDS,qType, qID,defaultSDate, defaultEDate)
    ds <- unique(fetchedRows$DeviceString)      
  }
  plotTitle <- paste('Average Systolic/Diastolic ',
                     sep = ""
  )
  
  S <- subset(fetchedRows,fetchedRows$DeviceString == "OMRON:OMRON HEARTVUE" 
              & (fetchedRows$MetricType == "Diastolic" | fetchedRows$MetricType == "Systolic" )
              & fetchedRows$db_date >= dateList[1] 
              & fetchedRows$db_date <= dateList[2]) 
  
  S_F_Avg <- cast(subset(S,S$MetricType == "Diastolic" | S$MetricType == "Systolic"), db_date ~ MetricType, value="AvgValue")
  S_F_Min <- cast(subset(S,S$MetricType == "Diastolic"), db_date ~ MetricType, value="MinValue")
  S_F_Max <- cast(subset(S,S$MetricType == "Systolic"), db_date ~ MetricType, value="MaxValue")
  S_F_Max <- dplyr::rename(S_F_Max, MaxSystolic = Systolic)
  S_F_Min <- dplyr::rename(S_F_Min, MinDiastolic = Diastolic)
  S_F_Avg$MidPoint <- (S_F_Avg$Systolic + S_F_Avg$Diastolic) / 2
  
  S_F <- merge(merge(S_F_Avg, S_F_Min), S_F_Max)
  P1 <- ggplot(S_F, aes(db_date))
  P1 <- P1 + geom_boxplot(aes(ymin = S_F$MinDiastolic, lower = S_F$Diastolic, middle = S_F_Avg$MidPoint, upper = S_F$Systolic, ymax = S_F$MaxSystolic),
                          stat = "identity")
  P1 <- P1 + ggtitle(paste('Average Systolic/Diastolic Blood Pressure',sep = ""))
  P1 <- P1 + labs(y='mmHg')
  P1 <- P1 + theme(plot.title = element_text(size=20, face="bold"), axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=20))

  S <- subset(fetchedRows,fetchedRows$DeviceString == "OMRON:OMRON HEARTVUE" 
              & fetchedRows$MetricType == "Heart Rate" 
              & fetchedRows$db_date >= dateList[1] 
              & fetchedRows$db_date <= dateList[2])  
  
  P2 <- ggplot(S,
               aes(
                 x = db_date,
                 y = AvgValue,
                 group = MetricType,
                 colour = MetricType
               ))
  P2 <- P2 + geom_point(size=3)
  P2 <- P2 + geom_smooth(method = "lm", se=FALSE, color="black", size = 1, aes(group=MetricType, color=MetricType))
  P2 <- P2 + ggtitle(paste('Average Heart Rate',sep = ""))
  P2 <- P2 + labs(y='BPM')
  P2 <- P2 + scale_size_area(max_size = 6)
  P2 <- P2 + theme(legend.position = "none")
  P2 <- P2 + theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=20))
  P2 <- P2 + theme(legend.title = element_blank())
  P2 <- P2 + theme(plot.title = element_text(size=20, face="bold")) # Change size, color and face of plot title 
  P2 <- P2 + theme(axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P2 <- P2 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P2 <- P2 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 
  
  fileNameUnique <- paste(filePrefix, qID, dateList[1], dateList[2], sep="")
 
  ggarrange(P1, P2, nrow =2) %>%
    ggexport(filename = paste("Data/P",fileNameUnique, ".png", sep=""))
  
  list[JSONLink, GraphicLink] <- writeGraphicFile (config$AWS_S3, fileNameUnique
                    ,qID
                    ,"OMRON"
                    ,"OMRON HeartVue")
  output$JLOMRON_OMRON_HEARTVUE <- renderUI(tags$a(href = JSONLink, target = "_blank", "FHIR File"))
  output$GLOMRON_OMRON_HEARTVUE <- renderUI(tags$a(href = GraphicLink, target = "_blank", "Graphic File"))
  plot_grid(P1, P2, align = "v", nrow = 2, rel_heights = c(40, 20))
}
)
output$TSNOKIA_NOKIA_SCALE <- renderPlot({
  enable("ABNOKIA_NOKIA_SCALE")
  filePrefix <- "NO"
  queryString <- ""
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
  if (dateList[1] < defaultSDate | dateList[2] > defaultEDate) {
    defaultSDate <- dateList[1]
    defaultEDate <- dateList[2]
    fetchedRows<-retrieveRow(config$AWS_RDS,qType, qID,defaultSDate, defaultEDate)
    ds <- unique(fetchedRows$DeviceString)      
  }
  S <- subset(fetchedRows,fetchedRows$DeviceString == "NOKIA:NOKIA SCALE" & fetchedRows$db_date >= dateList[1] & fetchedRows$db_date <= dateList[2])  
  S_F <- cast(S, db_date ~ MetricType, value="AvgValue")
  S_F[["Bone Mass"]] = S_F[["Weight"]] * (S_F[["Bone Mass"]] / (S_F[["Bone Mass"]]+S_F[["Body Fat"]]+S_F[["Muscle Mass"]]))
  S_F[["Body Fat"]] = S_F[["Weight"]] * (S_F[["Body Fat"]] / (S_F[["Bone Mass"]]+S_F[["Body Fat"]]+S_F[["Muscle Mass"]]))
  S_F[["Muscle Mass"]] = S_F[["Weight"]] * (S_F[["Muscle Mass"]] / (S_F[["Bone Mass"]]+S_F[["Body Fat"]]+S_F[["Muscle Mass"]]))
  S_F <- melt(S_F, id=c("db_date"))
  S <- droplevels(subset(S_F,S_F$MetricType == "Bone Mass" | S_F$MetricType == "Muscle Mass" | S_F$MetricType == "Body Fat"))
  S_Water <- droplevels(subset(S_F,S_F$MetricType == "Body Water"))

  L <- "lbs"
  if (input$MeasurementOptions == "2")  {
    S$value <- S$value * 0.45359237
    S_Water$value <- S_Water$value * 0.45359237
    L <- "Kg"
  }
  P1 <- ggplot(S,
               aes(
                 x = db_date,
                 y = value,
                 group = MetricType,
                 colour = MetricType,
                 fill = MetricType
               ))
  P1 <- P1 + scale_size_area(max_size = 6)
  P1 <- P1 + scale_fill_brewer( palette = "Reds") 
  P1 <- P1 + geom_bar(position = position_stack(reverse = TRUE), stat = "identity")
  P1 <- P1 + ggtitle(paste('Average Weight per Day',sep = ""))
  P1 <- P1 + labs(y=L)
  #P1 <- P1 + scale_size_area(max_size = 6)
  P1 <- P1 + theme(legend.position = "bottom")
  P1 <- P1 + theme(plot.title = element_text(size=20))
  P1 <- P1 + theme(legend.title = element_blank()) # Change size, color and face of legend title 
  P1 <- P1 + theme(title = element_text(size=16, face="bold")) # Change size, color and face of plot title 
  P1 <- P1 + theme(axis.title.x = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P1 <- P1 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P1 <- P1 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 
  #P <- P + geom_line(data=S_Water, aes(x=db_date, y=value))

  S <- subset(fetchedRows,fetchedRows$DeviceString == "NOKIA:NOKIA SCALE" 
              & fetchedRows$MetricType == "Body Mass Index"
              & fetchedRows$db_date >= dateList[1] 
              & fetchedRows$db_date <= dateList[2])
   
  P2 <- ggplot(arrange(S, desc(MetricType)),
               aes(
                 x = db_date,
                 y = AvgValue
               ))
  P2 <- P2 + geom_point(size=3)
  P2 <- P2 + ggtitle(paste('BMI',sep = ""))
  P2 <- P2 + labs(y='BMI')
  P2 <- P2 + scale_size_area(max_size = 6)
  P2 <- P2 + theme(legend.position = "none")
  P2 <- P2 + theme(plot.title = element_text(size=20), axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=20))
  P2 <- P2 + theme(legend.title = element_blank())
  P2 <- P2 + theme(axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P2 <- P2 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P2 <- P2 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 

  S <- subset(fetchedRows,fetchedRows$DeviceString == "NOKIA:NOKIA SCALE" 
              & fetchedRows$db_date >= dateList[1] 
              & fetchedRows$db_date <= dateList[2])  

  S_F <- cast(S, db_date ~ MetricType, value="AvgValue")
  S_F[["Body Water"]] <- S_F[["Body Water"]] / S_F[["Weight"]] * 100
  S_F <- melt(S_F, id=c("db_date"))  # problem child
  S <- droplevels(subset(S_F,S_F$MetricType == "Body Water"))
  P3 <- ggplot(arrange(S, desc(MetricType)),
               aes(
                 x = db_date,
                 y = value
               ))
  P3 <- P3 + geom_line(size=1)
  P3 <- P3 + ggtitle(paste('Body Water',sep = ""))
  P3 <- P3 + labs(y='Percent')
  P3 <- P3 + scale_size_area(max_size = 6)
  P3 <- P3 + theme(legend.position = "none")
  P3 <- P3 + theme(plot.title = element_text(size=20), axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=20))
  P3 <- P3 + theme(legend.title = element_blank())
  P3 <- P3 + theme(axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P3 <- P3 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P3 <- P3 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 
  
  fileNameUnique <- paste(filePrefix, qID, dateList[1], dateList[2], sep="")
  
  ggarrange(P1, P3, P2, nrow =3) %>%
    ggexport(filename = paste("Data/P", fileNameUnique, ".png", sep=""))
  list[JSONLink, GraphicLink] <- writeGraphicFile (config$AWS_S3, fileNameUnique
                                                                ,qID
                                                                ,"NOKIA"
                                                                ,"NOKIA Scale")
  output$JLNOKIA_NOKIA_SCALE <- renderUI(tags$a(href = JSONLink, target = "_blank", "FHIR File"))
  output$GLNOKIA_NOKIA_SCALE <- renderUI(tags$a(href = GraphicLink, target = "_blank", "Graphic File"))
  plot_grid(P1, P3, P2, align = "v", nrow = 3, rel_heights = c(40, 20, 20))
}
)
output$TSFITBIT_FITBIT_WATCH <- renderPlot({
  enable("ABFITBIT_FITBIT_WATCH")
  filePrefix <- "FB"
  queryString <- ""
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
  if (dateList[1] < defaultSDate | dateList[2] > defaultEDate) {
    defaultSDate <- dateList[1]
    defaultEDate <- dateList[2]
    fetchedRows<-retrieveRow(config$AWS_RDS,qType, qID,defaultSDate, defaultEDate)
    ds <- unique(fetchedRows$DeviceString)      
  }
  
  S <- subset(fetchedRows,fetchedRows$DeviceString == "FITBIT:FITBIT WATCH" & fetchedRows$db_date >= dateList[1] & fetchedRows$db_date <= dateList[2])
  S <- subset(S,substr(S$MetricType, 1,3) == "Cal" | substr(S$MetricType, 1,3) == "Min")  
  S_Flattened <- cast(S, db_date ~ MetricType, value="AvgValue")
  S_Flattened[["Calories Level 1"]] <- S_Flattened[["Calories Level 1"]] / S_Flattened[["Minutes Level 1"]]
  S_Flattened[["Calories Level 2"]] <- S_Flattened[["Calories Level 2"]] / S_Flattened[["Minutes Level 2"]]
  S_Flattened[["Calories Level 3"]] <- S_Flattened[["Calories Level 3"]] / S_Flattened[["Minutes Level 3"]]
  S_Flattened[["Calories Level 4"]] <- S_Flattened[["Calories Level 4"]] / S_Flattened[["Minutes Level 4"]]
  
  S_Melted <- melt(S_Flattened, id=c("db_date"))
  S_MeltedSubset <- subset(S_Melted,substr(S_Melted$MetricType, 1,3) == "Min")
  
  S_MeltedSubset$MetricType <- factor(S_MeltedSubset$MetricType, labels = c("Low", "Fat Burn", "Cardio", "Peak"))
  
  P1 <- ggplot(arrange(S_MeltedSubset, desc(MetricType)),
               aes(
                 x = db_date,
                 y = value,
                 group = MetricType,
                 colour = MetricType,
                 fill = MetricType
               )) 
  P1 <- P1 + scale_size_area(max_size = 6)
  P1 <- P1 + scale_fill_brewer( palette = "Reds") 
  P1 <- P1 + geom_bar(position = position_stack(reverse = TRUE), stat = "identity")
  P1 <- P1 + labs(title=paste('Minutes per Heart Rate Level per Day',sep = ""), subtitle=paste("Low(30-80)", "Fat Burn(80-112)", "Cardio(112-136)", "Peak(136-220)",sep=","), y='Min')
  #P1 <- P1 + scale_size_area(max_size = 6)
  P1 <- P1 + theme(legend.position = "bottom")
  P1 <- P1 + theme(plot.title = element_text(size=20, hjust=0.5), plot.subtitle=element_text(size=14, hjust=0.5))
  P1 <- P1 + theme(legend.title = element_blank()) # Change size, color and face of legend title 
  P1 <- P1 + theme(title = element_text(size=16, face="bold")) # Change size, color and face of plot title 
  P1 <- P1 + theme(axis.title.x = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P1 <- P1 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P1 <- P1 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 
  
  S <- subset(fetchedRows,fetchedRows$DeviceString == "FITBIT:FITBIT WATCH" 
              & fetchedRows$MetricType == "Heart Rate"
              & fetchedRows$db_date >= dateList[1] 
              & fetchedRows$db_date <= dateList[2])
  
  P2 <- ggplot(arrange(S, desc(MetricType)),
               aes(
                 x = db_date,
                 y = AvgValue,
                 group = MetricType,
                 colour = MetricType,
                 fill = MetricType
               ))
  P2 <- P2 + geom_point(size=3)
  P2 <- P2 + scale_colour_discrete(name  = "Heart Rate") # Change legend title 
  P2 <- P2 + geom_smooth(method = "lm", se=FALSE, color="black", size = 1, aes(group=MetricType, color=MetricType, fill=MetricType))
  P2 <- P2 + ggtitle(paste('Average Resting Heart per Day',sep = ""))
  P2 <- P2 + labs(x='Date', y='BPM')
  P2 <- P2 + scale_size_area(max_size = 6)
  P2 <- P2 + theme(legend.position = "none")
  P2 <- P2 + theme(plot.title = element_text(size=20), axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=20))
  P2 <- P2 + theme(legend.title = element_blank())
  P2 <- P2 + theme(title = element_text(size=16, face="bold")) # Change size, color and face of plot title 
  P2 <- P2 + theme(axis.text = element_text(size = 12), axis.title = element_text(size = rel(1.0))) # Change size of the text in the axis-labels and axis title 
  P2 <- P2 + theme(legend.key.size =  unit(0.5, "in")) # Change key size in the legend 
  P2 <- P2 + theme(legend.text = element_text(size=12)) # Change the size labels in the legend 
  
  fileNameUnique <- paste(filePrefix, qID, dateList[1], dateList[2], sep="")
  
  ggarrange(P1, P2, nrow =2) %>%
    ggexport(filename=paste("Data/P", fileNameUnique, ".png", sep="")) 
  list[JSONLink, GraphicLink] <- writeGraphicFile (config$AWS_S3, fileNameUnique
                                                                ,qID
                                                                ,"FITBIT"
                                                                ,"FITBIT Watch")
  output$JLFITBIT_FITBIT_WATCH <- renderUI(tags$a(href = JSONLink, target = "_blank", "FHIR File"))
  output$GLFITBIT_FITBIT_WATCH <- renderUI(tags$a(href = GraphicLink, target = "_blank", "Graphic File"))
  plot_grid(P1, P2, align = "v", nrow = 2, rel_heights = c(40, 20))
}
)
observeEvent(input$ABOMRON_OMRON_HEARTVUE, {
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
#  print(paste("Thank you for clicking ABOMRON_OMRON_HEARTVUE",dateList[1],dateList[2]," P",qID,dateList[1],dateList[2],".json",sep=""))
  pBody <- writeDocumentReference("OM"
                                  ,qID
                                  ,"OMRON HeatyVue Data for"
                                  ,encounter
                                  ,dateList[1]
                                  ,dateList[2]
                                  ,isolate(input$TIOMRON_OMRON_HEARTVUE))
  result <- httpPOST(paste(serverURL,
                           "/DocumentReference",
                           sep=""), accessToken, paste("Data/BOM",qID,dateList[1],dateList[2],".json",sep=""))
  #print(result$status_code)
  if(result$status_code == 201) {
    disable("ABOMRON_OMRON_HEARTVUE")
    output$DROMRON_OMRON_HEARTVUE <- renderUI("Saved")
    delay(1000, output$DROMRON_OMRON_HEARTVUE <- renderUI(""))
  } else {
    print(result)
  }
  })
observeEvent(input$ABFITBIT_FITBIT_WATCH, {
#  print('Thank you for clicking ABFITBIT_FITBIT_WATCH')
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
  pBody <- writeDocumentReference("FB"
                                  ,qID
                                  ,"FITBIT Watch Data for"
                                  ,encounter
                                  ,dateList[1]
                                  ,dateList[2]
                                  ,isolate(input$TIFITBIT_FITBIT_WATCH))
  result <- httpPOST(paste(serverURL,
                          "/DocumentReference",
                          sep=""), accessToken, paste("Data/BFB",qID,dateList[1],dateList[2],".json",sep=""))
  if(result$status_code == 201) {
    disable("ABFITBIT_FITBIT_WATCH")
    output$DRFITBIT_FITBIT_WATCH <- renderUI("Saved")
    delay(1000, output$DRFITBIT_FITBIT_WATCH <- renderUI(""))
  } else {
    print(result)
  }
})
observeEvent(input$ABNOKIA_NOKIA_SCALE, {
 # print('Thank you for clicking ABNOKIA_NOKIA_SCALE')
  dateList <- dateRanges(input$DateRangeOptions, input$Dates[1], input$Dates[2])
  pBody <- writeDocumentReference("NO"
                                  ,qID
                                  ,"Nokia Scale Data for"
                                  ,encounter
                                  ,dateList[1]
                                  ,dateList[2]
                                  ,isolate(input$TINOKIA_NOKIA_SCALE))
  result <- httpPOST(paste(serverURL,
                           "/DocumentReference",
                           sep=""), accessToken, paste("Data/BNO",qID,dateList[1],dateList[2],".json",sep=""))
  if(result$status_code == 201) {
    disable("ABNOKIA_NOKIA_SCALE")
    output$DRNOKIA_NOKIA_SCALE <- renderUI("Saved")
    delay(1000, output$DRNOKIA_NOKIA_SCALE <- renderUI(""))
  } else {
    print(result)
  }
})
}
retrieveRow <- function(settings, Type, ID, sDate, eDate) {
  con <-
    dbConnect(
      RMariaDB::MariaDB(),
      user = settings$user,
      password = settings$password,
      dbname = 'oauth',
      host = 'ellumenwearablesconnect.cprpowhv3d0j.us-east-2.rds.amazonaws.com',
      port = 3306
    )
  SDate <- strftime(Sys.Date() - 31, "%Y-%m-%d")
  EDate <- strftime(Sys.Date(), "%Y-%m-%d")
  
  if (Type == "Unique") {
    queryString <- paste("SELECT `UniqueID`,`db_date`,`UserID`,`PartnerID`,`DeviceType`, `DeviceString`,`MetricType`,`MetricUnit`,`NoOfObservations`,`AvgValue`,`MinValue`,`MaxValue`",
                         " FROM `Daily_SummaryUniqueID` where UniqueID = '", sep="")
  } else {
    queryString <- paste("SELECT `UserID`,`db_date`,`PartnerID`,`DeviceType`, `DeviceString`,`MetricType`,`MetricUnit`,`NoOfObservations`,`AvgValue`,`MinValue`,`MaxValue`",
                         " FROM `Daily_SummaryPatientID` where UserID= '" , sep="")   
  }
  
  queryString <- paste(
    queryString,
    ID,
    "' and `db_date` between '",
    sDate,
    "' and '",
    eDate,
    "';",
    sep = ""
  )
  res <- dbSendQuery(con, queryString)
  fetchedRows <- data.frame(dbFetch(res))
  dbClearResult(res)
  dbDisconnect(con)
  fetchedRows$MetricType <- as.factor(fetchedRows$MetricType)
  return(fetchedRows)
}
dateRanges <- function(Opt,SDate, EDate) {
  if (Opt == "8") {
    sDate <- checkDate(SDate)
    eDate <- checkDate(EDate)
    if (sDate > eDate) { 
      tDate <- sDate
      sDate <- eDate
      rDate <- tDate
    } else if(sDate == eDate) {
      sDate <- sDate - 1
    }
    SDate <- strftime(sDate, "%Y-%m-%d")
    EDate <- strftime(eDate, "%Y-%m-%d")
  } else {
    EDate <- strftime(Sys.Date(), "%Y-%m-%d")
    if (Opt == "1") {
      SDate <- strftime(Sys.Date() - 1, "%Y-%m-%d")
    }
    else if (Opt == "2") {
      SDate <- strftime(Sys.Date() - 7, "%Y-%m-%d")
    }
    else if (Opt == "3") {
      SDate <-  strftime(Sys.Date() - 31, "%Y-%m-%d")
    }
    else if (Opt == "4") {
      SDate <-  strftime(Sys.Date() - 62, "%Y-%m-%d")
    }
    else if (Opt == "5") {
      SDate <- strftime(Sys.Date() - 183, "%Y-%m-%d")
    }
    else if (Opt == "6") {
      SDate <- strftime(Sys.Date() - 366, "%Y-%m-%d")
    }
    else if (Opt == "7") {
      SDate <- strftime(Sys.Date() - 732, "%Y-%m-%d")
    }
  }
  return(c(SDate,EDate))
}
prefixText <- function(x, y){
  if(y =="") {
    return("")
  }
  return(paste(x,y,sep=""))
}
checkURLString <- function(x, y) {
  if (x[y] == "NULL"){
    return("");
  }
  return(x[y]);
}
checkJsonString <- function(x) {
  if (x == "NULL"){
    return("");
  }
  return(x);
}
httpGET <- function(url, token) {
  httr::GET(url
            , accept("application/json+fhir")
            , content_type("application/json+fhir")
            , add_headers(Authorization = paste("Bearer",token)))
}
httpPOST <- function(url, token, pbody) {
  httr::POST(url
            , accept("application/json+fhir")
            , content_type("application/json+fhir")
            , add_headers(Authorization = paste("Bearer",token))
            , body = upload_file(pbody)
           # , verbose(TRUE)
            )
}
writeDocumentReference <- function(fileType, patientId, titlePrefix, encounter, sdate, edate, comments) {
  fileName <- paste(fileType, patientId, sdate,edate, sep="")
  #print(fileName)
  header = paste(titlePrefix," Patient: ", patientId, "<br />", "From ", sdate, " to ", edate,sep="")
  title = paste(titlePrefix," Patient: ", patientId, " from ", sdate, " to ", edate,sep="")
  fileNamePNG <- paste("Data/P", fileName, ".png", sep="")
  # print(fileNamePNG)
  graphicImage <- base64Encode(readBin(fileNamePNG, "raw", file.info(fileNamePNG)[1, "size"]))
  outer <- read_file("_templateEmbeddedDocumentReference.txt")
  outer <- sub("\\{PATIENT\\}", patientId, outer) 
  outer <- sub("\\{DATETIME\\}", formatInstant(Sys.Date(), Sys.time(), Sys.timezone()),  outer) 
  outer <- sub("\\{ENCOUNTER\\}", encounter,  outer) 
  outer <- sub("\\{TITLE\\}", title,  outer)
  inner <- read_file("_templateDocumentReferenceXHMTL.txt")
  inner <- sub("\\{TITLE\\}", title,  inner)
  inner <- sub("\\{HEADER\\}", header,  inner)
  inner <- sub("\\{COMMENTS\\}", cleanFun(comments), inner)
  inner <- sub("\\{DATA\\}", graphicImage,  inner)
  write_file(inner, "log_file_inner.txt")
  inner <- base64Encode(inner)
  outer <- sub("\\{DATA\\}", inner,  outer)
  #write_file(outer, "log_file_outer.txt")
  fileNameBody <- paste("Data/B", fileName, ".json", sep="")
  write_file( outer, fileNameBody)
  return(fileNameBody)
}
formatInstant <- function(d,t,z) {
  D <- strftime(d, "%Y-%m-%d")
  T <- strftime(t, "%H:%M:%S")
  return(paste(D,"T", T,"Z",sep=""))
}
getDocumentReference <- function(url, patient, token) {
  httpDocumentReference <- httpGET(paste(url,
                                   "/DocumentReference/$docref?patient="
                                   , patient
                                   , "&type=http://loinc.org|11506-3"
                                   , sep="")
                             , token)
  repeat  {
    nextURL <- ""
    jsonDocumentReference <- content(httpDocumentReference, as="parsed", type="application/json")
    if (jsonDocumentReference$resourceType !="Bundle") {
      jsonDocumentReferences <- jsonDocumentReference
    } else {     
      jsonDocumentReferences <-  jsonDocumentReference$entry
      for (i in 1:length( jsonDocumentReference$link)) {
        if ( jsonDocumentReference$link[[i]]$relation == "next"){
          nextURL <-  jsonDocumentReference$link[[i]]$url
        }
      }
    }
    for (i in 1:length( jsonDocumentReferences)) {
      if (!is.null(jsonDocumentReferences[[i]]$resource$content$attachment[[1]]$data)) {
        fileNameHTML <- paste(patient,strftime(Sys.time(), "%Y%m%d%H%M%S"),".html",sep="")
        write_file(jsonDocumentReferences[[i]]$resource$content$attachment[[1]]$data, fileNameHTML, append=TRUE )
      }
    }
    if (nextURL == "") break
    httpDocumentReference <- httpGET(nextURL, token)
  }
}
writeGraphicFile <- function(settings, fileName, patient,partnerid,devicetype){
  fileNamePNG <- paste("P", fileName, ".png", sep="")
#  print(fileNamePNG
  f <- paste("Data/", fileNamePNG, sep="")
  gI <- base64Encode(readBin(f, "raw", file.info(f)[1, "size"]))
  
  template <- read_file("_templateEmbeddedGraphic.txt")
  template <- sub("\\{ID\\}", patient,  template) 
  template <- sub("\\{DATE", strftime(Sys.Date(), "%Y-%m-%d"),  template) 
  template <- sub("TIME\\}", strftime(Sys.time(), " %H:%M:%S"),  template) 
  template <- sub("\\{PATIENT\\}", patient ,  template) 
  template <- sub("\\{PRACTITIONER\\}", partnerid,  template) 
  template <- sub("\\{DEVICETYPE\\}", devicetype,  template) 
  template <- sub("\\{GRAPHICIMAGE\\}", gI,  template)
  
  fileNameJSON <- paste("G", fileName,".json",sep="")
#  print(fileNameJSON)
  fileConn<-file(paste("Data/", fileNameJSON, sep=""))
  writeLines(template, fileConn)
  close(fileConn) 
  # system2("aws", args ="s3 cp Test.png s3://ellumenwearablesconnectdata/Data/ --acl public-read")
  put_object(file = paste("Data/", fileNameJSON, sep="")
             , object = fileNameJSON
             , bucket = "ellumenwearablesconnectdata"
             , acl = "public-read"
             #, folder = "Data"
             , region = "us-east-2"
             , key = settings$key
             , secret = settings$secret
  )
  
  # system2("aws", args ="s3 cp Test.png s3://ellumenwearablesconnectdata/Data/ --acl public-read")
  put_object(file = paste("Data/", fileNamePNG, sep="")
             , object = fileNamePNG
             , bucket = "ellumenwearablesconnectdata"
             , acl = "public-read"
             #, folder = "Data"
             , region = "us-east-2"
             , key = settings$key
             , secret = settings$secret
  )
  jsonLink <- paste('https://s3.us-east-2.amazonaws.com/ellumenwearablesconnectdata/',
                    fileNameJSON,
                    sep="")    
  graphicLink <- paste('https://s3.us-east-2.amazonaws.com/ellumenwearablesconnectdata/',
                       fileNamePNG,
                       sep="")
  return(list(jsonLink, graphicLink, f))  
}
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
checkDate <- function(text){
  d <- try( as.Date( text, format= "%Y-%m-%d" ) )
  if( class( d ) == "try-error" || is.na( d ) ) { 
    d <- Sys.Date()
  }
  return (d)
}
shinyApp(ui = ui, server = server)
