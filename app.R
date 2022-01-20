## Necessary libraries
library(readr)
library(shiny)
library(stringr)
library(shinyjs)
library(rmarkdown)
library(here)
library(googlesheets4)
library(tidyverse)
paperURL <- a("Link to Full Text", href="https://doi.org/10.1038/s41592-021-01156-w")
templateURL <- a("Download metadata template (remember to save as CSV)", href="https://docs.google.com/spreadsheets/d/e/2PACX-1vSgQL01kj0UDk_TTJn6ZyGVY3SBqFEEDxTvkVMYhmtCJwO_be3uYitwlphHip3cb_Q2dfvhxAM4fQeZ/pub?output=xlsx", target="_blank")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
database <-  as_sheets_id("https://docs.google.com/spreadsheets/d/1hxw2bsIQFB9pn555de7FbTlE8j4Smc7kT8NrAfy-q_8/edit?usp=sharing")
sheet_list <- sheet_names(database)
#check typed input vs. cores available. IF new, state that a new sheet will be created
#check text for correctness and completeness
checkLetters <- function(myStr) grepl("^(?:[A-Za-z]+)(?:[A-Za-z0-9 _]*)$", myStr)
checkReq <- function(df){
  a <- "Metadata_category" %in% colnames(df)
  b <- "Instructions" %in% colnames(df)
  c <- length(colnames(df))>2
  if(a & b & c){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

ui <- fluidPage(
  titlePanel("Microscopy Metadata Checklist Generator (MicCheck) File Uploader"),
  sidebarLayout(
    sidebarPanel(
      helpText("Use this Shiny app to upload metadata for your lab's or core's microscopes for use in the MicCheck app"),
      textInput("myOrg", "University, Institute, or Company", value = "", width = NULL, placeholder = "Name your organization"),
      textInput("myCore", "Microscopy Core or Lab", value = "", width = NULL, placeholder = "Name your core or lab"),
      checkboxGroupInput("warning", ""),
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      templateURL,
      tags$style("#file1_progress { margin-bottom: -10px } #a { margin-top: 0px}"),
      
      tags$hr(),
      tags$head(tags$style("#warning, #warning2, #warning3{color: red;
                                 font-size: 16px;
                                 font-style: italic;
                                 }")
      ),
      actionButton("upload", "Upload Metadata"),
      textOutput("complete"),
      checkboxGroupInput("warning2", ""),
      checkboxGroupInput("warning3", ""),
      helpText("For more information, see our paper: Montero Llopis, P., Senft, R.A., Ross-Elliott, T.J. et al. Best practices and tools for reporting reproducible fluorescence microscopy methods. Nat Methods 18, 1463–1476 (2021)."),
      paperURL,
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  val <- reactiveValues()
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    else{
      val$df <- read.csv(inFile$datapath, header = TRUE)
    }
  })
  observeEvent(req(input$myCore,input$myOrg),{
    val$mySheet <- paste(input$myOrg, input$myCore, sep=" - ")
    warn=""
    if (val$mySheet %in% sheet_list){
      warn1 <- "WARNING: core sheet already exists and will be overwritten upon upload"
    }else{
      warn1 <- ""
    }
    if(nchar(val$mySheet)>50){
      warn2 <- "Error: character limit exceeded."
      val$checkname=FALSE
    }else if(any(checkLetters(c(input$myCore, input$myOrg))==FALSE)){
      warn2 <- "Error: Invalid character used."
      val$checkname=FALSE
    }else if(val$mySheet=="Use default examples"){
      warn2 <- "Error: Invalid name."
      val$checkname=FALSE
    }else{
      warn2 <-""
      val$checkname=TRUE
    }
    updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning", label=paste(warn1, warn2))
  })
  observeEvent(input$file1, {
    checkpass <- checkReq(val$df)
    if(!checkpass){
      updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning3", label="ERROR: Incorrect CSV template")
    }
    val$checkpass <- checkpass
    })
  observeEvent(input$upload, {
    if(is.null(input$myCore) | input$myCore==""| is.null(input$myOrg) | input$myOrg==""){
      updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning2", label="ERROR: Missing org or core/lab ID")
    }else{
      updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning2", label="")
    }
    if(!is.data.frame(val$df) | !val$checkpass){
      updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning3", label="ERROR: Upload a valid csv")
    }else{
      updateCheckboxGroupInput(session=getDefaultReactiveDomain(), "warning3", label="")
    }
    if(!is.null(input$myCore) & input$myCore!="" & !is.null(input$myOrg) & input$myOrg!=""& is.data.frame(val$df) & val$checkpass & val$checkname){
      showModal(modalDialog("Writing to database", footer=NULL))
      sheet_write(val$df, ss = database, sheet = val$mySheet)
      removeModal()
      output$complete <- renderText({ "Upload Successful!" })
    }
  })
}
shinyApp(ui, server)