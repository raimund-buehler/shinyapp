## READ DATA FILE and coerce to data.table

data_reac <- reactiveValues()
observeEvent(input$file, {
  req(input$file)
  data_reac$DT <- as.data.table(read.spss(input$file$datapath))
})

#CHECK COLNAMES
repcols <- reactiveValues()
observeEvent(input$file, {
  repcols$DT <- {
    req(input$file)
    data <- data_reac$DT
    source(here("check_colnames_shiny.R"), local = TRUE)
    report.colnames
  }
})

output$checkCols <- renderPrint({
  ranktest(rma$res)
})


##RENDER DATA TABLE
output$table <- renderUI({
  req(input$file)
  box(title = "Datafile", width = NULL, dataTableOutput("DTable"))
}) 

output$DTable <- DT::renderDataTable(data_reac$DT,
                                     options = list(pageLength = 15, info = FALSE, lengthMenu = list(c(15,-1), c("15","All")))
)

##RENDER USER MENU FOR DATAFILE
output$choices <- renderUI({
  req(input$file)
  box(title = "Input",
      width = NULL,
      uiOutput("EStype"),
      uiOutput("EScolumn"),
      uiOutput("SEcolumn"),
      uiOutput("Year"),
      uiOutput("SampleSize"),
      uiOutput("StudyID"),
      uiOutput("PubStatus"),
      uiOutput("PubValuePub"),
      uiOutput("PubValueUnpub"),
      
      uiOutput("primarySelect"),
      uiOutput("candidateSelect"),
      uiOutput("primaryPublSelect"),
      uiOutput("candidatePublSelect"),
      fluidRow(
        column(width = 12, align = 'center',
               actionButton("SubmitButton", "Submit")))
  )
  
})