
#preselect earliest overall studies in dataset (column "studyname" required)
primary_temp <- reactive({
  # req(input$file)
  req(input$year)
  req(input$id)
  data <- data_reac$DT
  min <- which(na.omit(data[[input$year]]) == min(na.omit(data[[input$year]])))
  data[[input$id]][min]
})

#preselect earliest published studies
# primary_publ_temp <- reactive({
#   data <- data_reac$DT
#   data[pub == "pub", .SD[year == min(year)]]$studyname
# })

#Let user choose the earliest study, if more than one study of same (earliest) year is found
#if less than one: just confirm the study that was found 
output$primarySelect <- renderUI({
  # req(input$file)
  if(length(primary_temp())>1){
    # if(input$SE == ""){
    box(background = "yellow", width = NULL, height = "140px",      
      radioButtons("primaryChoice", label = "Please select the earliest study in your dataset", choices = c("None selected" = "", primary_temp(), "unsure")))
    # } else {
    #   radioButtons("primaryChoice", label = "Please select the earliest study in your dataset", choices = c("None selected" = "", primary_temp(), "unsure"), selected = tail(input$primaryChoice))
    # }
  } else {
    # if(input$SE == ""){
    box(background = "yellow", width = NULL, height = "140px",  
      radioButtons(inputId = "primaryChoice", label = paste("Is this the earliest study in your dataset:", primary_temp(), "?"), choices = c("None selected" = "", "yes", "no")))
    # } else {
    #   radioButtons(inputId = "primaryChoice", label = paste("Is this the earliest study in your dataset:", primary_temp(), "?"), choices = c("None selected" = "", "yes", "no"), selected = tail(input$primaryChoice))
    # }
  }
})

observe({
  req(input$primaryChoice)
  #updateinput
  updateRadioButtons(session, "primaryChoice")
  if(input$primaryChoice == "yes"){
    para$prim <- primary_temp()
  } else if (input$primaryChoice %in% primary_temp()){
    para$prim <- input$primaryChoice
  }
})

observeEvent(input$id, {
  updateRadioButtons(session,"primaryChoice", selected = "")
  para$prim <- NULL
})

# observeEvent(input$file$name, {
#   updateRadioButtons(session, inputId = "primaryChoice", label = "Please select the earliest study in your dataset", choices = c("None selected" = "", primary_temp(), "unsure"))
# })

#If user selects unsure, checkboxes are displayed of the same preselected studies and user can choose two candidates
output$candidateSelect <- renderUI({
  req(input$primaryChoice)
  if(input$primaryChoice == "unsure"){
    box(background = "yellow", width = NULL, height = "140px",  
    checkboxGroupInput("candidateChoice", label = "Please select the TWO candidate studies that are most likely the earliest studies!", choices = primary_temp()))
  } else if(input$primaryChoice == "no"){
    "We need the earliest study in your dataset. Please evaluate (e.g. by checking web of science) which study is the earliest study in your dataset!\n\n"
  }
})

#Set maximum of checkbox choices for primary candidates to two
observe({
  if(length(input$candidateChoice) > 2){
    updateCheckboxGroupInput(session, inputId = "candidateChoice", selected = tail(input$candidateChoice, 2))
  }
})

#Same thing, but for published studies only
# output$primaryPublSelect <- renderUI({
#   req(input$primaryChoice)
#   if(length(primary_publ_temp())>1){
#     if(input$primaryChoice == ""){
#       radioButtons("primaryPublChoice", label = "Please select the earliest PUBLISHED study in your dataset", choices = c("None selected" = "", primary_publ_temp(), "unsure"))
#     } else {
#       radioButtons("primaryPublChoice", label = "Please select the earliest PUBLISHED study in your dataset", choices = c("None selected" = "", primary_publ_temp(), "unsure"), selected = tail(input$primaryPublChoice))  
#     }  
#   } else {
#     if(input$primaryChoice == ""){
#       radioButtons(inputId = "primaryPublChoice", label = paste("Is this the earliest PUBLISHED study in your dataset:", primary_publ_temp(), "?"), choices = c("None selected" = "", "yes", "no"))
#     } else {
#       radioButtons(inputId = "primaryPublChoice", label = paste("Is this the earliest PUBLISHED study in your dataset:", primary_publ_temp(), "?"), choices = c("None selected" = "", "yes", "no"), selected = tail(input$primaryPublChoice)) 
#     }
#   }
# })
# 
# observeEvent(input$file$name, {
#   updateRadioButtons(session, inputId = "primaryPublChoice", label = "Please select the earliest PUBLISHED study in your dataset", choices = c("None selected" = "", primary_publ_temp(), "unsure"))
# })
# 
# #select candidates
# output$candidatePublSelect <- renderUI({ 
#   req(input$chooseEScol)
#   req(input$primaryPublChoice)
#   if(input$primaryPublChoice == "unsure"){
#     checkboxGroupInput("candidatePublChoice", label = "Please select the TWO candidate PUBLISHED studies that are most likely the earliest studies!", choices = primary_publ_temp())
#   }
# })
# 
# #limit amount of checked boxes to two
# observe({
#   if(length(input$candidatePublChoice) > 2){
#     updateCheckboxGroupInput(session, inputId = "candidatePublChoice", selected= tail(input$candidatePublChoice, 2))
#   }
# })