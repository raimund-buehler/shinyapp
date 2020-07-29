# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Decline Effects App"),
  
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "file", icon = icon("table")),
      
      
      menuItem("Meta-Analysis", tabName = "MA", selected = TRUE,
               menuSubItem("Meta-Analysis", tabName = "MAsub"),
               menuSubItem("Subgroup Analysis", tabName = "MoA"),
               menuSubItem("Meta-Regression", tabName = "metareg"), icon = icon("calculator")),
      
      menuItem("Plots", tabName = "plots",
               menuSubItem("Forest Plot", tabName = "forest"),
               menuSubItem("Funnel Plot", tabName = "funnel"), icon = icon("chart-area")),
      
      menuItem("Publication Bias", tabName = "PB",
               menuSubItem("Begg & Mazumdar's Rank Test", tabName = "B_M"),
               menuSubItem("Sterne & Egger's Regression", tabName = "S_E"),
               menuSubItem("Trim-and-Fill", tabName = "trif"),
               menuSubItem("p-curve", tabName = "pcurve"),
               menuSubItem("p-uniform and p-uniform*", tabName = "puni"),
               menuSubItem("Selection Models", tabName = "SelMod"),
               menuSubItem("Test of Excess Significance", tabName = "TES"), icon = icon("bolt"))
      
    )
  ),
  
  # Body ----
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "font_correction.css")),
    tabItems(
      
      # ** About ----
      tabItem(tabName = "about",
              h2("ABOUT PAGE")),
      
      # ** Data input ----
      tabItem(tabName = "file",
              fluidRow(
                column(width = 4,
                       box(
                         width = NULL,
                         fileInput(inputId = "file", label = "Please select a .sav file", accept = ".sav", placeholder = "No file selected")
                       ),
                       uiOutput("choices")
                ),
                column(width = 8,
                       uiOutput("table")
                )
              ),
              fluidRow(
                downloadButton(outputId = "dwn_dat", label = "Download Data With Converted Effect Sizes")
              )
              
      ),
      
      # ** Forest Plot ----
      tabItem(tabName = "forest", 
              fluidRow(
                column(7, align = "center",
                       # Action Buttons for normal and cumulative forest plot
                       actionButton(inputId = "normalforest",
                                    label = "Standard"),
                       actionButton(inputId = "cumulforest",
                                    label = "Cumulative")
                       
                )),
              
              # plot forest plot
              fluidRow(
                column(7,
                       uiOutput("forest")),
                column(5,
                       # select forest plot variant
                       selectInput(inputId = "forestvariant",
                                   label = "Select Forest Plot Variant",
                                   choices = c("Classic Forest Plot", "Thick Forest Plot", "Rainforest Plot"),
                                   selected = "classic"),
                       
                       selectInput(inputId = "forestcol",
                                   label = "Select Colors",
                                   choices = c("Blues", "Greys", "Oranges", "Greens",
                                               "Reds", "Purples")),
                       
                       # select variable to sort forest plot
                       uiOutput("sortingvar"),
                       
                       checkboxInput(inputId = "descendingforest",
                                     label = "Descending Order"),
                       sliderInput("forestheight", label = "Please choose plot height (px)",
                                   min = 100,
                                   max = 2000,
                                   value = 800)

                       )
                
              ),
              fluidRow(downloadButton("dwn_forest"))),
      
      # ** Funnel Plot ----
      tabItem(tabName = "funnel", 
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput("normal_funnel"), 
                            plotOutput("sunset_funnel"))
              ),
              
              checkboxInput(inputId = "choice_trimfill",
                            label = "Show Studies Imputed by Trim-and-Fill"),
              checkboxInput(inputId = "choice_egger",
                            label = "Show Egger's Regression Line"),
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            downloadButton("dwn_funnel",
                                           label = "Download Funnel Plot"),
                            downloadButton("dwn_funnelsunset",
                                           label = "Download Sunset Funnel Plot"
                            )
                )
              )
      ),
      
      # ** Meta-Analysis ----
      tabItem(tabName = "MAsub", 
              radioButtons(inputId = "metamodel",
                           label = "Select Meta-Analytic Model",
                           choiceNames = c("Fixed-effect Model", "Random-effects Model"),
                           choiceValues = c("fe", "re"),
                           selected = "re"),
              uiOutput("select_re_type"),
              h3("Results"),
              p(),
              h4("Model Type"),
              textOutput("meta_out_1"),
              p(),
              h4("Summary Effect"),
              textOutput("meta_out_2"),
              p(),
              h4("Heterogeneity Statistics"),
              tableOutput("meta_out_3"),
              p(),
              h4("Test for Heterogeneity"),
              textOutput("meta_out_4"),
              p(),
              verbatimTextOutput("meta_res"),
              downloadButton(outputId = "dwn_meta_res",
                             label = "Download Results"),
              downloadButton(outputId = "dwn_meta_res_obj",
                             label = "Download R-Object with Results"),
              plotOutput("meta_sens")
      ),
      
      # ** Subgroup Analysis ----
      tabItem(tabName = "MoA",
              h3("Subgroup Analysis With One Categorical Moderator"),
              
              uiOutput("select_catmod"),
              uiOutput("select_ref_mod"),
              checkboxInput(inputId = "mod_intrcpt", 
                            label = "Model with Intercept",
                            value = FALSE),
              checkboxInput("knha_mod", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE),
              verbatimTextOutput("mod_res"),
              plotOutput("plot_subgroup")
      ),
      
      
      # ** Meta-Regression ----
      tabItem(tabName = "metareg",
              h3("Meta-Regression"),
              uiOutput("select_reg_mod"),
              checkboxInput("knha_reg", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE),
              verbatimTextOutput("meta_reg")
      ),
      
      tabItem(tabName = "B_M", verbatimTextOutput("BM")),
      tabItem(tabName = "S_E", verbatimTextOutput("SterneEgger")),
      tabItem(tabName = "trif", verbatimTextOutput("TRFI")),
      tabItem(tabName = "pcurve", verbatimTextOutput("pcur")),
      tabItem(tabName = "puni", verbatimTextOutput("p_uni"), verbatimTextOutput("p_uni_star")),
      tabItem(tabName = "SelMod", verbatimTextOutput("modone"), verbatimTextOutput("sevone"), verbatimTextOutput("modtwo"), verbatimTextOutput("sevtwo")),
      tabItem(tabName = "TES", verbatimTextOutput("TestOfExc"))
      
    )
  )
)
