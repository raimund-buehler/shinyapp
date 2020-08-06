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
      
      menuItem("Publication Bias", tabName = "PB", icon = icon("bolt"),
               menuSubItem("Begg & Mazumdar's Rank Test", tabName = "B_M"),
               menuSubItem("Sterne & Egger's Regression", tabName = "S_E"),
               menuSubItem("Trim-and-Fill", tabName = "trif"),
               menuSubItem("p-curve", tabName = "pcurve"),
               menuSubItem("p-uniform and p-uniform*", tabName = "puni"),
               menuSubItem("Selection Models", tabName = "SelMod"),
               menuSubItem("Test of Excess Significance", tabName = "TES"),
               menuSubItem("Summary", tabName = "pubsum"),
               menuSubItem("Summary Bias Analyses", tabName = "pubbias_summary")
               ),
      menuItem("Report & Downloads", tabName = "dwn", icon = icon("download"))
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
                       
                       materialSwitch(inputId = "descendingforest",
                                     label = "Descending Order",
                                     value = FALSE,
                                     status = "success"),
                       sliderInput("forestheight", label = "Please choose plot height (px)",
                                   min = 100,
                                   max = 2000,
                                   value = 800)

                       )
                
              )
      ),
      
      # ** Funnel Plot ----
      tabItem(tabName = "funnel", 
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput("normal_funnel"), 
                            plotOutput("sunset_funnel"))
              ),
              
              materialSwitch(inputId = "choice_trimfill",
                            label = "Show Studies Imputed by Trim-and-Fill",
                            value = FALSE,
                            status = "success"),
              materialSwitch(inputId = "choice_egger",
                            label = "Show Egger's Regression Line",
                            value = FALSE,
                            status = "success")
                
              
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
              plotOutput("meta_sens")
      ),
      
      # ** Subgroup Analysis ----
      tabItem(tabName = "MoA",
              h3("Subgroup Analysis With One Categorical Moderator"),
              
              uiOutput("select_catmod"),
              uiOutput("select_ref_mod"),
              materialSwitch(inputId = "mod_intrcpt", 
                            label = "Model with Intercept",
                            value = FALSE,
                            status = "success"),
              materialSwitch("knha_mod", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE,
                            status = "success"),
              verbatimTextOutput("mod_res"),
              plotOutput("plot_subgroup")
      ),
      
      
      # ** Meta-Regression ----
      tabItem(tabName = "metareg",
              h3("Meta-Regression"),
              uiOutput("select_reg_mod"),
              materialSwitch("knha_reg", 
                            label = "Knapp and Hartung Adjustment",
                            value = FALSE,
                            status = "success"),
              verbatimTextOutput("meta_reg")
      ),
      ## ** Pubbias ----
      tabItem(tabName = "B_M", fluidRow(infoBoxOutput("BMhelp"), valueBoxOutput("BMtau"), valueBoxOutput("BMp")), textOutput("BMref")),
      tabItem(tabName = "S_E", fluidRow(infoBoxOutput("SEhelp"), valueBoxOutput("SEz"), valueBoxOutput("SEp"), box(title = "Model Results", tableOutput("SEmodel"))), textOutput("SEref")),
      tabItem(tabName = "trif", fluidRow(infoBoxOutput("TRFIhelp"), valueBoxOutput("TRFIk0"), valueBoxOutput("TRFIside"), valueBoxOutput("TRFIest"), box(title = "Model Results", tableOutput("TRFImodel"))), textOutput("TRFIref")),
      
      # pcurve ----
      tabItem(tabName = "pcurve", 
              fluidRow(
              column(width = 7,
                     box(title = "pcurve Plot", collapsible = TRUE, collapsed = TRUE, width = 12,
              plotOutput("pcurve_plot", height = "600px"),
             # downloadButton("dwn_pcurve_plot", label = "Download Plot")
              )),
              column(width = 5,
                     box(title = "Input for pcurve Web-App", collapsible = TRUE,
                         collapsed = TRUE, width = 12,
                     verbatimTextOutput("pcurve_input"))
                     )
              ),
              fluidRow(
                h3("Results of Binomial and Continuous Tests", align = "center"),
                tableOutput("pcurve_table"),
                h3("Statistical Power", align = "center"),
                textOutput("pcurve_power"),
                textOutput("pcurve_power_ci"))
              ),
      tabItem(tabName = "puni", tableOutput("p_uni"), verbatimTextOutput("p_uni_star")),
      tabItem(tabName = "SelMod", verbatimTextOutput("modone"), verbatimTextOutput("sevone"), verbatimTextOutput("modtwo"), verbatimTextOutput("sevtwo")),
      tabItem(tabName = "TES", verbatimTextOutput("TestOfExc")),

      tabItem(tabName = "pubsum", uiOutput("pubboxes1"), uiOutput("pubboxes2"), uiOutput("pubboxes3")),
      
      # ** Summary Page ----
      tabItem(tabName = "pubbias_summary",
              fluidRow(
                box(title = "Thresholds", width = 12, collapsible = TRUE, collapsed = TRUE,
                    h4("Thresholds based on p-values", align = "center"),
                    fluidRow(
                      box(textInput("BM_p", label = "Begg & Mazumdar", value = "0.10"), width = 4),
                      box(textInput("SE_p", label = "Sterne & Egger", value = "0.10"), width = 4),
                      box(textInput("PETPEESE_p", label = "PET-PEESE", value = "0.10"), width = 4),
                    ),
                    fluidRow(
                      box(textInput("TES_p", label = "Test of Excess Significance", value = "0.10"), width = 4),
                      box(textInput("puni_p", label = "Puniform", value = "0.05"), width = 4),
                      box(textInput("punistar_p", label = "Puniform*", value = "0.05"), width = 4)
                    ),
                    h4("Thresholds based on discrepancy between summary effect and adjusted estimate", 
                       align = "center"),
                    fluidRow(
                      box(textInput("tf_adj", label = "Trim-and-Fill", value = "0.20"), width = 6),
                      box(textInput("sel_adj", label = "Selection Models", value = "0.20"), width = 6)
                    ))
                
              ),
              fluidRow(
                h3("Small Study Effects", align = "center"),
                box("Begg & Mazumdar", width = 3, collapsible = TRUE),
                box("Sterne & Egger", width = 3, collapsible = TRUE),
                box("PET-PEESE", width = 3, collapsible = TRUE),
                box("Trim-and-Fill", width = 3, collapsible = TRUE)
              ),
              fluidRow(
                h3("p-value Based Methods", align = "center"),
                box("pcurve", width = 4, collapsible = TRUE),
                box("puniform", width = 4, collapsible = TRUE),
                box("puniform*", width = 4, collapsible = TRUE)
              ),
              fluidRow(
                h3("Other Methods", align = "center"),
                box("Test of Excess Significance", width = 6, collapsible = TRUE),
                box("Selection Models (Vevea & Woods)", width = 6, collapsible = TRUE)
              )
              ),
      
      # ** Download Page ----
      tabItem("dwn",
              br(),
              fluidRow(
              # h2("Report", align = "center"),
              column(width = 5, offset = 4,
              downloadBttn("dwn_report", label = "Download Full Report", 
                           style = "stretch", size = "lg")
              ),
              br(),
              ),
              fluidRow(
                h4("Download Single Results, Plots, and R-Objects", align = "center"),
                br(),
                
                # **** Dwn: Data ----
                box(title = "Data (Including Converted Effect Sizes)", collapsible = TRUE, collapsed = TRUE, width = 6,
                                    prettyRadioButtons(
                                      inputId = "dwn_select_filetype",
                                      label = "Choose Output Format:", 
                                      choices = c(".sav", ".rds"),
                                      selected = ".sav",
                                      icon = icon("check"), 
                                      bigger = TRUE,
                                      # status = "info",
                                      animation = "jelly"
                                    ),
                    downloadBttn("dwn_data",
                                 label = "Download Data",
                                 block = TRUE,
                                 style = "stretch")
                    
                                    
                           ),
                
                # **** Dwn: Meta ----
                box(title = "Meta-Analysis", collapsible = TRUE, collapsed = TRUE, width = 6,
                    tabBox(id = "dwnbx_meta", width = 12, side = "right", selected = "Meta-Analysis",
                           tabPanel("Meta-Regression",
                                    downloadBttn(outputId = "dwn_metareg_res",
                                                 label = "Download Results as Text (.txt)",
                                                 style = "stretch",
                                                 block = TRUE,
                                                 size = "s"),
                                    downloadBttn(outputId = "dwn_metareg_res_obj",
                                                 label = "Download R-Object with Results (.rds)",
                                                 style = "stretch",
                                                 block = TRUE,
                                                 size = "s")
                                    ),
                           tabPanel("Subgroup Analysis",
                                    downloadBttn(outputId = "dwn_sbgrp_res",
                                                 label = "Download Results as Text (.txt)",
                                                 style = "stretch",
                                                 block = TRUE,
                                                 size = "s"),
                                    downloadBttn(outputId = "dwn_sbgrp_res_obj",
                                                 label = "Download R-Object with Results (.rds)",
                                                 style = "stretch",
                                                 block = TRUE,
                                                 size = "s")
                           ),
                           tabPanel("Meta-Analysis",
                                    downloadBttn(outputId = "dwn_meta_res",
                                                 label = "Download Results as Text (.txt)",
                                                 style = "stretch",
                                                 block = TRUE,
                                                 size = "s"),
                                    downloadBttn(outputId = "dwn_meta_res_obj",
                                                   label = "Download R-Object with Results (.rds)",
                                                  style = "stretch",
                                                 block = TRUE,
                                                 size = "s")
                           )))
              ),
              
              # **** Dwn: Plots ----
              fluidRow(
                box(title = "Plots", 
                    collapsible = TRUE, collapsed = TRUE, width = 6,
                 tabBox(id = "dwnbx_plots", width = 12, side = "right", selected = "Forest Plots",
                        tabPanel("Sensitivity Analysis (Estimator)",
                                 box(width = 12,
                                     dropdown(
                                       h6("Preview"),
                                       plotOutput("sens_example"),
                                       style = "stretch", icon = icon("search"),
                                       status = "primary", 
                                       width = "500px",
                                       animate = animateOptions(
                                         enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig),
                                       tooltip = tooltipOptions(title = "Click to see preview")
                                     ),
                                     
                                     downloadBttn("dwn_sens",
                                                  label = "Download Plot (.png)",
                                                  size = "s",
                                                  style = "stretch",
                                                  block = TRUE))),
                        tabPanel("Subgroup Analysis",
                                 dropdown(
                                   h6("Preview"),
                                   plotOutput("sbgrp_example"),
                                   style = "stretch", icon = icon("search"),
                                   status = "primary", 
                                   width = "500px",
                                   animate = animateOptions(
                                     enter = animations$fading_entrances$fadeInLeftBig,
                                     exit = animations$fading_exits$fadeOutRightBig),
                                   tooltip = tooltipOptions(title = "Click to see preview")
                                 ),
                                 
                                 downloadBttn("dwn_sbgrp",
                                              label = "Download Plot (.png)",
                                              size = "s",
                                              style = "stretch",
                                              block = TRUE)),
                 
                        tabPanel("Funnel Plots",
                                 box(width = 6,
                                          dropdown(
                                            h6("Preview"),
                                            plotOutput("funnel_example"),
                                            style = "stretch", icon = icon("search"),
                                            status = "primary", 
                                            width = "500px",
                                            animate = animateOptions(
                                              enter = animations$fading_entrances$fadeInLeftBig,
                                              exit = animations$fading_exits$fadeOutRightBig),
                                            tooltip = tooltipOptions(title = "Click to see preview")
                                          ),
                                        
                                        downloadBttn("dwn_funnel",
                                                      label = "Download Funnel Plot",
                                                     size = "xs",
                                                     style = "stretch",
                                                     block = TRUE)
                                        ),
                                 box(width = 6,
                                        dropdown(
                                          h6("Preview"),
                                          plotOutput("funnelsunset_example"),
                                          style = "stretch", icon = icon("search"),
                                          status = "primary", 
                                          width = "500px",
                                          animate = animateOptions(
                                            enter = animations$fading_entrances$fadeInLeftBig,
                                            exit = animations$fading_exits$fadeOutRightBig),
                                          tooltip = tooltipOptions(title = "Click to see preview")
                                        ),
                                        
                                        downloadBttn("dwn_funnelsunset",
                                                      label = "Download Sunset Funnel Plot",
                                                     size = "xs",
                                                     style = "stretch",
                                                     block = TRUE)
                                 )
                        ),

                        tabPanel("Forest Plots",
                                 dropdown(
                                   h6("Preview"),
                                   plotOutput("forest_example"),
                                   style = "stretch", icon = icon("search"),
                                   status = "primary", 
                                   width = "500px",
                                   animate = animateOptions(
                                     enter = animations$fading_entrances$fadeInLeftBig,
                                     exit = animations$fading_exits$fadeOutRightBig),
                                   tooltip = tooltipOptions(title = "Click to see preview")
                                   ),
                                 downloadBttn(outputId = "dwn_forest",
                                              label = "Download Forest Plot (.png)",
                                              size = "s",
                                              style = "stretch",
                                              block = TRUE)
                                 ))),
                
                # **** Dwn: Pubbias ----
                box(title = "Publication Bias Analyses", 
                    collapsible = TRUE, collapsed = TRUE, width = 6,
                 tabBox(id = "dwnbx_pubbias", width = 12, side = "right",
                        tabPanel("Small Study Effects",
                                 prettyRadioButtons(
                                   inputId = "dwn_select_filetype_sse",
                                   label = "Choose Output Format:", 
                                   choices = c("Results (.txt)", "R-Object (.rds)"),
                                   selected = "Results (.txt)",
                                   icon = icon("check"), 
                                   bigger = TRUE,
                                   # status = "info",
                                   animation = "jelly"
                                 ),
                                 downloadBttn("dwn_bm", "Download Results Begg & Mazumdar",
                                              style = "stretch",
                                              size = "s",
                                              block = TRUE),
                                 downloadBttn("dwn_se", "Download Results Sterne & Egger",
                                              style = "stretch",
                                              size = "s",
                                              block = TRUE),
                                 downloadBttn("dwn_petpeese", "Download Results PET-PEESE",
                                              style = "stretch",
                                              size = "s",
                                              block = TRUE),
                                 downloadBttn("dwn_tf", "Download Results Trim-and-Fill",
                                              style = "stretch",
                                              size = "s",
                                              block = TRUE),
                                 ),
                        tabPanel("p-value Based Methods",
                                 box(width = 6,
                                   h6("p-curve"),
                                   dropdown(
                                     h6("Preview"),
                                     plotOutput("pcurve_example"),
                                     style = "stretch", icon = icon("search"),
                                     status = "primary", 
                                     width = "500px",
                                     animate = animateOptions(
                                       enter = animations$fading_entrances$fadeInLeftBig,
                                       exit = animations$fading_exits$fadeOutRightBig),
                                     tooltip = tooltipOptions(title = "Click to see preview")
                                   ),
                                   downloadBttn(outputId = "dwn_pcurve_plot",
                                                label = "Download pcurve Plot (.png)",
                                                size = "s",
                                                style = "stretch",
                                                block = TRUE)
                                   
                                   
                                   
                                 ),
                                 box(width = 6,
                                     h6("puniform and puniform*")
                                     )
                                 ),
                        tabPanel("Other Methods")))
              ),
              fluidRow(
                
                # **** Dwn: Decline Effect
                box(title = "Decline Effect", collapsible = TRUE, collapsed = TRUE, width = 6,
                    tabBox(id = "dwnbx_decl", width = 12))
              )
              )
    )
  )
)
