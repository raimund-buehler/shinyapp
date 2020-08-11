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
                       box(uiOutput("forest"), width = NULL)),
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
                
                            box(plotOutput("normal_funnel", width = "80%", height = "600px")), 
                            box(plotOutput("sunset_funnel", width = "80%", height = "600px"))),
              
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
              fluidRow(
                column(width = 2,
                              h4("Input"),
                              box(width = NULL,
                                radioButtons(inputId = "metamodel",
                                label = "Select Meta-Analytic Model",
                                choiceNames = c("Fixed-effect Model", "Random-effects Model"),
                                choiceValues = c("fe", "re"),
                                selected = "re"),
                                uiOutput("select_re_type"))),
                column(width = 5, 
                              h4("Results"),
                                  box(title = "Model Type", width = NULL, height = "150px",
                                    tableOutput("meta_out_1")),
                                  box(title = "Summary Effect", width = NULL, height = "150px",
                                    tableOutput("meta_out_2"), background = "light-blue")),
                column(width = 5,
                              h4("Heterogeneity Statistics"),
                              box(width = NULL, height = "150px",
                                tableOutput("meta_out_3")),
                              box(title = "Test for Heterogeneity", width = NULL, height = "150px",
                                div(textOutput("meta_out_4"), style = "position: absolute; bottom: 60px")))),
              fluidRow(         box(title = "Sensitivity Analysis", width = 12, plotOutput("meta_sens"))),
              fluidRow(
                                box(title = "Full R Output", verbatimTextOutput("meta_res"), width = 12, collapsible = TRUE, collapsed = TRUE))
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
      ## **** Begg & Mazumdar ----
      tabItem(tabName = "B_M", fluidRow(
        column(width = 3,
               infoBoxOutput("BMhelp", width = NULL)), 
        column(width = 3,
               valueBoxOutput("BMtau", width = NULL), 
               valueBoxOutput("BMp", width = NULL)),
        column(width = 6, 
               box(title = "Funnel Plot", plotOutput("FunnelBM", width = "80%"), width = NULL, align = "center"))
        ), 
        htmlOutput("BMref")
      ),
      ## **** Sterne & Egger ----
      tabItem(tabName = "S_E", fluidRow(
        column(width = 3,
               infoBoxOutput("SEhelp", width = NULL)),
        column(width = 3,
               valueBoxOutput("SEz", width = NULL), 
               valueBoxOutput("SEp", width = NULL)),
        column(width = 6,
               box(title = "Model Results", div(tableOutput("SEmodel"), style = "font-size: 115%"), width = NULL),
               box(title = "Funnel Plot", plotOutput("FunnelSE", width = "80%"), width = NULL, align = "center"))
        ), 
        htmlOutput("SEref")
      ),
      ## **** Trim and Fill ----
      tabItem(tabName = "trif", fluidRow( 
        column(width = 3, 
               infoBoxOutput("TRFIhelp", width = NULL)), 
        column(width = 3, 
               valueBoxOutput("TRFIk0", width = NULL), 
               valueBoxOutput("TRFIside", width = NULL), 
               valueBoxOutput("TRFIest", width = NULL),
               valueBoxOutput("TRFIunadj", width = NULL),
               valueBoxOutput("TRFIperc", width = NULL)),
        column(width = 6, 
               box(title = "Model Results", div(tableOutput("TRFImodel"), style = "font-size: 115%"), width = NULL),
               box(title = "Funnel Plot", plotOutput("FunnelTRFI", width = "80%"), width = NULL, align = "center"))
        ),
        htmlOutput("TRFIref")
      ),
      
      # **** pcurve ----
      tabItem(tabName = "pcurve", 
              fluidRow(
                column(width = 3,
                       infoBoxOutput("pcurveHelp", width = NULL)
                ),
                column(width = 9,
                       box(
                            h3("Results of Binomial and Continuous Tests", align = "center"),
                            tableOutput("pcurve_table"),
                            h3("Statistical Power", align = "center"),
                            tableOutput("pcurve_power"),
                            #textOutput("pcurve_power_ci"),
                            width = NULL
                            )
                       )
                ), 
              fluidRow(
                column(width = 7,
                       box(title = "pcurve Plot", collapsible = TRUE, collapsed = TRUE, width = NULL,
                           plotOutput("pcurve_plot", height = "600px"),
                           # downloadButton("dwn_pcurve_plot", label = "Download Plot")
                       )),
                column(width = 5,
                       box(title = "Input for pcurve Web-App", collapsible = TRUE,
                           collapsed = TRUE, width = NULL,
                           verbatimTextOutput("pcurve_input"))
                )
              )
              ),
      ## **** puniform ----
      tabItem(tabName = "puni", 
            fluidRow(
                  column(width = 4,
                         box(title = "P-uniform and P-uniform*", textOutput("puniHelp"), width = NULL)),
                  column(width = 4,
                         box(title = "P-uniform",
                              column(width = 8, offset = 2,
                                h4("Publication Bias Test", align = "center"), br(),
                                valueBoxOutput("puni_L.pb", width = NULL),
                                valueBoxOutput("puni_pval.pb", width = NULL), br(),
                                h4("Adjusted effect size", align = "center"),
                                div(tableOutput("p_uni_est"), style = "font-size: 115%"),
                                h4("Unadjusted effect size", align = "center"),
                                div(tableOutput("puni_est_fe"), style = "font-size: 115%")
                           ), width = NULL, align = "center"
                         )
                  ),
                  column(width = 4, 
                         box(title = "P-uniform*",
                             column(width = 8, offset = 2,
                                    h4("Publication Bias Test", align = "center"), br(),
                                    valueBoxOutput("puni_star_L.pb", width = NULL),
                                    valueBoxOutput("puni_star_pval.pb", width = NULL), br(),
                                    h4("Adjusted effect size", align = "center"),
                                    div(tableOutput("puni_star_est"), style = "font-size: 115%"),
                                    h4("Unadjusted effect size", align = "center"),
                                    div(tableOutput("puni_star_est_fe"), style = "font-size: 115%")
                             ), width = NULL, align = "center"
                         )
                  )
            ), htmlOutput("puniref")
      ),
      ## **** Selection Models ----
      tabItem(tabName = "SelMod", fluidRow(
              column(width = 4,
              box(title = "Vevea and Woods Selection Model", textOutput("SelHelp"), width = NULL)
              ),
              column(width = 8,
                     fluidRow(
                box(width = 6, title = "Moderate Model, One-Tailed", valueBoxOutput("modone"), valueBoxOutput("modone_unadj"), valueBoxOutput("modone_perc")),
                box(width = 6, title = "Severe Model, One-Tailed", valueBoxOutput("sevone"), valueBoxOutput("sevone_unadj"), valueBoxOutput("sevone_perc")),
                box(width = 6, title = "Moderate Model, Two-Tailed", valueBoxOutput("modtwo"), valueBoxOutput("modtwo_unadj"), valueBoxOutput("modtwo_perc")),
                box(width = 6, title = "Severe Model, Two-Tailed", valueBoxOutput("sevtwo"), valueBoxOutput("sevtwo_unadj"), valueBoxOutput("sevtwo_perc"))
              )))),
      tabItem(tabName = "TES", verbatimTextOutput("TestOfExc")),

      tabItem(tabName = "pubsum", uiOutput("pubboxes1"), uiOutput("pubboxes2"), uiOutput("pubboxes3")),
      
      # **** Summary Page ----
      tabItem(tabName = "pubbias_summary",
              fluidRow(
                box(title = "Thresholds", width = 12, collapsible = TRUE, collapsed = TRUE,
                    h4("Thresholds based on p-values", align = "center"),
                    fluidRow(
                      box(textInput("BM_p", label = "Begg & Mazumdar", value = "0.10"), width = 4),
                      box(textInput("SE_p", label = "Sterne & Egger", value = "0.10"), width = 4),
                      box(textInput("pcurve_p", label = "P-Curve", value = "0.05"), width = 4)
                      #box(textInput("PETPEESE_p", label = "PET-PEESE", value = "0.10"), width = 4),
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
                      box(textInput("sel_adj", label = "Selection Model", value = "0.20"), width = 6)
                    ))
                
              ),
              fluidRow(
                h3("Small Study Effects", align = "center"),
                box(column(width = 4, offset = 4, valueBoxOutput("BMsum", width = NULL)), title = "Begg & Mazumdar", width = 4, collapsible = TRUE),
                box(valueBoxOutput("SEsum", width = 12), title = "Sterne & Egger", width = 4, collapsible = TRUE),
                #box("PET-PEESE", width = 3, collapsible = TRUE),
                box(valueBoxOutput("TRFIsum", width = 12), title = "Trim-and-Fill", width = 4, collapsible = TRUE)
              ),
              fluidRow(
                h3("p-value Based Methods", align = "center"),
                box(title = "P-curve", width = 4, collapsible = TRUE,
                    fluidRow(
                      h4("Studies contain evidential value:", align = "center"),
                      valueBoxOutput("pcurvebinsum"),
                      valueBoxOutput("pcurvefullsum"),
                      valueBoxOutput("pcurvehalfsum")
                    ),
                    fluidRow(
                      h4("Studies evidential value is inadequate", align = "center"),
                      valueBoxOutput("pcurvebinsum33"),
                      valueBoxOutput("pcurvefullsum33"),
                      valueBoxOutput("pcurvehalfsum33")
                    )
                ),
                box(title = "P-uniform", br(), br(), valueBoxOutput("punisum", width = 12), width = 4, collapsible = TRUE),
                box(valueBoxOutput("punistar_sum", width = 12), width = 4, collapsible = TRUE)
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
                                                block = TRUE),
                                   downloadBttn(outputId = "dwn_pcurve_inputstring",
                                                label = "Download Input String for WebApp",
                                                size = "s",
                                                style = "stretch",
                                                block = TRUE),
                                   downloadBttn(outputId = "dwn_pcurve_res",
                                                label = "Download p-curve Results",
                                                size = "s",
                                                style = "stretch",
                                                block = TRUE)
                                   
                                   
                                   
                                 ),
                                 box(width = 6,
                                     h6("puniform and puniform*"),
                                     prettyRadioButtons(
                                       inputId = "dwn_select_filetype_puni",
                                       label = "Choose Output Format:", 
                                       choices = c("Results (.txt)", "R-Object (.rds)"),
                                       selected = "Results (.txt)",
                                       icon = icon("check"), 
                                       bigger = TRUE,
                                       # status = "info",
                                       animation = "jelly"
                                     ),
                                     downloadBttn(outputId = "dwn_puni_res",
                                                  label = "Download p-uniform Results",
                                                  size = "s",
                                                  style = "stretch",
                                                  block = TRUE),
                                     downloadBttn(outputId = "dwn_punistar_res",
                                                  label = "Download p-uniform* Results",
                                                  size = "s",
                                                  style = "stretch",
                                                  block = TRUE),

                                     )
                                 ),
                        tabPanel("Other Methods",
                                 downloadBttn(outputId = "dwn_tes_res",
                                              label = "Download Test of Excess Significance Results (.txt)",
                                              size = "s",
                                              style = "stretch",
                                              block = TRUE))))
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
