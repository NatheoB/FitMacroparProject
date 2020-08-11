sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs",
        menuItem("Bibliography", tabName = "biblio", icon = icon("search")),
        menuItem("Hamelin and Lewis' model", tabName = "model", icon = icon("percentage")),
        menuItem("Fit application", tabName = "app", icon = icon("chart-bar"), selected = TRUE),
        menuItem("Results", tabName = "results", icon = icon("file-excel")),
        menuItem("Article", tabName = "article", icon = icon("microscope")),
        menuItem("Help", tabName = "help", icon = icon("info"))
    ),
    br(), br(),
    actionButton(inputId = "exitApp", label = "Exit", icon = icon("times"))
)


body <- dashboardBody(
    tabItems(
        # Bibliography
        tabItem(tabName = "biblio",
            htmlOutput("pdfviewerBiblio")
        ),
        
        
        # Hamelin and Lewis' model
        tabItem(tabName = "model",
            fluidRow(
                column(width = 12, offset = 3, box(width = 6, title = h2("Variables definition"),
                    h3("Multiplicity"),
                    withMathJax("\\(M_k(t)\\) : density of hosts infected with k parasites at time \\(t\\)"),
                    h3("Parameters"),
                    "\\(F\\) : force of infection (applying to the host population)",
                    br(),
                    "\\(\\mu\\) : host mortality (balanced such that host population density remains constant)",
                    br(),
                    "\\(\\gamma\\) : parasite mortality (or detachment for ectoparasites) rate"
                ))
            ),
            fluidRow(
                box(title = h2("Model ignoring parasite mortality (Geom)"),
                    h3("Infection dynamics"),
                    "$$\\dot M_0 = \\mu - (F + \\mu)M_0$$",
                    "$$\\dot M_k = FM_{k-1} - (F + \\mu)M_k\\qquad ,k \\ge 1$$",
                    h3("At equilibrium"),
                    "$$\\bar M_0 = \\frac {\\mu}{F + \\mu}$$",
                    "$$\\bar M_k = \\frac {F}{F + \\mu}M_{k-1}$$",
                    "\\(\\bar M_k\\) follow a Geometric distribution of parameter \\(p = \\bar M_0 = \\frac {\\mu}{F + \\mu}\\)",
                    "$$\\bar M_k = (1 - p)^kp\\qquad ,k \\ge 0$$"
                ),
                box(title = h2("Model with parasite mortality (HLmodel)"),
                    h3("Infection dynamics"),
                    "$$\\dot M_0 = \\mu - (F + \\mu)M_0 + \\gamma M_1$$",
                    "$$\\dot M_k = FM_{k-1} - (F + \\mu + k\\gamma)M_k + (k+1)\\gamma M_{k+1}\\qquad ,k \\ge 1$$",
                    h3("At equilibrium"),
                    "$$\\bar M_1 = \\frac {\\bar M_0(F + \\mu) - \\mu}{\\gamma}$$",
                    "$$\\bar M_{k+1} = \\frac {(F + \\mu + k\\gamma)\\bar M_k - F\\bar M_{k-1}}{(k+1)\\gamma}\\qquad k \\ge 1$$",
                    "By relating parasite counts within age-classes and population-wise :",
                    "$$\\bar M_0 = \\int_0^\\infty{\\exp\\biggl(\\frac{-F(1-\\exp(-\\gamma a))}{\\gamma}\\biggr)\\mu \\exp(-\\mu a)da}$$"
                )
            )
        ),

        
        # Fit application
        tabItem(tabName = "app",
            navbarPage(title = "FitMacropar",
               tabPanel(title = "Dataset",
                    fluidRow(
                        box(width = 4, title = "Create your dataset...",
                            fluidRow(
                                column(width = 6, 
                                    numericInput(
                                        "dataMaxBurden", "Maximum parasite burden",
                                        value = 0, min = 0
                                    ),
                                    actionButton("validateMaxBurden", "Initialize table"),
                                    br(),br(),br(),br(),
                                    downloadButton("dlCreatedData", "Download table")
                                ),
                                column(width = 6, rHandsontableOutput("dataCreatedInput"))
                            ),
                        collapsible = TRUE, collapsed = TRUE),
                        box(width = 4, title = "... or import your dataset...",
                            fileInput(inputId = "dataFileInput", label = NULL),
                            collapsible = TRUE, collapsed = TRUE),
                        box(width = 4, title = "... or test from pre-existing dataset.",
                            radioButtons(inputId = "dataExampleInput",
                                         label = NULL,
                                         choiceNames = data.names,
                                         choiceValues = 1:length(data.names),
                                         selected = 5
                            ),
                            collapsible = TRUE, collapsed = TRUE) 
                    ),
                    fluidRow(
                        box(width = 12,
                            radioButtons(
                                inputId = "dataChoiceInput",
                                label = "Which dataset to use ?",
                                choices = list(
                                    "Created dataset" = 1,
                                    "Imported dataset" = 2,
                                    "Pre-existing dataset" = 3
                                ),
                                selected = 3, inline = TRUE
                            ),
                            textInput("dataName", "Dataset name..."),
                            actionButton("submitData", "Submit dataset")
                        )
                    ),
                    fluidRow(
                        box(width = 12, title = "Overview",
                            column(width = 12, tableOutput("table"), style = "overflow-x: scroll"),
                            br(), br(),
                            verbatimTextOutput("stats")
                        )
                    )
               ),
                tabPanel(title = "Fits",
                    fluidRow(column(width = 12, offset = 4, box(width = 4,
                        fluidRow(column(width = 12, offset = 3, uiOutput("dataTitleOutput.pars", width = 6))),
                        fluidRow(
                            column(width = 4, offset = 1, actionButton("fit.all", "Fit all models")),
                            column(width = 4, offset = 1, downloadButton("dlParams", "Download parameters"))
                        )
                    ))),
                    fluidRow(
                        box(width = 3, title = "Poisson", collapsible = TRUE, collapsed = FALSE,
                            numericInput(
                                inputId = "pars.pois.lambda", label = "Lambda",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            actionButton("fit.pois", "Fit to data")
                        ),
                        box(width = 3, title = "Negative binomial", collapsible = TRUE, collapsed = FALSE,
                            numericInput(
                                inputId = "pars.nb.size", label = "Size",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            numericInput(
                                inputId = "pars.nb.mu", label = "Mu",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            actionButton("fit.nb", "Fit to data")
                        ),
                        box(width = 3, title = "Geom", collapsible = TRUE, collapsed = FALSE, 
                            numericInput(
                                inputId = "pars.geom.M0", label = "M0",
                                value = 0.5, min = 0, max = 1, step = 0.01
                            ),
                            actionButton("fit.geom", "Fit to data")
                        ),
                        box(width = 3, title = "Hamelin and Lewis' model", collapsible = TRUE, collapsed = FALSE,
                            numericInput(
                                inputId = "pars.HLmodel.force", label = "Force",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            numericInput(
                                inputId = "pars.HLmodel.mu", label = "Mu",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            numericInput(
                                inputId = "pars.HLmodel.gamma", label = "Gamma",
                                value = 0.5, min = 0, step = 0.01
                            ),
                            br(),
                            sliderInput("HLnit", "Number of fit iterations", min = 1, max = 10, step = 1, value = 3),
                            helpText("Find the best fit over n fits"),
                            actionButton("fit.HLmodel", "Fit to data")
                        )
                    ),
                ),
                tabPanel(title = "Results",
                    fluidRow(
                        box(width = 7, title = "Histogram", 
                            uiOutput("maxK.out"),
                            checkboxGroupInput(
                                       "hist.models", "Models to show on histrogram", 
                                       choices = list("Poisson" = 1, "Negative binomial" = 2, "Geom" = 3, "HLmodel" = 4, "Observed" = 5),
                                       selected = c(1, 2, 3, 4, 5),
                                       inline = TRUE
                            ),
                            plotOutput(outputId = "hist"),
                            helpText("Right-click to download histogram.")
                        ),
                        box(width = 5, title = "Khi2 test",
                            tableOutput(outputId = "khi2"),
                            downloadButton("dlKhi2", "Download Khi2 results")
                        ),
                        box(width = 5, title = "Models comparison",
                            tableOutput(outputId = "comparison"),
                            downloadButton("dlComp", "Download models comparison")
                        )
                    )
                )
            )
        ),

        
        # Results
        tabItem(tabName = "results",
            downloadButton("dlFullResults", "Download results sheet"),
            br(), br(),
            tabsetPanel(
                id = 'dataset.results',
                tabPanel("Description", br(), DT::dataTableOutput("data_description")),
                tabPanel("Khi2", br(), DT::dataTableOutput("data_khi2")),
                tabPanel("Ranking", br(), DT::dataTableOutput("data_ranking")),
                tabPanel("Parameters", br(), DT::dataTableOutput("data_param")),
                tabPanel("References", br(), helpText(
                        "Brattey, J., 1988. Life history and population biology of adult Acanthocephalus lucii (Acantocephala : echinorhynchidae). J. Parasit. 74(1), 72-80",
                        br(), br(),
                        "Chubb, J.C., 1963. Seasonal occurrence and maturation of Triaenophorus nodulosus (Pallas, 1781) (Cestoda : Pseudophyllidea) in the Pike Esox lucius L. of Llyn Tegid. Parasitology 53, 419-433.",
                        br(), br(),
                        "Milne A., 1943. The comparison of sheep-tick populations (Ixodes ricinus L.). Annals of Applied Biology 30, 240-253.",
                        br(), br(),
                        "Schmid, W.D., Robinson, E.J. Jr., 1972. The pattern of a host-parasite distribution. The Journal of Parasitology 58(5), 907-910.",
                        br(), br(),
                        "Stromberg, P.C., Toussant, M.J., Dubey, J.P., 1978. Population biology of Paragonimus kellicotti metacercariae in central Ohio. Parasitology 77, 13-18."
                ))
            )
        ),

        
        # Article
        tabItem(tabName = "article",
            h1("Introduction"),
            h1("Results"),
            h1("Discussion"),
            h1("References")
        ),
        
        
        # Help
        tabItem(tabName = "help",
            fluidRow(
                # Contact
                box(title = "Contact",
                    "Shiny app creator : BEAUCHAMP Natheo", br(),
                    "Mail : <natheo.beauchamp@agrocampus-ouest.fr>", br(), br(),
                    
                    "Intership supervisor : HAMELIN Frederic", br(), br(),
                    
                    "Model creators : LEWIS Mark and HAMELIN Frederic"
                ),
                    
                # Software and libraries
                box(title = "Software and libraries",
                    "R studio software", br(),
                    "Shiny library"
                )
            )
        )
    )
)

    
dashboardPage(skin = "blue",
    dashboardHeader(title = "Macroparasites modelling", titleWidth = 300),
    sidebar,
    body
)
