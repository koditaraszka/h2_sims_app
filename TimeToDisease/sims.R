fluidPage(
    titlePanel("Simulated Age-of-Onset"),
    fluidRow(
        column(4,
            fluidRow(style = "height: 60vh; overflow-y: auto; margin 5px 5px 5px 5px; background-color:rgba(149, 165, 166, 0.15);",
            column(12,
            h3("Model Input"),
            fluidRow(
                column(6,
                    numericInput("ageonsetSims", 
                        label = h4("Number of Runs:"), 
                        value = 1, step=1,min=1, max=100
                    )
                ),
                column(6,
                    numericInput("ageonsetN",
                        label = h4("Sample Size:"),
                        value = 500, step=100, min=1
                    )
                ),
            ),
            fluidRow(
                column(6,
                    sliderInput("ageonsetH2",
                        label = h4("SNP Heritability:"),
                        min = 0, max = 1, value = 0.5
                    )
                ),
                column(6,
                    sliderInput("ageonsetK",
                        label = h4("Proportion of Cases:"),
                        min = 0, max = 1, value = 0.1
                    )
                )
            ),
            fluidRow(
                column(4,
                    numericInput("ageonsetM", 
                        label = h4("Number of SNPs:"),
                        value = 100, step=100, min=1
                    )
                ),
                column(8,
                    sliderInput("ageonsetC",
                        label = h4("Causal SNPs (%):"),
                        min = 0, max = 100, value = 100
                    )
                )
            ),
            fluidRow(
                column(12,
                       selectInput("ageonsetWeibull", 
                                          label = h4("Simulation Framework:"),
                                          choices = list("Weibull(1,liability)" = 1,
                                                         "Weibull(2,liability)" = 2,
                                                         "Weibull(3,liability)" = 3
                                          ),
                                          selected = 1
                       )
                )
            ),
            fluidRow(
                column(8,
                       checkboxGroupInput("ageonsetModels", 
                                          label = h4("Models to compare:"),
                                          choices = list("Cox Frailty" = 1,
                                                         "Case-Control" = 2,
                                                         "BoxCox Age-of-onset" = 3,
                                                         "Log Age-of-Onset" = 4,
                                                         "RINT Age-of-Onset" = 5
                                          ),
                                          selected = c(1,2,3,4,5)
                       )
                ),
                column(4,
                       radioButtons("ageonsetLM", 
                                    label = h4("Linear Model:"),
                                    choices = list("REML" = 1,
                                                   "HE Reg" = 2
                                    ),
                                    selected = 1
                       )
                )
            ),
            br(),
            fluidRow(
                column(width = 12,
                    actionButton("ageonsetRun", 
                        label = h4("Simulate Data"),
                        class = 'btn btn-primary',
                        width ="100%"
                    )
                )
            ),
            br(),
            br(),
            br()
            )),
            h3("Details"),
            fluidRow(
                column(width=12,
                    p("For an overview of the problem and the methods compared, check out the ", span("Introduction to the Problem", style = "font-weight: bold"), 
                      "tab under the ", span("About", style = "font-style: italic"), " tab."),
                    p("For a description of the generative model and the corresponding R code, check out the ", span("Age-of-Onset (Survival)", style = "font-weight: bold"), 
                        "tab under the ", span("Overview of the Generative Models", style = "font-style: italic"), " tab."),
                    p("After reading over these details, select the model parameters of interest and hit ", span("Simulate Data", style = "font-weight: bold"), "directly above.")
                )
            )
        ),
        column(8,
            style = "overflow-y: auto;",
            fluidRow(
                column(width = 12,
                    plotOutput("ageonsetPlots", width = "100%", height = "800px"),
                    
                    br(),
                    br()
                )
            )
        )
    )
)