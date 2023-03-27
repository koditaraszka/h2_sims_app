# Variables: ageSims, ageN, ageH2, ageC, ageM, ageK, ageP, ageCen, ageOnset, ageWeibull, ageModels, ageInfo, ageRun
fluidPage(
    titlePanel("Simulate age-of-onset (survival)"),
    fluidRow(
        column(4,
            fluidRow(style = "height: 60vh; overflow-y: auto; margin 5px 5px 5px 5px; background-color:rgba(149, 165, 166, 0.15);",
                column(12,
                    h3("Model Input"),
                    fluidRow(
                        column(6,
                            numericInput("ageSims", 
                             label = h4("Runs:"), 
                             value = 10, step=1,min=1, max=100
                            )
                        ),
                        column(6,
                            numericInput("ageN",
                                     label = h4("Sample size:"),
                                     value = 2000, step=100, min=100, max=5000
                             )
                         )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            sliderInput("ageH2",
                                label = h4("SNP H2:"),
                                min = 0, max = 1, value = 0.5
                            )
                        ),
                        column(6,
                            sliderInput("ageC",
                                label = h4("Causal SNPs:"),
                                min = 0, max = 1, value = 1
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            numericInput("ageM", 
                                label = h4("SNPs:"),
                                value = 1000, step=100, min=100, max=5000
                            )
                        ),
                        column(6,
                            sliderInput("ageK", 
                                label = h4("Population prop of cases:"),
                                min = 0, max = 1, value = 0.5
                            )
                        ),
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            radioButtons("ageP",
                                label = h4("Sample prop of cases:"),
                                choices = list("Equal to population" = 1,
                                               "50% of sample" = 2
                                ),
                                selected = 1
                            )
                        ),
                        column(6, align="center",
                            selectInput("ageWeibull",
                                label = h4("Weibull shape parameter:"),
                                choices = list("1 (exponential)" = 1,
                                               "2 (chi-squared)" = 2,
                                               "3 (normal)" = 3
                                ),
                                selected = 1
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            sliderInput("ageOnset", 
                                label = h4("Age-of-onset range:"),
                                min = 0, max = 100, value = c(20, 40)
                            )
                        ),
                        column(6,
                            radioButtons("ageCen", 
                                label = h4("Age-at-censoring choices:"),
                                choices = list("Set to max age-of-onset" = 1#,
                                               #"Simulated according to Weibull" = 2
                                ),
                                selected = 1
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            checkboxGroupInput("ageModels", 
                                label = h4("Models:"),
                                choices = list("Cox Frailty" = 1,
                                           "Case-Control" = 2,
                                           "BoxCox Age-of-onset" = 3,
                                           "Log Age-of-Onset" = 4,
                                           "RINT Age-of-Onset" = 5
                                ),
                                selected = c(1,2,3,4,5)
                            )
                        ),
                        column(6,
                            radioButtons("ageInfo", 
                                label = h4("Liability is:"),
                                choices = list("uninformative of age-of-onset" = 1,
                                           "informative of age-of-onset" = 2
                                ),
                                selected = 2
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(width = 12,
                            actionButton("ageRun", 
                                label = h4("Simulate Data"),
                                class = 'btn btn-primary',
                                width ="100%"
                            )
                        )
                    ),
                    br(),
                    br(),
                    br()
                )
            ),
            h3("Details"),
            fluidRow(
                column(width=12,
                    p("For an overview of the problem and the methods compared, check out the ", span("Introduction to the Problem", style = "font-weight: bold"), 
                      "tab under the ", span("About", style = "font-style: italic"), " tab."),
                    p("For a description of the generative model and the corresponding R code, check out the ", span("Fully Observed Case-Control Status", style = "font-weight: bold"), 
                      "tab under the ", span("Overview of the Generative Models", style = "font-style: italic"), " tab."),
                    p("After reading over these details, select the model parameters of interest and hit ", span("Simulate Data", style = "font-weight: bold"), "directly above.")
                )
            )
        ),
        column(8,
            style = "overflow-y: auto;",
            fluidRow(
                column(width = 12,
                    plotOutput("agePlots", width = "100%", height = "800px"),
                    br(),
                    br()
                )
           )
        )
    )
)