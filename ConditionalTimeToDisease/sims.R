fluidPage(
    titlePanel("Simulated Time-to-Disease"),
    fluidRow(
        column(4,
            fluidRow(style = "height: 60vh; overflow-y: auto; margin 5px 5px 5px 5px; background-color:rgba(149, 165, 166, 0.15);",
            column(12,
            h3("Model Input"),
            fluidRow(
                column(6,
                    numericInput("fullccSims", 
                        label = h4("Number of Runs:"), 
                        value = 1, step=1,min=1, max=100
                    )
                ),
                column(6,
                    numericInput("fullccN",
                        label = h4("Sample Size:"),
                        value = 500, step=100, min=1
                    )
                ),
            ),
            fluidRow(
                column(6,
                    sliderInput("fullccH2",
                        label = h4("SNP Heritability:"),
                        min = 0, max = 1, value = 0.5
                    )
                ),
                column(6,
                    sliderInput("fullccK",
                        label = h4("Proportion of Cases:"),
                        min = 0, max = 1, value = 0.1
                    )
                )
            ),
            fluidRow(
                column(4,
                    numericInput("fullccM", 
                        label = h4("Number of SNPs:"),
                        value = 100, step=100, min=1
                    )
                ),
                column(8,
                    sliderInput("fullccC",
                        label = h4("Causal SNPs (%):"),
                        min = 0, max = 100, value = 100
                    )
                )
            ),
            fluidRow(
                column(6,
                    sliderInput("fullccOnset", 
                        label = h4("Onset age range:"),
                        min = 0, max = 100, value = c(18, 40)
                    )
                ),
              column(6,
                     sliderInput("fullccCen", 
                                 label = h4("Censor age range:"),
                                 min = 0, max = 100, value = c(45, 80)
                     )
              )
            ),
            fluidRow(
                column(12,
                    checkboxGroupInput("fullccModels", 
                        label = h4("Models to compare:"),
                        choices = list("Cox Frailty" = 1,
                                       "Case-Control" = 2,
                                       "BoxCox Age-of-onset" = 3,
                                       "Log Age-of-Onset" = 4,
                                       "RINT Age-of-Onset" = 5
                        ),
                        selected = c(1,2,3,4,5)
                    )
                )
            ),
            br(),
            fluidRow(
                column(width = 12,
                    actionButton("fullccRun", 
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
                    plotOutput("fullccPlots", width = "100%", height = "800px"),
                    
                    br(),
                    br()
                )
            )
        )
    )
)