fluidPage(
    titlePanel("Continuous Liability"),
    fluidRow(
        column(4,
            fluidRow(style = "height: 60vh; overflow-y: auto; margin 5px 5px 5px 5px; background-color:rgba(149, 165, 166, 0.15);",
            column(12,
            h3("Model Input"),
            fluidRow(
                column(6,
                    numericInput("liabSims", 
                        label = h4("Number of Runs:"), 
                        value = 1, step=1,min=1, max=100
                    )
                ),
                column(6,
                    numericInput("liabN",
                        label = h4("Sample Size:"),
                        value = 2000, step=100, min=100, max=5000
                    )
                ),
            ),
            fluidRow(
                column(6,
                    sliderInput("liabH2",
                        label = h4("SNP Heritability:"),
                        min = 0, max = 1, value = 0.5
                    )
                ),
                column(6,
                    sliderInput("liabOnset", 
                        label = h4("Onset age range:"),
                        min = 0, max = 100, value = c(18, 40)
                    )
                )
            ),
            fluidRow(
                column(4,
                    numericInput("liabM", 
                        label = h4("Number of SNPs:"),
                        value = 100, step=100, min=100, max=5000
                    )
                ),
                column(8,
                    sliderInput("liabC",
                        label = h4("Causal SNPs (%):"),
                        min = 0, max = 100, value = 100
                    )
                )
            ),
            fluidRow(
                column(8,
                    checkboxGroupInput("liabModels", 
                        label = h4("Models to compare:"),
                        choices = list("Cox Frailty" = 1,
                                       "Original Liability" = 2,
                                       "RINT Liability" = 3
                        ),
                        selected = c(2,3)
                    )
                ),
                column(4,
                       radioButtons("liabLM", 
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
                    actionButton("liabRun", 
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
                    plotOutput("liabPlots", width = "100%", height = "800px"),
                    
                    br(),
                    br()
                )
            )
        )
    )
)