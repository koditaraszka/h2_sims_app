#Variables: ccSims, ccN, ccH2, ccC, ccM, ccChoice, ccP, ccK, ccUnobs, ccCen, ccAgeDist, ccOnset, ccModels, ccInfo, ccRun
fluidPage(
    titlePanel("Simulate case-control status"),
    fluidRow(
        column(4,
            fluidRow(style = "height: 60vh; overflow-y: auto; margin 5px 5px 5px 5px; background-color:rgba(149, 165, 166, 0.15);",
                column(12,
                    h3("Model Input"),
                    fluidRow(
                        column(6,
                            numericInput("ccSims", 
                                label = h4("Runs:"), 
                                value = 10, step=1,min=1, max=100
                            )
                        ),
                        column(6,
                            numericInput("ccN",
                                label = h4("Sample size:"),
                                value = 2000, step=100, min=100, max=5000
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            sliderInput("ccH2",
                                label = h4("SNP H2:"),
                                min = 0, max = 1, value = 0.5
                            )
                        ),
                        column(6,
                            sliderInput("ccC",
                                label = h4("Causal SNPs:"),
                                min = 0, max = 1, value = 1
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            numericInput("ccM", 
                                label = h4("SNPs:"),
                                value = 1000, step=100, min=100, max=5000
                            )
                        ),
                        column(6,
                            radioButtons("ccP", 
                                label = h4("Sample prop of cases:"),
                                choices = list("Equal to population" = 1,
                                               "50% of sample" = 2
                                ),
                                selected = 1
                            )
                        ),
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            sliderInput("ccK",
                                label = h4("Population prop of cases:"),
                                min = 0, max = 1, value = 0.5
                            )
                        ),
                        column(6,
                            radioButtons("ccChoice", 
                                label = h4("Parameter to use:"),
                                choices = list("Subset of cases are unobserved" = 1,
                                               "Set the age-at-censoring" = 2
                                ),
                                selected = 1
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            sliderInput("ccUnobs", 
                                label = h4("Prop of cases unobserved:"),
                                min = 0, max = 1, value = 0
                            )
                        ),
                        column(6,
                            sliderInput("ccCen", 
                                label = h4("Age-at-censoring range:"),
                                min=1, max=100, value = c(45, 70)
                               )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(7,
                            radioButtons("ccAgeDist", 
                                label = h4("Age-of-onset distribution:"),
                                choices = list("logistic function" = 1,
                                               "truncated Gaussian" = 2
                                ),
                                selected = 1
                            )
                        ),
                        column(5,
                            sliderInput("ccOnset", 
                                label = h4("Age-of-onset range:"),
                                min = 0, max = 100, value = c(20, 40)
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        column(6,
                            checkboxGroupInput("ccModels", 
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
                            radioButtons("ccInfo", 
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
                            actionButton("ccRun", 
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
                          plotOutput("ccPlots", width = "100%", height = "800px"),
                          
                          br(),
                          br()
                   )
               )
        )
    )
)