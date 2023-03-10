fluidRow(
    br(),
    column(width=12, h3("Partially Observed Case-Control Status")),
    br(),
    navlistPanel("Overview of Generative Model",
        tabPanel(h4("Quick Description"),
            column(width=12,
                withMathJax(), 
                h5(HTML(mark_html(knit("PartialCaseControl/quick_description.Rmd", quiet = T))))
            )
        ),
        tabPanel(h4("Detailed Description"),
            column(width=12,
                withMathJax(), 
                h5(HTML(mark_html(knit("PartialCaseControl/detailed_description.Rmd", quiet = T))))
            )
        ),
       "Code for Simulated Data",
        tabPanel(h4("Main Method"),
            column(width=12,
                h5(HTML(mark_html(knit("PartialCaseControl/main_method.Rmd", quiet = T))))
            )
        ),
        navbarMenu(h4("Helper Functions"),
            tabPanel(h4("Genetic Liability"),
                column(width=12,
                    h5(HTML(mark_html(knit("genetic_liability.Rmd", quiet = T))))
                )
            ),
            tabPanel(h4("Set Age"),
                column(width=12,
                    h5(HTML(mark_html(knit("PartialCaseControl/set_age.Rmd", quiet = T))))
                )
            )
        )
    )
)
