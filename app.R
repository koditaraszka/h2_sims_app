library(shiny)
library(knitr)
library(shinythemes)
library(markdown)
library(ggplot2)
library(ggpattern)
library(patchwork)
library(dplyr)
library(coxmeg)
source("updated_func_reml.R")
source("plotting.R")
source("analyses.R")

ui = div(style = 
        "width: 100%; margin: 0 0 0 0; padding: 0 0 0 0; left: 0; right: 10px; position: absolute;",
        navbarPage("Heritability of Disease",
            theme=shinytheme('flatly'),
            collapsible = TRUE,
            navbarMenu("About",
                tabPanel("Introduction to the App",
                    source("about/about_app.R")$value
                ),
                tabPanel("Introduction to the Problem",
                    source("about/about_problem.R")$value
                )#,
                #tabPanel("Introduction to the Authors",
                #    source("About/about_intro.R")$value
                #)
                          
            ),
            navbarMenu("Overview of the Generative Models",
                tabPanel("Continous Liability",
                    source("liability/overview.R")$value
                ),
                tabPanel("Case-Control Status",
                    source("caseControl/overview.R")$value
                ),
                tabPanel("Age-of-Onset (Survival)",
                    source("ageOnset/overview.R")$value
                )
            ),
            navbarMenu("Run Simulations",
                tabPanel("Continuous Liability Simulations",
                    source("liability/sims.R")$value
                ),
                tabPanel("Case-Control Status Simulations",
                    source("caseControl/sims.R")$value
                ),
                tabPanel("Age-of-Onset (Survival) Simulations",
                    source("ageOnset/sims.R")$value
                )
            )
        )
    )   
    
server <- function(input, output) {
    
    #fully observed case-control
    output$ccPlots <- renderPlot({
        
        if (input$ccRun == 0)
            return()
        
        input$ccRun
        
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Generating Plots", value = 0)
        results = NULL
        isolate({
            first = casecontrol(input$ccN, input$ccM, input$ccH2, input$ccC, input$ccChoice, input$ccK, input$ccP, input$ccAgeDist, 
                                input$ccOnset[1], input$ccOnset[2], input$ccUnobs, input$ccCen[1], input$ccCen[2], input$ccInfo)
            detail = overview(first)
            for(i in 1:input$ccSims){
                progress$inc(1/input$ccSims, detail = paste("Running Simulation", i))
                x=casecontrol(input$ccN, input$ccM, input$ccH2, input$ccC, input$ccChoice, input$ccK, input$ccP, input$ccAgeDist, 
                               input$ccOnset[1], input$ccOnset[2], input$ccUnobs, input$ccCen[1], input$ccCen[2], input$ccInfo)
                results = rbind(results, runMethods(x, input$ccModels, input$ccK, "casecontrol"))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "BinGRMR", "LogGRMR", "BoxCoxGRMR", "QnormGRMR", "BinGRMH", "LogGRMH", "BoxCoxGRMH", "QnormGRMH")
            main_plot = plot_results(results, input$ccModels, input$ccH2, input$ccSims)
        })
        topdesign="
            123
        "
        bottomdesign="
            111
            111
            111
            111
            222
        "
        design = "
            111
            111
            222
            222
            222
            222
        "
        
        top = (detail$risk + detail$incidence + detail$ageDist) + plot_layout(guides = 'collect', design=topdesign) 
        bottom = (main_plot$main + theme(legend.position = "bottom") + main_plot$text) + plot_layout(design=bottomdesign)
        top/bottom + plot_layout(design = design)
    })
    
    output$liabPlots <- renderPlot({
        
        if (input$liabRun == 0)
            return()
        
        input$liabRun
        
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Generating Plots", value = 0)
        results = NULL
        isolate({
            first = liability(input$liabN, input$liabM, input$liabH2, input$liabC, input$liabAgeDist, input$liabOnset[1], input$liabOnset[2], input$liabCenRate, input$liabCenDist, input$liabInfo)
            detail = overview(first, "liab")
            for(i in 1:input$liabSims){
                progress$inc(1/input$liabSims, detail = paste("Running Simulation", i))
                x=liability(input$liabN, input$liabM, input$liabH2, input$liabC, input$liabAgeDist, input$liabOnset[1], input$liabOnset[2], input$liabCenRate, input$liabCenDist, input$liabInfo)
                results = rbind(results, runMethods(x, input$liabModels, (1-input$liabCenRate), "liab"))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "BinGRMR", "LogGRMR", "BoxCoxGRMR", "QnormGRMR", "BinGRMH", "LogGRMH", "BoxCoxGRMH", "QnormGRMH")
            main_plot = plot_results(results, input$liabModels, input$liabH2, input$liabSims)
        })
        
        topdesign="
            123
        "
        bottomdesign="
            111
            111
            111
            111
            222
        "
        design = "
            111
            111
            222
            222
            222
            222
        "
        
        top = (detail$risk + detail$incidence + detail$ageDist) + plot_layout(guides = 'collect', design=topdesign) 
        bottom = (main_plot$main + theme(legend.position = "bottom") + main_plot$text) + plot_layout(design=bottomdesign)
        top/bottom + plot_layout(design = design)
    })
    
    # age-of-onset plot
    output$ageonsetPlots <- renderPlot({
        
        if (input$ageonsetRun == 0)
            return()
        
        input$ageonsetRun
        
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Generating Plots", value = 0)
        results = NULL
        isolate({
            first = ageonset(input$ageonsetN, input$ageonsetM, input$ageonsetH2, input$ageonsetK, input$ageonsetP, input$ageonsetC/100, input$ageonsetRange[1], input$ageonsetRange[2], as.numeric(input$ageonsetWeibull))
            detail = overview(first, "ageonset")
            for(i in 1:input$ageonsetSims){
                progress$inc(1/input$ageonsetSims, detail = paste("Running Simulation", i))
                x = ageonset(input$ageonsetN, input$ageonsetM, input$ageonsetH2, input$ageonsetK, input$ageonsetP, input$ageonsetC/100, input$ageonsetRange[1], input$ageonsetRange[2], as.numeric(input$ageonsetWeibull))
                results = rbind(results, runMethods(x, input$ageonsetModels, input$ageonsetK, "ageonset"))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "BinGRMR", "LogGRMR", "BoxCoxGRMR", "QnormGRMR", "BinGRMH", "LogGRMH", "BoxCoxGRMH", "QnormGRMH")
            main_plot = plot_results(results, input$ageonsetModels, input$ageonsetH2, input$ageonsetSims)
        })
        
        topdesign="
            123
        "
        bottomdesign="
            111
            111
            111
            111
            222
        "
        design = "
            111
            111
            222
            222
            222
            222
        "
        
        top = (detail$risk + detail$incidence + detail$ageDist) + plot_layout(guides = 'collect', design=topdesign) 
        bottom = (main_plot$main + theme(legend.position = "bottom") + main_plot$text) + plot_layout(design=bottomdesign)
        top/bottom + plot_layout(design = design)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
