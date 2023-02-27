library(shiny)
library(knitr)
library(shinythemes)
library(markdown)
library(ggplot2)
library(patchwork)
library(dplyr)
library(coxmeg)
source("updated_func_reml.R")
source("generic_plotting.R")

ui = div(style = 
        "width: 100%; margin: 0 0 0 0; padding: 0 0 0 0; left: 0; right: 10px; position: absolute;",
        navbarPage("Heritability of Disease",
            theme=shinytheme('flatly'),
            collapsible = T,
            navbarMenu("About",
                tabPanel("Introduction to the App",
                    source("About/about_app.R")$value
                ),
                tabPanel("Introduction to the Problem",
                    source("About/about_problem.R")$value
                )#,
                #tabPanel("Introduction to the Authors",
                #    source("About/about_intro.R")$value
                #)
                          
            ),
            navbarMenu("Overview of the Generative Models",
                tabPanel("Continous Liability",
                    source("LiabilityOnly/overview.R")$value
                ),
                tabPanel("Fully Observed Case-Control Status",
                    source("FullyCaseControl/overview.R")$value
                ),
                #tabPanel("Partially Observed Case-Control Status",
                #    source("basic_sims.R")$value
                #),
                tabPanel("Age-of-Onset (Survival)",
                    source("TimeToDisease/overview.R")$value
                )#,
                #tabPanel("Conditional Time-to-Disease (Survival) Simulations",
                #    source("basic_sims.R")$value
                #)
            ),
            navbarMenu("Run Simulations",
                tabPanel("Continuous Liability Simulations",
                    source("LiabilityOnly/sims.R")$value
                ),
                tabPanel("Fully Observed Case-Control Simulations",
                    source("FullyCaseControl/sims.R")$value
                ),
                #tabPanel("Partially Observed Case-Control Simulations",
                #    source("basic_sims.R")$value
                #),
                tabPanel("Age-of-Onset (Survival) Simulations",
                    source("TimeToDisease/sims.R")$value
                )#,
                #tabPanel("Conditional Time-to-Disease (Survival) Simulations",
                #    source("basic_sims.R")$value
                #)
            )
        )
    )   
    
server <- function(input, output) {
    
    #fully observed case-control
    output$fullccPlots <- renderPlot({
        
        if (input$fullccRun == 0)
            return()
        
        input$fullccRun
        
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Generating Plots", value = 0)
        results = NULL
        isolate({
            first = fullcc(input$fullccN, input$fullccM, input$fullccH2, input$fullccK, input$fullccP, input$fullccC/100, input$fullccCen[1], input$fullccCen[2], input$fullccOnset[1], input$fullccOnset[2])
            detail = overview(first)
            for(i in 1:input$fullccSims){
                progress$inc(1/input$fullccSims, detail = paste("Running Simulation", i))
                x=fullcc(input$fullccN, input$fullccM, input$fullccH2, input$fullccK, input$fullccP, input$fullccC/100, input$fullccCen[1], input$fullccCen[2], input$fullccOnset[1], input$fullccOnset[2])
                results = rbind(results, runMethods(x, input$fullccModels, input$fullccK, input$fullccLM))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "ScaledTau", "BinGRM", "ScaledBinGRM", "LogGRM", "ScaledLogGRM", 'BoxCoxGRM', 'ScaledBoxCoxGRM', 'QnormGRM', 'ScaledQnormGRM')
            main_plot = plot_results(results, input$fullccModels, input$fullccH2, input$fullccSims)
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
            first = liab(input$liabN, input$liabM, input$liabH2, input$liabC/100, input$liabOnset[1], input$liabOnset[2])
            detail = overview(first, TRUE)
            for(i in 1:input$liabSims){
                progress$inc(1/input$liabSims, detail = paste("Running Simulation", i))
                x=liab(input$liabN, input$liabM, input$liabH2, input$liabC/100, input$liabOnset[1], input$liabOnset[2])
                results = rbind(results, runMethods(x, input$liabModels, 0, input$liabLM, T))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "ScaledTau", "BinGRM", "ScaledBinGRM", "LogGRM", "ScaledLogGRM", 'BoxCoxGRM', 'ScaledBoxCoxGRM', 'QnormGRM', 'ScaledQnormGRM')
            main_plot = plot_results(results, input$liabModels, input$liabH2, input$liabSims, TRUE)
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
            first = ageonset(input$ageonsetN, input$ageonsetM, input$ageonsetH2, input$ageonsetK, input$ageonsetC/100, as.numeric(input$ageonsetWeibull))
            detail = overview(first)
            for(i in 1:input$ageonsetSims){
                progress$inc(1/input$ageonsetSims, detail = paste("Running Simulation", i))
                x = ageonset(input$ageonsetN, input$ageonsetM, input$ageonsetH2, input$ageonsetK, input$ageonsetC/100, as.numeric(input$ageonsetWeibull))
                results = rbind(results, runMethods(x, input$ageonsetModels, input$ageonsetK, input$ageonsetLM))
            }
            results = data.frame(results)
            colnames(results) = c("Tau", "ScaledTau", "BinGRM", "ScaledBinGRM", "LogGRM", "ScaledLogGRM", 'BoxCoxGRM', 'ScaledBoxCoxGRM', 'QnormGRM', 'ScaledQnormGRM')
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
