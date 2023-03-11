#library(ggplot2)
#library(ggpattern)
#library(patchwork)
library(dplyr)
library(coxmeg)
library(msm)
#source("plotting.R")
source("h2_shinyapp/ClusterScripts/analyses.R")
source("h2_shinyapp/ClusterScripts/methods.R")

args = commandArgs(trailingOnly=TRUE)

Sims = as.numeric(args[1])
N=as.numeric(args[2]) #N in 500, 2500, 5000
H2=as.numeric(args[3]) # 0.2, 0.8
K=as.numeric(args[4]) # 0.01, 0.1, 0.4
P=as.numeric(args[5]) # 1, 2
Informative=as.numeric(args[6]) # 1, 2

M=500
C=100
Onset = c(20,40)
Cen = c(45, 65)
Models = c(1,2,3,4,5)
liabModels = c(1,2,3)

#overviewPlot = F
#first = fullcc(N, M, H2, K, P, C/100, Cen[1], Cen[2], Onset[1], Onset[2], Informative)
#detail = overview(first)
#first = liab(N, M, H2, C/100, Onset[1], Onset[2], Informative)
#detail = overview(first, "liab")
#for(Weibull in c(1,2,3)){
#    first = ageonset(ageonsetN, ageonsetM, ageonsetH2, ageonsetK, ageonsetC/100, Onset[1], Onset[2], as.numeric(Weibull))
#    detail = overview(first, "ageonset")
#}
#first = partcc(N, M, H2, K, P, C/100, Onset[1], Onset[2], Unobs, Informative)
#detail = overview(first)

results = NULL
x=fullcc(N, M, H2, K, P, C/100, Cen[1], Cen[2], Onset[1], Onset[2], Informative)
results = rbind(results, c("FullyCC", runMethods(x, Models, K, "casecontrol")))
    
x=liab(N, M, H2, C/100, Onset[1], Onset[2], Informative)
results = rbind(results, c("Liability", runMethods(x, liabModels, 0, "liab")))
   
for(Weibull in c(1,2,3)){ 
    x = ageonset(N, M, H2, K, P, C/100, Onset[1], Onset[2], Weibull)
    results = rbind(results, c(paste0("Weibull(",Weibull,",l)"), runMethods(x, Models, K, "ageonset")))
}

for(Unobs in c(0, 0.1, 0.5)){
    x=partcc(N, M, H2, K, P, C/100, Onset[1], Onset[2], Unobs, Informative)
    results = rbind(results, c(paste0("PartialCC(",Unobs,")"), runMethods(x, Models, K, "casecontrol")))
}

results = data.frame(results)
colnames(results) = c("Model", "Tau", "BinGRMR", "LogGRMR", "BoxCoxGRMR", "QnormGRMR", "BinGRMH", "LogGRMH", "BoxCoxGRMH", "QnormGRMH")
results["N"] = N
results["H2"] = H2
results["K"] = K
results["P"] = P
results = subset(results, select = c(Model, H2, N, K, P, Tau, 
                                     BinGRMR, LogGRMR, BoxCoxGRMR, QnormGRMR, 
                                     BinGRMH, LogGRMH, BoxCoxGRMH, QnormGRMH))
if(Informative==1){
    write.table(results, paste0("results_notinformative_", Sims, ".txt"), sep = '\t', row.names = F, col.names = F, append = T)
} else{
    write.table(results, paste0("results_informative_", Sims, ".txt"), sep = '\t', row.names = F, col.names = F, append = T)
}

#main_plot = plot_results(results, Models, H2, Sims)
#main_plot = plot_results(results, Models, H2, Sims, "liab") 
#main_plot = plot_results(results, ageonsetModels, ageonsetH2, ageonsetSims)
#main_plot = plot_results(results, Models, H2, Sims)

#arrange plot
#topdesign="
#    123
#"
    
#bottomdesign="
#    111
#    111
#    111
#    111
#    222
#"
    
#design = "
#    111
#    111
#    222
#    222
#    222
#    222
#"
    
#top = (detail$risk + detail$incidence + detail$ageDist) + plot_layout(guides = 'collect', design=topdesign) 
#bottom = (main_plot$main + theme(legend.position = "bottom") + main_plot$text) + plot_layout(design=bottomdesign)
#top/bottom + plot_layout(design = design)