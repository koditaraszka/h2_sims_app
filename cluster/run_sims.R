
# assumes you're in the h2_shinyapp/cluster directory
# either go there or update the source paths
# there are 3 real arguments to set

source("cluster/packages.R") # check if packages are installed
library(dplyr)
source("updated_func_reml.R")
source("cluster/methods.R")

N=2500
M=1000
C=1

minOnset=20
maxOnset=45
minCen=50
maxCen=75

models = c(1,2,3,4,5)
ageDists=c(1,2)
info = 2 #infos=c(1,2) only dealing with informative now
# choice: 1==unobserved cases, 2==min/max censoring

args = commandArgs(trailingOnly=TRUE)

sim = args[1]
H2 = as.numeric(args[2])/100
K = as.numeric(args[3])/100
P = args[4]

final = NULL

for(ageDist in ageDists){
    ageName="Logistic"
    if(ageDist==2){
        ageName="Gaussian"
    }
    
    choice==1
    x = casecontrol(N, M, H2, C, choice, K, P, ageDist, minOnset, maxOnset, unobs, minCen, maxCen, info)
    final = rbind(final, c("case-control", ageName, NA, runMethods(x, models, K, "casecontrol")))
      
    choice==2
    for(unobs in c(0.1, 0.4)){
        x = casecontrol(N, M, H2, C, choice, K, P, ageDist, minOnset, maxOnset, unobs, minCen, maxCen, info)
        final = rbind(final, c("case-control", ageName, unobs, runMethods(x, models, K, "casecontrol")))
    }
      
    x=liability(N, M, H2, C, ageDist, minOnset, maxOnset, (1-K), info)
    final = rbind(final, c("liability", ageName, NA, runMethods(x, models, K, "liab")))
}

cen=1 #For now, cen=2 too hard
for(wei in c(1,2,3)){
    x = ageonset(N, M, H2, C, K, P, cen, minOnset, maxOnset, wei, info)
    final = rbind(final, c(paste0("weibull",wei), NA, NA, runMethods(x, models, K, "ageonset")))
}


final = data.frame(final)
colnames(final) = c("Model", "AgeDist", "Unobs", "Tau", "BinGRMR", "LogGRMR", "BoxCoxGRMR", 
                    "QnormGRMR", "BinGRMH", "LogGRMH", "BoxCoxGRMH", "QnormGRMH")

H2 = 100*H2
K = 100*K
name = paste0("sim_h2", H2, "_K", K)
if(P==2){
  name = paste0(name, "_ascertained_", sim, ".txt")
} else{
  name = paste0(name, "_", sim, ".txt")
}

# only need col names in first row
if(sim==1){
  write.table(final, name, quote = F, sep = '\t', row.names = F, col.names = T) 
} else{
  write.table(final, name, quote = F, sep = '\t', row.names = F, col.names = F) 
}

