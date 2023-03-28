# How to install all packages
# Assuming you have none installed already

if (!require("msm", quietly = TRUE))
  install.packages("msm")

if (!require("dplyr", quietly = TRUE))
  install.packages("dplyr")

## commented out for R version < 4.1 :(
## you'll need to install coxmeg version < 1.13 (not 14) from source
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#if (!require("SNPRelate", quietly = TRUE))
#  BiocManager::install("SNPRelate")

#if (!require("SeqArray", quietly = TRUE))
#  BiocManager::install("SeqArray")

#if (!require("GENESIS", quietly = TRUE))
#  BiocManager::install("GENESIS")

#if (!require("coxmeg", quietly = TRUE))
#  install.packages("coxmeg")



