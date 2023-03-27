# How to install all packages
# Assuming you have none installed already

if (!require("knitr", quietly = TRUE))
  install.packages("knitr")

if (!require("dplyr", quietly = TRUE))
  install.packages("dplyr")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("SNPRelate", quietly = TRUE))
  BiocManager::install("SNPRelate")

if (!require("SeqArray", quietly = TRUE))
  BiocManager::install("SeqArray")

if (!require("GENESIS", quietly = TRUE))
  BiocManager::install("GENESIS")

if (!require("coxmeg", quietly = TRUE))
  install.packages("coxmeg")

if (!require("markdown", quietly = TRUE))
  install.packages("markdown")

if (!require("shiny", quietly = TRUE))
  install.packages("shiny")

if (!require("shinythemes", quietly = TRUE))
  install.packages("shinythemes")

if (!require("ggplot2", quietly = TRUE))
  install.packages("ggplot2")

if (!require("ggpattern", quietly = TRUE))
  install.packages("ggpattern")

if (!require("ggtext", quietly = TRUE))
  install.packages("ggtext")

if (!require("patchwork", quietly = TRUE))
  install.packages("patchwork")

if (!require("msm", quietly = TRUE))
  install.packages("msm")
