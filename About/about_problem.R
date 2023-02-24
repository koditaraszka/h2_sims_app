fluidRow(
  column(12,
         p("Quantifying the heritability of disease susceptibility is a central premise of genetics. Traditionally, analyses model case-control status as the phenotype and assume patients harbor a continuous liability for the disease with only cases exceeding the liability threshold. However, this presumes all individuals without the disease at the time of data collection (i.e. censoring) will remain controls indefinitely. Previous work has compared logistic regression and Cox Fraility regression and found the Cox Frailty resulted in increased GWAS discovery power; however, limited work has explored the estimation of time-to-event heritability. In practice, time-to-event phenotypes such as age-of-onset are treated as an case-only quantitative trait and analyzed using a linear mixed model (LMM). Here we compare three approaches to quantifying the heritability of disease susceptibility. Each patient has a time-to-event phenotype representing either the age-of-onset or censoring age and an indicator for whether the patient developed the disease or was censored."
          ),
         p("Will eventually add description of methods")
  )
)