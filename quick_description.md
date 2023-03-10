---
title: "Age-of-Onset (Survival): Quick Description"
output: html_document
---

The phenotype of interest, $A$, is a continuous time-to-event trait, typically age-of-onset for a disease, measured for $N$ unrelated individuals generated based on the Weibull distribution with a fixed shape parameter, $k \in \{1,2,3\}$ the slope, $r$, based on the liability threshold model, $Weibull(k,r)$. 

We assume that not everyone experiences the event. For individuals who do experience the event, $Y$ corresponds to the disease age-of-onset. For those who do not experience the event, $Y$ corresponds to the censoring age which we arbitrarily assume is being interviewed for the study. In order to distinguish between the two sources for the phenotype, each individual also has an indicator $E$ denoting whether the individual experienced the event. Another major underlying assumption is that every person would eventually experience the event if time approached infinity (e.g. everyone lived forever).

For individuals who do not experience the event, the age at censoring follows a Gaussian distribution. This distribution is centered such that approximately the expected number of events occurred. This can be thought of approximating the population proportion of cases under the case-control simulation framework.
