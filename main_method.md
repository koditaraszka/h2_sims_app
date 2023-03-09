---
title: "Age-of-Onset (Survival): Main Method"
output: html_document
---


```r
ageonset = function(N, M, h2, K, C, minAge, maxAge, shape){
  
  MAF=0.05
  frac = (K*N)/25
  maf = 1/frac
  MAF = max(MAF, maf)
  
  gen = gen_liab(N, M, C, h2, MAF)
  l_g = gen$liab
  cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
  l = l_g + l_e
  
  y = weibull(N, shape, l)
  t = quantile(y, probs = K)
  
  death = as.numeric(y <= t)
  
  y <- pmin(y, t)
  y = setage_survival(y, minAge, maxAge)
  
  return(list("Y"=death, "age"=y, "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts" = cuts))
  
}
```
