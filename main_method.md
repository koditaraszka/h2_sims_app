---
title: "Age-of-Onset (Survival): Main Method"
output: html_document
---


```r
ageonset = function(N, M, h2, K, C, shape){
  MAF=0.05
  # helper function for genetics
  # returns genetic liability and GRM
  gen = gen_liab(N, M, C, h2, MAF)
  l_g = gen$liab
  l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
  l = l_g + l_e
  
  y = 45 + weibull(N, shape, l)
  sdY = sd(y)/4
  death = rep(1,N)
  
  t = quantile(y, probs = K) + rnorm(N, sd=sdY)
  
  death = as.numeric(y <= t)
  y <- pmin(y, t)
  
  return(list("Y"=death, "age"=y, "liab"=l, "gen"=l_g, "GRM"=gen$GRM))
  
}
```
