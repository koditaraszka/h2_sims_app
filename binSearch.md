---
title: "Binary Search"
output: html_document
---


```r
# Binary Search for mean shift for censoring age
# Such that the censoring rate is +/- 0.01 cenRate

## Variables:
## N: number of individuals
## ageOnset: age-of-onset for everyone
## ageCensor: age-at-censoring for everyone
## cenRate: rate of censoring needed

binSearch = function(N, ageOnset, ageCensor, cenRate){
  # tolerance for observed censoring rate
  tol = 0.01
  
  minV = 0
  maxV = 1
  # starting at best guess not 0.5
  value = 1 - cenRate
  
  shifted = ageCensor + qnorm(value)
  rate = length(which(ageOnset >= shifted))/N
  while(!(cenRate - tol <= rate & cenRate + tol >= rate)){
    if(rate < cenRate){
      maxV = value
    } else{
      minV = value
    }
    value = (minV+maxV)/2
    shifted = ageCensor + qnorm(value)
    rate = length(which(ageOnset >= shifted))/N
  }
  return(shifted)
}
```
