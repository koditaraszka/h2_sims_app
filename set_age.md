---
title: "Age-of-Onset (Survival): Set Age"
output: html_document
---


```r
setage_survival = function(onset, minAge, maxAge){
  
  maxTTE = max(onset)
  minTTE = min(onset)
  onset = minAge + (onset-minTTE)/(maxTTE-minTTE)*(maxAge - minAge)
  onset = round(onset, 1)
  
}
```
