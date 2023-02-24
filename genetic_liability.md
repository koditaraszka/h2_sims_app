---
output: html_document
runtime: shiny
---


```r
gen_liab = function(N, M, C, h2, MAF){
  
  # set allele frequeny uniformly
  aFreq = runif(M, min=MAF, max=(1-MAF))
  G = matrix(NA,nrow=N,ncol=M)
  for(m in 1:M){
    # 0, 1, 2 based on allele freq
    G[,m] = rbinom(N, 2, prob = aFreq[m])
  }
  X = scale(G)
  GRM = X %*% t(X) / M
  
  # non-causal SNPs beta = 0
  beta = rep(0, M)
  causal = sample(1:M, C*M)
  # causal SNPs beta normally distributed
  beta[causal] = rnorm(C*M,0,1)
  # var(xb) = h2
  xb = sqrt(h2)*scale(X %*% beta)
  return(list("liab"=xb, "GRM"=GRM))
  
}
```
