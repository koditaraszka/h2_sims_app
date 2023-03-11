
setage_fullcc = function(data, minCen, maxCen, minOnset, maxOnset, informative){
  
  if(informative==1){
    
    # set age for controls
    controls = length(which(data$Y==0))
    cenStd = (maxCen-minCen)/4
    cenMean = mean(c(minCen, maxCen))
    
    controlQuants = runif(controls, 0.025, 0.975)
    cenAge = qnorm(controlQuants, cenMean, cenStd)
    
    # set age for cases
    cases = length(which(data$Y==1))
    onsetStd = (maxOnset-minOnset)/4
    onsetMean = mean(c(minOnset, maxOnset))
    
    casesQuants = runif(cases, 0.025, 0.975)
    onsetAge = qnorm(casesQuants, onsetMean, onsetStd)
    
  } else{
    
    controls = length(which(data$Y==0))
    cenStd = (maxCen-minCen)/4
    cenMean = mean(c(minCen, maxCen))
    controlQuants = runif(controls, 0.025, 0.975)
    cenAge = qnorm(controlQuants, cenMean, cenStd)
    
    # set age for cases
    p_liab = stats::pnorm(data$liab[which(data$Y==1)], lower.tail = F)
    move = -1*log(unique(data$K)/p_liab - 1)
    
    minMove= min(move)
    maxMove = max(move)
    
    onsetAge = minOnset + ((move - minMove)/(maxMove - minMove))*(maxOnset-minOnset)
    
  }
  
  age = rep(0, length(data$Y))
  age[which(data$Y==0)] = round(cenAge, 1)
  age[which(data$Y==1)] = round(onsetAge, 1)
  return(age)
  
}

fullcc = function(N, M, h2, K, P, C, minCen, maxCen, minOnset, maxOnset, informative){
  MAF=0.05
  if(P==1){
    frac=(K*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
    gen = gen_liab(N, M, C, h2, MAF)
    
    l_g = gen$liab
    cuts = quantile(l_g,probs=seq(0.1, 0.9, 0.1))
    
    l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
    l = l_g + l_e
    
    t = qnorm((1-K))
    Y = as.numeric(l >= t)
    
    data = data.frame('Y'=Y, 'liab'=l, 'K'=K)
    age = setage_fullcc(data, minCen, maxCen, minOnset, maxOnset, informative)
    
    return(list("Y"=Y, "age"=age, "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts"=cuts))
  } 
  
  frac = (0.5*N)/25
  maf = 1/frac
  MAF = max(MAF, maf)
  
  gen = gen_liab(N/K, M, C, h2, MAF)
  
  l_g = gen$liab
  cuts = quantile(l_g,probs=seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(N/K, 0, 1))
  l = l_g + l_e
  
  t = qnorm((1-K))
  Y = as.numeric(l >= t)
  
  keepCases = sample(which(Y==1), 0.5*N, replace=F)
  keepControls = sample(which(Y==0), 0.5*N, replace=F)
  
  Y = Y[sort(c(keepCases, keepControls))]
  gen$GRM = gen$GRM[sort(c(keepCases, keepControls)), sort(c(keepCases, keepControls))]
  
  l_g = l_g[sort(c(keepCases, keepControls))]
  l = l[sort(c(keepCases, keepControls))]
  
  # helper function for setting age
  data.frame('Y'=Y, 'liab'=l, 'K'=K)
  age = setage_fullcc(data, minCen, maxCen, minOnset, maxOnset, informative)
  
  return(list("Y"=Y, "age"=age, "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts"=cuts))
  
}

setage_partialcc = function(data, select, minAge, maxAge, informative){
  # select are the true cases, but observed as controls
  
  if(informative==1){
    
    meanAge = mean(c(minAge, maxAge))
    stdAge = (maxAge-minAge)/4
    
    controls = length(which(data$Y==0))
    controlQuants = runif(controls, 0.025, 0.975)
    cenAge = qnorm(controlQuants, meanAge, stdAge)
    
    # set age for cases
    cases = length(which(data$Y==1))
    casesQuants = runif(cases, 0.025, 0.975)
    caseAge = qnorm(casesQuants, meanAge, stdAge)
    
    age = rep(0, length(data$Y))
    age[which(data$Y==0)] = round(cenAge, 1)
    age[which(data$Y==1)] = round(caseAge, 1)
    
    return(age)
    
  }
  
  real_cases = which(data$Y==1)
  real_controls = which(data$Y==0)
  if(length(select)!=0){
    real_cases = sort(c(which(data$Y==1), select))
    real_controls = real_controls[-which(real_controls %in% select)]
  }
  
  p_liab = stats::pnorm(data$liab[real_cases], lower.tail = F)
  move = scale(-1*log(unique(data$K)/p_liab - 1))
  
  minMove= min(move)
  maxMove = max(move)
  
  onsetAge = minOnset + ((move - minMove)/(maxMove - minMove))*(maxOnset-minOnset)
  
  controls = length(real_controls)
  cenStd = (maxAge-minAge)/4
  cenMean = mean(c(minAge, maxAge))
  controlQuants = runif(controls, 0.025, 0.975)
  cenAge = qnorm(controlQuants, cenMean, cenStd)
  
  age = rep(0, length(data$Y))
  age[real_controls] = round(cenAge, 1)
  age[real_cases] = round(caseAge, 1)
  
  for(i in select){
    # since these people are unobserved, their age is < age of onset
    age[i] = round(runif(1, min=minAge, max=(age[i]-1)), 1) 
  }
  
  return(age)
  
}

partcc = function(N, M, h2, K, P, C, minAge, maxAge, unobserved, informative){
  # helper function for genetics
  # returns genetic liability and GRM
  MAF=0.05
  if(P==1){
    
    frac = (K*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
    gen = gen_liab(N, M, C, h2, MAF)
    l_g = gen$liab
    cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
    
    l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
    l = l_g + l_e
    
    t = qnorm((1-K))
    Y = as.numeric(l >= t)
    
    drop = round(unobserved*length(which(Y==1)))
    select = sample(which(Y==1), drop, replace = F)
    Y[select] = 0
    
    # helper function for setting age
    age = setage_partialcc(data.frame("Y" = Y, "liab" = l, 'K' = K), select, minAge, maxAge, informative)
    
    return(list("Y" = Y, "age" = age, "liab" = l, "gen" = l_g, "GRM" = gen$GRM, "cuts" = cuts))
  }
  
  frac = (0.5*N)/25
  maf = 1/frac
  MAF = max(MAF, maf)
  
  gen = gen_liab(2*N/K, M, C, h2, MAF)
  
  l_g = gen$liab
  cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(2*N/K, 0, 1))
  l = l_g + l_e
  
  t = qnorm((1-K))
  Y = as.numeric(l >= t)
  
  keepCases = sample(which(Y==1), (0.5+unobserved)*N, replace = F)
  keepControls = sample(which(Y==0), (0.5-unobserved)*N, replace = F)
  
  Y = Y[sort(c(keepCases, keepControls))]
  gen$GRM = gen$GRM[sort(c(keepCases, keepControls)), sort(c(keepCases, keepControls))]
  
  l_g = l_g[sort(c(keepCases, keepControls))]
  l = l[sort(c(keepCases, keepControls))]
  
  drop = round(unobserved*length(which(Y==1)))
  select = sample(which(Y==1), drop, replace = F)
  Y[select] = 0
  
  # helper function for setting age
  age = setage_partialcc(data.frame("Y" = Y, "liab" = l, 'K' = K), select, minAge, maxAge, informative)
  
  return(list("Y" = Y, "age" = age, "liab" = l, "gen" = l_g, "GRM" = gen$GRM, "cuts" = cuts))
  
} 

setage_liability = function(N, liability, minOnset, maxOnset, informative){
  
  if(informative==1){
    
    # set age for cases
    onsetStd = (maxOnset-minOnset)/4
    onsetMean = mean(c(minOnset, maxOnset))
    ageQuants = runif(N, 0.025, 0.975)
    onsetAge = qnorm(ageQuants, onsetMean, onsetStd)
    return(round(onsetAge, 1))
    
  }
  
  # set age for cases
  p_liab = stats::pnorm(liability, lower.tail = F)
  move = -1*log(1/p_liab - 1)
  
  minMove= min(move)
  maxMove = max(move)
  
  onsetAge = minOnset + ((move - minMove)/(maxMove - minMove))*(maxOnset-minOnset)
  return(round(onsetAge, 1))
  
}

liab = function(N, M, h2, C, minOnset, maxOnset, informative){
  MAF=0.05
  # helper function for genetics
  # returns genetic liability and GRM
  gen = gen_liab(N, M, C, h2, MAF)
  
  l_g = gen$liab
  cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
  l = l_g + l_e
  
  # helper function for setting age
  age = setage_liability(N, l, minOnset, maxOnset, informative)
  
  return(list("Y"=rep(1,N), "age"=age, "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts" = cuts))
  
}

setage_survival = function(onset, minAge, maxAge){
  
  maxTTE = max(onset)
  minTTE = min(onset)
  onset = minAge + (onset-minTTE)/(maxTTE-minTTE)*(maxAge - minAge)
  onset = round(onset, 1)
  
}

weibull = function(N, shape, scale){
  
  base = exp(scale)
  u = log(1-runif(N))
  return((-1*u/base)^(1/shape))
  
}

ageonset = function(N, M, h2, K, P, C, minAge, maxAge, shape){
  
  MAF=0.05
  if(P==1){
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
  
  frac = (0.5*N)/25
  maf = 1/frac
  MAF = max(MAF, maf)
  
  gen = gen_liab(N/K, M, C, h2, MAF)
  l_g = gen$liab
  cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(N/K, 0, 1))
  l = l_g + l_e
  
  y = weibull(N, shape, l)
  t = quantile(y, probs = K)
  
  death = as.numeric(y <= t)
  
  y <- pmin(y, t)
  
  keepCases = sample(which(death==1), 0.5*N, replace=F)
  keepControls = sample(which(death==0), 0.5*N, replace=F)
  
  death = death[sort(c(keepCases, keepControls))]
  gen$GRM = gen$GRM[sort(c(keepCases, keepControls)), sort(c(keepCases, keepControls))]
  
  l_g = l_g[sort(c(keepCases, keepControls))]
  l = l[sort(c(keepCases, keepControls))]
  
  y = y[sort(c(keepCases, keepControls))]
  y = setage_survival(y, minAge, maxAge)

  return(list("Y"=death, "age"=y, "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts" = cuts))
  
}