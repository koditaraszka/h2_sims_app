# Helper Functions

## binary search
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

## convers observed scale to liabilty scale
obs2lia <- function(h2=NULL,K=NULL, P=NULL, reciprical = F){
  
  X <- qnorm(K,lower.tail=FALSE)
  z <- dnorm(X)
  num = K^2*(1-K)^2
  denom = P*(1-P)*z^2
  if(reciprical){
    return(h2*denom/num)
  }
  return(h2*num/denom)
  
}

## runs COXMEG and REML methods
runMethods_reml = function(data, models, k, method){
  
  tau = NA
  binGRM = NA
  logGRM = NA
  boxcoxGRM = NA
  qnormGRM = NA
  
  # onset split from casecontrol/liability in case no correction
  # currently everything has a correction
  
  if(method=='ageonset'){
    
    if("1" %in% models){
      tau = coxmeg(cbind(data$age, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
      tau = 2*tau/(1+tau)
    }
    
    # case-control status
    if("2" %in% models){
      binGRM = aiML(list(data$GRM), data$Y, c(0.5,0.5), verbose = F)$h2
      binGRM = obs2lia(binGRM, k, length(which(data$Y==1))/length(data$Y), F)
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      boxcoxGRM = aiML(list(data$GRM), scale(boxcox_y, center = T), c(0.5,0.5), verbose = F)$h2
      boxcoxGRM = 4*boxcoxGRM/(1+boxcoxGRM)
    }
    
    if("4" %in% models){
      log_y = log(age)
      logGRM = aiML(list(data$GRM), scale(log_y, center = T), c(0.5,0.5), verbose = F)$h2
      logGRM = 4*logGRM/(1+logGRM)
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
      qnormGRM = 4*qnormGRM/(1+qnormGRM)
    }
    
  }
  
  if(method=='casecontrol'){
    
    if("1" %in% models){
      tau = coxmeg(cbind(data$age, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
      #tau = 2*tau/(1+tau)
    }
    
    # case-control status
    if("2" %in% models){
      binGRM = aiML(list(data$GRM), data$Y, c(0.5,0.5), verbose = F)$h2
      binGRM = obs2lia(binGRM, k, length(which(data$Y==1))/length(data$Y), F)
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      boxcoxGRM = aiML(list(data$GRM), scale(boxcox_y, center = T), c(0.5,0.5), verbose = F)$h2
      boxcoxGRM = 4*boxcoxGRM/(1+boxcoxGRM)
    }
    
    if("4" %in% models){
      log_y = log(age)
      logGRM = aiML(list(data$GRM), scale(log_y, center = T), c(0.5,0.5), verbose = F)$h2
      logGRM = 4*logGRM/(1+logGRM)
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
      qnormGRM = 4*qnormGRM/(1+qnormGRM)
    }
    
  }
  
  if(method=='liab'){
    
    if("1" %in% models){
      tau = coxmeg(cbind(data$age, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
    }
    
    # case-control status
    if("2" %in% models){
      binGRM = aiML(list(data$GRM), data$Y, c(0.5,0.5), verbose = F)$h2
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      boxcoxGRM = aiML(list(data$GRM), scale(boxcox_y, center = T), c(0.5,0.5), verbose = F)$h2
    }
    
    if("4" %in% models){
      log_y = log(age)
      logGRM = aiML(list(data$GRM), scale(log_y, center = T), c(0.5,0.5), verbose = F)$h2
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
    }
    
  } 
  
  return(c(tau, binGRM, logGRM, boxcoxGRM, qnormGRM)) 
  
}

## runs HE Regression
runMethods_hereg = function(data, models, k, method){
  
  binGRM = NA
  logGRM = NA
  boxcoxGRM = NA
  qnormGRM = NA
  
  # onset split from casecontrol/liability in case no correction
  # currently everything has a correction
  if(method=='ageonset'){
    # case-control status
    if("2" %in% models){
      GRM2 = obs2lia(data$GRM, k, length(which(data$Y==1))/length(data$Y), T)
      GRM2 = GRM2[upper.tri(GRM2)]
      pp=scale(data$Y) %*% t(scale(data$Y))
      binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM2))$coef[2,1]
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    GRM = data$GRM[upper.tri(data$GRM)]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      pp=scale(boxcox_y) %*% t(scale(boxcox_y))
      boxcoxGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      boxcoxGRM = 4*boxcoxGRM/(1+boxcoxGRM)
    }
    
    if("4" %in% models){
      log_y = log(age)
      pp=scale(log_y) %*% t(scale(log_y))
      logGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      logGRM = 4*logGRM/(1+logGRM)
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      pp=qnorm_y %*% t(qnorm_y)
      qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      qnormGRM = 4*qnormGRM/(1+qnormGRM)
    }
  }
  
  if(method=='casecontrol'){
    # case-control status
    if("2" %in% models){
      GRM2 = obs2lia(data$GRM, k, length(which(data$Y==1))/length(data$Y), T)
      GRM2 = GRM2[upper.tri(GRM2)]
      pp=scale(data$Y) %*% t(scale(data$Y))
      binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM2))$coef[2,1]
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    GRM = data$GRM[upper.tri(data$GRM)]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      pp=scale(boxcox_y) %*% t(scale(boxcox_y))
      boxcoxGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      boxcoxGRM = 4*boxcoxGRM/(1+boxcoxGRM)
    }
    
    if("4" %in% models){
      log_y = log(age)
      pp=scale(log_y) %*% t(scale(log_y))
      logGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      logGRM = 4*logGRM/(1+logGRM)
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      pp=qnorm_y %*% t(qnorm_y)
      qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      qnormGRM = 4*qnormGRM/(1+qnormGRM)
    }
  }
  
  if(method=='liab'){
    # case-control status
    if("2" %in% models){
      GRM2 = data$GRM[upper.tri(data$GRM)]
      pp=scale(data$Y) %*% t(scale(data$Y))
      binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM2))$coef[2,1]
    }
    
    #cases-only
    samples = sort(which(data$Y==1))
    age = data$age[samples]
    data$GRM = data$GRM[samples, samples]
    GRM = data$GRM[upper.tri(data$GRM)]
    
    if("3" %in% models){
      bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
      lambda <- bc$x[which.max(bc$y)]
      boxcox_y = (age ^ lambda - 1) / lambda
      if(lambda==0){
        boxcox_y = log(age)
      }
      pp=scale(boxcox_y) %*% t(scale(boxcox_y))
      boxcoxGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    
    if("4" %in% models){
      log_y = log(age)
      pp=scale(log_y) %*% t(scale(log_y))
      logGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      pp=qnorm_y %*% t(qnorm_y)
      qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
  }
  
  return(c(binGRM, logGRM, boxcoxGRM, qnormGRM))
}

##xf calls runMethods_reml and runMethods_hereg
runMethods = function(data, models, k, method){
  
  reml = runMethods_reml(data, models, k, method)
  hereg = runMethods_hereg(data, models, k, method)
  return(c(reml, hereg))
  
}

# Liability Model
## set age
setage_liability = function(N, liability, ageDist, minOnset, maxOnset, cenRate, info){
  
  # Uninformative
  if(info==1){
    # reset liability to be random
    liability = rnorm(N)
  }  
  
  # logistic age-of-onset
  if(ageDist==1){
    p_liab = stats::pnorm(liability, lower.tail = F)
    move = -1*log(1/p_liab - 1)
  } 
  
  # Gaussian age-of-onset
  if(ageDist==2){
    move = -1*liability
  }
  
  minMove = min(move)
  maxMove = max(move)
  ageOnset = minOnset + ((move - minMove)/(maxMove - minMove))*(maxOnset - minOnset)
  
  cenAge = rep(Inf, N)
  if(cenRate>0){
    
    #pseudo-liability for censoring
    cenLiab = rnorm(N)
    
    # logistic age-of-censoring
    if(ageDist==1){
      p_liab = stats::pnorm(cenLiab, lower.tail = F)
      censor = -1*log(1/p_liab - 1)
    } 
    
    # Gaussian age-of-onset
    if(ageDist==2){
      censor = -1*cenLiab
    }
    
    cenAge = minOnset + ((censor - minMove)/(maxMove - minMove))*(maxOnset - minOnset)
    
    # helper function to choose mean shift to censor  +/- 0.01 of cenRate
    cenAge = binSearch(N, ageOnset, cenAge, cenRate)
    
  }
  
  return(list("onset"=ageOnset, "censor"=cenAge))
}

## main method
liability = function(N, M, h2, C, ageDist, minOnset, maxOnset, cenRate, info){
  
  MAF=0.05
  # helper function for genetic liability
  # returns: genetic liability and GRM
  gen = gen_liab(N, M, C, h2, MAF)
  
  l_g = gen$liab
  cuts = quantile(l_g,probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(N, 0, 1))
  l = l_g + l_e
  
  # helper function which returns age-of-onset and age-at-censoring
  # returns: age-of-onset and age-at-censoring
  age = setage_liability(N, l, ageDist, minOnset, maxOnset, cenRate, info)
  
  # indicator (Y=1 age-of-onset observed, Y=0, age-of-onset is censored)
  Y = as.numeric(age$onset<=age$censor)
  age <- pmin(age$onset, age$censor)
  
  return(list("Y"=Y, "age"=round(age, 1), "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts" = cuts))
  
}


# Case-Control Model
## set age
setage_casecontrol = function(Y, liability, choice, K, P, ageDist, minOnset, maxOnset, minCen, maxCen, info){
  
  # Uninformative
  if(info==1){
    # reset liability to be random
    liability = rnorm(N)
  }
  
  #pseudo-liability for censoring
  cenLiab = rnorm(length(which(Y==0)))
  
  # logistic age-of-onset
  if(ageDist==1){
    # cases
    p_liab = stats::pnorm(liability, lower.tail = F)
    move = -1*log(K/p_liab[which(Y==1)] - 1)
    
    # controls
    p_liab = stats::pnorm(cenLiab, lower.tail = F)
    censor = -1*log(1/p_liab - 1)
  } 
  
  # Gaussian age-of-onset
  if(ageDist==2){
    move = -1*liability
    censor = -1*cenLiab
  }
  
  
  minMove = min(move)
  maxMove = max(move)
  ageOnset = minOnset + ((move - minMove)/(maxMove - minMove))*(maxOnset - minOnset)
  
  minC = min(censor)
  maxC = max(censor)
  if(choice==1){
    cenAge = minOnset + ((censor - minC)/(maxC - minC))*(maxOnset - minOnset)
    
  } else{
    cenAge = minCen + ((censor - minC)/(maxC - minC))*(maxCen - minCen)
    
  } 
  
  age = rep(NA, length(Y))
  age[which(Y==1)] = ageOnset
  age[which(Y==0)] = cenAge
  return(age)
  
}  

## main method
casecontrol = function(N, M, h2, C, choice, K, P, ageDist, minOnset, maxOnset, unobs, minCen, maxCen, info){
  
  MAF=0.05
  simN = N
  # sample proportion equals population proportion
  if(P==1){
    frac=(K*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
    # going to down sample so only observe K cases
    if(choice==1){
      simN = N/K
    }
    
  } else{
    frac = (0.5*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
    #increase sim N so you can select 50/50 case-control
    simN = N/K # probably overkill generally but not when K is "big"
    
  }  
  
  # helper function for genetic liability
  # returns: genetic liability and GRM
  gen = gen_liab(simN, M, C, h2, MAF)
  
  l_g = gen$liab
  cuts = quantile(l_g,probs=seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(simN, 0, 1))
  l = l_g + l_e
  
  t = qnorm((1-K))
  Y = as.numeric(l >= t)
  
  # down sample to 50/50 case-control 
  # no unobserved cases
  if(P!=1 | choice != 2){
    
    # random sample case-controls
    if(P==2 & choice==2){
      # P = 50% and all cases observed
      keepCases = sample(which(Y==1), 0.5*N, replace=F)
      keepControls = sample(which(Y==0), 0.5*N, replace=F)
      
    } else if(P==2 & choice==1){
      # P = 50% and not all cases observed
      # want unobs*0.5+0.5 case and 1-(unobs*0.5+0.5) controls
      switch = unobs*0.5
      keepCases = sample(which(Y==1), (switch+0.5)*N, replace=F)
      keepControls = sample(which(Y==0), (1-(switch+0.5))*N, replace=F)
      
    } else{
      # P == 1 & choice == 1
      # P = K and not all cases observed
      # want K+unobs*K casea and 1-(K+unobs*K) controls
      switch = unobs*K
      keepCases = sample(which(Y==1), (switch+K)*N, replace=F)
      keepControls = sample(which(Y==0), (1-(switch+K))*N, replace=F)
      
    }
    
    #subset the data based on the retained case/controls
    Y = Y[sort(c(keepCases, keepControls))]
    gen$GRM = gen$GRM[sort(c(keepCases, keepControls)), sort(c(keepCases, keepControls))]
    
    l_g = l_g[sort(c(keepCases, keepControls))]
    l = l[sort(c(keepCases, keepControls))]
    
  }
  
  # helper function which returns age-of-onset and age-at-censoring
  # returns: age-of-onset and age-at-censoring
  age = setage_casecontrol(Y, l, choice, K, P, ageDist, minOnset, maxOnset, minCen, maxCen, info)
  
  if(choice==1 & unobs>0){
    # select unobserved to switch from case to control
    select = sort(sample(which(Y==1), switch*N, replace = F))
    # move "true" cases to controls
    for(i in select){
      age[i] = runif(1, min=minOnset, max=age[i])
    }
  }
  
  return(list("Y"=Y, "age"=round(age, 1), "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts"=cuts))
  
}


# Age-of-onset
## Weibull
weibull = function(N, shape, scale){
  
  #set hazard based on liability
  base = exp(scale)
  
  # simulate baseline hazard
  u = log(1-runif(N))
  
  # return weibull distribution
  return((-1*u/base)^(1/as.numeric(shape)))
  
}

## set age
setage_survival = function(simN, K, onset, cen, minOnset, maxOnset, shape, info){
  
  # Uninformative
  if(info==1){
    # reset liability to be random
    liability = rnorm(simN)
    onset = weibull(simN, shape, liability)
    
  }
  
  if(cen==1){
    # all censored individuals are set to max age
    #using the quantile for K proportion of cases
    censor = quantile(onset, probs = K)
    
    # indicator for who was censored
    y = as.numeric(onset <= censor)
    onset = pmin(onset, censor)
    
    minTTE = min(onset)
    maxTTE = max(onset)
    onsetAge = minOnset + (onset-minTTE)/(maxTTE-minTTE)*(maxOnset - minOnset)
    
  } else{
    
    minTTE = min(onset)
    maxTTE = max(onset)
    onsetAge = minOnset + (onset-minTTE)/(maxTTE-minTTE)*(maxOnset - minOnset)
    
    # censoring is drawn from a distribution
    #pseudo-liability for censoring
    cenLiab = rnorm(simN)
    censor = weibull(simN, shape, cenLiab)
    cenAge = minOnset + ((censor - minTTE)/(maxTTE - minTTE))*(maxOnset - minOnset)
    
    # helper function to choose mean shift to censor  +/- 0.01 of censoring rate = 1-K
    cenAge = binSearch(simN, onsetAge, cenAge, 1-K)
    
    y = as.numeric(onsetAge <= cenAge)
    onsetAge = pmin(onsetAge, cenAge) 
    
  }
  
  return(list('age'=onsetAge, 'y' = y))
  
}

## main method
ageonset = function(N, M, h2, C, K, P, cen, minOnset, maxOnset, shape, info){
  
  MAF=0.05
  simN = N
  
  if(P==1){
    frac=(K*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
  } else{
    frac = (0.5*N)/25
    maf = 1/frac
    MAF = max(MAF, maf)
    
    #increase sim N so you can select 50/50 case-control
    simN = N/K # probably overkill generally but not when K is "big"
    
  }  
  
  gen = gen_liab(simN, M, C, h2, MAF)
  l_g = gen$liab
  cuts = quantile(l_g, probs = seq(0.1, 0.9, 0.1))
  
  l_e = sqrt(1-h2)*scale(rnorm(simN, 0, 1))
  l = l_g + l_e
  
  
  onset = weibull(simN, shape, l)
  values = setage_survival(simN, K, onset, cen, minOnset, maxOnset, shape, info)
  
  age = values$age
  y = values$y
  
  if(P==2){
    keepCases = sample(which(y==1), 0.5*N, replace=F)
    keepControls = sample(which(y==0), 0.5*N, replace=F)
    
    y = y[sort(c(keepCases, keepControls))]
    gen$GRM = gen$GRM[sort(c(keepCases, keepControls)), sort(c(keepCases, keepControls))]
    
    l_g = l_g[sort(c(keepCases, keepControls))]
    l = l[sort(c(keepCases, keepControls))]
    
    age = age[sort(c(keepCases, keepControls))]
  }
  
  return(list("Y"=y, "age"=round(age, 1), "liab"=l, "gen"=l_g, "GRM"=gen$GRM, "cuts" = cuts))
  
}

