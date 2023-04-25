rmarkdown::render("genetic_liability.Rmd")
rmarkdown::render("binSearch.Rmd")
rmarkdown::render("caseControl/main_method.Rmd")
rmarkdown::render("caseControl/set_age.Rmd")
rmarkdown::render("liability/main_method.Rmd")
rmarkdown::render("liability/set_age.Rmd")
rmarkdown::render("ageOnset/main_method.Rmd")
rmarkdown::render("ageOnset/weibull.Rmd")
rmarkdown::render("ageOnset/set_age.Rmd")

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
      tau = coxmeg(cbind(data$age, data$Y), corr = data$GRM, spd = F, type = 'dense', detap='exact', solver=1, opt='bobyqa', verbose = F)$tau
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

runMethods = function(data, models, k, method){
  
  if(k==1){
    # if there are no controls don't run case-control
    models = models[-which(models==2)]
  }
  
  if(k==0){
    # if there are no cases don't run case-control/case only age-of-onset
    models = models[-which(models%in%c(2,3,4,5))]
  }
  
  reml = runMethods_reml(data, models, k, method)
  hereg = runMethods_hereg(data, models, k, method)
  return(c(reml, hereg))

}
