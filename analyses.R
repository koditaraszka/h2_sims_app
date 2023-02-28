rmarkdown::render("genetic_liability.Rmd")
rmarkdown::render("FullyCaseControl/main_method.Rmd")
rmarkdown::render("FullyCaseControl/set_age.Rmd")
rmarkdown::render("LiabilityOnly/main_method.Rmd")
rmarkdown::render("LiabilityOnly/set_age.Rmd")
rmarkdown::render("TimeToDisease/main_method.Rmd")
rmarkdown::render("TimeToDisease/weibull.Rmd")

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

runMethods = function(data, models, k, lm, liab = F){
  
  GRM=NA
  if(liab){
    if(lm=="2"){
      GRM = data$GRM[upper.tri(data$GRM)]
    }
    tau = NA
    scale_tau = NA
    #coxmeg
    if("1" %in% models){
      tau = coxmeg(cbind(data$liab, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
    }
    
    binGRM = NA
    scale_binGRM = NA
    # case-control status
    if("2" %in% models){
      if(lm=="1"){
        binGRM = aiML(list(data$GRM), data$liab, c(0.5,0.5), verbose = F)$h2
      } else {
        pp=scale(data$liab) %*% t(scale(data$liab))
        binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      }
    }
    
    logGRM = NA
    scale_logGRM = NA
    
    boxcoxGRM = NA
    scale_boxcoxGRM = NA
    
    qnormGRM = NA
    scale_qnormGRM = NA
    if("3" %in% models){
      qnorm_y = qnorm(rank(data$liab, ties.method = "random")/(length(data$liab)+1))
      if(lm=="1"){
        qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
      } else {
        pp=qnorm_y %*% t(qnorm_y)
        qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
      }
    }
    return(c(tau, scale_tau, binGRM, scale_binGRM, logGRM, scale_logGRM, boxcoxGRM, scale_boxcoxGRM, qnormGRM, scale_qnormGRM))
  }
  
  tau = NA
  scale_tau = NA
  #coxmeg
  if("1" %in% models){
    tau = coxmeg(cbind(data$age, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
    scale_tau = 2*tau/(1+tau)
  }
  
  binGRM = NA
  scale_binGRM = NA
  # case-control status
  if("2" %in% models){
    if(lm=="1"){
      binGRM = aiML(list(data$GRM), data$Y, c(0.5,0.5), verbose = F)$h2
      scale_binGRM = obs2lia(binGRM, k, length(which(data$Y==1))/length(data$Y), F)
    } else {
      GRM2 = obs2lia(data$GRM, k, length(which(data$Y==1))/length(data$Y), T)
      GRM2 = GRM2[upper.tri(GRM2)]
      pp=scale(data$Y) %*% t(scale(data$Y))
      binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM2))$coef[2,1]
      scale_binGRM = NA
    }
  }
  
  #cases-only
  samples = sort(which(data$Y==1))
  age = data$age[samples]
  data$GRM = data$GRM[samples, samples]
  if(lm=="2"){
    GRM = data$GRM[upper.tri(data$GRM)]
  }
  logGRM = NA
  scale_logGRM = NA
  if("4" %in% models){
    log_y = log(age)
    if(lm=="1"){
      logGRM = aiML(list(data$GRM), scale(log_y, center = T), c(0.5,0.5), verbose = F)$h2
    } else {
      pp=scale(log_y) %*% t(scale(log_y))
      logGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    scale_logGRM = 4*logGRM/(1+logGRM)
  }
  
  boxcoxGRM = NA
  scale_boxcoxGRM = NA
  if("3" %in% models){
    bc = MASS::boxcox(lm(age ~ 1,x = TRUE, y = TRUE), plotit =F)
    lambda <- bc$x[which.max(bc$y)]
    boxcox_y = (age ^ lambda - 1) / lambda
    if(lambda==0){
      boxcox_y = log(age)
    }
    if(lm=="1"){
      boxcoxGRM = aiML(list(data$GRM), scale(boxcox_y, center = T), c(0.5,0.5), verbose = F)$h2
    } else {
      pp=scale(boxcox_y) %*% t(scale(boxcox_y))
      boxcoxGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    scale_boxcoxGRM = 4*boxcoxGRM/(1+boxcoxGRM)
    
  }
  
  qnormGRM = NA
  scale_qnormGRM = NA
  if("5" %in% models){
    qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
    if(lm=="1"){
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
    } else {
      pp=qnorm_y %*% t(qnorm_y)
      qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    scale_qnormGRM = 4*qnormGRM/(1+qnormGRM)
  }
  return(c(tau, scale_tau, binGRM, scale_binGRM, logGRM, scale_logGRM, boxcoxGRM, scale_boxcoxGRM, qnormGRM, scale_qnormGRM))
  
}


