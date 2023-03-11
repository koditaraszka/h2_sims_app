# Utility function for calculating log of determinant
logdet <- function(p) {
  det_l <- determinant(p,log=T)
  
  if ( det_l$sign[1] == 1 ) return(det_l$modulus[1])
  else return(det_l$modulus[1] * -1);
}

# Performs checks that REML input is consistent
validate_input <- function( A , y , Var , X = NULL ) {
  N = length(y)
  
  # Check VC lengths
  if ( length(A) + 1 != length(Var) ) {
    cat("ERROR: Number of variance components doesn't match number of items in Kinship list\n")
    return(FALSE)
  }
  
  # Test each matrix
  for ( i in 1:length(A) ) {
    if ( !isSymmetric(A[[i]]) ) {
      cat("ERROR: Matrix ",i," is not symmetric\n",sep='')
      if ( isSymmetric( round(A[[i]],4)) ) {
        cat("Matrix ",i," is approximately symmetric, continuing\n",sep='')
      } else {
        return(FALSE)
      }
    }
    if ( nrow(A[[i]]) != N || ncol(A[[i]]) != N ) {
      cat("ERROR: Matrix ",i," is not the correct size - ",dim(A[[i]]),"\n",sep='')
      return(FALSE)
    }
  }
  
  # Test fixed effects
  if ( !is.null(X) ) {
    if ( nrow(X) != N ) { 
      cat("ERROR: Matrix of fixed effects is incorrect size - ",dim(X),"\n",sep='')
      return(FALSE)
    }
  }
  
  return( TRUE )
}

# Execute Average-Information ML (no fixed effects) until convergence
aiML <- function( A, y , Var , verbose=TRUE , CALC_SE=TRUE , BOUNDED=FALSE ){
  
  if ( !validate_input( A , y , Var ) ) { return(NA) }
  
  r <- length(A) + 1
  N <- length(y)
  
  # Add matrix of residuals to A
  A[[r]] <- diag(N)
  
  AI <- matrix(0, ncol=r, nrow=r)
  S <- matrix(0, ncol=r, nrow=r)
  s <- matrix(0, ncol=1, nrow=r)
  
  l_dif <- 10
  it <- 0
  
  Var <- c(var(y)) * Var
  
  # Perform a single iteration of EM-based REML to initiate parameters
  if(verbose) cat("Performing a single iteration of EM-REML\n")
  V <- 0
  for ( i in 1:r ) V <- V + A[[i]] * Var[i]
  #Vinv <- solve(V)
  Vinv <- try(solve(V),silent=TRUE)
  if ('try-error' %in% class(Vinv)){
    return( list( "h2" = NA , "vc" = NA ))
  }
  P <- Vinv
  logL <- -0.5 * ( logdet(V) + t(y) %*% P %*% y )
  
  if(verbose)
    cat("Prior values from EM-REML:")
  for ( i in 1:r ) {
    Var[i] <- (Var[i]^2 * t(y) %*% P %*% A[[i]] %*% P %*% y + sum(diag(Var[i]*diag(N) - Var[i]^2 * P %*% A[[i]])) )/N
    if(verbose)
      cat(" ",Var[i],sep='')
  }
  if(verbose)
    cat('\n')
  
  V <- 0
  for ( i in 1:r ) V <- V + A[[i]] * Var[i]
  #Vinv <- solve(V)
  Vinv <- try(solve(V),silent=TRUE)
  if ('try-error' %in% class(Vinv)){
    return( list( "h2" = NA , "vc" = NA ))
  }
  P <- Vinv
  logL <- -0.5 * ( logdet(V) + t(y) %*% P %*% y )
  
  if(verbose)
    cat ("EM:\t",logL,'\n',sep='')
  
  # Iterate AI REML until convergence
  # while ( abs(l_dif) >= 10^-4 & it < 100 ){
  
  # ** GCTA style:
  while ( it < 100 & ( abs(l_dif) >= 10^-4 | (abs(l_dif) < 10^-2 & l_dif < 0)) ){
    
    it <- it + 1
    
    # Average information matrix
    for ( i in 1:r ) {
      for( ii in 1:r ) {
        if ( i == r && ii == r ) AI[r,r] <- t(y) %*% P %*% P %*% P %*% y
        else if ( i == r ) AI[r,ii] <- t(y) %*% P %*% P %*% A[[ii]] %*% P %*% y
        else if ( ii == r ) AI[i,r] <- t(y) %*% P %*% A[[i]] %*% P %*% P %*% y
        else AI[i,ii] <- t(y) %*% P %*% A[[i]] %*% P %*% A[[ii]] %*% P %*% y
      }
    }
    AI <- 0.5*AI
    
    # Vector of first derivatives of log likelihood  function
    for ( i in 1:r ) {
      if ( i == r ) s[r,1] <- sum(diag(( P ))) - ( t(y) %*% P %*% P %*% y )
      else s[i,1] <- sum(diag(( P %*% A[[i]] ))) - ( t(y) %*% P %*% A[[i]] %*% P %*% y )
    }
    s <- -0.5*s
    
    # New variance components from AI and likelihood
    # Var <- Var + solve(AI) %*% s
    
    # ** GCTA style:
    solvedAI <- try(solve(AI),silent=TRUE)
    if ('try-error' %in% class(solvedAI)){
      return( list( "h2" = NA , "vc" = NA ))
    }
    if ( l_dif > 1 ) Var <- Var + 0.316*(solvedAI %*% s)
    else Var <- Var + solvedAI %*% s
    
    # Re-calculate V and P matrix
    V <- 0
    for ( i in 1:r ) V <- V + A[[i]] * Var[i]
    #Vinv <- solve(V)
    Vinv <- try(solve(V),silent=TRUE)
    if ('try-error' %in% class(Vinv)){
      return( list( "h2" = NA , "vc" = NA ))
    }
    P <- Vinv 
    
    # Likelihood of the MLM (ignoring constants)
    new_logL <- -0.5 * ( logdet(V) + t(y) %*% P %*% y )
    l_dif <- new_logL - logL
    logL <- new_logL
    
    if(verbose) {
      cat(it,'\t',logL,sep='')
      for( i in 1:r ) cat( '\t',Var[i],sep='' )
      cat('\n')
    }
    
    if(BOUNDED) {
      if( min(Var/sum(Var)) < 0 ) {
        if(verbose) cat("VC has escaped parameter space, bounding and exiting\n")
        break()
      }
    }
  }
  if(verbose)
    cat('\n')
  
  if ( !CALC_SE ) {
    return( list( "h2" = Var[1]/sum(Var) , "vc" = Var ))
    
  } else {
    
    # Calculate matrix for standard errors (same as AI matrix but w/out y)
    for( i in 1:r ) {
      for ( ii in 1:r ) {
        S[i,ii] <- sum(diag(P %*% A[[i]] %*% P %*% A[[ii]] ))
      }
    }
    S <- 0.5*S
    #Sinv <- solve(S)
    Sinv <- try(solve(S),silent=TRUE)
    if ('try-error' %in% class(Sinv)){
      return( list( "h2" = NA , "vc" = NA ))
    }
    if(verbose){
      for( i in 1:r ) cat( "V(G",i,")\t",Var[i],'\t',sqrt(Sinv[i,i]),'\n',sep="")
    }
    
    # Construct string equation for delta method "~x1+x2 ... +xn"
    sum_all.eq = ""
    for(i in 1:r) {
      if ( i == 1 ) sum_all.eq <- paste(sum_all.eq,"x",i,sep='')
      else sum_all.eq <- paste(sum_all.eq,"+x",i,sep='')
    }
    SE.p <- deltamethod( as.formula(paste("~",sum_all.eq)),Var,Sinv,ses=T)
    
    if( verbose ) {
      cat( "Vp\t",sum(Var),'\t',SE.p,'\n',sep="")
    }
    
    SE.i <- rep(0,r)
    
    for( i in 1:r ) {
      # Construct string equation for delta method "~xi/(x1 ... +xn)"
      SE.eq <- paste("~x",i,"/(",sum_all.eq,")",sep='')
      SE.i[i] <- deltamethod(as.formula(SE.eq),Var,Sinv,ses=T)
      
      if(verbose)
        cat( "V(G",i,")/Vp\t",Var[i]/sum(Var),'\t',SE.i[i],'\n',sep='')
    }
    
    return( list( "h2" = Var[1]/sum(Var) , "se" = SE.i , "vc" = Var ))
  }
}

# simulate the genetic liability
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

# convert observed scale to liability scale
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
    
  if(method == 'liab'){
    # time-to-event
    if("1" %in% models){
      tau = coxmeg(cbind(data$liab, data$Y), corr = data$GRM, spd = F, type = 'dense', solver = 'NM', verbose = F)$tau
    }
    
    # case-control status
    if("2" %in% models){
      binGRM = aiML(list(data$GRM), data$liab, c(0.5,0.5), verbose = F)$h2
    }
      
    # quantile normalized
    if("3" %in% models){
      qnorm_y = qnorm(rank(data$liab, ties.method = "random")/(length(data$liab)+1))
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
    }
    return(c(tau, binGRM, logGRM, boxcoxGRM, qnormGRM))
  }

  #coxmeg
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
  
    return(c(tau, binGRM, logGRM, boxcoxGRM, qnormGRM)) 
    
  }
  
  if(method=='casecontrol'){
    
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
    }
    
    if("4" %in% models){
      log_y = log(age)
      logGRM = aiML(list(data$GRM), scale(log_y, center = T), c(0.5,0.5), verbose = F)$h2
    }
    
    if("5" %in% models){
      qnorm_y = qnorm(rank(age, ties.method = "random")/(length(age)+1))
      qnormGRM = aiML(list(data$GRM), qnorm_y, c(0.5,0.5), verbose = F)$h2
    }
    
    return(c(tau, binGRM, logGRM, boxcoxGRM, qnormGRM)) 
    
  }
}

runMethods_hereg = function(data, models, k, method){
  
  binGRM = NA
  logGRM = NA
  boxcoxGRM = NA
  qnormGRM = NA
  
  GRM=NA
  if(method=='liab'){
    GRM = data$GRM[upper.tri(data$GRM)]
    
    # case-control status
    if("2" %in% models){
      pp=scale(data$liab) %*% t(scale(data$liab))
      binGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    
    if("3" %in% models){
      qnorm_y = qnorm(rank(data$liab, ties.method = "random")/(length(data$liab)+1))
      pp=qnorm_y %*% t(qnorm_y)
      qnormGRM = summary(lm(pp[upper.tri(pp)] ~ GRM))$coef[2,1]
    }
    
    return(c(binGRM, logGRM, boxcoxGRM, qnormGRM))
  }
  
  if(method=='ageonset' | method == 'casecontrol'){
  
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
  
    return(c(binGRM, logGRM, boxcoxGRM, qnormGRM))
  }
}

runMethods = function(data, models, k, method){
  
  reml = runMethods_reml(data, models, k, method)
  hereg = runMethods_hereg(data, models, k, method)
  return(c(reml, hereg))

}
