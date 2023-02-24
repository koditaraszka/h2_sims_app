# Library required for deltamethod computation of SE
library('msm')

# Utility function for calculating log of determinant
`logdet` <-
  function(p) {
    det_l <- determinant(p,log=T)
    
    if ( det_l$sign[1] == 1 ) return(det_l$modulus[1])
    else return(det_l$modulus[1] * -1);
  }

# Performs checks that REML input is consistent
`validate_input` <-
  function( A , y , Var , X = NULL ) {
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
# A = list of GRM
# y = vector of phenotype entries
# Var = initial variance components (percent)

`aiML` <-
  function( A, y , Var , verbose=TRUE , CALC_SE=TRUE , BOUNDED=FALSE ){
    
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
