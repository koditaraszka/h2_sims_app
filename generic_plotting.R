rmarkdown::render("genetic_liability.Rmd")
rmarkdown::render("FullyCaseControl/main_method.Rmd")
rmarkdown::render("FullyCaseControl/sanity_check.Rmd")
rmarkdown::render("FullyCaseControl/set_age.Rmd")
rmarkdown::render("LiabilityOnly/main_method.Rmd")
rmarkdown::render("LiabilityOnly/sanity_check.Rmd")
rmarkdown::render("LiabilityOnly/set_age.Rmd")
rmarkdown::render("TimeToDisease/main_method.Rmd")
rmarkdown::render("TimeToDisease/weibull.Rmd")

all_theme = theme_bw() + theme(strip.background = element_blank(), axis.text =element_text( size = 12, color = "black"), 
                               axis.title = element_text(size = 12, color = "black"), legend.text = element_text(size = 12, color = "black"), 
                               legend.title = element_text(size = 12, color = "black"), plot.title = element_text(hjust = 0.5, size=14, color="black"), 
                               strip.text = element_text(size = 12, color = "black"), plot.tag = element_text(size=12, color = "black", face = "bold"))

decile_colors = c("Decile 10" = "#a50026","Decile 9" = "#d73027","Decile 8" = "#f46d43", "Decile 7" = "#fdae61", "Decile 6" = "#fee090",
                  "Decile 5" = "#e0f3f8", "Decile 4" = "#abd9e9", "Decile 3" = "#74add1", "Decile 2" = "#4575b4", "Decile 1" = "#313695")

methods_colors = c("Cox Frailty" = "#e41a1c", "Case-Control Status" = "#ff7f00", "BoxCox Age-of-Onset" = "#377eb8", 
                   "Log Age-of-Onset" = "#4daf4a", "RINT Age-of-Onset" = "#984ea3")
# Age Distribution Plot

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

age_dist = function(first){
  
  return(ggplot(first) + geom_histogram(aes(x=Age, y = ..count.., fill=Status), binwidth = 2, alpha =0.8) + 
    geom_density(aes(x=Age, y = 2*..count..)) + ylab("Count") + ggtitle("Age Distribution") + all_theme)
  
}

age_dist_liab = function(first){
  
  return(ggplot(first) + geom_histogram(aes(x=Age, y = ..count.., fill="#F8766D"), binwidth = 2, alpha =0.8) + 
           geom_density(aes(x=Age, y = 2*..count..)) + ylab("Count") + ggtitle("Age Distribution") + all_theme + theme(legend.position = "none") )
  
}

# Risk by Genetic Liability Plot
risk_bar = function(first){

  data = first[which(first$Status=="Case"),] %>% count(`Genetic Liability`) %>% mutate(prop = n/nrow(first))
  for(i in c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10")){
    already = unique(data$`Genetic Liability`)
    if(!(i %in% already)){
      data = rbind(data, c(i, 0, 0))
    }
  }
  data$n = as.numeric(data$n)
  data$prop = as.numeric(data$prop)
  data$`Genetic Liability` = factor(data$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                    levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
  
  risk = ggplot(data, aes(x=`Genetic Liability`, y=prop, fill = `Genetic Liability`)) + geom_bar(stat ='identity') + 
    scale_fill_manual(values = decile_colors) + ggtitle("Lifetime Risk") + all_theme + ylab("Proportion of Cases") + xlab("Genetic Liability Decile") +
    scale_x_discrete(labels=c(c("Decile 1" = "1", "Decile 2" = "2", "Decile 3" = "3", "Decile 4" = "4", "Decile 5" = "5", 
                                "Decile 6" = "6", "Decile 7" = "7", "Decile 8" = "8", "Decile 9" = "9", "Decile 10" = "10")), drop =F)
  return(risk)
  
}

risk_bar_liab = function(first){
  
  data = first %>% count(`Genetic Liability`) %>% mutate(prop = n/nrow(first))
  for(i in c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10")){
    already = unique(data$`Genetic Liability`)
    if(!(i %in% already)){
      data = rbind(data, c(i, 0, 0))
    }
  }
  data$n = as.numeric(data$n)
  data$prop = as.numeric(data$prop)
  data$`Genetic Liability` = factor(data$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                    levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
  
  risk = ggplot(data, aes(x=`Genetic Liability`, y=prop, fill = `Genetic Liability`)) + geom_bar(stat ='identity') + 
    scale_fill_manual(values = decile_colors) + ggtitle("Lifetime Risk") + all_theme + ylab("Proportion of Cases") + xlab("Genetic Liability Decile") +
    scale_x_discrete(labels=c(c("Decile 1" = "1", "Decile 2" = "2", "Decile 3" = "3", "Decile 4" = "4", "Decile 5" = "5", 
                                "Decile 6" = "6", "Decile 7" = "7", "Decile 8" = "8", "Decile 9" = "9", "Decile 10" = "10")), drop =F)
  return(risk)
  
}

# Cumulative Incidence Rate Plot
smooth_incidence = function(first){
  
  temp = data.frame(table(first$Age, first$`Genetic Liability`))
  temp = subset(temp, select = c(Var1, Var2))
  colnames(temp) = c("Age", "Genetic Liability")
  temp["Incidence"] = 0
  temp$Age = as.numeric(as.character(temp$Age))
  for(j in unique(first$`Genetic Liability`)){
    prev=length(which(first$`Genetic Liability`==j))
    add=0
    for(i in sort(unique(temp$Age))){
      cases = dim(first[which(first$Age==i & first$`Genetic Liability`==j & first$Status=="Case"),])[1]
      controls = dim(first[which(first$Age==i & first$`Genetic Liability`==j & first$Status=="Control"),])[1]
      remain = prev - (cases + controls)
      denom = remain + (0.5*cases + 0.5*controls) # all lost mid year
      update = add + cases/denom
      temp[which(temp$Age==i & temp$`Genetic Liability`==j), "Incidence"] = update
      prev = remain
      add = update
    }
  }
  temp$`Genetic Liability` = factor(temp$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                    levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
  
  incidence = ggplot(temp, aes(x=Age, y=Incidence, color=`Genetic Liability`)) + geom_line(show.legend = FALSE) + 
    scale_color_manual(values = decile_colors) + all_theme + ggtitle("Cumulative Incidence Rate")
  return(incidence)
  
}

# Cumulative Incidence Rate Plot
smooth_incidence_liab = function(first){
  
  temp = data.frame(table(first$Age, first$`Genetic Liability`))
  temp = subset(temp, select = c(Var1, Var2))
  colnames(temp) = c("Age", "Genetic Liability")
  temp["Incidence"] = 0
  temp$Age = as.numeric(as.character(temp$Age))
  for(j in unique(first$`Genetic Liability`)){
    prev=50
    add=0
    for(i in sort(unique(temp$Age))){
      cases = dim(first[which(first$Age==i & first$`Genetic Liability`==j),])[1]
      remain = prev - cases
      denom = remain + 0.5*cases # all lost mid year
      update = add + cases/denom
      temp[which(temp$Age==i & temp$`Genetic Liability`==j), "Incidence"] = update
      prev = remain
      add = update
    }
  }
  temp$`Genetic Liability` = factor(temp$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                    levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
  
  incidence = ggplot(temp, aes(x=Age, y=Incidence, color=`Genetic Liability`)) + geom_line(show.legend = FALSE) + 
    scale_color_manual(values = decile_colors) + all_theme + ggtitle("Cumulative Incidence Rate")
  return(incidence)
  
}

#Overview Plots
overview = function(first, liab=F){
  
  if(liab){
    first = data.frame(first$age, rep(1, length(first$Y)), first$Y, first$gen)
    colnames(first) = c("Age", "Status", "Liability", "GenLiab")
    first$Status = factor(first$Status, levels = c("Case", "Control"))
    cuts = quantile(first$GenLiab,probs = seq(0.1, 0.9, 0.1))
    first["Genetic Liability"] = "Decile 1"
    first[which(first$GenLiab>cuts[1]),"Genetic Liability"] = "Decile 2"
    first[which(first$GenLiab>cuts[2]),"Genetic Liability"] = "Decile 3"
    first[which(first$GenLiab>cuts[3]),"Genetic Liability"] = "Decile 4"
    first[which(first$GenLiab>cuts[4]),"Genetic Liability"] = "Decile 5"
    first[which(first$GenLiab>cuts[5]),"Genetic Liability"] = "Decile 6"
    first[which(first$GenLiab>cuts[6]),"Genetic Liability"] = "Decile 7"
    first[which(first$GenLiab>cuts[7]),"Genetic Liability"] = "Decile 8"
    first[which(first$GenLiab>cuts[8]),"Genetic Liability"] = "Decile 9"
    first[which(first$GenLiab>cuts[9]),"Genetic Liability"] = "Decile 10"
    first$`Genetic Liability` = factor(first$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                       levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
    
    ageDist = age_dist_liab(first)
    risk = risk_bar_liab(first)
    incidence = smooth_incidence_liab(first)
    return(list('risk'=risk, 'incidence' = incidence, 'ageDist'=ageDist))
  
  } else {
    first = data.frame(first$age, first$Y, first$liab, first$gen)
    colnames(first) = c("Age", "Status", "Liability", "GenLiab")
    first[which(first$Status==0),'Status'] = "Control"
    first[which(first$Status==1),'Status'] = "Case"
    first$Status = factor(first$Status, levels = c("Control", "Case"))
    cuts = quantile(first$GenLiab,probs = seq(0.1, 0.9, 0.1))
    first["Genetic Liability"] = "Decile 1"
    first[which(first$GenLiab>cuts[1]),"Genetic Liability"] = "Decile 2"
    first[which(first$GenLiab>cuts[2]),"Genetic Liability"] = "Decile 3"
    first[which(first$GenLiab>cuts[3]),"Genetic Liability"] = "Decile 4"
    first[which(first$GenLiab>cuts[4]),"Genetic Liability"] = "Decile 5"
    first[which(first$GenLiab>cuts[5]),"Genetic Liability"] = "Decile 6"
    first[which(first$GenLiab>cuts[6]),"Genetic Liability"] = "Decile 7"
    first[which(first$GenLiab>cuts[7]),"Genetic Liability"] = "Decile 8"
    first[which(first$GenLiab>cuts[8]),"Genetic Liability"] = "Decile 9"
    first[which(first$GenLiab>cuts[9]),"Genetic Liability"] = "Decile 10"
    first$`Genetic Liability` = factor(first$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                       levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
    
    ageDist = age_dist(first)
    risk = risk_bar(first)
    incidence = smooth_incidence(first)
    return(list('risk'=risk, 'incidence'=incidence, 'ageDist'=ageDist))
  }

}

runMethods = function(data, models, k, lm, liab = F){

  GRM=NA
  if(liab){
    print('here')
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
        pp=data$liab %*% t(data$liab)
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
      pp=data$Y %*% t(data$Y)
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
      logGRM = aiML(list(data$GRM), scale(log_y, scale = F, center = T), c(0.5,0.5), verbose = F)$h2
    } else {
      pp=log_y %*% t(log_y)
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
      boxcoxGRM = aiML(list(data$GRM), scale(boxcox_y, scale = F, center = T), c(0.5,0.5), verbose = F)$h2
    } else {
      pp=boxcox_y %*% t(boxcox_y)
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

plot_results = function(results, models, h2, sims, liab = F){
  
  if(liab){
    missing = NULL
    final = data.frame("Models" = rep(models,1), "Scaling" = rep("Observed Scale", length(models)))
    final["H2"] = NA
    final["SE"] = NA
    if("1" %in% models){
      f = dim(results[which(is.na(results$Tau)),])[1]
      if(f > 0){
        missing = c(missing, paste("Cox Frailty has", f, "missing simulations"))
      }
      final[which(final$Models=="1" & final$Scaling=="Observed Scale"),"H2"] = mean(results$Tau, na.rm = T)
      final[which(final$Models=="1" & final$Scaling=="Observed Scale"),"SE"] = sd(results$Tau, na.rm = T)/sqrt(sims-f)
      final[which(final$Models=="1" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledTau, na.rm = T)
      final[which(final$Models=="1" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledTau, na.rm = T)/sqrt(sims-f)
    }

    if("3" %in% models){
      f = dim(results[which(is.na(results$QnormGRM)),])[1]
      if(f > 0){
        missing = c(missing, paste("RINT Age-of-Onset has", f, "missing simulations"))
      }
      final[which(final$Models=="3" & final$Scaling=="Observed Scale"),"H2"] = mean(results$QnormGRM, na.rm = T)
      final[which(final$Models=="3" & final$Scaling=="Observed Scale"),"SE"] = sd(results$QnormGRM, na.rm = T)/sqrt(sims-f)
      final[which(final$Models=="3" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledQnormGRM, na.rm = T)
      final[which(final$Models=="3" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledQnormGRM, na.rm = T)/sqrt(sims-f)
    }
    
    if("2" %in% models){
      f = dim(results[which(is.na(results$BinGRM)),])[1]
      if(f > 0){
        missing = c(missing, paste("Case-Control Status has", f, "missing simulations"))
      }
      final[which(final$Models=="2" & final$Scaling=="Observed Scale"),"H2"] = mean(results$BinGRM, na.rm = T)
      final[which(final$Models=="2" & final$Scaling=="Observed Scale"),"SE"] = sd(results$BinGRM, na.rm = T)/sqrt(sims-f)
      final[which(final$Models=="2" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledBinGRM, na.rm = T)
      final[which(final$Models=="2" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledBinGRM, na.rm = T)/sqrt(sims-f)
    }
    
    final$Models = factor(final$Models, levels = c( "1", "2", "3"),
                          labels = c("Cox Frailty", "Case-Control Status", "RINT Age-of-Onset"))
    final$Scaling = factor(final$Scaling, levels = c("Observed Scale", "Liability Scale"))
    main_plot = ggplot(final, aes(x=Scaling, y=H2, ymin=H2-SE, ymax=H2+SE, fill = Models)) + 
      geom_bar(stat="identity", color="black", position=position_dodge(1)) + geom_errorbar(width=.2, position=position_dodge(1)) + scale_fill_manual(values=methods_colors) +
      geom_hline(yintercept = h2) + ylab('Estimated Heritability') + ggtitle("Estimated Heritability by Method") + all_theme
    
    if(is.null(missing)){
      missing = "There are no missing data points due to singularity"
    } else{
      missing = paste(missing,collapse='\n')
    }
    
    text <- ggplot(data = data.frame(x = 1:2, y = 1:10)) +
      labs(subtitle = missing) +
      theme_void() +
      theme(
        plot.subtitle = ggtext::element_textbox_simple(
          hjust = 0,
          halign = 0,
          margin = margin(20, 0, 0, 0)
        ),
        plot.margin = margin(0, 0, 0, 0)
      )
    return(list('main'=main_plot, 'text'=text))
  }
  missing = NULL
  final = data.frame("Models" = rep(models,2), "Scaling" = c(rep("Observed Scale", length(models)), rep("Liability Scale", length(models))))
  final["H2"] = NA
  final["SE"] = NA
  if("1" %in% models){
    f = dim(results[which(is.na(results$Tau)),])[1]
    if(f > 0){
      missing = c(missing, paste("Cox Frailty has", f, "missing simulations"))
    }
    final[which(final$Models=="1" & final$Scaling=="Observed Scale"),"H2"] = mean(results$Tau, na.rm = T)
    final[which(final$Models=="1" & final$Scaling=="Observed Scale"),"SE"] = sd(results$Tau, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="1" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledTau, na.rm = T)
    final[which(final$Models=="1" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledTau, na.rm = T)/sqrt(sims-f)
  }
  
  if("3" %in% models){
    f = dim(results[which(is.na(results$BoxCoxGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("BoxCox Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="3" & final$Scaling=="Observed Scale"),"H2"] = mean(results$BoxCoxGRM, na.rm = T)
    final[which(final$Models=="3" & final$Scaling=="Observed Scale"),"SE"] = sd(results$BoxCoxGRM, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="3" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledBoxCoxGRM, na.rm = T)
    final[which(final$Models=="3" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledBoxCoxGRM, na.rm = T)/sqrt(sims-f)
  }
  
  if("4" %in% models){
    f = dim(results[which(is.na(results$LogGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("Log Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="4" & final$Scaling=="Observed Scale"),"H2"] = mean(results$LogGRM, na.rm = T)
    final[which(final$Models=="4" & final$Scaling=="Observed Scale"),"SE"] = sd(results$LogGRM, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="4" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledLogGRM, na.rm = T)
    final[which(final$Models=="4" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledLogGRM, na.rm = T)/sqrt(sims-f)
  }
  
  if("5" %in% models){
    f = dim(results[which(is.na(results$QnormGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("RINT Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="5" & final$Scaling=="Observed Scale"),"H2"] = mean(results$QnormGRM, na.rm = T)
    final[which(final$Models=="5" & final$Scaling=="Observed Scale"),"SE"] = sd(results$QnormGRM, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="5" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledQnormGRM, na.rm = T)
    final[which(final$Models=="5" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledQnormGRM, na.rm = T)/sqrt(sims-f)
  }
  
  if("2" %in% models){
    f = dim(results[which(is.na(results$BinGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("Case-Control Status has", f, "missing simulations"))
    }
    final[which(final$Models=="2" & final$Scaling=="Observed Scale"),"H2"] = mean(results$BinGRM, na.rm = T)
    final[which(final$Models=="2" & final$Scaling=="Observed Scale"),"SE"] = sd(results$BinGRM, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="2" & final$Scaling=="Liability Scale"),"H2"] = mean(results$ScaledBinGRM, na.rm = T)
    final[which(final$Models=="2" & final$Scaling=="Liability Scale"),"SE"] = sd(results$ScaledBinGRM, na.rm = T)/sqrt(sims-f)
  }
  
  final$Models = factor(final$Models, levels = c( "1", "2", "3", "4", "5"),
                        labels = c("Cox Frailty", "Case-Control Status", "BoxCox Age-of-Onset", "Log Age-of-Onset", "RINT Age-of-Onset"))
  final$Scaling = factor(final$Scaling, levels = c("Observed Scale", "Liability Scale"))
  main_plot = ggplot(final, aes(x=Scaling, y=H2, ymin=H2-SE, ymax=H2+SE, fill = Models)) + 
    geom_bar(stat="identity", color="black", position=position_dodge(1)) + geom_errorbar(width=.2, position=position_dodge(1)) + scale_fill_manual(values=methods_colors) +
    geom_hline(yintercept = h2) + ylab('Estimated Heritability') + ggtitle("Estimated Heritability by Method") + all_theme
  
  if(is.null(missing)){
    missing = "There are no missing data points due to singularity"
  } else{
    missing = paste(missing,collapse='\n')
  }
  
  text <- ggplot(data = data.frame(x = 1:2, y = 1:10)) +
    labs(subtitle = missing) +
    theme_void() +
    theme(
      plot.subtitle = ggtext::element_textbox_simple(
        hjust = 0,
        halign = 0,
        margin = margin(20, 0, 0, 0)
      ),
      plot.margin = margin(0, 0, 0, 0)
    )
  return(list('main'=main_plot, 'text'=text))
  
}

