all_theme = theme_bw() + theme(strip.background = element_blank(), axis.text =element_text( size = 12, color = "black"), 
                               axis.title = element_text(size = 12, color = "black"), legend.text = element_text(size = 12, color = "black"), 
                               legend.title = element_text(size = 12, color = "black"), plot.title = element_text(hjust = 0.5, size=14, color="black"), 
                               strip.text = element_text(size = 12, color = "black"), plot.tag = element_text(size=12, color = "black", face = "bold"))

decile_colors = c("Decile 10" = "#a50026","Decile 9" = "#d73027","Decile 8" = "#f46d43", "Decile 7" = "#fdae61", "Decile 6" = "#fee090",
                  "Decile 5" = "#e0f3f8", "Decile 4" = "#abd9e9", "Decile 3" = "#74add1", "Decile 2" = "#4575b4", "Decile 1" = "#313695")

methods_colors = c("Cox Frailty" = "#e41a1c", "Case-Control Status" = "#ff7f00", "BoxCox Age-of-Onset" = "#377eb8", 
                   "Log Age-of-Onset" = "#4daf4a", "RINT Age-of-Onset" = "#984ea3")


age_dist = function(first, liab = F){
  if(liab){
    return(ggplot(first) + geom_histogram(aes(x=Age, y = ..count.., fill="#F8766D"), binwidth = 2, alpha =0.8) + 
             geom_density(aes(x=Age, y = 2*..count..)) + ylab("Count") + ggtitle("Age Distribution") + all_theme + theme(legend.position = "none"))
  }
  return(ggplot(first) + geom_histogram(aes(x=Age, y = ..count.., fill=Status), binwidth = 2, alpha =0.8) + 
    geom_density(aes(x=Age, y = 2*..count..)) + ylab("Count") + ggtitle("Age Distribution") + all_theme)
  
}

# Risk by Genetic Liability Plot
risk_bar = function(first, liab = F){

  if(liab){
    
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
    
    return(ggplot(data, aes(x=`Genetic Liability`, y=prop, fill = `Genetic Liability`)) + geom_bar(stat ='identity') + 
      scale_fill_manual(values = decile_colors) + ggtitle("Lifetime Risk") + all_theme + ylab("Proportion of Cases") + xlab("Genetic Liability Decile") +
      scale_x_discrete(labels=c(c("Decile 1" = "1", "Decile 2" = "2", "Decile 3" = "3", "Decile 4" = "4", "Decile 5" = "5", 
                                  "Decile 6" = "6", "Decile 7" = "7", "Decile 8" = "8", "Decile 9" = "9", "Decile 10" = "10")), drop =F))
  }
  
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
  
  return(ggplot(data, aes(x=`Genetic Liability`, y=prop, fill = `Genetic Liability`)) + geom_bar(stat ='identity') + 
    scale_fill_manual(values = decile_colors) + ggtitle("Lifetime Risk") + all_theme + ylab("Proportion of Cases") + xlab("Genetic Liability Decile") +
    scale_x_discrete(labels=c(c("Decile 1" = "1", "Decile 2" = "2", "Decile 3" = "3", "Decile 4" = "4", "Decile 5" = "5", 
                                "Decile 6" = "6", "Decile 7" = "7", "Decile 8" = "8", "Decile 9" = "9", "Decile 10" = "10")), drop =F))
  
}

# Cumulative Incidence Rate Plot
smooth_incidence = function(first, liab = F){
  
  if(liab){
    
    temp = data.frame(table(first$Age, first$`Genetic Liability`))
    temp = subset(temp, select = c(Var1, Var2))
    colnames(temp) = c("Age", "Genetic Liability")
    temp["Incidence"] = 0
    temp$Age = as.numeric(as.character(temp$Age))
    for(j in unique(first$`Genetic Liability`)){
      prev=length(which(first$`Genetic Liability`==j))
      add=0
      denom = 0
      for(i in sort(unique(temp$Age))){
        cases = dim(first[which(first$Age==i & first$`Genetic Liability`==j),])[1]
        remain = prev - cases
        denom = denom + remain + 0.5*cases # all lost mid year
        update = add + cases
        temp[which(temp$Age==i & temp$`Genetic Liability`==j), "Incidence"] = update
        prev = remain
        add = update
      }
      temp[which(temp$`Genetic Liability`==j), "Incidence"] = temp[which(temp$`Genetic Liability`==j), "Incidence"]/denom
    }
    temp$`Genetic Liability` = factor(temp$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                      levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
    
    return(ggplot(temp, aes(x=Age, y=Incidence, color=`Genetic Liability`)) + geom_line(show.legend = FALSE) + 
      scale_color_manual(values = decile_colors) + all_theme + ggtitle("Cumulative Incidence Rate"))
    
  }
  
  temp = data.frame(table(first$Age, first$`Genetic Liability`))
  temp = subset(temp, select = c(Var1, Var2))
  colnames(temp) = c("Age", "Genetic Liability")
  temp["Incidence"] = 0
  temp$Age = as.numeric(as.character(temp$Age))
  for(j in unique(first$`Genetic Liability`)){
    prev=length(which(first$`Genetic Liability`==j))
    add=0
    denom = 0
    for(i in sort(unique(temp$Age))){
      cases = dim(first[which(first$Age==i & first$`Genetic Liability`==j & first$Status=="Case"),])[1]
      controls = dim(first[which(first$Age==i & first$`Genetic Liability`==j & first$Status=="Control"),])[1]
      remain = prev - (cases + controls)
      denom = denom + remain + (0.5*cases + 0.5*controls) # all lost mid year
      update = add + cases
      temp[which(temp$Age==i & temp$`Genetic Liability`==j), "Incidence"] = update
      prev = remain
      add = update
    }
    temp[which(temp$`Genetic Liability`==j), "Incidence"] = temp[which(temp$`Genetic Liability`==j), "Incidence"]/denom
  }
  temp$`Genetic Liability` = factor(temp$`Genetic Liability`, labels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"),
                                    levels = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))
  
  incidence = ggplot(temp, aes(x=Age, y=Incidence, color=`Genetic Liability`)) + geom_line(show.legend = FALSE) + 
    scale_color_manual(values = decile_colors) + all_theme + ggtitle("Cumulative Incidence Rate")
  return(incidence)
  
}

#Overview Plots
overview = function(first, liab=F){
  cuts = first$cuts
  if(liab){
    
    first = data.frame(first$age, rep(1, length(first$Y)), first$Y, first$gen)
    colnames(first) = c("Age", "Status", "Liability", "GenLiab")
    first$Status = factor(first$Status, levels = c("Case", "Control"))
    
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
    
    ageDist = age_dist(first, T)
    risk = risk_bar(first, T)
    incidence = smooth_incidence(first, T)
    return(list('risk'=risk, 'incidence' = incidence, 'ageDist'=ageDist))
    
  }
  
  first = data.frame(first$age, first$Y, first$liab, first$gen)
  colnames(first) = c("Age", "Status", "Liability", "GenLiab")
  first[which(first$Status==0),'Status'] = "Control"
  first[which(first$Status==1),'Status'] = "Case"
  first$Status = factor(first$Status, levels = c("Control", "Case"))
  
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

plot_results = function(results, models, h2, sims, liab = F){
  
  if(liab){
    missing = NULL
    final = data.frame("Models" = rep(models,2), "Methods" = c(rep("REML", length(models)), rep("HE Reg", length(models))))
    if("1" %in% models){
      mods = models[models!=1]
      final = data.frame("Models" = c("1", rep(mods, 2)), "Methods" = c(rep("REML", 1+length(mods)), rep("HE Reg", length(mods))))
    }
    
    final["H2"] = NA
    final["SE"] = NA
    if("1" %in% models){
      f = dim(results[which(is.na(results$Tau)),])[1]
      if(f > 0){
        missing = c(missing, paste("Cox Frailty has", f, "missing simulations"))
      }
      final[which(final$Models=="1" & final$Methods=="REML"),"H2"] = mean(results$Tau, na.rm = T)
      final[which(final$Models=="1" & final$Methods=="REML"),"SE"] = sd(results$Tau, na.rm = T)/sqrt(sims-f)
    }
    
    if("2" %in% models){
      f = dim(results[which(is.na(results$BinGRM)),])[1]
      if(f > 0){
        missing = c(missing, paste("Case-Control Status has", f, "missing simulations"))
      }
      final[which(final$Models=="2" & final$Methods=="REML"),"H2"] = mean(results$BinGRMR, na.rm = T)
      final[which(final$Models=="2" & final$Methods=="REML"),"SE"] = sd(results$BinGRMR, na.rm = T)/sqrt(sims-f)
      final[which(final$Models=="2" & final$Methods=="HE Reg"),"H2"] = mean(results$BinGRMH, na.rm = T)
      final[which(final$Models=="2" & final$Methods=="HE Reg"),"SE"] = sd(results$BinGRMH, na.rm = T)/sqrt(sims-f)
    }
    
    if("3" %in% models){
      f = dim(results[which(is.na(results$QnormGRM)),])[1]
      if(f > 0){
        missing = c(missing, paste("RINT Age-of-Onset has", f, "missing simulations"))
      }
      final[which(final$Models=="3" & final$Methods=="REML"),"H2"] = mean(results$QnormGRMR, na.rm = T)
      final[which(final$Models=="3" & final$Methods=="REML"),"SE"] = sd(results$QnormGRMR, na.rm = T)/sqrt(sims-f)
      final[which(final$Models=="3" & final$Methods=="HE Reg"),"H2"] = mean(results$QnormGRMH, na.rm = T)
      final[which(final$Models=="3" & final$Methods=="HE Reg"),"SE"] = sd(results$QnormGRMH, na.rm = T)/sqrt(sims-f)
    }
    
    final$Models = factor(final$Models, levels = c( "1", "2", "3"),
                          labels = c("Cox Frailty", "Case-Control Status", "RINT Age-of-Onset"))
    final$Methods = factor(final$Methods, levels = c("REML", "HE Reg"))
    main_plot = ggplot(final, aes(x=Methods, y=H2, ymin=H2-SE, ymax=H2+SE, fill = Models, pattern = Methods)) + 
      geom_bar_pattern(position = position_dodge(1), color = "black", pattern_fill = "black", pattern_angle = 45, stat = 'identity', orientation = 'x',
                       pattern_density = 0.1, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6, show.legend = F) + scale_fill_manual(values=methods_colors) +
      scale_pattern_manual(values = c(REML = "none", `HE Reg` = "stripe")) + geom_errorbar(width=.2, position=position_dodge(1)) +
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
  final = data.frame("Models" = rep(models,2), "Methods" = c(rep("REML", length(models)), rep("HE Reg", length(models))))
  if("1" %in% models){
    mods = models[models!=1]
    final = data.frame("Models" = c("1", rep(mods, 2)), "Methods" = c(rep("REML", 1+length(mods)), rep("HE Reg", length(mods))))
  }
  
  final["H2"] = NA
  final["SE"] = NA
  if("1" %in% models){
    f = dim(results[which(is.na(results$Tau)),])[1]
    if(f > 0){
      missing = c(missing, paste("Cox Frailty has", f, "missing simulations"))
    }
    final[which(final$Models=="1" & final$Methods=="REML"),"H2"] = mean(results$Tau, na.rm = T)
    final[which(final$Models=="1" & final$Methods=="REML"),"SE"] = sd(results$Tau, na.rm = T)/sqrt(sims-f)
  }
  
  if("2" %in% models){
    f = dim(results[which(is.na(results$BinGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("Case-Control Status has", f, "missing simulations"))
    }
    final[which(final$Models=="2" & final$Methods=="REML"),"H2"] = mean(results$BinGRMR, na.rm = T)
    final[which(final$Models=="2" & final$Methods=="REML"),"SE"] = sd(results$BinGRMR, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="2" & final$Methods=="HE Reg"),"H2"] = mean(results$BinGRMH, na.rm = T)
    final[which(final$Models=="2" & final$Methods=="HE Reg"),"SE"] = sd(results$BinGRMH, na.rm = T)/sqrt(sims-f)
  }
  
  if("3" %in% models){
    f = dim(results[which(is.na(results$BoxCoxGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("BoxCox Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="3" & final$Methods=="REML"),"H2"] = mean(results$BoxCoxGRMR, na.rm = T)
    final[which(final$Models=="3" & final$Methods=="REML"),"SE"] = sd(results$BoxCoxGRMR, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="3" & final$Methods=="HE Reg"),"H2"] = mean(results$BoxCoxGRMH, na.rm = T)
    final[which(final$Models=="3" & final$Methods=="HE Reg"),"SE"] = sd(results$BoxCoxGRMH, na.rm = T)/sqrt(sims-f)
  }
  
  if("4" %in% models){
    f = dim(results[which(is.na(results$LogGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("Log Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="4" & final$Methods=="REML"),"H2"] = mean(results$LogGRMR, na.rm = T)
    final[which(final$Models=="4" & final$Methods=="REML"),"SE"] = sd(results$LogGRMR, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="4" & final$Methods=="HE Reg"),"H2"] = mean(results$LogGRMH, na.rm = T)
    final[which(final$Models=="4" & final$Methods=="HE Reg"),"SE"] = sd(results$LogGRMH, na.rm = T)/sqrt(sims-f)
  }
  
  if("5" %in% models){
    f = dim(results[which(is.na(results$QnormGRM)),])[1]
    if(f > 0){
      missing = c(missing, paste("RINT Age-of-Onset has", f, "missing simulations"))
    }
    final[which(final$Models=="5" & final$Methods=="REML"),"H2"] = mean(results$QnormGRMR, na.rm = T)
    final[which(final$Models=="5" & final$Methods=="REML"),"SE"] = sd(results$QnormGRMR, na.rm = T)/sqrt(sims-f)
    final[which(final$Models=="5" & final$Methods=="HE Reg"),"H2"] = mean(results$QnormGRMH, na.rm = T)
    final[which(final$Models=="5" & final$Methods=="HE Reg"),"SE"] = sd(results$QnormGRMH, na.rm = T)/sqrt(sims-f)
  }
  
  final$Models = factor(final$Models, levels = c( "1", "2", "3", "4", "5"),
                        labels = c("Cox Frailty", "Case-Control Status", "BoxCox Age-of-Onset", "Log Age-of-Onset", "RINT Age-of-Onset"))
  final$Methods = factor(final$Methods, levels = c("REML", "HE Reg"))
  main_plot = ggplot(final, aes(x=Methods, y=H2, ymin=H2-SE, ymax=H2+SE, fill = Models, pattern = Methods)) + 
    geom_bar_pattern(position = position_dodge(1), color = "black", pattern_fill = "black", pattern_angle = 45, stat = 'identity', orientation = 'x',
                                pattern_density = 0.1, pattern_spacing = 0.025, pattern_key_scale_factor = 0.6, show.legend = F) + scale_fill_manual(values=methods_colors) +
    scale_pattern_manual(values = c(REML = "none", `HE Reg` = "stripe")) + geom_errorbar(width=.2, position=position_dodge(1)) +
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

