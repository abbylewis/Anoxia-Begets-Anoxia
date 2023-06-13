#'This code file contains several functions needed for the regression analyses in "11 - Regressions.Rmd"
#'
#'Function list
#'
#'Utilities: 
#'  na_to_zero()
#'  zscore()
#'  standardize_data()
#'Renamers (functions to rename variables in paper and poster figures): 
#'  renamer()
#'  renamer_poster()
#'AIC model selection: 
#'  aic_calculator_lmer()
#'  aic_calculator_onevar()
#'Within-lakes analysis
#'  mod_by_lake()
#'Plotting
#'  plot_effects_lmer()
#'  plot_effects_by_lake_lmer_ridge()


###
#Utilities
###

#Calculate the zscore of data in a vector
zscore = function(data, na.rm = T){(data-mean(data, na.rm = na.rm))/sd(data, na.rm = na.rm)}
#Sets NAs to 0
na_to_zero = function(vector){ifelse(is.na(vector),0,vector)}
#Standardize data
standardize_data = function(dataset,responses,potential_drivers){
  dataset = dataset%>%
    ungroup()%>%
    dplyr::select(all_of(c(potential_drivers,responses,"LakeID")))%>%
    filter(if_all(where(is.numeric),is.finite))%>%
    #group_by(LakeID)%>%
    #mutate(across(everything(),zscore))%>%
    mutate(across(-LakeID,zscore))
  return(dataset)
}

###
#Renamers
###

#Rename variables in paper figures
renamer = function(text_vect){
  output=text_vect
  i = 1
  while(i <= length(text_vect)){
    text = text_vect[i]
    if(text=="demand_lag")         output[i]="Oxygen demand (t-1)"
    if(text=="demand_mean")        output[i]="Hist. mean VHOD"
    if(text=="chla_lag")           output[i]="Epi. chl-a (t-1)"
    if(text=="chla_lag2")          output[i]="Epi. chl-a (t-2)"
    if(text=="Chla_ugL_EPI")       output[i]="Epi. chl-a"
    if(text=="TP_ugL_EPI")         output[i]="Epi. TP"
    if(text=="strat_TP_ugL_EPI")   output[i]="Epi. TP"
    if(text=="strat_TP_date_EPI")  output[i]="TP measurement\ndate (lake-year mean)"
    if(text=="Temp_C_HYPO")        output[i]="Hypo. temperature"
    if(text=="strat_Temp_C_HYPO")  output[i]="Hypo. temperature"
    if(text=="summer_temp")        output[i]="Summer air\ntemperature"
    if(text=="spring_temp")        output[i]="Spring air\ntemperature"
    if(text=="winter_temp")        output[i]="Winter air\ntemperature"
    if(text=="summer_precip")      output[i]="Summer\nprecipitation"
    if(text=="spring_precip")      output[i]="Spring\nprecipitation"
    if(text=="winter_precip")      output[i]="Winter\nprecipitation"
    if(text=="DO_demand_mgLd_HYPO")output[i]="Oxygen demand"
    if(text=="TP_ugL_HYPO")        output[i]="Hypo. TP"
    if(text=="strat_TP_ugL_HYPO")  output[i]="Hypo. TP\n(stratified)"
    if(text=="hypo_p_lag")         output[i]="Hypo. TP (t-1)"
    if(text=="epi_p_lag")          output[i]="Epi. TP (t-1)"
    if(text=="vhod5_lag")          output[i]="Temp-corrected VHOD (t-1)"
    if(text=="buoyancy_freq_EPI")  output[i]="Maximum\nbuoyancy freq."
    if(text=="strat_buoyancy_freq_EPI")  output[i]="Maximum\nbuoyancy freq."
    if(text=="do_lag")             output[i]="Hypo. DO (t-1)"
    if(text=="epi_n_lag")          output[i]="Epi. TN (t-1)"
    if(text=="DO_mgL_HYPO")        output[i]="Hypo. DO"
    if(text=="TN_ugL_EPI")         output[i]="Epi. TN"
    if(text=="DO_date_HYPO")       output[i]="Oxygen measurement\ndate (lake-year mean)"
    if(text=="SA_vol_ratio_HYPO")  output[i]="Hypo. surface area\nto volume ratio"
    i = i+1
  }
  return(output)
}

#Rename variables in poster figures
renamer_poster = function(text_vect){
  output=text_vect
  i = 1
  while(i <= length(text_vect)){
    text = text_vect[i]
    if(text=="demand_lag")         output[i]="Oxygen demand\n(previous year)"
    if(text=="demand_mean")        output[i]="Historical\nmean VHOD"
    if(text=="chla_lag")           output[i]="Surface phytoplankton\n(previous year)"
    if(text=="chla_lag2")          output[i]="Epi. chl-a (t-2)"
    if(text=="Chla_ugl_EPI")       output[i]="Epi. chl-a"
    if(text=="TP_ugL_EPI")          output[i]="Surface\nphosphorus"
    if(text=="Temp_C_HYPO")        output[i]="Bottom water\ntemperature"
    if(text=="strat_Temp_C_HYPO")  output[i]="Bottom water\ntemperature"
    if(text=="summer_temp")        output[i]="Summer air\ntemperature"
    if(text=="spring_temp")        output[i]="Spring air\ntemperature"
    if(text=="winter_temp")        output[i]="Winter air\ntemperature"
    if(text=="summer_precip")      output[i]="Summer\nprecipitation"
    if(text=="spring_precip")      output[i]="Spring\nprecipitation"
    if(text=="winter_precip")      output[i]="Winter\nprecipitation"
    if(text=="DO_demand_mgLd_HYPO")output[i]="Oxygen consumption"
    if(text=="TP_ugL_HYPO")         output[i]="Bottom water\nphosphorus"
    if(text=="hypo_p_lag")         output[i]="Bottom water\nphosphorus\n(previous year)"
    if(text=="epi_p_lag")          output[i]="Surface phsophorus\n(previous year)"
    if(text=="vhod5_lag")          output[i]="Temp-corrected VHOD (t-1)"
    if(text=="buoyancy_freq_EPI")  output[i]="Stratification strength"
    if(text=="do_lag")             output[i]="Hypo. DO (t-1)"
    if(text=="epi_n_lag")          output[i]="Epi. TN (t-1)"
    if(text=="DO_mgL_HYPO")        output[i]="Bottom water oxygen"
    if(text=="TN_ugL_EPI")         output[i]="Surface nitrogen"
    if(text=="DO_date_HYPO")       output[i]="Oxygen measurement\ndate (lake-year mean)"
    i = i+1
  }
  return(output)
}


###
# AIC model selection
###

# LMER model selection
aic_calculator_lmer = function(dataset,responses,potential_drivers,interaction="+"){
  drivers = c(paste(potential_drivers, collapse = " "))
  dataset = dataset%>%
    ungroup()%>%
    dplyr::select(all_of(c(potential_drivers,responses,"LakeID")))%>%
    filter(if_all(where(is.numeric),is.finite))%>%
    mutate(across(-LakeID,zscore))
  
  for(response in responses){
    if(sum(complete.cases(dataset))>0){
      model = lmer(as.formula(paste0(response, "~", paste0(paste0(potential_drivers, collapse = interaction),"+(1|LakeID)"))), data = dataset)
      AICcs = c(AICc(model))
    } else{
      AICcs = c(NA)
    }
    for (num_drivers in 1:(length(potential_drivers)-1)){
      driver_matrix = combn(potential_drivers,num_drivers)
      for(option_num in 1:ncol(driver_matrix)){
        if(sum(complete.cases(dataset[c(response,driver_matrix[,option_num])]))>0){
          model = lmer(as.formula(paste0(response, "~", paste0(paste0(driver_matrix[,option_num], collapse = interaction), "+(1|LakeID)"))), data = dataset)
          AICcs = c(AICcs, AICc(model))
        } else{
          AICcs = c(AICcs, NA)
        }
        drivers = c(drivers, paste(driver_matrix[,option_num], collapse = " "))
      }
    }
    if(response == responses[1]){
      output = data.frame(drivers,AICcs)
      colnames(output)=c("drivers",response)
    } else{
      output[response] = AICcs
    }
  }
  output = output%>%
    filter(!is.na(get(responses)))
  
  for(response in responses){
    best_val = min(output[response][!is.na(output[response])&output[response]>-Inf])
    print(paste0(response, ":"))
    print(paste0(output$drivers[output[response]<=best_val+2],": AIC = ",round(output[response][output[response]<=best_val+2])))
  }
}

#Model selection where models can only include one predictor
aic_calculator_onevar = function(dataset,responses,potential_drivers){
  dataset = dataset%>%
    ungroup()%>%
    dplyr::select(all_of(c(potential_drivers,responses)))%>%
    na.omit()%>%
    mutate(across(everything(),zscore))
  
  for(response in responses){
    AICcs = c()
    for(i in 1:length(potential_drivers)){
      model = lm(as.formula(paste0(response, "~", potential_drivers[i])), data = dataset)
      AICcs = c(AICcs, AICc(model))
    }
    if(response == responses[1]){
      output = data.frame(potential_drivers,AICcs)
      colnames(output)=c("drivers",response)
    } else{
      output[response] = AICcs
    }
  }
  output = output%>%
    filter(!is.na(get(responses)))
  for(response in responses){
    best_val = min(output[response][!is.na(output[response])&output[response]>-Inf])
    print(paste0(response, ":"))
    print(paste0(output$drivers[output[response]<=best_val+2],": AIC = ",round(output[response][output[response]<=best_val+2])))
  }
  return()
}

###
# Within-lakes analysis
###

#Run a given model at each lake and extract coefficient estimates
mod_by_lake = function(dataset,responses,selected_drivers,interaction = "+"){
  lakes = dataset%>%
    ungroup()%>%
    dplyr::select(all_of(c(selected_drivers,responses,"LakeID","Year")))%>%
    na.omit()%>%
    group_by(LakeID)%>%
    filter(length(unique(Year))>=10)
  lakes_using = unique(lakes$LakeID)
  output = data.frame(matrix(nrow = 0,ncol = 3+length(selected_drivers)))
  colnames(output) = c("Intercept",selected_drivers,"R2","n")
  for(lake in lakes_using){
    data = lakes%>%
      filter(LakeID == lake)%>%
      ungroup()%>%
      dplyr::select(all_of(c(selected_drivers,responses)))%>%
      na.omit()%>%
      mutate(across(everything(),zscore),
             across(everything(),na_to_zero))
    mod = lm(as.formula(paste0(responses, "~", paste0(selected_drivers, collapse = interaction))), data = data)
    output = output%>%full_join(data.frame(t(mod$coefficients))%>%
                                  rename(Intercept = 'X.Intercept.')%>%
                                  mutate(LakeID = lake,
                                         R2 = summary(mod)$r.squared,
                                         n = nrow(data)))
  }
  return(output)
}


###
#Plotting
###

plot_effects_lmer = function(mod, var_name, save = T, poster = F){
  effects = data.frame(summary(mod)$coefficients)%>%
    rename(std_error = Std..Error)
  effects$Var = row.names(effects)
  confs = data.frame(confint(mod))
  confs$Var = row.names(confs)
  effects2 = effects%>%
    left_join(confs)%>%
    rename(xmin = X2.5..,
           xmax = X97.5..)
  p = effects2%>%
    filter(!Var=="(Intercept)")%>%
    mutate(Var = if(poster){renamer_poster(Var)}else{renamer(Var)},
           Var = factor(Var, levels = Var[order(abs(Estimate))]))%>%
    ggplot(aes(y = Var,x=Estimate))+
    geom_vline(xintercept = 0)+
    geom_point()+
    geom_errorbar(aes(xmin=xmin, xmax = xmax),width=0.1)+
    ggtitle(var_name,
            subtitle = paste0("n = ",length(summary(mod)$residuals)," lake-years; n = ",summary(mod)$ngrps," lakes"))+
    theme_bw()+
    theme(axis.title.y = element_blank())
  if(save == T){
    var_name_save = gsub("\\/","",var_name)
    ggsave(paste0("../Figures/MLR/Parameter estimate-",var_name_save,".jpeg"),
           width = 4, height = 4, units = "in")
  }
  return(p)
}

plot_effects_by_lake_lmer_ridge = function(all_lakes, var_name, mod, poster = F){
  effects = all_lakes%>%
    pivot_longer(!c(LakeID,n,R2))%>%
    filter(!name%in%c("Intercept"))
  order = effects%>%
    mutate(Var = if(poster){renamer_poster(name)}else{renamer(name)})%>%
    group_by(Var)%>%
    dplyr::summarize(median = abs(median(value,na.rm = T)))%>%
    mutate(Var = factor(Var, levels = Var[order(median)]))
  effects_lmer = data.frame(summary(mod)$coefficients)%>%
    rename(std_error = Std..Error)
  effects_lmer$Var = row.names(effects_lmer)
  confs = data.frame(confint(mod))
  confs$Var = row.names(confs)
  effects_lmer2 = effects_lmer%>%
    left_join(confs)%>%
    rename(xmin = X2.5..,
           xmax = X97.5..)%>%
    filter(!Var=="(Intercept)")%>%
    mutate(Var = if(poster){renamer_poster(Var)}else{renamer(Var)},
           Var = factor(Var, levels = Var[order(abs(Estimate))]))
  p2 = effects%>%
    mutate(Var = if(poster){renamer_poster(name)}else{renamer(name)},
           Var = factor(Var, levels = effects_lmer2$Var[order(abs(effects_lmer2$Estimate))]))%>%
    ggplot(aes(y = Var))+
    geom_vline(xintercept = 0, color = "grey40", linewidth=2)+
    xlim(-1,1)+
    geom_density_ridges(
      aes(x=value),
      quantile_lines = TRUE,
      jittered_points = TRUE,
      alpha = 1,
      point_alpha = 1,
      fill = "grey96",
      point_color = "grey50",
      point_fill = "white",
      #aes(point_fill = Var),
      point_shape = 21,
      scale=0.8)+
    stat_density_ridges(
      quantile_lines = T,
      geom = "density_ridges_gradient", calc_ecdf = TRUE,aes(fill = factor(after_stat(quantile)),x=value),
      scale=0.8)+
    #geom_errorbar(aes(xmin=xmin, xmax = xmax, y = as.numeric(Var) -0.1),width=0.1, data = effects_lmer2, color = "grey40")+
    geom_point(aes(x=Estimate, 
                   y = as.numeric(Var)# -0.1
                   ), data= effects_lmer2, size = 2, color = "black",fill="white", shape = 21, stroke = 1.5)+
    ggtitle(var_name,
            subtitle = paste0("n = ",length(unique(effects$LakeID))," lakes"))+
    theme_bw()+
    scale_fill_viridis_d(alpha=0.2)+
    scale_color_viridis_c()+
    xlab("Estimate")+
    theme(axis.title.y = element_blank(),
          legend.position = "none",
          axis.text.y = element_text(vjust = 0))+
    scale_y_discrete(expand = expand_scale(add = c(.2,.8)))
  var_name_save = gsub("\\/","",var_name)
  ggsave(paste0("../Figures/MLR-lmer/Parameter estimate-",var_name_save,"-all_lakes-ridge.jpeg"),
         plot = p2,
         width = 4, height = 4, units = "in",bg="white")
  return(p2+ggtitle(var_name))
}
