library(openair)
split_year = 1800

sen_slope_custom <- function(df,var){
  output = df%>%
    group_by(LakeID)%>%
    dplyr::summarize(n = n(),
                     trend = NA,
                     sig = NA,
                     min_year = NA,
                     max_year = NA)
  for(lake in unique(df$LakeID)){
      print(paste(lake)) #, era))
      filt = df%>%
        mutate(year = year(date))%>%
        filter(LakeID==lake)
      filt = filt%>%
        filter(year>split_year)%>%
        dplyr::select(-year)
      if(length(unique(year(filt$date)))>=5){
        sen = TheilSen(filt, pollutant = var, avg.time = "year", plot = F)$data$res2
        output$trend[output$LakeID==lake]<-sen$slope[1]
        output$sig[output$LakeID==lake]<-sen$p[1]
        output$min_year[output$LakeID==lake]<-min(year(filt$date))
        output$max_year[output$LakeID==lake]<-max(year(filt$date))
      }
      #}
    #}
  }
  return(output)
}
