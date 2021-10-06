gera_pieplot=function(dados,index){
  name=names(dados)[index]
  data=as.data.frame(table(dados[,index]))
  data=data[order(data$Var1,decreasing =T),]
  data$ypos=cumsum(data$Freq)-0.5*data$Freq
  n=sum(data$Freq)
  
  ggplot(data,
         aes(x='',
             fill=Var1,
             y=Freq))+
    geom_bar(width=10,stat='identity')+
    coord_polar('y',start=0)+
    geom_text(aes(y=ypos,
                  label = paste0(100*round(Freq/n,4),'%')),
              color = "white", size=6) +
    scale_fill_hue(name)+
    theme_void()
}

gera_kp_plot=function(dados,index){
  kp_est=survfit(Surv(dados$tenure,ifelse(dados$Churn=='Yes',0,1))~dados[,index])
  
  plot=ggplot()+
    labs(title='Função de sobrevivência estimada por Kaplan-Meier')+
    scale_y_continuous('Probabilidade de sobrevivência',limits=c(0.0,1.0))+
    scale_x_continuous('Tempo')+
    theme_bw()
  
  full_time=c()
  full_surv=c()
  full_names=c()
  full_r_time=c()
  full_lower=c()
  full_upper=c()
  full_r_names=c()
  marker=cumsum(c(1,kp_est$strata))
  for(i in c(1:length(names(kp_est$strata)))){
    name=substr(names(kp_est$strata)[i],16,5000)
    
    time=kp_est$time[marker[i]:(marker[i+1]-1)]
    upper=kp_est$upper[marker[i]:(marker[i+1]-1)]
    lower=kp_est$lower[marker[i]:(marker[i+1]-1)]
    surv=kp_est$surv[marker[i]:(marker[i+1]-1)]
    
    ribbon_time=sort(c(time-0.001,time+0.001),decreasing=F)[-c(1)]
    ribbon_lower=sort(c(lower,lower),decreasing=T,na.last=T)[-c(length(time)*2)]
    ribbon_upper=sort(c(upper,upper),decreasing=T,na.last=T)[-c(length(time)*2)]
    
    full_time=c(full_time,time)
    full_surv=c(full_surv,surv)
    full_names=c(full_names,rep(name,length(time)))
    
    full_r_time=c(full_r_time,ribbon_time)
    full_lower=c(full_lower,ribbon_lower)
    full_upper=c(full_upper,ribbon_upper)
    full_r_names=c(full_r_names,rep(name,length(ribbon_time)))
  }
  line_frame=data.frame(time=full_time,surv=full_surv,name=full_names)
  ribbon_frame=data.frame(time=full_r_time,lower=full_lower,upper=full_upper,name=full_r_names)
  
  
  plot=plot+geom_step(aes(x=line_frame$time,y=line_frame$surv,color=line_frame$name,fill=line_frame$name))+
    geom_ribbon(aes(x=ribbon_frame$time,
                    ymin=ribbon_frame$lower,
                    ymax=ribbon_frame$upper,
                    fill=ribbon_frame$name,
                    color=ribbon_frame$name),
                #color=NULL,
                alpha=0.5)
  
  ggplotly(plot)
}