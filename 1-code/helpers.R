
# helpers -----------------------------------------------------------------


# functions ---------------------------------------------------------------



f.barplot=function(data='data',var='var',var_name=''){
  
  # data=don_all
  # var='spik_fertility_bm'
  
  var_name=ifelse(var_name=='',var,var_name)
  sub=data%>%
    group_by(Treatment,Variety)%>%
    # summarize(MS_m=mean(MS),LAI_m=mean(LAI),LI_m=mean(LI),An_m=mean(An),Tr_m=mean(Tr),WUE_m=mean(WUE),
    #           MS_sd=sd(MS),LAI_sd=sd(LAI),LI_sd=sd(LI),An_sd=sd(An),Tr_sd=sd(Tr),WUE_sd=sd(WUE))%>%
    summarize(m=mean(get(var),na.rm=T),sd=sd(get(var),na.rm=T))%>%
    ungroup()
  
  perct=sub%>%
    arrange(Variety,Treatment)%>%
    group_by(Variety)%>%
    mutate(dif_m=(m-first(m))/first(m)*100)
  
  
  subPerc=merge(sub,perct,all.x=T)
  
  
  
  ### tuckey for each variety
  donV_all=NULL
  for (v in unique(data$Variety)){
    # v='V3'
    modelTuckeyV<-aov(get(var)~Treatment, data=data%>%
                        filter(Variety==v))
    
    outV <- HSD.test(modelTuckeyV,"Treatment", group=TRUE,console=F,
                     main="")
    
    resV=data.frame(outV$means)
    resGV=data.frame(outV$groups)
    colnames(resV)[1]=var
    colnames(resGV)[1]=var
    
    donV=merge(resV%>%
                 mutate(Variety=v,
                        Treatment=row.names(data.frame(outV$means))),resGV%>%mutate(Treatment=row.names(resGV)))
    
    donV_all=rbind(donV_all,donV)
    
  }
  
  
  donV_all=donV_all%>%
    group_by(Variety)%>%
    arrange(Variety,Treatment)%>%
    mutate(groupC=first(groups))%>%
    ungroup()%>%
    mutate(sig=ifelse(test = str_detect(string = groups,pattern = groupC),yes = 'ns',no = '*'))
  
  
  
  gr=ggplot()+
    geom_col(data=subPerc,aes(x=Treatment,y=m,fill=Treatment,group=paste(Variety,Treatment)),position='dodge',col=1)+
    geom_errorbar(data=subPerc,aes(x=Treatment,ymax=m+sd,ymin=m,group=paste(Variety,Treatment)),width=.2,position=position_dodge(.8))+
    geom_text(data=donV_all,aes(x=Treatment,y=1.1*get(var)+1.8*std,label=groups))+
    
    geom_label(data=subPerc%>%filter(Treatment!='C'),aes(x=Treatment,y=0.5*m,label=paste(round(dif_m),'%')),size=4)+
    facet_wrap(~Variety,scale='free_x',ncol=2)+
    ylab(var_name)+
    xlab('')+
    scale_color_manual(name='Treatment',values = color_Trt)+
    scale_fill_manual(name='Treatment',values = color_Trt)
  
  tab=donV_all%>%
    arrange(Variety,Treatment,groups)%>%
    mutate(value=paste0(ifelse(test = get(var)>10,yes = ifelse(test = get(var)>100,yes = round(get(var)),no = round(get(var),1)),no = get(var)),groups))%>%
    select(Variety,Treatment,value)
  colnames(tab)[colnames(tab)=='value']=var_name

  
  out=list(gr=gr,tab=tab)
  return(out)
}



f.sig=function(p){
  sig=ifelse(p<=0.001,'***',ifelse(p<=0.01,"**",ifelse(p<0.05,'*','ns')))
  return(sig)
}

# create new coord : inherit coord_polar
coord_radar <- 
  function(theta='x', start=0, direction=1){
    # input parameter sanity check
    match.arg(theta, c('x','y'))
    
    ggproto(
      NULL, CoordPolar, 
      theta=theta, r=ifelse(theta=='x','y','x'),
      start=start, direction=sign(direction),
      is_linear=function() TRUE)
  }


f.ACP=function(don=don,vars='vars',table_type=table_type){
  
  
  
  select=don%>%select(vars)
  
  #######acp2 normalized
  acp <- dudi.pca(select,scannf=F,nf=5)
  acpI <- inertia.dudi(acp,row.inertia=T,col.inertia=T)
  acp$l1####vecteurs propres des lignes
  acp$li####coordonées des lignes
  ###espace des variables (colonnes)
  acp$c1###vecteurs propres des colonnes
  acp$co###coordonées des colonnes
  acp$li$simu=rownames(acp$li)
  
  print(acp$co)
  
  dataEigen=data.frame(eigen=acpI$tot.inertia$inertia/sum(acpI$tot.inertia$inertia)*100,Component=1:length(acpI$tot.inertia$inertia))

  
  circle=data.frame(x=cos(seq(0,2*pi,length.out = 360)),y=sin(seq(0,2*pi,length.out = 360)),z=sin(seq(0,2*pi,length.out = 360)))
  dataCircle=data.frame(x=as.numeric(acp$co[,'Comp1']),y=as.numeric(acp$co[,'Comp2']),z=as.numeric(acp$co[,'Comp3']),var=rownames(acp$co['Comp2']))
  dataCircle$contrib=ifelse(dataCircle$x**2>dataCircle$y**2,dataCircle$x**2,dataCircle$y**2)
  
  dataCircle=merge(dataCircle,table_type)
  
  gr_vp=ggplot(data=dataEigen,aes(x=Component,y=eigen))+
    geom_col()+
    geom_text(aes(label=round(eigen)), vjust=1.6, color="white", size=3.5)+
    xlab('Principal component')+
    ylab('EigenValue')
  

  gr_corr12=ggplot(data=dataCircle,aes(x = x,y = y))+
    geom_path(data=circle,aes(x = x,y = y))+
    geom_segment(aes(x=0,y=0,xend=x,yend=y,col=type),arrow = arrow(length = unit(0.3,"cm")))+
    geom_text_repel(data=dataCircle,aes(x = x,y = y,col=type,label=var),segment.colour = NA,parse=T)+
    ylim(limits=c(-1,1))+
    xlim(limits=c(-1,1))+
    ylab(paste('PC2 (',round(dataEigen[2,'eigen']),'%)',sep=''))+
    xlab(paste('PC1 (',round(dataEigen[1,'eigen']),'%)',sep=''))+
    # scale_color_manual(name='',values =colors_var )+
    theme(legend.position='right')
  
  gr_corr13=ggplot(data=dataCircle,aes(x = x,y = z))+
    geom_path(data=circle,aes(x = x,y = z))+
    geom_segment(aes(x=0,y=0,xend=x,yend=z,col=type),arrow = arrow(length = unit(0.3,"cm")))+
    geom_text_repel(data=dataCircle,aes(x = x,y = z,col=type,label=var),segment.colour = NA,parse=T)+
    ylim(limits=c(-1,1))+
    xlim(limits=c(-1,1))+
    ylab(paste('PC3 (',round(dataEigen[3,'eigen']),'%)',sep=''))+
    xlab(paste('PC1 (',round(dataEigen[1,'eigen']),'%)',sep=''))+
    # scale_color_manual(name='',values =colors_var )+
    theme(legend.position='right')
  
  
  sumItot=round(sum(acpI$tot.inertia$inertia[1:2]/sum(acpI$tot.inertia$inertia)*100))
  sumI1=round(sum(acpI$tot.inertia$inertia[1]/sum(acpI$tot.inertia$inertia)*100))
  sumI2=round(sum(acpI$tot.inertia$inertia[2]/sum(acpI$tot.inertia$inertia)*100))
  sumI3=round(sum(acpI$tot.inertia$inertia[3]/sum(acpI$tot.inertia$inertia)*100))
  
  select$x=acp$li$Axis1
  select$y=acp$li$Axis2
  select$z=acp$li$Axis3
  
  
  data=merge(x=don,y =select,all.x=T)%>%
    dplyr::select(all_of(c('Id','Plant','Variety','Treatment',vars,'x','y','z')))%>%
    na.omit()%>%
    droplevels()
  
  
  
  
  
  
  find_med=function(x,y){
    
    fram=data.frame(x=x,y=y)
    res=kmeans(fram[,c('x','y')],centers=1)$centers
    colnames(res)=c('x_mid','y_mid')
    return(res)
  }
  
  data=data%>%
    group_by(Treatment,Variety)%>%
    mutate(x_mid=find_med(x = x,y = y)[1,1],
           y_mid=find_med(x = x,y = y)[1,2],
           x_midz=find_med(x = x,y = z)[1,1],
           z_mid=find_med(x = x,y = z)[1,2])%>%
    ungroup()
  
  
  
  gr_proj12=data%>%
    ggplot()+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)+
    geom_point(aes(x = x,y = y,col=Treatment),alpha=0.6,size=4)+ ##to adapt with rep name
    geom_label(aes(x = x_mid,y = y_mid,fill=Treatment,label=paste(Variety)),alpha=0.4,size=4)+
    geom_segment(aes(x = x_mid,xend=x,yend=y,y = y_mid,col=Treatment,alpha=0.4))+
    guides(col=guide_legend(title=''))+
    xlab(paste('PC1 (',sumI1,'%)',sep=''))+
    # theme(legend.position=c(0.8,0.9))+
    scale_color_manual(values = color_Trt)+
    scale_fill_manual(values = color_Trt)+
    ylab(paste('PC2 (',sumI2,'%)',sep=''))+
    theme(legend.title = element_blank())
  
  gr_proj13=data%>%
    ggplot()+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)+
    geom_point(aes(x = x,y = z,col=Treatment),alpha=0.6,size=4)+ ##to adapt with rep name
    geom_label(aes(x = x_midz,y = z_mid,fill=Treatment,label=paste(Variety)),alpha=0.4,size=4)+
    geom_segment(aes(x = x_midz,xend=x,yend=z,y = z_mid,col=Treatment,alpha=0.4))+
    guides(col=guide_legend(title=''))+
    scale_color_manual(values = color_Trt)+
    scale_fill_manual(values = color_Trt)+
    xlab(paste('PC1 (',sumI1,'%)',sep=''))+
    # theme(legend.position=c(0.8,0.9))+
    ylab(paste('PC3 (',sumI3,'%)',sep=''))+
    theme(legend.title = element_blank())
  
  
  res=list(gr_vp=gr_vp,gr_corr12=gr_corr12,gr_corr13=gr_corr13,gr_proj12=gr_proj12,gr_proj13=gr_proj13)
  return(res)
  
}
