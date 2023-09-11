library(ggplot2)
library(colorspace)
library(dplyr)
library(tidyr)
library(patchwork)

hcl_palettes(plot = TRUE)


# Building the metrics table ----

metric_df <- readRDS("./res_metrics/metric_df.rds")
metric_df$extent = metric_df$ext
metric_df$ext = gsub("^L","1_L",metric_df$ext)
metric_df$ext = gsub("^R","2_R",metric_df$ext)
metric_df$ext = gsub("^N","3_N",metric_df$ext)

rbind(metric_df |> head(), metric_df |> tail())


# Metrics comparison ----


# wrapper function for plotting Fig.3 and Fig.4

labels <- c("Local","Regional","National")

gg_mc <- function(i){
  if(i== "R2"){
    p <- ggplot(metric_df,aes(x = as.factor(size),y=R2, fill=as.factor(ext)))+
      ylab(expression("R" ^ "2"))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  if(i== "MEC"){
    p <- ggplot(metric_df,aes(x = as.factor(size),y=MEC, fill=as.factor(ext)))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  if(i== "RMSE"){
    p <- ggplot(metric_df,aes(x = as.factor(size),y=RMSE, fill=as.factor(ext)))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    }
  if(i== "ME"){
    p <- ggplot(metric_df,aes(x = as.factor(size),y=ME, fill=as.factor(ext)))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  
    p + geom_boxplot(colour="gray35")+
  facet_wrap(sdgn~., ncol=2, scales = "free_x")+
  scale_fill_manual(name = "Extents", labels = labels,
                      values = diverge_hcl(3, palette = "Blue-Red 2"))+
  xlab("Sample size")+ theme_minimal()
 
}


# Fig.3 - ME and RMSE ----
(gg_mc("ME")/ gg_mc("RMSE")) +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom') 

# Fig.4 - R2 and MEC ----
(gg_mc("R2")/ gg_mc("MEC")) +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom') 


# Fig.5 -  The mean RMSE of 100 repetitions -----
labels <- c("1_Local" = "Local","2_Regional" = "Regional","3_National" = "National")

metric_df |> group_by(sdgn, ext, size) |> 
  summarize(me_m = mean(RMSE), me_sd= sd(RMSE)) |> 
  
  ggplot(aes(x = size, color=sdgn)) + 
  geom_line(aes(y = me_m, color = sdgn), size = 1.2) + 
  geom_ribbon(aes(y = me_m, ymin = me_m - me_sd, ymax = me_m + me_sd, fill = sdgn), alpha = .25) +
  facet_wrap(ext~.,labeller = labeller(ext = labels))+
  scale_fill_manual(name = "Sampling Design", values = diverge_hcl(2, palette = "Tropic"))+
  scale_color_manual(name = "Sampling Design",values = diverge_hcl(2, palette = "Tropic"))+
  ylab("RMSE")+
  theme_bw()+ 
  # geom_hline(yintercept = 0, color= "grey")+
  theme(legend.position="bottom")






# Fig.8 ----
metric_df$ext_label = paste0(metric_df$extent, metric_df$ext_nbr)

labels <- c("Local","Regional")

metric_df |> filter(extent!="National") |> 
  
  ggplot(aes(x = as.factor(size),y=RMSE, fill=as.factor(ext_nbr)))+
    geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray") +
    geom_boxplot()+
    facet_grid(extent~sdgn, scales = "free_x")+
    scale_fill_manual(name = "Extents",# labels = labels,
                      values = qualitative_hcl(5, palette = "Dark 3"))+
    scale_color_manual(name = "Extents",# labels = labels,
                       values = qualitative_hcl(5, palette = "Dark 3"))+
    xlab("Sample size")+ theme_minimal() + 
  theme(legend.position='bottom') 



# Supplementary materials ----
## Fig.S1 ----

labels <- c("L","R","N")

gg_mi <- function(i){
  if(i== "R2"){
    p <-  metric_df |>  group_by(ext, size, sdgn) |> 
      summarize(R2 =mean(R2))|> ungroup() |> 
      ggplot(aes(x = as.factor(ext),y=R2,group=sdgn,colour=sdgn))+
      ylab(expression("R" ^ "2"))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  
  if(i== "ME"){ 
    p <-  metric_df |>  group_by(ext, size, sdgn) |> 
      summarize(ME = mean(ME))|> ungroup() |> 
      ggplot(aes(x = as.factor(ext),y=ME,group=sdgn,colour=sdgn))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  
  if(i== "MEC"){ 
    p <-  metric_df |>  group_by(ext, size, sdgn) |> 
      summarize(MEC = mean(MEC))|> ungroup() |> 
      ggplot(aes(x = as.factor(ext),y=MEC,group=sdgn,colour=sdgn))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  }
  
  if(i== "RMSE"){
    p <-  metric_df |>  group_by(ext, size, sdgn) |> 
      summarize(RMSE=mean(RMSE))|> ungroup() |> 
      ggplot(aes(x = as.factor(ext),y=RMSE,group=sdgn,colour=sdgn))+
      geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")
    
  } 
  
  p + geom_point()+  geom_line(size=0.8)+ facet_wrap(size~., ncol=6)+#, scales = "free_x")+
    scale_colour_manual(name = "Sample design", values = diverge_hcl(2, palette = "Blue-Red"))+
    scale_x_discrete(name = "Extents" , labels = labels) +
    theme_minimal()
}


(gg_mi("ME")/ gg_mi("RMSE") / gg_mi("R2") / gg_mi("MEC")) +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom') 

## Fig.S2 ----
metric_df |> 
  ggplot(aes(x = as.factor(size),y=RMSE, fill=as.factor(ext_nbr), color=as.factor(ext_nbr)))+
  geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")+
  geom_boxplot()+
  facet_grid(sdgn~extent)+
  scale_fill_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  scale_color_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  xlab("Sample size")+ theme_minimal()+theme(legend.position='bottom')

## Fig.S3 ----
metric_df |> 
  ggplot(aes(x = as.factor(size),y=ME, fill=as.factor(ext_nbr), color=as.factor(ext_nbr)))+
  geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")+
  geom_boxplot()+
  facet_grid(sdgn~extent)+
  scale_fill_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  scale_color_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  xlab("Sample size")+ theme_minimal()+theme(legend.position='bottom')



## Fig.S4 ----
metric_df |> 
  ggplot(aes(x = as.factor(size),y=R2, fill=as.factor(ext_nbr), color=as.factor(ext_nbr)))+
  geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")+
  geom_boxplot()+
  facet_grid(sdgn~extent)+
  scale_fill_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  scale_color_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  xlab("Sample size")+ theme_minimal()+theme(legend.position='bottom')


## Fig.S5 ----
metric_df |> 
  ggplot(aes(x = as.factor(size),y=MEC, fill=as.factor(ext_nbr), color=as.factor(ext_nbr)))+
  geom_hline(yintercept = c(0), linetype=1, size=1, colour="gray")+
  geom_boxplot()+
  facet_grid(sdgn~extent)+
  scale_fill_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  scale_color_manual(name = "Extents",values = qualitative_hcl(5, palette = "Dark 3"))+
  xlab("Sample size")+ theme_minimal()+theme(legend.position='bottom')


