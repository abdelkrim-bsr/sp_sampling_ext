library(dplyr)
library(tidyr)



metric_df <- readRDS("./res_metrics/metric_df.rds")

metric_df$ext = gsub("^L","1_L",metric_df$ext)
metric_df$ext = gsub("^R","2_R",metric_df$ext)
metric_df$ext = gsub("^N","3_N",metric_df$ext)
metric_df$sdgn = gsub("SRS","1_SRS",metric_df$sdgn)
metric_df$sdgn = gsub("LHS","2_LHS",metric_df$sdgn)



df_me = metric_df |> group_by(ext, sdgn, size) |> 
  summarise(s1_Min = min(ME), s2_Median = median(ME), s3_Mean = mean(ME), s4_SD = sd(ME), s5_Max = max(ME)) |> 
  ungroup()
df_me$stat = "ME"

df_rmse = metric_df |> group_by(ext, sdgn, size) |> 
  summarise(s1_Min = min(RMSE), s2_Median = median(RMSE), s3_Mean = mean(RMSE), s4_SD = sd(RMSE), s5_Max = max(RMSE)) |> 
  ungroup()
df_rmse$stat =  "RMSE"

df_r2 = metric_df |> group_by(ext, sdgn, size) |> 
  summarise(s1_Min = min(R2), s2_Median = median(R2), s3_Mean = mean(R2), s4_SD = sd(R2), s5_Max = max(R2)) |> 
  ungroup()
df_r2$stat = "R2"

df_mec = metric_df |> group_by(ext, sdgn, size) |> 
  summarise(s1_Min = min(MEC), s2_Median = median(MEC), s3_Mean = mean(MEC), s4_SD = sd(MEC), s5_Max = max(MEC)) |> 
  ungroup()
df_mec$stat = "MEC"

metric_stat = rbind(df_me,df_rmse,df_r2,df_mec)
metric_stat |> head()
rm(df_me,df_rmse,df_r2,df_mec)

metric_stat = metric_stat |> gather(key="idx", value = "value", -stat, -ext, -size, -sdgn)
metric_stat |> head()




write.table(metric_stat,"./res_csv/metric_stat.csv",sep=";",dec=",",row.names = F)



summary_metric_df <- metric_df |> group_by(ext, sdgn, size) |> 
  summarize_all(list(Min=min, Median=median, Mean=mean, SD=sd,Max=max)) |> ungroup() |> 
  gather(key=metric, value= value,-ext, -size, -sdgn) |>
  separate(metric, c("metric","stat"), sep = "_")

