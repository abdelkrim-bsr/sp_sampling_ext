
library(terra)
library(ggplot2)
library(dplyr)
library(moments) # skewness and kurtosis
library(patchwork)

library(raster)
library(rasterdiv)
library(doParallel)

library(tidyr)

## open target and covariate rasters
open_rast_as_df <- function(my_aoi){
  files <- 
    rst <- crop(rast( "./data/ABG1.tif"), my_aoi)
  rst[rst$ABG1==0] = NA
  rst = as.data.frame(rst, xy=T)
  rst = na.omit(rst)
  rst$ext = my_aoi$ext
  rst$nbr = my_aoi$nbr
  return(rst)
}

# Extents histograms and statistics
#//////////////////////////////////

exts_vec = vect("./data_vec/all_exts.shp")
extents = list()
for(ex in 1:length(exts_vec)){
  aoi = exts_vec[ex]
  extents[[ex]] = open_rast_as_df(aoi)
}

extents <- do.call(rbind,extents)

extents$extent = paste0(extents$ext,"-",extents$nbr)
extents$label = extents$ext
extents$label = gsub("^L","1_L",extents$label)
extents$label = gsub("^R","2_R",extents$label)
extents$label = gsub("^N","3_N",extents$label)
extents$nbr   = as.factor(extents$nbr)


# Table 2 - Descriptive statistics ----

# By individual extent
extents |> group_by(extent) |> 
  summarise(Min    = min(ABG1), 
            Median = median(ABG1),
            Mean   = mean(ABG1),
            SD     = sd(ABG1),
            Max    = max(ABG1),
            CV     = SD/Mean,
            skew   = skewness(ABG1),
            kurt   = kurtosis(ABG1)) |> 
  write.table("./res_csv/desc_stats.csv",sep=";",dec=",",row.names = F)

# By extent size
extents |> group_by(label) |> 
  summarise(Min    = min(ABG1), 
            Median = median(ABG1),
            Mean   = mean(ABG1),
            SD     = sd(ABG1),
            Max    = max(ABG1),
            CV     = SD/Mean,
            skew   = skewness(ABG1),
            kurt   = kurtosis(ABG1)) |> 
  write.table("./res_csv/desc_stats_one.csv",sep=";",dec=",",row.names = F)


head(extents);tail(extents)


# Fig.2 - Plot histograms ----

ext_labs <- c("Local", "Regional", "National")
names(ext_labs) <- c("1_Local", "2_Regional", "3_National")

ggplot(extents, aes(x=ABG1))+
  geom_histogram(binwidth = 15, fill=NA, colour="gray50",size=1)+
  geom_density(aes(y=15 * ..count..), colour="blue", size=1)+
  facet_grid(label~nbr, scales = "free_y",labeller = labeller(label = ext_labs))+
  labs(x="AGB", y="Count")+ theme_minimal()# + theme(axis.text = element_blank(),panel.grid = element_blank())







# Fig.6 - Plot maps and SDI ----
exts_vec = vect("./data_vec/all_exts.shp")
extents = list()
for(ex in 1:length(exts_vec)){
  aoi = exts_vec[ex]
  output_file = paste0("./data_ext/",aoi$ext,"_" ,aoi$nbr,".tif")
  crop(rast( "./data/ABG1.tif"), aoi, filename=output_file, overwrite=T)
  extents[[paste0(aoi$ext,"_" ,aoi$nbr)]] = raster(output_file)
} 
extents <- lapply(extents, function(x) {x[x==0] = NA; return(x)} )

# Shannon's Diversity Index
sha_win15na <- lapply(extents, function(x) Shannon(x,window=15,na.tolerance=0.1, np=3))
# saveRDS(sha_win15na,"sha_win15na.rds")
# sha_win15na = readRDS("sha_win15na.rds")

# Plot maps
reg = c("Regional_1","Regional_2","Regional_3","Regional_4","Regional_5")
ext_reg = extents[reg]
sha_reg = sha_win15na[reg]
ext_reg = lapply(ext_reg, rast)
sha_reg = lapply(sha_reg, rast)

par(mfrow=c(2,5))
plg_agb =list(title="AGB\n",shrink=0.15, cex=.8,title.cex = 1)
plg_sdi =list(title="SDI\n",shrink=0.15, cex=.8,title.cex = 1)
col = plotKML::SAGA_pal[[1]]
mar1=c(0,0.2,1.2,3)

range_exr=c(5,375)
range_sha=c(3,5.05)




plot(ext_reg$Regional_1, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Regional 1", cex.main=1)
plot(ext_reg$Regional_2, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Regional 2", cex.main=1)
plot(ext_reg$Regional_3, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Regional 3", cex.main=1)
plot(ext_reg$Regional_4, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Regional 4", cex.main=1)
plot(ext_reg$Regional_5, range=range_exr, axes=F, col = col, mar=mar1,plg=plg_agb, main= "Regional 5", cex.main=1)

plot(sha_reg$Regional_1, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_reg$Regional_2, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_reg$Regional_3, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_reg$Regional_4, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_reg$Regional_5, range=range_sha, axes=F, col = col, mar=mar1,plg=plg_sdi, main= "")



loc = c("Local_1","Local_2", "Local_3","Local_4", "Local_5")
ext_loc = extents[loc]
ext_loc = lapply(ext_loc, rast)

sha_loc = sha_win15na[loc]
sha_loc = lapply(sha_loc, rast)


range_exl=c(5,365)

plot(ext_loc$Local_1, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Local 1", cex.main=1)
plot(ext_loc$Local_2, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Local 2", cex.main=1)
plot(ext_loc$Local_3, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Local 3", cex.main=1)
plot(ext_loc$Local_4, range=range_exr, axes=F, col = col, mar=mar1,legend=FALSE, main= "Local 4", cex.main=1)
plot(ext_loc$Local_5, range=range_exr, axes=F, col = col, mar=mar1,plg=plg_agb, main= "Local 5", cex.main=1)


plot(sha_loc$Local_1, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_loc$Local_2, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_loc$Local_3, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_loc$Local_4, range=range_sha, axes=F, col = col, mar=mar1,legend=FALSE, main= "")
plot(sha_loc$Local_5, range=range_sha, axes=F, col = col, mar=mar1,plg=plg_sdi, main= "")




# Fig.7 - Box plot with letters ----
# ANOVA and TukeyHSD test
sha_loc_df = subset(sha_df,ext=="Local") |> na.omit()
aov_loc <- aov(sdi~nbr, data = sha_loc_df)
tukey_loc <- TukeyHSD(aov_loc)

sha_reg_df = subset(sha_df,ext=="Regional") |> na.omit()
aov_reg <- aov(sdi~nbr, data = sha_reg_df)
tukey_reg <- TukeyHSD(aov_reg)

get_letters = function(aov_m,tukey_m, df){
  cld = multcompLetters4(aov_m, tukey_m)
  Tk <- df |> dplyr::group_by(nbr) |> 
    dplyr::summarize(mean=mean(sdi), quant = quantile(sdi, probs = 0.75))  |> 
    dplyr::arrange(desc(mean))
  
  cld <- as.data.frame.list(cld$nbr)
  Tk$cld <- cld$Letters
  return(Tk)
}

loc = ggplot(sha_loc_df, aes(nbr, sdi)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE)+
  labs(x="Local extents", y="Shannon's Diversity Index")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = get_letters(aov_loc,tukey_loc,sha_loc_df),
            aes(x = nbr, y = quant, label = cld), size = 5, vjust=-1, hjust =-1)+
  scale_fill_brewer(palette = "Blues")

reg = ggplot(sha_reg_df, aes(nbr, sdi)) + 
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE)+
  labs(x="Regional extents", y="")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data = get_letters(aov_reg,tukey_reg,sha_reg_df),
            aes(x = nbr, y = quant, label = cld), size = 5, vjust=-1, hjust =-1)+
  scale_fill_brewer(palette = "Blues")

(loc + reg)


# Moran's I ----
rst_files = list.files("./data_ext/")
names(rst_files)<- gsub(".tif","",rst_files)

rst_moran = lapply(rst_files, function(fil){
  file = rast(paste0("./data_ext/",fil))
  file[file==0] = NA
  autocor(file)
})

rst_moran = rst_moran |> as.data.frame() |> round(3)
rownames(rst_moran)<-NULL
rst_moran |> write.csv("./global_moran.csv")






