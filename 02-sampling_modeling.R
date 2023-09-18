# packages ====
library(terra)
library(ranger)
library(doParallel)


# functions ===============

## eval function ====
eval = function(obs, pred){
    ME   = round(mean(pred - obs, na.rm = TRUE), digits = 3)
    RMSE = round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 3)
    R2   = round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 3)
    SSE  = sum((pred - obs) ^ 2, na.rm = TRUE)
    SST  = sum((obs - mean(obs, na.rm = TRUE)) ^ 2, na.rm = TRUE)
    MEC  = round((1 - SSE/SST), digits = 3)
  
  return(data.frame(ME = ME, RMSE = RMSE, R2 = R2, MEC = MEC))
}


## Simple Random Sampling (SRS) ====
SRS <- function(dat,n){
  idx = sample(nrow(dat),n)
  return(dat[idx,])
}

## Latin Hypercube Sampling (cLHS) =====
cLHS  <- function(dat,n){
  idx <- clhs::clhs(dat, size=n)
  return(dat[idx,])
}

## RF wrapper function =====
eval_rf <- function(ds,frml,newdata){
  RF <- ranger(formula = frml, data = ds)
  rf_pred_obj = predict(RF, data = newdata, type="response", )$predictions
  return(eval(obs = newdata$ABG1 , pred = rf_pred_obj))
}

## open target and covariate rasters
open_rast <- function(my_aoi, my_path='./data/'){
  files <- list.files(path = my_path, recursive = FALSE, pattern = "\\.tif$")
  rst <- terra::crop(terra::rast(paste0(my_path, files)), my_aoi)
  return(rst)
}

# Configuration ====

n_sizes = c(25, 50, 100, 200, 300, 500)
iter   = 100

# RF formula setting
predictors = c("AI_glob","CC_am","Clay","Elev","ETP_Glob","G_mean","NIR_mean",
               "OCS","Prec_am","Prec_Dm","Prec_seaso", "Prec_Wm","R_mean","Sand",
               "Sha_EVI","Slope","Soc","solRad_m","SolRad_sd","SWIR1_mean",
               "SWIR2_mean","T_am","T_mdq", "T_mwarmq","T_seaso","Terra_PP",
               "Vapor_m","Vapor_sd")
target="ABG1"
form_RF = as.formula(paste(target, "~", paste(predictors, collapse='+')))

exts = vect("./data_vec/all_exts.shp")
crs(exts)<- crs(rast("./data/ABG1.tif"))


# Starting point of sampling designs ====

metrics_df = data.frame()

cl <- makePSOCKcluster(70)
registerDoParallel(cl)

for(ex in 1:length(exts)){
  
  # data preparation
  aoi = exts[ex]
  rst_df = open_rast(aoi)
  rst_df[rst_df$ABG1==0] = NA
  rst_df = mask(rst_df, rst_df$ABG1)
  
  # convert to data frame
  rst_df = terra::as.data.frame(rst_df, xy=T)
  rst_df = na.omit(rst_df)
  
  print(paste0("Starting for extent: ",aoi$ext," - nbr : ",
                 aoi$nbr, " - cells : ",ncell(rst_df)/1000," x 10+e3 ====="))

  # Sampling designs running
  
  for (size in n_sizes) {
      for (i in 1:iter) {
        
        set.seed(size+i)
        
        pts <- SRS(dat=rst_df, n=size)
        SRS_rf_eval = eval_rf(ds=pts,frml=form_RF, newdata=rst_df)
        SRS_rf_eval$iter = i
        SRS_rf_eval$ext = aoi$ext
        SRS_rf_eval$ext_nbr = aoi$nbr
        SRS_rf_eval$sdgn = "SRS"
        SRS_rf_eval$size = size
        
        pts <- cLHS(dat=rst_df, n=size)
        LHS_rf_eval = eval_rf(ds=pts,frml=form_RF, newdata=rst_df)
        LHS_rf_eval$iter = i
        LHS_rf_eval$ext = aoi$ext
        LHS_rf_eval$ext_nbr = aoi$nbr
        LHS_rf_eval$sdgn = "cLHS"
        LHS_rf_eval$size = size

        metrics_df= rbind(metrics_df,SRS_rf_eval,LHS_rf_eval)

        message(paste0("Extent: ",aoi$ext," - nbr : ",aoi$nbr," Size: ",size," iter: ",i))	
      }#iter
    print(paste0("End of size: ",size, " ************"))
  }#size
  print(paste0("End of extent: ",aoi$ext," - nbr : ",aoi$nbr, " ************"))
  
}
stopCluster(cl)

saveRDS(metrics_df, "./res_metrics/metric_df.rds")




