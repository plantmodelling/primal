



#---------------------------------------------------------------
#---------------------------------------------------------------
# GENERAL PARAMETERS FOR THE ANALYSIS
#---------------------------------------------------------------
#---------------------------------------------------------------

base.dir           <- "YOUR_DIR/www/analysis/"; setwd(base.dir)

# Random Forest parameters


reps <- 10                                # number of reps (to see the influence of the random choic of images)
train_num <- seq(100, 900, by=100)
vec.models <- c(20)                 # Vector with the number of models to try
vec.trees <- c(30)                  # Vector with the number of tree to try in each model
to_est <-c("tot_root_length",         # Vector of parameters to estimate with the machine learning
                "n_primary","tot_prim_length","mean_prim_length",
                "n_laterals","tot_lat_length"
                )

to_comp <-c("length",         # Vector of parameters to estimate with the machine learning
           "tip_count","length","length",
           "tip_count","length"
)

#---------------------------------------------------------------
#---------------------------------------------------------------
# LOADING LIBRARIES
#---------------------------------------------------------------
#---------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(shapes)
library(reshape2)
library(lattice)
library(latticeExtra)
library(MASS)
library(ggrepel)
library(plyr)
library(grid)
source("machine_learning.r")  
PkgTest(c("randomForest", "ggplot2", "gridExtra"))

#---------------------------------------------------------------
#---------------------------------------------------------------
# LOAD THE PROCESSED DATASETS
#---------------------------------------------------------------
#---------------------------------------------------------------

# 1. TESTING DATASET

test.data <- read.csv("test_data.csv")
test.data$image <- gsub(".rsml", "", test.data$image)
rs <- melt(test.data, id.vars = "image")

test.descr <- read.csv("test_estimators.csv")
test.descr$image <- gsub(".tif", "", test.descr$image)

imgs <- unique(test.descr$image)
test.data <- test.data[test.data$image %in% imgs,]
colnames(test.data)[colnames(test.data) == "width"] <- "true_width"
colnames(test.data)[colnames(test.data) == "depth"] <- "true_depth"

# Re-compute the index vector for further uses
trans.data <- test.data[grepl("DH00",test.data$image),]
trans.descr <- test.descr[grepl("DH00",test.descr$image),]

test.data <- test.data[!grepl("DH00",test.data$image),]
test.descr <- test.descr[!grepl("DH00",test.descr$image),]

descr_ind <- c(2:(ncol(test.descr))) # Indexes of the descriptors

test <- merge(test.descr, test.data, by = "image")

cols <- colnames(test)
for(c in cols) test <- test[!is.nan(test[[c]]),] # Remove the row containing  NA values
for(c in cols) test <- test[!is.na(test[[c]]),] # Remove the row containing  NA values



# 2. TRAINING DATASET

train.data <- read.csv("training_data.csv")
train.data$image <- gsub(".rsml", "", train.data$image)
rs <- melt(train.data, id.vars = "image")

train.descr <- read.csv("training_estimators.csv")
train.descr$image <- gsub(".tif", "", train.descr$image)

imgs <- unique(train.descr$image)
train.data <- train.data[train.data$image %in% imgs,]
colnames(train.data)[colnames(train.data) == "width"] <- "true_width"
colnames(train.data)[colnames(train.data) == "depth"] <- "true_depth"

train.data <- rbind(train.data, trans.data)
train.descr <- rbind(train.descr, trans.descr)

dlength <- ncol(train.descr)


descr_ind <- c(2:(ncol(train.descr))) # Indexes of the descriptors

train <- merge(train.descr, train.data, by = "image")
cols <- colnames(train)
for(c in cols) train <- train[!is.nan(train[[c]]),] # Remove the row containing  NA values
for(c in cols) train <- train[!is.na(train[[c]]),] # Remove the row containing  NA values



remove(train.data, train.descr, trans.data, trans.descr, c)

#---------------------------------------------------------------
#---------------------------------------------------------------
# RANDOM FOREST EVALUATION
#---------------------------------------------------------------
#---------------------------------------------------------------


#-------------------------------------------------------------------
# PARAMETERS TO ESTIMATE
#-------------------------------------------------------------------

to_est_ind <- match(to_est, colnames(train))
to_comp_ind <- match(to_comp, colnames(train))
descr_ind <- c(descr_ind, to_est_ind)

#-------------------------------------------------------------------
# RUN THE MODEL
#-------------------------------------------------------------------


rf_accuracy <- NULL
rf_accuracy_all <- NULL
direct_accuracy <- NULL

for(tn in train_num){
  for(r in c(1:reps)){
    message(paste0("Trying out with ",tn," training images. Repetition: ",r))
    message("------------------------------------")
    train.id <- sample(1:nrow(train), tn)

    subtrain <- train[train.id,]
    
    models <- GenerateModels(fname = NULL, 
                             mat.data = subtrain, 
                             vec.models = vec.models, 
                             vec.trees = vec.trees, 
                             vec.f = to_est_ind, 
                             vec.p = descr_ind)
    
    vec.weights <- rep(1, length(to_est))
    model <- SelectModel(models, vec.weights)
    
    #-------------------------------------------------------------------
    # GET THE ESTIMATIONS FROM THE MODEL AND THE EXPECTED ERROR
    #-------------------------------------------------------------------
    
    results <- PredictRFs(model, test[,c(2:dlength)])
    
    write.csv(results, paste0("02_data/20170515/ML_results",tn, ".csv"))
    write.csv(test, paste0("02_data/20170515/ML_tests",tn, ".csv"))

    #-------------------------------------------------------------------
    # COMPARE THE MODELS WITH THE BEST CORRESPONDING  SINGLE LINEAR ESTIMATION
    #-------------------------------------------------------------------
    
    metaPrec <- data.frame(var1=character(), var2=character(), r1=numeric(), r2=numeric(), type=character())
    
    sumdiff <- 0
    sumdiff2 <- 0
    sumr2 <- 0
    # Look at the quality of the estimations
    for(te in to_est){
      
      estimation <- results[[te]] 
      truth <- test[[te]]
      
      rel_diff <- ((truth - estimation) / truth)^2
      rel_diff[is.infinite(rel_diff)] <- 0
      rrmse <- sqrt(mean(rel_diff))

      rel_diff <- abs((truth - estimation) / truth)
      rel_diff[is.infinite(rel_diff)] <- 0
      rrmse2 <- mean(rel_diff)      
      
      sumdiff <- sumdiff + rrmse
      
      sumdiff2 <- sumdiff2 + rrmse2      
      
      fit <- lm(truth ~ estimation)
      sumr2 <- sumr2 + summary(fit)$r.squared
      
      
      rf_accuracy_all <- rbind(rf_accuracy_all, data.frame(n=tn, 
                                                   rep = r,
                                                   variable = te,
                                                   value=summary(fit)$r.squared, 
                                                   type = "R-SQUARED [-]"))
      rf_accuracy_all <- rbind(rf_accuracy_all, data.frame(n=tn,
                                                   rep = r,
                                                   variable = te,
                                                   value=rrmse, 
                                                   type = "MEAN RELATIVE ERROR [-]"))
      rf_accuracy_all <- rbind(rf_accuracy_all, data.frame(n=tn,
                                                           rep = r,
                                                           variable = te,
                                                           value=(summary(fit)$r.squared + (1-rrmse))/2, 
                                                           type = "COMBINED [-]"))
      
    }
    rf_accuracy <- rbind(rf_accuracy, data.frame(n=tn, 
                                                 rep = r,
                                                 value=sumr2/length(to_est), 
                                                 type = "R-SQUARED [-]"))
    rf_accuracy <- rbind(rf_accuracy, data.frame(n=tn,
                                                 rep = r,
                                                 value=sumdiff/length(to_est), 
                                                 type = "MEAN RELATIVE ERROR [-]"))
    rf_accuracy <- rbind(rf_accuracy, data.frame(n=tn,
                                                 rep = r,
                                                 value=(sumr2/length(to_est) + (1-(sumdiff/length(to_est))))/2, 
                                                 type = "COMBINED [-]"))
  }
}



# GET THE EBST R-SQUARED VALUES FOR THE DIRECT ESTIMATION
direct_accuracy <- NULL
for(i in c(1:length(to_est))){
  truth <- test.data[[to_est[i]]]
  estimation <- test.descr[[to_comp[i]]] 

  fit <- lm(truth ~ estimation)
  r2 <- summary(fit)$r.squared
  
  rel_diff <- ((truth - estimation) / truth)^2
  rel_diff[is.infinite(rel_diff)] <- 0
  rrmse <- sqrt(mean(rel_diff))
  if(rrmse > 1.5) rrmse <- 1.5
  
  direct_accuracy <- rbind(direct_accuracy, 
                           data.frame(variable = to_est[i],
                                      ref=r2, 
                                      fit = "combined",
                                      type = "R-SQUARED [-]"))
  direct_accuracy <- rbind(direct_accuracy, 
                           data.frame(variable = to_est[i],
                                      ref=rrmse, 
                                      fit="combined",
                                      type = "MEAN RELATIVE ERROR [-]"))    
  
  direct_accuracy <- rbind(direct_accuracy, 
                           data.frame(variable = to_est[i],
                                      ref=(r2 + (1-rrmse))/2, 
                                      fit="combined",
                                      type = "COMBINED [-]")) 
}



dat <- merge(rf_accuracy_all, direct_accuracy[direct_accuracy$fit == "combined",], by=c("type", "variable"))


dat$ref2 <- (dat$ref.x + (1-dat$ref.y)) / 2

# Plot the results 

ggplot(data=dat[grepl("R-SQ", dat$type),], aes(x=factor(n), y=value)) + 
  geom_boxplot(width=0.6) + 
  geom_hline(aes(yintercept = ref), lty=2, colour="green", size=1.2)+
  # geom_hline(aes(yintercept = ref2), lty=3, colour="blue", size=1.2)+
  # geom_hline(aes(yintercept = ref), lty=4, colour="red", size=1.2)+
  facet_wrap(~variable, ncol=2, scales="free") +
  xlab(paste0("\n Number of images used for training [-]")) + 
  ylab(paste0("R-squares values [-]\n")) + 
  theme_bw() + 
  scale_colour_manual(values=leg.col) +
  theme(text=element_text(size=12), 
        axis.text.x = element_text(angle=90, hjust=1, size=11),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))




