#install.packages("dplyr")
library(dplyr)

# Adjust as needed
scriptDir <- "/Users/felixkim/Desktop/case-competition-/Experimental/Felix_files"
outputDir <- "/Users/felixkim/Desktop/case-competition-/Experimental/Felix_files/OutputGraphs"
featureEnum <- function() {
  list(Loss.Ratio = "Loss.Ratio", Total.Insured.Value = "Total.Insured.Value", Premium = "Premium",
       Premium.Per.100.TIV = "Premium.Per.100.TIV", Loss.Cost = "Loss.Cost")
}

# Function invocation examples, where TARGET_LOCATION is set to some integer:
# Plot all of the features for a location:  > plot_location_feature_ts(exposuresData, TARGET_LOCATION)
# Plot just one of the features for a location:  > plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Total.Insured.Value)
plot_location_feature_ts <- function(df, location, featureEnumString) {
  # Subset out the rows pertaining just to one location
  location_df <- df[df$Location == location,]
  
  # Augment the dataframe with additional features not found in raw dataset
  location_df <- transform(location_df, Loss.Ratio=Losses...Non.Catastrophe/Premium)
  location_df <- transform(location_df, Premium.Per.100.TIV=Premium/(Total.Insured.Value/100.0))
  location_df <- transform(location_df, Loss.Cost=Losses...Non.Catastrophe/Total.Insured.Value)
  
  # Either plot all the features or just one of the features for the given location
  if (missing(featureEnumString)) {
    png(filename = paste0(outputDir, "/Location/", "Location.", location, ".ExposuresData", ".png"), width = 6000, height = 4000, res=300)
    par(mfrow = c(3,2))
    for (feat in featureEnum()) {
      plot(select(location_df, PolicyYear, feat), type = "l", lwd = 2, lty = 2, col = "blue", 
           xlab = "year", ylab = "value", main = paste0("Location ", location, ": ", feat))
    }
  }
  else {
    png(filename = paste0(outputDir, "/Location/", "Location.", location, ".", featureEnumString, ".png"))
    plot(select(location_df, PolicyYear, featureEnumString), type = "l", lwd = 2, lty = 2, col = "blue", 
         xlab = "year", ylab = "value", main = paste0("Location ", location, ": ", featureEnumString))
  }
  
  invisible(dev.off())
}

# Function invocation examples for entire portfolio
# Plot all of the features:  > plot_portfolio_feature_ts(exposuresData)
# Plot just one of the features:  > plot_portfolio_feature_ts(exposuresData, featureEnum()$Total.Insured.Value)
plot_portfolio_feature_ts <- function(df, featureEnumString) {
  
  # Augment the dataframe with additional features not found in raw dataset
  df <- transform(df, Loss.Ratio=Losses...Non.Catastrophe/Premium)
  df <- transform(df, Premium.Per.100.TIV=Premium/(Total.Insured.Value/100.0))
  df <- transform(df, Loss.Cost=Losses...Non.Catastrophe/Total.Insured.Value)
  
  # Either plot all the features or just one of the features for the given location
  if (missing(featureEnumString)) {
    for (feat in featureEnum()) {
      png(filename = paste0(outputDir, "/Portfolio/", feat, ".png"), width = 6000, height = 4000, res = 300)
      subset_df = df[,c('PolicyYear', feat)]
      boxplot(df[[feat]] ~ PolicyYear,
              data=subset_df,
              main=paste0("Portfolio data for ", feat),
              xlab="Year",
              ylab=feat,
              col="orange",
              border="brown"
      )
      invisible(dev.off())
    }
  }
  else {
    png(filename = paste0(outputDir, "/Portfolio/", featureEnumString, ".png"), width = 6000, height = 4000, res = 300)
    subset_df = df[,c('PolicyYear', featureEnumString)]
    boxplot(df[[featureEnumString]] ~ PolicyYear,
            data=subset_df,
            main=paste0("Portfolio data for ", featureEnumString),
            xlab="Year",
            ylab=featureEnumString,
            col="orange",
            border="brown"
    )
    invisible(dev.off())
  }
}

setwd(scriptDir)

exposuresData <- read.csv("../dataset/Exposures.csv", skip = 4, header = TRUE, sep = ",")
str(exposuresData)

# Confirm that data read properly
#head(exposuresData, 10)
#print(colnames(exposuresData))
#sapply(exposuresData, class)

##### For a single location (Management Request 1)
##########
TARGET_LOCATION <- 2
## 1) Loss ratio = Losses / Premium
#plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Loss.Ratio)
## 2) TIV
#plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Total.Insured.Value)
## 3) Premium
#plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Premium)
## 4) Premium per $100 TIV over time
#plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Premium.Per.100.TIV)
## 5) Loss cost = Losses / TIV
#plot_location_feature_ts(exposuresData, TARGET_LOCATION, featureEnum()$Loss.Cost)
## 6) All plots aggregated for a location
## 6a) One Location
#plot_location_feature_ts(exposuresData, TARGET_LOCATION)
## 6b) Loop through all locations, one-by-one
for (i in 1:35) {
  plot_location_feature_ts(exposuresData, i)
}

##### For the entire portfolio (Management Request 1)
##########
# 1) Loss ratio = Losses / Premium
#plot_portfolio_feature_ts(exposuresData, featureEnum()$Loss.Ratio)
# 2) TIV
#plot_portfolio_feature_ts(exposuresData, featureEnum()$Total.Insured.Value)
# 3) Premium
#plot_portfolio_feature_ts(exposuresData, featureEnum()$Premium)
# 4) Premium per $100 TIV over time
#plot_portfolio_feature_ts(exposuresData, featureEnum()$Premium.Per.100.TIV)
# 5) Loss cost = Losses / TIV
#plot_portfolio_feature_ts(exposuresData, featureEnum()$Loss.Cost)
# 6) All plots aggregated
plot_portfolio_feature_ts(exposuresData)