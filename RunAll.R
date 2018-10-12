# Load packages
rm (list = ls(all=TRUE))
library(rstan)
library(rethinking)
library(lattice)
library(MASS)
library(graphics)
library(grid)
library(boot)      #for inverse logit function
library(stargazer) #for table of model outputs

# Set local directory: the path to the folder containing the subfolders Code, Plots, and Data
#setwd("C:/Users/jabunce/Dropbox/Matsigenka-Mestizo_project_2014/perception_analysis/R_scripts/xcult_comp_analysis_github_4oct18")
setwd("~/Manu_perceptions")

# Load helper functions
source("./Code/Functions.R")

# Format data
source("./Code/FormatData.R")

# Plot raw data
source("./Code/PlotRawData.R")

# Fit main stan models: m1, m4, m11, m19 
samps <- 1000 	#number of mcmc samples
num_chains <- 2 #number of mcmc chains
source("./Code/MainStanModels.R")

# Process stan output and make figures
source("./Code/ProcessM1.R")
source("./Code/ProcessM4.R")
source("./Code/ProcessM11.R")
source("./Code/ProcessM19.R")
source("./Code/CombinedPlots.R")

# Fit additional stan models
samps <- 1000 	#number of mcmc samples
num_chains <- 2 #number of mcmc chains
source("./Code/OtherStanModels.R")

# Compare all model WAIC weights and make tables (Latex output)
source("./Code/WAICcompare.R")

