# Add packages
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(PCAmixdata)
library(psych)
# Add data
gona_esa<-read.csv("data/EA_Reduction_Paper_Order.csv")

# Principle component analysis
gona_esa_pca <-prcomp(gona_esa[1:7],  scale = TRUE)
pcaplot<-fviz_pca_ind(gona_esa_pca, habillage= gona_esa$Type, # color by groups
                      addEllipses = TRUE, # Concentration ellipses
                      ellipse.type = "convex",
                      legend.title = "Type",
                      label="none")
pcaplot
res.var <- get_pca_var(gona_esa_pca)
variable_loadings<-data.frame(res.var$coord) 

pca2 <- psych::principal(gona_esa[1:7], nfactors=2, rotate="varimax", scores=F)
pca2

