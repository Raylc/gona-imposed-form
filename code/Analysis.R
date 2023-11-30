# Add packages
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(PCAmixdata)
library(psych)
library(patchwork)
library(figpatch)
# Data preparation
## loading data
gona_linear<-readr::read_csv("data/Gona_ESA_Linear.csv")
gona_3d<-readr::read_csv("data/Gona_ESA_3D.csv")
## creating a new column of unique identifier for merging
gona_linear$Name <- paste(gona_linear$Site, gona_linear$Catalog, sep= "-")
## merging two datasets based on Name
joined_gona <-dplyr::left_join(gona_linear, gona_3d, by = c("Name" = "Name"))
## Reordering column order for principle component analysis
joined_gona <- joined_gona %>% dplyr::relocate(GM_L,GM_Br1,GM_Br2,GM_Br3,GM_T1,GM_T2,GM_T3)
## Subsetting a dataframe that contains both linear and 3d data for measuremnt validation
overlap_gona <- joined_gona %>% tidyr::drop_na(Flaked.y)

### #######################Trial
gona_linear1<-readr::read_csv("data/EA_Reduction_Paper_41.csv")
gona_linear1$Name <- paste(gona_linear1$Site, gona_linear1$Catalog, sep= "-")
joined_gona1 <-dplyr::left_join(gona_linear1, gona_3d, by = c("Name" = "Name"))
joined_gona1 <- joined_gona1 %>% dplyr::relocate(GM_L,GM_Br1,GM_Br2,GM_Br3,GM_T1,GM_T2,GM_T3)
## Subsetting a dataframe that contains both linear and 3d data for measuremnt validation
overlap_gona1 <- joined_gona1 %>% tidyr::drop_na(Flaked)

gona_esa_pca12 <-prcomp(overlap_gona1[1:7],  scale = FALSE)
res.var <- get_pca_var(gona_esa_pca12)
variable_loadings123<-data.frame(res.var$coord) 
fviz_eig(gona_esa_pca12, addlabels=TRUE, hjust = -0.1)

##################################################

# Measurement Validation
## Principle component analysis
gona_esa_pca <-prcomp(overlap_gona[1:7],  scale = TRUE)
res.var <- get_pca_var(gona_esa_pca)
variable_loadings<-data.frame(res.var$coord) 

gona_esa_pca1 <-prcomp(joined_gona[1:7],  scale = FALSE)
res.var <- get_pca_var(gona_esa_pca1)
variable_loadings1<-data.frame(res.var$coord)

## Screeplot
fviz_eig(gona_esa_pca, addlabels=TRUE, hjust = -0.1)
# Results for individuals

res.ind <- get_pca_ind(gona_esa_pca)
Indiv_handaxe_scores<-data.frame(res.ind$coord)
# add data back to dataset
overlap_gona$PC1<-Indiv_handaxe_scores$Dim.1
overlap_gona$PC2<-Indiv_handaxe_scores$Dim.2

pca2 <- psych::principal(overlap_gona[1:7], nfactors=2, rotate="varimax", scores=F)
pca2


# Evaluating the correlations in PC values between caliper and 3d methods.
ggstatsplot::ggscatterstats(
  data  = overlap_gona,
  x     = "FAC1_1.x",
  y     = "3DGMPC1(10x10)",
  xlab  = "Caliper PC1",
  ylab  = "3DGMM PC1",
)
ggplot2::ggsave("3D PC1 correlation.png", path="figure.", width = 5, height = 5, dpi = 600)

ggstatsplot::ggscatterstats(
  data  = overlap_gona,
  x     = "FAC2_1",
  y     = "3DGMPC2(10x10)",
  xlab  = "Caliper PC2",
  ylab  = "3DGMM PC2",
)
ggplot2::ggsave("3D PC2 correlation.png", path="figure.", width = 5, height = 5, dpi = 600)




#####################################
gona_esa<-read.csv("data/EA_Reduction_Paper_2023_cleaned_and_corrected_for_CL.csv")
## creating a new column of unique identifier for merging
gona_esa$Name <- paste(gona_esa$Site, gona_esa$Catalog, sep= "-")
gona_3dgmm<-readr::read_csv("data/Gona3Ddata_GMM.csv")
gona_scars<-readr::read_csv("data/Gona3Ddata_scars.csv")
## Subsetting scar data frame because starting from th 7th column it lists the area of individual scars on an artifact, the number of which vary between artifacts.
gona_scars_6c<-gona_scars[1:6,]
joined_gona <-dplyr::left_join(gona_esa, gona_3dgmm, by = c("Name" = "Name"))
full_gona <-dplyr::left_join(joined_gona, gona_scars_6c, by = c("Name" = "Name"))
## Subsetting a dataframe that contains both capliper and 3d measurements.
overlap_gona <- full_gona %>% tidyr::drop_na(Type.y)
write.csv(overlap_gona, file = "data/overlap_gona.csv")

# Fig3b correlation between linearPC1 and 3dPC1


Fig3b <- ggstatsplot::ggscatterstats(
  data  = full_gona,
  x     = "3DGMPC1(10x10)",
  y     = "Linear_PC1",
  xlab  = "3DGM PC1",
  ylab  = "Linear PC1",
  results.subtitle = FALSE,
)
ggplot2::ggsave("Fig3b.3D PC1 correlation.png", path="figure.", width = 5, height = 5, dpi = 600)

Fig3c <- ggstatsplot::ggscatterstats(
  data  = full_gona,
  x     = "SDI.y",
  y     = "cSDI",
  xlab  = "3DGM SDI",
  ylab  = "SDI Linear Corrected",
  results.subtitle = FALSE,
)

Fig3d <- ggstatsplot::ggscatterstats(
  data  = full_gona,
  x     = "%FlakedVisual",
  y     = "%Flaked",
  xlab  = "%Flaked Visual",
  ylab  = "%Flaked 3DGM",
  results.subtitle = FALSE,
)
ggplot2::ggsave("Fig3d.3D visual flaked area correlation.png", path="figure.", width = 5, height = 5, dpi = 600)



img <- fig(image_path)
img <- fig("figure/Fig3a.png")
img = ggplot() +
  cowplot::draw_image("figure/Fig3a.png")
logo <- png::readPNG(img1_path)

wrap_plots(img, Fig3b, Fig3c, Fig3d)
img /
(Fig3b | Fig3c | Fig3d) 

ggplot2::ggsave("Fig3.png", path="figure.", width = 25, height = 10, dpi = 600)

img1 /
  (Fig3b | Fig3c | Fig3d) 

img1_path <- "figure/Fig3a.png"
img1 <- png::readPNG(img1_path, native = TRUE, info = TRUE)

design <- "AAA
           BCD"
wrap_plots(A = img, B = Fig3b,C = Fig3c,D = Fig3d, design = design)
#############################
#Fig4

ggstatsplot::ggscatterstats(
  data  = full_gona,
  x     = FAI,
  y     = PC1,
  col=factor(Base),
  xlab  = "FAI",
  ylab  = "PC1(flatness)",
)

library(statsExpressions)

results_expr <- corr_test(full_gona, FAI, PC1)$expression[[1]]
fig4a<-ggplot(full_gona, aes(FAI, PC1, color = as.factor(Base))) + 
  geom_point() + 
  labs(subtitle = results_expr)

results_expr1 <- corr_test(full_gona, FAI, PC2)$expression[[1]]
fig4b<-ggplot(full_gona, aes(FAI, PC2, color = as.factor(Base))) + 
  geom_point() + 
  labs(subtitle = results_expr1)
fig4a+fig4b
ggplot2::ggsave("Fig4.png", path="figure.", width = 25, height = 10, dpi = 600)


fig5a<-ggplot(full_gona, aes(FAI, PC1, color = as.factor(Flaked))) + 
  geom_point() + 
  labs(subtitle = results_expr)

results_expr1 <- corr_test(full_gona, FAI, PC2)$expression[[1]]
fig5b<-ggplot(full_gona, aes(FAI, PC2, color = as.factor(Flaked))) + 
  geom_point() + 
  labs(subtitle = results_expr1)
fig4a+fig4b
