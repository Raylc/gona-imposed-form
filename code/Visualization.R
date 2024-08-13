# Add packages
library(tidyverse)
library(patchwork)
library(cowplot)
library(mvtnorm) 
library(MASS) 
library(caret) 

# Data preparation
## loading data
gona_linear<-readr::read_csv("data/Gona_EA_for_Cheng_020824.csv")
gona_3d<-read.csv("data/3D-cleaned-for-Cheng-080224.csv")

## Figure for 3d comparison

fig4a <- ggplot(gona_3d, aes(X.3DGMPC110x10, FAC1_2)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  labs(x ="3DGM PC1", y = "Linear PC1")

fig4b <- ggplot(gona_3d, aes(SDI_3D, SDIBoxTimesPointFive)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  geom_abline(slope=1, intercept = 0, colour = "red", linewidth=1.2, alpha=0.5)+
  labs(x ="3D SDI", y = "cSDI")

fig4c <- ggplot(gona_3d, aes(Flaked, FlakedVisual)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  geom_abline(slope=1, intercept = 0, colour = "red", linewidth=1.2, alpha=0.5)+       
  labs(x ="Flaked percentage (3D)", y = "Flaked percentage (visual)")

patchwork <- (fig4a + fig4b + fig4c)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.3D comparison new1.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure for FAI by base

fig5a<-gona_linear %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked == "Flaked") %>%
  ggplot(aes(FAI, FAC1_1)) + 
  geom_point(alpha=0.6,size=2,aes(color= as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC1 (flatness)")+
  scale_y_continuous(limits=c(-1.5,2.5))+
  theme(legend.position="none")


fig5b<-gona_linear %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked == "Flaked") %>%
  ggplot(aes(FAI, FAC2_1)) + 
  geom_point(alpha=0.6,size=2,aes(color = as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC2 (convergence)")+
  labs(color='Base', shape = 'Base')+
  scale_y_continuous(limits=c(-3,3))+
  theme(legend.key.size = unit(0.2, "cm"))


patchwork <- (fig5a + fig5b)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by base new NEW.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure for FAI by flaked
PC1.mean <- gona_linear %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked == "Unmodified") %>%
  group_by(Flaked) %>%
  summarize(PC1mean = mean(FAC1_1))


PC2.mean <- gona_linear %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked == "Unmodified") %>%
  group_by(Flaked) %>%
  summarize(PC2mean = mean(FAC2_1))

fig6a<-gona_linear %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, FAC1_1, color = as.factor(Flaked))) + 
  geom_point(alpha=0.5,size=2,aes(color = as.factor(Flaked))) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC1.mean, linetype="dashed",aes( group = Flaked, yintercept = PC1mean, color = Flaked)) +
  annotate(geom = "point", x = 55 , y = 2.02, colour = "black", fill = "#F8766D", size= 1, stroke = 0.5) +
  annotate(geom = "text", x = 55 , y = 2.05, label = "OGS5:2013-1", vjust=2, size=2)+
  annotate(geom = "point", x = 0 , y = -0.66, colour = "black", fill = "#619CFF", size= 1, stroke = 0.5) +
  annotate(geom = "text", x = 0 , y = -0.66, label = "OGS5:54",  vjust=2, size=2)+
  labs(x ="FAI", y = "PC1 (flatness)\n")+
  scale_y_continuous(limits=c(-1.5,2.5))+
  theme(legend.position="none")

  
fig6b<-gona_linear %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, FAC2_1, color = as.factor(Flaked))) + 
  geom_point(alpha=0.5,size=2,aes(color = as.factor(Flaked))) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC2.mean, linetype="dashed",aes( group = Flaked, yintercept = PC2mean, color = Flaked)) +
  annotate(geom = "point", x = 5 , y = 2.47, colour = "black", fill = "#F8766D", size= 1,stroke = 0.5) +
  annotate(geom = "text", x = 5 , y = 2.47, label = "DAN5:54", vjust=2,  size=2)+
  annotate(geom = "point", x = 5 , y = -2.21, colour = "black", fill = "#F8766D", size= 1,stroke = 0.5) +
  annotate(geom = "text", x = 5 , y = -2.21, label = "OGS5:5", vjust=2,  size=2)+
  labs(x ="FAI", y = "PC2 (convergence)\n")+
  scale_y_continuous(limits=c(-2.5,3))+
  labs(color='Modification')+
  theme(legend.key.size = unit(0.2, "cm"))

fig6a1<-ggdraw(fig6a) + 
  draw_image("figure/Picture1.PNG",
             x = 0.02, y = 0.29, width = 0.12, height = 0.12)
fig6a2<-ggdraw(fig6a1) + 
  draw_image("figure/Picture2.PNG",
             x = 0.02, y = 0.8, width = 0.12, height = 0.12)
fig6b1<-ggdraw(fig6b) + 
  draw_image("figure/Picture3.PNG",
             x = 0.02, y = 0.16, width = 0.12, height = 0.12)
fig6b2<-ggdraw(fig6b1) + 
  draw_image("figure/Picture4.PNG",
             x = 0.02, y = 0.81, width = 0.12, height = 0.12)

patchwork <- (fig6a2 + fig6b2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by flaked new123.png", path="figure.", width = 9, height = 3, dpi = 300)


## heteroscedasticity test for mode1 and mode2 cores
MODE1CORE <- joined_gona %>%
  filter(Mode == "1", Flaked.x == "Flaked")
MODE2CORE <- joined_gona %>%
  filter(Mode == "2", Flaked.x == "Flaked")

lmMod1 <- lm(FAI ~ cSDI, data=MODE1CORE)
lmtest::bptest(lmMod1)
ggplot(MODE1CORE, aes(FAI, cSDI)) + 
  geom_point() + 
  geom_smooth(method=lm)
car::ncvTest(lmMod1)

lmMod2 <- lm(FAI ~ cSDI, data=MODE2CORE)
lmtest::bptest(lmMod2)
car::ncvTest(lmMod2)


### Locating the extreme value of PC1 and PC2 in the last figure
leftfig<-joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake")







## figures for demonstrating the conceptual model as requested by R3
gona_linear<- gona_linear %>% unite("FULLNAME", Locality:Catalog, remove = FALSE)
### mapping SDI and FAI
# fig0a <- ggplot(gona_linear, aes(cSDI, FAI, color = Typology)) +
#   geom_point() + 
#   # geom_smooth(method=lm)+
#   ggrepel::geom_text_repel(aes(label=FULLNAME))+
#   labs(x ="cSDI", y = "FAI")

# 
# 
# ### correlation between SDI/PC1 correlation and FAI/PC1
# correlation1<- gona_linear %>% group_by(Locality) %>%  summarise(SDIPC1R = abs(cor(cSDI, FAC1_1)))
# correlation2<- gona_linear %>% group_by(Locality) %>%  summarise(FAIPC1R = abs(cor(FAI, FAC1_1)))
# correlation12<- merge(correlation1,correlation2,by="Locality")
# 
# ### correlation between SDI/PC2 correlation and FAI/PC2
# 
# correlation3<- gona_linear %>% group_by(Locality) %>%  summarise(SDIPC2R = abs(cor(cSDI, FAC2_1)))
# correlation4<- gona_linear %>% group_by(Locality) %>%  summarise(FAIPC2R = abs(cor(FAI, FAC2_1)))
# correlation34<- merge(correlation3,correlation4,by="Locality")


### correlation between SDI/PC1 correlation and FAI/PC1
correlation1<- gona_linear %>% group_by(Typology) %>%  summarise(SDIPC1R = abs(cor(cSDI, FAC1_1)))
correlation2<- gona_linear %>% group_by(Typology) %>%  summarise(FAIPC1R = abs(cor(FAI, FAC1_1)))
correlation12<- merge(correlation1,correlation2,by="Typology")

### correlation between SDI/PC2 correlation and FAI/PC2

correlation3<- gona_linear %>% group_by(Typology) %>%  summarise(SDIPC2R = abs(cor(cSDI, FAC2_1)))
correlation4<- gona_linear %>% group_by(Typology) %>%  summarise(FAIPC2R = abs(cor(FAI, FAC2_1)))
correlation34<- merge(correlation3,correlation4,by="Typology")
  
fig0b <- ggplot(correlation12, aes(SDIPC1R, FAIPC1R)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label=Typology))+
  labs(x ="correlation between SDI and PC1", y = "correlation between FAI and PC1")

fig0c <- ggplot(correlation34, aes(SDIPC2R, FAIPC2R)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label=Typology))+
  labs(x ="correlation between SDI and PC2", y = "correlation between FAI and PC2")


patchwork <- (fig0b + fig0c)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.sdifai by typology(absolute value).png", path="figure.", width = 10, height = 5, dpi = 300)

# 
# patchwork <- (fig0a + fig0b + fig0c)
# patchwork + plot_annotation(tag_levels = 'A')
# ggplot2::ggsave("Fig.CONCEPTUAL.png", path="figure.", width = 27, height = 9, dpi = 300)

### RESIDUALS experiment
# 
# fitSDI <- lm(cSDI ~ FAC1_1 + FAC2_1, data=gona_linear)  
# fig00a<-ggplot(fitSDI, aes(x = .fitted, y = .resid, color = gona_linear$Typology)) +
#   geom_point() +
#   geom_hline(yintercept = 0)+
#   labs(x ="fitted cSDI value", y = "Residuals")
# 
# fitFAI <- lm(FAI ~ FAC1_1 + FAC2_1, data=gona_linear)  
# fig00B<-ggplot(fitFAI, aes(x = .fitted, y = .resid, color = gona_linear$Typology)) +
#   geom_point() +
#   geom_hline(yintercept = 0)+
#   labs(x ="fitted FAI value", y = "Residuals")
# 
# 
# patchwork <- (fig00a + fig00B)
# patchwork + plot_annotation(tag_levels = 'A')
# ggplot2::ggsave("Fig.RESIDUAL.png", path="figure.", width = 18, height = 9, dpi = 300)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### correlation by locality
# 
# gona_linear %>%  split(.$Locality)
# split.df <- split(gona_linear, gona_linear$Locality)
# for (I in 1:length(split.df)) { assign(unique(split.df[[I]]$Locality), split.df[[I]]) }
# 
# ### eg10 and ogs7 are oldowan
# 
# ### correlation between SDI/PC1 correlation and FAI/PC1
# correlationBSN17A<- BSN17 %>% group_by(Typology) %>%  summarise(SDIPC1R = cor(cSDI, FAC1_1))
# correlationBSN17B<- BSN17 %>% group_by(Typology) %>%  summarise(FAIPC1R = cor(FAI, FAC1_1))
# correlationBSN17AB<- merge(correlationBSN17A,correlationBSN17B,by="Typology")
# 
# ### correlation between SDI/PC2 correlation and FAI/PC2
# 
# correlationBSN17C<- BSN17 %>% group_by(Typology) %>%  summarise(SDIPC2R = cor(cSDI, FAC2_1))
# correlationBSN17D<- BSN17 %>% group_by(Typology) %>%  summarise(FAIPC2R = cor(FAI, FAC2_1))
# correlationBSN17CD<- merge(correlationBSN17C,correlationBSN17D,by="Typology")
# 
# figBSN1 <- ggplot(correlationBSN17AB, aes(SDIPC1R, FAIPC1R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC1", y = "correlation between FAI and PC1")+
#   ggtitle("BSN17")
# 
# figBSN2 <- ggplot(correlationBSN17CD, aes(SDIPC2R, FAIPC2R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC2", y = "correlation between FAI and PC2")+
#   ggtitle("BSN17")
# 
# 
# ### correlation between SDI/PC1 correlation and FAI/PC1
# correlationDAN5A<- DAN5 %>% group_by(Typology) %>%  summarise(SDIPC1R = cor(cSDI, FAC1_1))
# correlationDAN5B<- DAN5 %>% group_by(Typology) %>%  summarise(FAIPC1R = cor(FAI, FAC1_1))
# correlationDAN5AB<- merge(correlationDAN5A,correlationDAN5B,by="Typology")
# 
# ### correlation between SDI/PC2 correlation and FAI/PC2
# 
# correlationDAN5C<- DAN5 %>% group_by(Typology) %>%  summarise(SDIPC2R = cor(cSDI, FAC2_1))
# correlationDAN5D<- DAN5 %>% group_by(Typology) %>%  summarise(FAIPC2R = cor(FAI, FAC2_1))
# correlationDAN5CD<- merge(correlationDAN5C,correlationDAN5D,by="Typology")
# 
# figDAN1 <- ggplot(correlationDAN5AB, aes(SDIPC1R, FAIPC1R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC1", y = "correlation between FAI and PC1")+
#   ggtitle("DAN5")
# 
# figDAN2 <- ggplot(correlationDAN5CD, aes(SDIPC2R, FAIPC2R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC2", y = "correlation between FAI and PC2")+
#   ggtitle("DAN5")
# 
# 
# 
# 
# ### correlation between SDI/PC1 correlation and FAI/PC1
# correlationOGS5A<- OGS5 %>% group_by(Typology) %>%  summarise(SDIPC1R = cor(cSDI, FAC1_1))
# correlationOGS5B<- OGS5 %>% group_by(Typology) %>%  summarise(FAIPC1R = cor(FAI, FAC1_1))
# correlationOGS5AB<- merge(correlationOGS5A,correlationOGS5B,by="Typology")
# 
# ### correlation between SDI/PC2 correlation and FAI/PC2
# 
# correlationOGS5C<- OGS5 %>% group_by(Typology) %>%  summarise(SDIPC2R = cor(cSDI, FAC2_1))
# correlationOGS5D<- OGS5 %>% group_by(Typology) %>%  summarise(FAIPC2R = cor(FAI, FAC2_1))
# correlationOGS5CD<- merge(correlationOGS5C,correlationOGS5D,by="Typology")
# 
# figOGS51 <- ggplot(correlationOGS5AB, aes(SDIPC1R, FAIPC1R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC1", y = "correlation between FAI and PC1")+
#   ggtitle("OGS5")
# 
# figOGS52 <- ggplot(correlationOGS5CD, aes(SDIPC2R, FAIPC2R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC2", y = "correlation between FAI and PC2")+
#   ggtitle("OGS5")
# 
# 
# ### correlation between SDI/PC1 correlation and FAI/PC1
# correlationOGS12A<- OGS12 %>% group_by(Typology) %>%  summarise(SDIPC1R = cor(cSDI, FAC1_1))
# correlationOGS12B<- OGS12 %>% group_by(Typology) %>%  summarise(FAIPC1R = cor(FAI, FAC1_1))
# correlationOGS12AB<- merge(correlationOGS12A,correlationOGS12B,by="Typology")
# 
# ### correlation between SDI/PC2 correlation and FAI/PC2
# 
# correlationOGS12C<- OGS12 %>% group_by(Typology) %>%  summarise(SDIPC2R = cor(cSDI, FAC2_1))
# correlationOGS12D<- OGS12 %>% group_by(Typology) %>%  summarise(FAIPC2R = cor(FAI, FAC2_1))
# correlationOGS12CD<- merge(correlationOGS12C,correlationOGS12D,by="Typology")
# 
# figOGS121 <- ggplot(correlationOGS12AB, aes(SDIPC1R, FAIPC1R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC1", y = "correlation between FAI and PC1")+
#   ggtitle("OGS12")
# 
# figOGS122 <- ggplot(correlationOGS12CD, aes(SDIPC2R, FAIPC2R)) + 
#   geom_point() + 
#   ggrepel::geom_text_repel(aes(label=Typology))+
#   labs(x ="correlation between SDI and PC2", y = "correlation between FAI and PC2")+
#   ggtitle("OGS12")
# 
# 
# 
# 
# 
# 
# patchwork <- (figBSN1 + figBSN2 + figDAN1 + figDAN2 + figOGS51+ figOGS52 + figOGS121 + figOGS122)
# patchwork + plot_annotation(tag_levels = 'A')
# ggplot2::ggsave("Fig.TYPOLOGY BY LOCALITY.png", path="figure.", width = 15, height = 15, dpi = 300)


### LDFA

theme_set(theme_classic()) 

# Load the data 
# gona_lineardfa <- gona_linear %>% filter(Typology == c("Pick" , "Handaxe" , "Knife"))

gona_lineardfa <- gona_linear %>% filter(Typology == "Pick" | Typology =="Handaxe" | Typology == "Knife")
gona_lineardfa <- dplyr::select(gona_lineardfa, FAI, FAC1_1, cSDI, FAC2_1, Typology)


# Split the data into training (80%) and test set (20%) 
set.seed(123) 
training.individuals <- gona_lineardfa$Typology %>%  
  createDataPartition(p = 0.8, list = FALSE) 
train.data <- gona_lineardfa[training.individuals, ] 
test.data <- gona_lineardfa[-training.individuals, ] 

# Estimate preprocessing parameters 
preproc.parameter <- train.data %>%  
  preProcess(method = c("center", "scale")) 

# Transform the data using the estimated parameters 
train.transform <- preproc.parameter %>% predict(train.data) 
test.transform <- preproc.parameter %>% predict(test.data) 

# Fit the model 
model <- lda(Typology~., data = train.transform) 

# Make predictions 
predictions <- model %>% predict(test.transform) 

# Model accuracy 
mean(predictions$class==test.transform$Typology) 

model <- lda(Typology~., data = train.transform) 
model 
# Typology_colors <- c("Handaxe" = "red", "Pick" = "blue", "Knife" = "green")
# plot(model, col = Typology_colors, main="Visualization of Linear Discriminant Analysis")

lda.data <- cbind(train.transform, predict(model)$x)
P<-ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = Typology))
P1<-ggExtra::ggMarginal(P+ theme(legend.position = "left"),type="histogram")
# ggscatterstats(
#   data  = ggplot2::lda.data,
#   x     = LD1,
#   y     = LD2,
#   xlab  = "LD1",
#   ylab  = "LD2",
# )
P1

save_plot("figure/Fig.LDA123.png", P1, base_height = 8, base_aspect_ratio = 1.4, dpi = 300)