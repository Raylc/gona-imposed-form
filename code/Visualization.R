# Add packages
library(tidyverse)
library(patchwork)
library(cowplot)

# Data preparation
## loading data
gona_linear<-readr::read_csv("data/Gona_ESA_Linear1.csv")
gona_3d<-read.csv("data/Gona_ESA_3D.csv")
## creating a new column of unique identifier for merging
gona_linear$Name <- paste(gona_linear$Site, gona_linear$Catalog, sep= "-")
## merging two datasets based on Name
joined_gona <-dplyr::left_join(gona_linear, gona_3d, by = c("Name" = "Name"))
## Reordering column order for principle component analysis
joined_gona <- joined_gona %>% dplyr::relocate(GM_L,GM_Br1,GM_Br2,GM_Br3,GM_T1,GM_T2,GM_T3)
## Subsetting a dataframe that contains both linear and 3d data for measuremnt validation
overlap_gona <- joined_gona %>% tidyr::drop_na(Flaked.y)

## Figure for 3d comparison

fig4a <- ggplot(overlap_gona, aes(X.3DGMPC110x10, Linear_PC1)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  labs(x ="3DGM PC1", y = "Linear PC1")

fig4b <- ggplot(overlap_gona, aes(SDI.y, SDILinearCorrected)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  labs(x ="SDI (3DGM)", y = "SDILinearCorrected")

fig4c <- ggplot(overlap_gona, aes(Flaked.y, FlakedVisual)) + 
  geom_point() + 
  geom_smooth(method=lm)+
  labs(x ="Flaked percentage (3DGM)", y = "Flaked percentage (visual)")

patchwork <- (fig4a + fig4b + fig4c)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.3D comparison.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure for FAI by base

fig5a<-joined_gona %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked.x == "Flaked") %>%
  ggplot(aes(FAI, PC1)) + 
  geom_point(alpha=0.6,size=2,aes(color= as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC1 (flatness)")+
  theme(legend.position="none")


fig5b<-joined_gona %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked.x == "Flaked") %>%
  ggplot(aes(FAI, PC2)) + 
  geom_point(alpha=0.6,size=2,aes(color = as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC2 (convergence)")+
  labs(color='Base', shape = 'Base')+
  theme(legend.key.size = unit(0.2, "cm"))


patchwork <- (fig5a + fig5b)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by base new.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure for FAI by flaked
PC1.mean <- joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked.x == "Unmodified") %>%
  group_by(Flaked.x) %>%
  summarize(PC1mean = mean(PC1))

fig6a<-joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, PC1, color = as.factor(Flaked.x))) + 
  geom_point(alpha=0.5,size=2,aes(color = as.factor(Flaked.x))) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC1.mean, linetype="dashed",aes( group = Flaked.x, yintercept = PC1mean, color = Flaked.x)) +
  labs(x ="FAI", y = "PC1 (flatness)")+
  scale_y_continuous(limits=c(-1.5,2.5))+
  theme(legend.position="none")

PC2.mean <- joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked.x == "Unmodified") %>%
  group_by(Flaked.x) %>%
  summarize(PC2mean = mean(PC2))

fig6b<-joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, PC2, color = as.factor(Flaked.x))) + 
  geom_point(alpha=0.5,size=2,aes(color = as.factor(Flaked.x))) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC2.mean, linetype="dashed",aes( group = Flaked.x, yintercept = PC2mean, color = Flaked.x)) +
  labs(x ="FAI", y = "PC2 (convergence)")+
  scale_y_continuous(limits=c(-2.5,3))+
  labs(color='Modification')+
  theme(legend.key.size = unit(0.2, "cm"))

fig6a1<-ggdraw(fig6a) + 
  draw_image("figure/Picture1.PNG",
             x = 0, y = 0.29, width = 0.12, height = 0.12)
fig6a2<-ggdraw(fig6a1) + 
  draw_image("figure/Picture2.PNG",
             x = 0, y = 0.83, width = 0.12, height = 0.12)
fig6b1<-ggdraw(fig6b) + 
  draw_image("figure/Picture3.PNG",
             x = 0, y = 0.12, width = 0.12, height = 0.12)
fig6b2<-ggdraw(fig6b1) + 
  draw_image("figure/Picture4.PNG",
             x = 0, y = 0.81, width = 0.12, height = 0.12)

patchwork <- (fig6a2 + fig6b2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by flaked new.png", path="figure.", width = 9, height = 3, dpi = 300)


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
  filter(Contexts == "Acheulean", Base == "flake", Flaked.x == "Unmodified")

