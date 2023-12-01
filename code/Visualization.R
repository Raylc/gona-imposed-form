# Add packages
library(tidyverse)
library(patchwork)

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
  filter(Contexts == "Acheulean", Mode == "2") %>%
  ggplot(aes(FAI, PC1, color = as.factor(Base))) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC1 (flatness)")+
  theme(legend.position="none")

fig5b<-joined_gona %>%
  filter(Contexts == "Acheulean", Mode == "2") %>%
  ggplot(aes(FAI, PC2, color = as.factor(Base))) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC2 (pointedness)")+
  labs(color='Base')

patchwork <- (fig5a + fig5b)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by base.png", path="figure.", width = 7, height = 3, dpi = 300)


## Figure for FAI by flaked
PC1.mean <- joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked.x == "Unmodified") %>%
  group_by(Flaked.x) %>%
  summarize(PC1mean = mean(PC1))

fig6a<-joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, PC1, color = as.factor(Flaked.x))) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC1.mean, linetype="dashed",aes( group = Flaked.x, yintercept = PC1mean, color = Flaked.x)) +
  labs(x ="FAI", y = "PC1 (flatness)")+
  theme(legend.position="none")

PC2.mean <- joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake", Flaked.x == "Unmodified") %>%
  group_by(Flaked.x) %>%
  summarize(PC2mean = mean(PC2))

fig6b<-joined_gona %>%
  filter(Contexts == "Acheulean", Base == "flake") %>%
  ggplot(aes(FAI, PC2, color = as.factor(Flaked.x))) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm)+
  geom_hline(data = PC2.mean, linetype="dashed",aes( group = Flaked.x, yintercept = PC2mean, color = Flaked.x)) +
  labs(x ="FAI", y = "PC2 (pointedness)")+
  labs(color='Flaked')

patchwork <- (fig6a + fig6b)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("Fig.FAI by flaked.png", path="figure.", width = 7, height = 3, dpi = 300)


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
