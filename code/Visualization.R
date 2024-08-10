# Add packages
library(tidyverse)
library(patchwork)
library(cowplot)


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

