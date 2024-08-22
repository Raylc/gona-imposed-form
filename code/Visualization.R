# Add packages
library(tidyverse)
library(patchwork)
library(cowplot)
library(mvtnorm) 
library(MASS) 
library(caret) 
library(lsr)
library(diptest)
library(DFA.CANCOR)

# Data preparation
## loading data
gona_linear<-readr::read_csv("data/Gona_EA_for_Cheng_020824.csv")
gona_3d<-read.csv("data/3D-cleaned-for-Cheng-080224.csv")

# Visualization

## Figure 4 (3d and caliper measurement comparison)

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
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("Fig.4.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure 5 (DFA results)
### LDFA (typology based)


theme_set(theme_classic()) 
gona_linearfil <- gona_linear %>% filter(Typology == "Pick" | Typology =="Handaxe" | Typology == "Knife")
gona_lineardfa <- dplyr::select(gona_linearfil, FAI, FAC1_1, cSDI, FAC2_1, Typology)


DFA_Field=DFA.CANCOR::DFA(data = gona_lineardfa, 
              groups = 'Typology', 
              variables = c('FAI', 'FAC1_1', 'cSDI', 'FAC2_1'),
              predictive = TRUE, 
              priorprob = 'EQUAL',   
              covmat_type='within', # altho better to use 'separate' for these data
              verbose = TRUE)
DFA_Field$dfa_scores$Function.2<- (-1)*(DFA_Field$dfa_scores$Function.2)

P0<-ggplot(DFA_Field$dfa_scores, aes(Function.1, Function.2)) + geom_point(aes(color = group))+
  labs(x ="DF1", y = "DF2")
P1<-ggExtra::ggMarginal(P0+ theme(legend.position = "left"),type="histogram")

gona_lineardfa1 <- dplyr::select(gona_linearfil, FAI, FAC1_1, cSDI, FAC2_1, Base, Typology)

DFA_Field=DFA.CANCOR::DFA(data = gona_lineardfa1, 
                          groups = 'Typology', 
                          variables = c('FAI', 'FAC1_1', 'cSDI', 'FAC2_1'),
                          predictive = TRUE, 
                          priorprob = 'EQUAL',   
                          covmat_type='within', # altho better to use 'separate' for these data
                          verbose = TRUE)
dfaresults <- cbind(DFA_Field$dfa_scores, gona_lineardfa1$Base)
dfaresults$Function.2 <- (-1)*(dfaresults$Function.2)

P2<-ggplot(dfaresults, aes(Function.1, Function.2)) + geom_point(aes(color = gona_lineardfa1$Base))+
  labs(x ="DF1", y = "DF2")+guides(color=guide_legend(title="group"))+scale_color_brewer(palette="Accent")
P3<-ggExtra::ggMarginal(P2+ theme(legend.position = "left"),type="histogram")


patchwork <- patchwork::wrap_elements(P1) + patchwork::wrap_elements(P3)
patchwork + plot_annotation(tag_levels = 'a')

ggplot2::ggsave("Fig.5.png", path="figure.", width = 10, height = 4, dpi = 300)

dip_test_result <- dip.test(DFA_Field$dfa_scores$Function.1)
print(dip_test_result)

dip_test_result1 <- dip.test(DFA_Field1$dfa_scores$Function.1)
print(dip_test_result1)














## Figure 6 (FAI by base)

fig5a<-gona_linear %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked == "Flaked") %>%
  ggplot(aes(FAI, FAC1_1)) + 
  geom_point(alpha=0.6,size=2,aes(color= as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC1 (flatness, higher = flatter)")+
  scale_y_continuous(limits=c(-1.5,2.5))+
  theme(legend.position="none", axis.title=element_text(size=8))


fig5b<-gona_linear %>%
  filter(Contexts == "Acheulean", Mode == "2", Flaked == "Flaked") %>%
  ggplot(aes(FAI, FAC2_1)) + 
  geom_point(alpha=0.6,size=2,aes(color = as.factor(Base),shape=as.factor(Base))) + 
  geom_smooth(method=lm, aes(group = 1))+
  labs(x ="FAI", y = "PC2 (pointedness, lower = more pointed)")+
  labs(color='Base', shape = 'Base')+
  scale_y_continuous(limits=c(-3,3))+
  theme(legend.key.size = unit(0.2, "cm"), axis.title=element_text(size=8))


patchwork <- (fig5a + fig5b)
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("Fig.6.png", path="figure.", width = 9, height = 3, dpi = 300)


## Figure 7 (FAI by flaked)
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
  labs(x ="FAI", y = "PC1 (flatness, higher = flatter)")+
  scale_y_continuous(limits=c(-1.5,2.5))+
  theme(legend.position="none", axis.title=element_text(size=8))

  
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
  labs(x ="FAI", y = "PC2 (pointedness, lower = more pointed)")+
  scale_y_continuous(limits=c(-2.5,3))+
  labs(color='Modification')+
  theme(legend.key.size = unit(0.2, "cm"), axis.title=element_text(size=8))

fig6a1<-ggdraw(fig6a) + 
  draw_image("figure/Picture1.PNG",
             x = -0.045, y = 0.22, width = 0.12, height = 0.12)
fig6a2<-ggdraw(fig6a1) + 
  draw_image("figure/Picture2.PNG",
             x = -0.045, y = 0.8, width = 0.12, height = 0.12)
fig6b1<-ggdraw(fig6b) + 
  draw_image("figure/Picture3.PNG",
             x = -0.045, y = 0.12, width = 0.12, height = 0.12)
fig6b2<-ggdraw(fig6b1) + 
  draw_image("figure/Picture4.PNG",
             x = -0.045, y = 0.81, width = 0.12, height = 0.12)

patchwork <- (fig6a2 + fig6b2)
patchwork + plot_annotation(tag_levels = 'a')
ggplot2::ggsave("Fig.7.png", path="figure.", width = 15, height = 5, dpi = 300)


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



dip_test_result <- dip.test(lda.data$LD1)
print(dip_test_result)

dip_test_result <- dip.test(lda.data$LD2)
print(dip_test_result)