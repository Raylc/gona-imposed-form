library(tidyverse)

list_of_files <- list.files(path = "/Users/raylc/Downloads/Boxgrove Taylor work/Handaxes/boxgrove_measurements/plan_2", recursive = TRUE, full.names = TRUE)

# Read all the files and create a FileName column to store filenames
df <- list_of_files %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")
Handaxes/boxgrove_measurements













plan_filenames_160<-list.files("Handaxes/boxgrove_measurements/plan_822_160", pattern="*.txt")
profile_filenames_160<-list.files("Handaxes/boxgrove_measurements/profile_822_160", pattern="*.txt")
plan_filenames_162<-list.files("Handaxes/boxgrove_measurements/plan_2", pattern="*.txt")
profile_filenames_162<-list.files("Handaxes/boxgrove_measurements/profile_2", pattern="*.txt")



merge_shape_data_function_160<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}


library(readtext)
readtext(paste0(plan_filenames_160, "Handaxes/boxgrove_measurements/plan_822_160/*.txt"),
         docvarsfrom = "filenames", 
         docvarnames = c("variable", "measurment point", "value"),
         dvsep = "_")

open_read(plan_filenames_160, plan_822_160)

readtext(paste0(plan_filenames_160))





merge_shape_data_function_160<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}

merge_shape_data_function_162<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4,
           shape_width_mm=shape_width_mm-(shape_width_mm*0.01477)) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*25.4,
           shape_thickness_mm=shape_thickness_mm-(shape_thickness_mm*0.01477 # adjusts scale to correct for 1:162 issue
           )) %>%
    select(-c(variable,measurement)) %>%
    dcast(individual ~ measurement_point,value.var="shape_thickness_mm")
  
  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("individual")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_shape_data)
}

#Create combined width/thickness data for two different scales (1:160 & 1:162)

boxgrove_measurements_160<-merge_shape_data_function_160(plan_filenames_160,profile_filenames_160,"Handaxes/boxgrove_measurements/plan_822_160","Handaxes/boxgrove_measurements/profile_822_160")
boxgrove_measurements_162<-merge_shape_data_function_162(plan_filenames_162,profile_filenames_162,"Handaxes/boxgrove_measurements/plan_2","Handaxes/boxgrove_measurements/profile_2")

# Merge 1:160 & 1:162 (scales have been corrected in merge_shape_data_function_160 & merge_shape_data_function_162 functions)

boxgrove_measurements<-
  full_join(boxgrove_measurements_160,boxgrove_measurements_162)











caliper<-read.csv("C:/FilesVC/GMM-exp-boxgrove-gona/data/caliper.csv")
ImageJ<-read.csv("C:/FilesVC/GMM-exp-boxgrove-gona/data/ImageJ.csv")

caliper<- dplyr::select(caliper, -c("Br", "T"))
bepca1 <-prcomp(caliper[,-8],  scale = FALSE)
#bepca1 <-prcomp(caliper[,-10],  scale = FALSE)
bepca2 <-prcomp(ImageJ[,-20],  scale = FALSE)

factoextra::fviz_pca_ind(bepca1, habillage= caliper$Type, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups",
             label="none")+
  ggplot2::labs(title ="caliper")

ggplot2::ggsave("caliper PCA.png", path="C:/FilesVC/GMM-exp-boxgrove-gona/figure", dpi = 600)

factoextra::fviz_pca_ind(bepca2, habillage= ImageJ$Type, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups",
             label="none")+
  ggplot2::labs(title ="Image J")
ggplot2::ggsave("Image J PCA.png", path="C:/FilesVC/GMM-exp-boxgrove-gona/figure", dpi = 600)


factoextra::fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 1, top = 10)
factoextra::fviz_contrib(boxgrove_experiment_pca, choice = "var", axes = 2, top = 10)



res.ind <- factoextra::get_pca_ind(bepca1)
Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates

# add data back to dataset

caliper$PC1<-Indiv_handaxe_scores$Dim.1
caliper$PC2<-Indiv_handaxe_scores$Dim.2




res.ind <- factoextra::get_pca_ind(bepca2)
Indiv_handaxe_scores2<-data.frame(res.ind$coord)        # Coordinates

# add data back to dataset

ImageJ$PC11<-Indiv_handaxe_scores2$Dim.1
ImageJ$PC12<-Indiv_handaxe_scores2$Dim.2

PCregression <- caliper


caliper$index <- 1:nrow(caliper)
ImageJ$index <- 1:nrow(ImageJ)


total1 <- dplyr::left_join(caliper, ImageJ, by= "index")

ggstatsplot::ggscatterstats(
  data  = total1,
  x     = PC1,
  y     = PC11,
  xlab  = "Caliper PC1",
  ylab  = "ImageJ PC1",
)
ggplot2::ggsave("PC1 correlation.png", path="C:/FilesVC/GMM-exp-boxgrove-gona/figure", dpi = 600)

ggstatsplot::ggscatterstats(
  data  = total1,
  x     = PC2,
  y     = PC12,
  xlab  = "Caliper PC2",
  ylab  = "ImageJ PC2",
)
ggplot2::ggsave("PC2 correlation.png", path="C:/FilesVC/GMM-exp-boxgrove-gona/figure", dpi = 600)



