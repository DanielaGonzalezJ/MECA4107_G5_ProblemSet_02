#Working directory
setwd("C://Users//AlfredoRP//OneDrive - INALDE Business School - Universidad de La Sabana//Attachments//Economia//ML//Problem set 2 Poverty")

#Library

require(pacman)
p_load(tidyverse,
       caret,
       glmnet,
       estimatr)

##Import data##
train_hogares <- read.csv("train_hogares.csv")
train_personas <- read.csv("train_personas.csv")
test_hogares <- read.csv("test_hogares.csv")
test_personas <- read.csv("test_personas.csv")

##Función de procesamiento

process_personas <- function(data, ...) {
  
  t_personas <- t_personas %>% mutate(
    
    
  )
  select()
}

process_hogares <- function(data, ...) {
  
  t_hogares <- t_hogares %>% mutate(
    
    
  )
  select()
}

##Formato de predicción
  ##Predicción

predict <- basededatos %>% 
  mutate(pobre_ = predict(modelo, newdata = basededatos, type = "raw"
                          ) %>% select(id, pobre_)
    ##Formato Kaggle
    predict <- predict %>% 
      mutate(pobre = ifelse(pobre_=="Yes",1,0))
       %>% select(id, pobre)
    
    write.csv(predict, "Nombredemodelo.csv", row.names = FALSE)
  
