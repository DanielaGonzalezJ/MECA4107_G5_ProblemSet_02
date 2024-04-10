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

##Función de procesamiento personas

process_personas <- function(data, ...) {
  train_personas <- train_personas %>% mutate(
  Niveleduc = ifelse(P6210==9,0,P6210),
  Segsoc = ifelse(P6100==4,3,P6100),
  Tiempotrab = P6426,
  Ocupado = ifelse(is.na(Oc),0,1),
  genero = P6020,
  pension = P6920,
  jefe = ifelse(P6050==1,1,0)
  ) %>% 
    select(id,Orden,Niveleduc,Segsoc,Tiempotrab,Ocupado,genero,pension,jefe)
    
}
  
  train_personas <- process_personas(train_personas)
  test_personas <- process_personas(test_personas)


##Modelo1 #Train

train_personas_nivel_hogar<- train_personas %>% 
  group_by(id) %>% 
  summarize(njefe=sum(jefe,na.rm=TRUE),
            seguridad=sum(Segsoc,na.rm=TRUE),
            Pension=sum(pension,na.rm=TRUE),
            maxEducLevel=max(Niveleduc,na.rm=TRUE),
            nocupados=sum(Ocupado,na.rm=TRUE)
  )

train_personas_hogar<- train_personas %>% 
  filter(jefe==1) %>% 
  select(id,Segsoc,Niveleduc,Ocupado,pension,genero) %>% 
  rename(jefe_Educ_level=Niveleduc,
         jefe_ocupado=Ocupado,
         jefe_pension=pension,
         jefe_genero=genero,
         jefe_seguridad=Segsoc) %>% 
  left_join(train_personas_nivel_hogar)

##Modelo1 #Test

test_personas_nivel_hogar<- test_personas %>% 
  group_by(id) %>% 
  summarize(njefe=sum(jefe,na.rm=TRUE),
            seguridad=sum(Segsoc,na.rm=TRUE),
            Pension=sum(pension,na.rm=TRUE),
            maxEducLevel=max(Niveleduc,na.rm=TRUE),
            nocupados=sum(Ocupado,na.rm=TRUE)
  )

test_personas_hogar<- train_personas %>% 
  filter(jefe==1) %>% 
  select(id,Segsoc,Niveleduc,Ocupado,pension,genero) %>% 
  rename(jefe_Educ_level=Niveleduc,
         jefe_ocupado=Ocupado,
         jefe_pension=pension,
         jefe_genero=genero,
         jefe_seguridad=Segsoc) %>% 
  left_join(test_personas_nivel_hogar)

##Ajustar bases hogares

train_hogares <- train_hogares %>% select(id, Dominio, Pobre)
test_hogares <- test_hogares %>% select(id,Dominio,)


## Juntar bases

train<- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  select(-id) #no longer need id

test<- test_hogares %>% 
  left_join(test_personas_hogar)

## A factores

train<- train %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         jefe_Educ_level=factor(jefe_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )

test<- test %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         jefe_Educ_level=factor(jefe_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )

##Modelo1

modelo1 <- glm(Pobre ~Dominio + jefe_Educ_level + maxEducLevel + jefe_Ocupado + jefe_pension + jefe_seguridad)

   ##Predicción

predictSample <- test   %>% 
  mutate(pobre_lab = predict(modelo1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)


    ##Formato Kaggle
    predictSample <- predictSample %>% 
      mutate(pobre = ifelse(pobre_lab=="Yes",1,0)) %>% select(id, pobre)
    
    write.csv(predictSample, "Logit.csv", row.names = FALSE)
  
