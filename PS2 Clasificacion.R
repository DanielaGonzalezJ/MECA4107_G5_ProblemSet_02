###  Modelo Cart//Random Forest
#Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” es menor a la Linea de pobreza que le corresponde al hogar.

#rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        rpart, #Árbol
        rpart.plot #Graficos de Árbol
)

#Cargamos la base de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/Taller II")
train_hogares<-read.csv("Data/train_hogares.csv")
train_personas<-read.csv("Data/train_personas.csv")
test_hogares<-read.csv("data/test_hogares.csv")
test_personas<-read.csv("data/test_personas.csv")

colnames(train_hogares)
colnames(train_personas)

#Preprocesamiento Personas AÑADIR MUCHAS MÁS

ptrain_personas<- train_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), # 1 si es Mujer
  H_Head = ifelse(P6050== 1, 1, 0), # 1 si es jefe de hogar
  menor = ifelse(P6040<=6,1,0), # Menores de 6 años
  EducLevel = ifelse(P6210==9,0,P6210), #No sabe no responde es 0
  ocupado = ifelse(is.na(Oc),0,1), 
  contributivo=ifelse(P6090==1|P6090==2,1,0), #No sabe no responde es 0 (1 )
  trabaja=ifelse(P6240==1,1,0), # 1 si el individuo trabaja
  Oficio=ifelse(is.na(Oficio),0,Oficio), 
  trabaja_solo=ifelse(P6870==1,1,0), # 1 Si el individuo trabaja solo
  h_trabajadas=ifelse(is.na(P6800),0,P6800)
  ) %>% 
  select(id, Orden,mujer,H_Head,menor,EducLevel,trabaja_solo,h_trabajadas,ocupado,contributivo,trabaja,Oficio)
colnames(ptrain_personas)

#Replica para la base de test

ptest_personas<- test_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), # 1 si es Mujer
  H_Head = ifelse(P6050== 1, 1, 0), # 1 si es jefe de hogar
  menor = ifelse(P6040<=6,1,0), # Menores de 6 años
  EducLevel = ifelse(P6210==9,0,P6210), #No sabe no responde es 0
  ocupado = ifelse(is.na(Oc),0,1), 
  contributivo=ifelse(P6090==1|P6090==2,1,0), #No sabe no responde es 0 (1 )
  trabaja=ifelse(P6240==1,1,0), # 1 si el individuo trabaja
  Oficio=ifelse(is.na(Oficio),0,Oficio), 
  trabaja_solo=ifelse(P6870==1,1,0), # 1 Si el individuo trabaja solo
  h_trabajadas=ifelse(is.na(P6800),0,P6800)
) %>% 
  select(id, Orden,mujer,H_Head,menor,EducLevel,trabaja_solo,h_trabajadas,ocupado,contributivo,trabaja,Oficio)
colnames(ptest_personas)

#Preprocesamiento Hogares
train_personas_nivel_hogar<- ptrain_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE)
  )

train_personas_hogar<- ptrain_personas %>% 
  filter(H_Head==1) %>% 
  select(id,mujer,EducLevel,ocupado) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado) %>% 
  left_join(train_personas_nivel_hogar)


#Replica para la base de test
test_personas_nivel_hogar<- ptest_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE)
  )

test_personas_hogar<- ptest_personas %>% 
  filter(H_Head==1) %>% 
  select(id,mujer,EducLevel,ocupado) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado) %>% 
  left_join(test_personas_nivel_hogar)

#Criterio de Pobreza del DANE
ptrain_hogares<- train_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  select(id,Dominio,arrienda,Pobre)


ptest_hogares<- test_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  select(id,Dominio,arrienda) 

#Unificamos la base de hogares a partir de la información de los hogares

train<- ptrain_hogares %>% 
  left_join(train_personas_hogar) %>% 
  select(-id) #no longer need id

test<- ptest_hogares %>% 
  left_join(test_personas_hogar)


#Convertimos las variables categóricas en factores
#Para Train
train<- train %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )
#Para Test
test<- test %>% 
  mutate(Dominio=factor(Dominio),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )
colnames(train)
#Entrenamiento del modelo

set.seed(2618)
arbol_clasificacion_rpart <- rpart(Pobre~., 
                                   data    = train,
                                   method = "class",
                                   parms = list(split = "Gini"))

arbol_clasificacion_rpart

#Arbol gráfico

prp(arbol_clasificacion_rpart, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")

# Error de test del modelo final con Martiz de Confusión
predicciones_rpart <- data.frame(
  obs = test$Pobre,                                    ## Observados
  pred = predict(arbol_clasificacion_rpart, newdata = test, type = "class")    ## Predichos
)


#Envío para Kaggle

predictSample <- test   %>% 
  mutate(pobre_lab = predict(arbol_clasificacion_rpart, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)





predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

template<-read.csv("Data/sample_submission.csv")
write.csv(predictSample,"classification_CARTS.csv", row.names = FALSE)

