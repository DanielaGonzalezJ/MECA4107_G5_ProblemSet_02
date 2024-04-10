###  Modelo Cart//Random Forest
#Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” es menor a la Linea de pobreza que le corresponde al hogar.

#rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        rpart, #Árbol
        rpart.plot, #Graficos de Árbol
        ranger #Random Forest
)

#Cargamos la base de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/Taller II")
train_hogares<-read.csv("Data/train_hogares.csv")
train_personas<-read.csv("Data/train_personas.csv")
test_hogares<-read.csv("data/test_hogares.csv")
test_personas<-read.csv("data/test_personas.csv")

#Preprocesamiento Personas

ptrain_personas<- train_personas %>% mutate(
  genero = ifelse(P6020==2,1,0),# 1 si es hombre
  cabecera = Clase,# 1 si reside en áreas urbanas
  edad=P6040,
  jefe = ifelse(P6050== 1, 1, 0), # 1 si es jefe de hogar
  menor = ifelse(P6040<=12,1,0), # Menores de 12 años
  EducLevel = ifelse(P6210==9|P6210==0,0,P6210), #No sabe no responde es 0
  ocupado = ifelse(is.na(Oc),0,1), 
  contributivo=ifelse(P6090==1|P6090==2,1,0), #No sabe no responde es 0 (1 )
  trabaja=ifelse(P6240==1,1,0), # 1 si el individuo trabaja
  Oficio=ifelse(is.na(Oficio),0,Oficio), 
  trabaja_solo=ifelse(P6870==1|is.na(P6870),1,0), # 1 Si el individuo trabaja solo
  h_trabajadas=ifelse(is.na(P6800),0,P6800),
  tiempotrab = P6426,# Tiempo que lleva trabajando en la empresa
  sin_pension = ifelse((P7500s2 == 2 | is.na(P7500s2)) & ((edad >= 62 & genero == 0) | (edad >= 57 & genero == 1)), 1, 0), #Sin pension cuando debería
  p=P7500s2,
  pet=ifelse(Pet==1,1,0)# Hace parte de la pob en edad de trabajar
  ) %>% 
select(id, Orden,genero,edad,cabecera,jefe,menor,EducLevel,trabaja_solo,contributivo,h_trabajadas,ocupado,contributivo,trabaja,Oficio,tiempotrab,sin_pension,pet)

colnames(ptrain_personas)

#Replica para la base de test

ptest_personas<- test_personas %>% mutate(
  genero = ifelse(P6020==2,1,0),# 1 si es hombre
  cabecera = Clase,# 1 si reside en áreas urbanas
  edad=P6040,
  jefe = ifelse(P6050== 1, 1, 0), # 1 si es jefe de hogar
  menor = ifelse(P6040<=12,1,0), # Menores de 12 años
  EducLevel = ifelse(P6210==9|P6210==0,0,P6210), #No sabe no responde es 0
  ocupado = ifelse(is.na(Oc),0,1), 
  contributivo=ifelse(P6090==1|P6090==2,1,0), #No sabe no responde es 0 (1 )
  trabaja=ifelse(P6240==1,1,0), # 1 si el individuo trabaja
  Oficio=ifelse(is.na(Oficio),0,Oficio), 
  trabaja_solo=ifelse(P6870==1|is.na(P6870),1,0), # 1 Si el individuo trabaja solo
  h_trabajadas=ifelse(is.na(P6800),0,P6800),
  tiempotrab = P6426,# Tiempo que lleva trabajando en la empresa
  sin_pension = ifelse((P7500s2 == 2 | is.na(P7500s2)) & ((edad >= 62 & genero == 0) | (edad >= 57 & genero == 1)), 1, 0), #Sin pension cuando debería
  p=P7500s2,
  pet=ifelse(Pet==1,1,0)# Hace parte de la pob en edad de trabajar
) %>% 
  select(id, Orden,genero,edad,cabecera,jefe,menor,EducLevel,trabaja_solo,contributivo,h_trabajadas,ocupado,contributivo,trabaja,Oficio,tiempotrab,sin_pension,pet)

colnames(ptest_personas)

#Preprocesamiento Hogares
train_personas_nivel_hogar<- ptrain_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(genero,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            rel_pet=sum(pet,na.rm=TRUE)/max(Orden,na.rm=TRUE),
            horas_trab_por_persona=sum(h_trabajadas)/max(Orden,na.rm=TRUE)
  )

train_personas_hogar<- ptrain_personas %>% 
  filter(jefe==1) %>% 
  select(id,genero,EducLevel,ocupado,trabaja_solo,contributivo,edad,genero,sin_pension) %>% 
  rename(jefe_mujer=genero,
         jefe_Educ_level=EducLevel,
         jefe_ocupado=ocupado,
         jefe_solo=trabaja_solo,
         jefe_contrib=contributivo,
         jefe_sin_pension=sin_pension,
         jefe_edad=edad) %>% 
  left_join(train_personas_nivel_hogar)


#Replica para la base de test
test_personas_nivel_hogar<- ptest_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(genero,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            rel_pet=sum(pet,na.rm=TRUE)/max(Orden,na.rm=TRUE),
            horas_trab_por_persona=sum(h_trabajadas)/max(Orden,na.rm=TRUE)
  )

test_personas_hogar<- ptest_personas %>% 
  filter(jefe==1) %>% 
  select(id,genero,EducLevel,ocupado,trabaja_solo,contributivo,edad,genero,sin_pension) %>% 
  rename(jefe_mujer=genero,
         jefe_Educ_level=EducLevel,
         jefe_ocupado=ocupado,
         jefe_solo=trabaja_solo,
         jefe_contrib=contributivo,
         jefe_sin_pension=sin_pension,
         jefe_edad=edad) %>% 
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
         arrienda=factor(arrienda),
         Dominio=factor(Dominio),
         jefe_Educ_level=factor(jefe_Educ_level,levels=c(1:6), labels=c('Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         jefe_contrib=factor(jefe_contrib),
         jefe_sin_pension=factor(jefe_sin_pension),
         jefe_mujer=factor(jefe_mujer),
         jefe_ocupado=factor(jefe_ocupado),
         jefe_solo=factor(jefe_solo)
         
         
  )
#Para Test
test<- test %>% 
  mutate(Dominio=factor(Dominio),
         arrienda=factor(arrienda),
         jefe_Educ_level=factor(jefe_Educ_level,levels=c(1:6), labels=c('Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         jefe_contrib=factor(jefe_contrib),
         jefe_sin_pension=factor(jefe_sin_pension),
         jefe_mujer=factor(jefe_mujer),
         jefe_ocupado=factor(jefe_ocupado),
         jefe_solo=factor(jefe_solo)
         
  )

#Entrenamiento del Modelo Random Forest

set.seed(2618)
tree_ranger_grid <- train(
  Pobre~.,
  data=train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mtry = c(1,2,3),
    splitrule = "variance",
    min.node.size = c(1,3,5)),
  importance="impurity"
)



#Entrenamiento del modelo Carts

set.seed(2618)
arbol_clasificacion_rpart <- rpart(Pobre~., 
                                   data    = train,
                                   method = "class",
                                   parms = list(split = "Gini"))

arbol_clasificacion_rpart

#Arbol gráfico
prp(arbol_clasificacion_rpart, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")


#Envío para Kaggle

predictSample <- test   %>% 
  mutate(pobre_lab = predict(arbol_clasificacion_rpart, newdata = test, type = "class")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

write.csv(predictSample,"classification_CARTS.csv", row.names = FALSE)
 