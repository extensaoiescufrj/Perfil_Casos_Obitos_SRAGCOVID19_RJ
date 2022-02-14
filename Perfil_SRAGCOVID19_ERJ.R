# Universidade Federal do Rio de Janeiro
# Instituto de Estudos em Saúde Coletiva
# Projeto de extensão: Ações de apoio de vigilãncia epidemiológica contra a epidemia da covid-19
# Titulo: Características cínicas e demográficas 
# Estudantes: Pedro Henrique, Mariana Costa e Ednei cesar
# Local: Rio de Janeiro
# Período: 03 de março de 2020 a 10 de outubro de 2021 
# SRAG hospitalizados e letalidade por covid-19 em 2020 e 2021 
# UF residencia: RJ
# download das bases de dados em 10/10/2021
# filtro: classificação final - SRAG por COVID-19

############################ PACTOES 

library(rio) # importar base de dados 
library(dplyr) # manipular usando %>% chamar sempre que eu for usar o pacote
library(tidyr) # usar comando replace_na
library(gtsummary)
library(stats)
library(ggplot2)
library(esquisse)
library(scales)
library(readxl)
library(stringr)
library(reshape2)
library(openxlsx)
library(gganimate)
library(lubridate)
library(PHEindicatormethods)
library(geobr)
library(crul)
library(janitor)
library(patchwork)
library(crul)
library(sf)

############################# MUDANDO O DIRETORIO

#Session >> Set working directory >> choose...

####### Bases

sivep_gripe_2020 <- read.csv("sivep_gripe_2020.csv", sep=";")
sivep_gripe_2021 <- read.csv("sivep_gripe_2021.csv", sep=";")


######## Jutando as duas bases ##########
 
SRAG_RJ <- rbind(sivep_gripe_2020, sivep_gripe_2021)

 ############################## come?a aqui ###############

# base de dados da pop

pop.fxetaria <- read_excel("PopTotal_RJ_2020-pormunicp.xlsx")

#### Filtrar so para municipio do RJ ##

#UF - RJ

table(SRAG_RJ$UF.de.residência)

SRAG_RJ <- SRAG_RJ %>%
  filter(UF.de.residência == "RJ")

SRAG_RJ <- SRAG_RJ %>%
  filter(Classificação.final.do.caso == "5 - COVID-19")

table(SRAG_RJ$Unidade.da.idade)


SRAG_RJ$Idade.anos <- NULL # criando uma variavel vazia
SRAG_RJ$idade.anos[SRAG_RJ$Unidade.da.idade == "3 - Anos"] <- SRAG_RJ$Idade[SRAG_RJ$Unidade.da.idade == "3 - Anos"]
SRAG_RJ$idade.anos[SRAG_RJ$Unidade.da.idade == "2 - Meses"] <- SRAG_RJ$Idade[SRAG_RJ$Unidade.da.idade == "2 - Meses"]/12 # transformando meses em anos
SRAG_RJ$idade.anos[SRAG_RJ$Unidade.da.idade == "1 - Dias"] <- SRAG_RJ$Idade[SRAG_RJ$Unidade.da.idade == "1 - Dias"]/365.25 # transformando dias em anos

summary(SRAG_RJ$idade.anos)

# criando faixa etaria

SRAG_RJ <- SRAG_RJ %>%
  mutate(idade.anos = as.numeric(idade.anos))

# faixas: '0 a 4', '5 a 9', '10 a 14', '15 a 19', '20 a 24', '25 a 29', '30 a 34','35 a 39', 
#'40 a 44', '45 a 49', '50 a 54', '55 a 59', '60 a 64', '65 a 69', '70 a 74', '75 a 79', '80 ou mais'

SRAG_RJ <- SRAG_RJ %>%
  mutate(fx_etaria = cut(idade.anos, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                                                55, 60, 65, 70, 75, 80, Inf), right = FALSE))

# renomeando as categorias de fx etaria

SRAG_RJ <- SRAG_RJ %>%
  mutate(fx_etaria = cut(idade.anos, breaks = c (0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                                                 55, 60, 65, 70, 75, 80, Inf), right = FALSE, 
                         labels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", 
                                    "30 a 34","35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", 
                                    "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 ou mais")))

table(SRAG_RJ$fx_etaria) 



############################ Análise descritiva do SRAG #############

srag <- SRAG_RJ %>%
  select(Semana.epidemiológica.dos.primeiros.sintomas,idade.anos, Sexo, Evolução.do.caso, Raça.cor, Idade,  Uso.de.suporte.ventilatório ,
         Data.dos.primeiros.sintomas, Esfera.administrativa.da.unidade.notificante, Internado.em.UTI,
         Sintomas...febre, Sintomas...tosse,Sintomas...garganta ,Sintomas...dispneia ,Sintomas...desconforto.respiratório,                          
         Sintomas...saturação.O2.95., Sintomas...diarreia,Sintomas...vômito,Sintomas...dor.abdominal,Sintomas...fadiga,                                            
         Sintomas...perda.do.olfato, Sintomas...perda.do.paladar, Possui.fatores.de.risco.comorbidades, 
         Fatores.de.risco...puérpera,Fatores.de.risco...doença.cardiovascular.crônica,
         Fatores.de.risco...doença.hematológica.crônica,Fatores.de.risco...síndrome.de.Down,
         Fatores.de.risco...doença.hepática.crônica,Fatores.de.risco...asma,Fatores.de.risco...diabetes.mellitus,                         
         Fatores.de.risco...doença.neurológica.crônica,Fatores.de.risco...outra.pneumopatia.crônica,                 
         Fatores.de.risco...imunodeficiência.imunodepressão, Fatores.de.risco...doença.renal.crônica,                      
         Fatores.de.risco...obesidade,Fatores.de.risco...IMC.obesidade, Fatores.de.risco...tem.outros,Fatores.de.risco...todos,
         Fatores.de.risco...outros)                             



############# Análise descritiva dos casos

srag <- srag %>%
  mutate(Evolução.do.caso = recode(Evolução.do.caso, `1 - Cura` = "Cura", `2 - Óbito` = "Óbito", `3 - Óbito por outras causas` = "Óbito por outras causas",
                                   `9 - Ignorado` = "Ignorado"),
         Raça.cor = recode(Raça.cor, `1 - Branca` = "Branca", `2 - Preta` = "Preta", `3 - Amarela` = "Amarela", `4 - Parda` = "Parda", `5 - Indígena` = "Indígena", `9 - Ignorado` = "Ignorado"),
         Sexo = recode(Sexo, `1 - Masculino` = "Masculino", `2 - Feminino` = "Feminino", `9 - Ignorado` = "Ignorado"),
         Uso.de.suporte.ventilatório = recode(Uso.de.suporte.ventilatório, `1 - Sim, invasivo` = "Sim, invasivo", `2 - Sim, não invasivo` = "Sim, não invasivo", `3 - Não` = "Não", `9 - Ignorado` = "Ignorado"))

summary(srag$Sexo)
summary(srag$Evolução.do.caso)
summary(srag$Raça.cor)
summary(srag$Uso.de.suporte.ventilatório)


####### Criando faixa etaria (fx_etaria)

srag <- srag %>%
  mutate(fx_etaria = cut(idade.anos, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, Inf),
                         right = FALSE,
                         labels = c("0 a 19", "20 a 29", "30 a 39", "40 a 49",
                                    "50 a 59", "60 a 69", "70 a 79", "80 ou mais"))) 


table(srag$fx_etaria)

####### Pirâmide para TODO. Distribuição de casos segundo por covid-19

piramide <- srag %>%
  filter(Sexo != "Ignorado") %>%
  group_by(fx_etaria, Sexo) %>%
  tally

srag %>% filter (Sexo == "Masculino") %>% 
  group_by(fx_etaria) %>% tally()


ggplot(data = piramide, 
       mapping = aes(x = fx_etaria,
                     y = ifelse(test = Sexo == "Feminino",  yes = n, no = -n), fill = Sexo))+
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide$n))* c(-1,1)) +
  labs(y = "Número de casos", x = "Faixa etária (em anos)",
       title= "Figura 1: Distribuição de casos por covid-19",
       subtitle = "ERJ, 01/03/2020 a 10/10/2021" ,
       caption = "Fonte dos dados: SIVEP-Gripe") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  coord_flip() +
  theme_minimal(base_size = 15)+
  theme(legend.position= "bottom",
        legend.text = element_text(size= 15),
        legend.title = element_text(size= 15))


#### Criando categorias óbito nao obito por SRAG COVID-19


srag$Desfecho <- ifelse(srag$Evolução.do.caso == "Óbito", "Óbito por SRAG", "Cura/Em cuidados hospitalares") 
srag$Desfecho <- as.factor(srag$Desfecho)
summary(srag$Desfecho)


####### Colacando "" para ignorado

srag$Evolução.do.caso[ srag$Evolução.do.caso == ""] <- "Ignorado"
srag$Raça.cor[ srag$Raça.cor == ""] <- "Ignorado"
srag$Uso.de.suporte.ventilatório[ srag$Uso.de.suporte.ventilatório == " "] <- "Ignorado"


summary(srag$Evolução.do.caso)
summary(srag$Raça.cor)
summary(srag$Uso.de.suporte.ventilatório)


####### Retirando a categoria que "" que esta zerada
srag$Evolução.do.caso <- factor( srag$Evolução.do.caso )
srag$Raça.cor <- factor( srag$Raça.cor )
srag$Uso.de.suporte.ventilatório <- factor( srag$Uso.de.suporte.ventilatório )
srag$Sexo <- factor(srag$Sexo)

####### Verificando a tabela
summary(srag$Evolução.do.caso)
summary(srag$Raça.cor)
summary(srag$Uso.de.suporte.ventilatório)

####### Mudando a ordem

levels(srag$Raça.cor)

srag$Raça.cor <- factor(srag$Raça.cor, levels = c ("Branca","Parda/Preta", "Parda", "Preta", "Amarela", "Indígena", "Ignorado"  ))

srag$Raça.cor[srag$Raça.cor == "Parda" | srag$Raça.cor == "Preta"] <- "Parda/Preta"
srag$Raça.cor <- factor(srag$Raça.cor)

table(srag$Raça.cor) 
table(srag$Sexo)



srag <- srag %>%
  mutate(Sintomas...desconforto.respiratório = recode(Sintomas...desconforto.respiratório,
                                                      `1 - Sim` = "Sim", `2 - Não` = "Não", `9 - Ignorado` = "Ignorado"),
         Sintomas...dispneia = recode(Sintomas...dispneia, `1 - Sim` = "Sim", `2 - Não` = "Não", `9 - Ignorado` = "Ignorado"),
         Sintomas...saturação.O2.95. = recode(Sintomas...saturação.O2.95., `1 - Sim` = "Sim", `2 - Não` = "Não", `9 - Ignorado` = "Ignorado"),
         Internado.em.UTI = recode(Internado.em.UTI, `1 - Sim` = "Sim", `2 - Não` = "Não", `9 - Ignorado` = "Ignorado"))

####### Criando srag$num.comorbidades

dados.aux <- srag %>%
  select(Fatores.de.risco...puérpera,                                  
         Fatores.de.risco...doença.cardiovascular.crônica,             
         Fatores.de.risco...doença.hematológica.crônica,               
         Fatores.de.risco...síndrome.de.Down,                          
         Fatores.de.risco...doença.hepática.crônica,                   
         Fatores.de.risco...asma,                                      
         Fatores.de.risco...diabetes.mellitus,                         
         Fatores.de.risco...doença.neurológica.crônica,                
         Fatores.de.risco...outra.pneumopatia.crônica,                 
         Fatores.de.risco...imunodeficiência.imunodepressão,           
         Fatores.de.risco...doença.renal.crônica,                      
         Fatores.de.risco...obesidade,
         Fatores.de.risco...tem.outros,
         Fatores.de.risco...todos,
         Fatores.de.risco...outros)


dados.aux$Fatores.de.risco...puérpera <- ifelse(dados.aux$Fatores.de.risco...puérpera == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...doença.cardiovascular.crônica <- ifelse(dados.aux$Fatores.de.risco...doença.cardiovascular.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...doença.hematológica.crônica <- ifelse(dados.aux$Fatores.de.risco...doença.hematológica.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...síndrome.de.Down <- ifelse(dados.aux$Fatores.de.risco...síndrome.de.Down == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...doença.hepática.crônica <- ifelse(dados.aux$Fatores.de.risco...doença.hepática.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...asma <- ifelse(dados.aux$Fatores.de.risco...asma == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...diabetes.mellitus <- ifelse(dados.aux$Fatores.de.risco...diabetes.mellitus == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...doença.neurológica.crônica <- ifelse(dados.aux$Fatores.de.risco...doença.neurológica.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...outra.pneumopatia.crônica <- ifelse(dados.aux$Fatores.de.risco...outra.pneumopatia.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...imunodeficiência.imunodepressão <- ifelse(dados.aux$Fatores.de.risco...imunodeficiência.imunodepressão == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...doença.renal.crônica <- ifelse(dados.aux$Fatores.de.risco...doença.renal.crônica == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...obesidade <- ifelse(dados.aux$Fatores.de.risco...obesidade == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...tem.outros <- ifelse(dados.aux$Fatores.de.risco...tem.outros == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...todos <- ifelse(dados.aux$Fatores.de.risco...todos == "1 - Sim", 1, 0)
dados.aux$Fatores.de.risco...outros <- ifelse(dados.aux$Fatores.de.risco...outros == "1 - Sim", 1, 0)


####### Somando as linhas de dados.aux
srag$num.comorbidades <- apply(dados.aux, MARGIN = 1, sum)
table(srag$num.comorbidades)

srag$num.comorbidades.cat <- cut(srag$num.comorbidades, breaks = c( 0, 1, 2, 3, Inf), right = FALSE,
                                 labels =c("0", "1", "2", "3 ou mais"))
table(srag$num.comorbidades.cat)


####### Criando srag$Esfera.Admin

srag$Esfera.administrativa.da.unidade.notificante <- factor(srag$Esfera.administrativa.da.unidade.notificante)

summary(srag$Esfera.administrativa.da.unidade.notificante)
levels(srag$Esfera.administrativa.da.unidade.notificante)



srag$Esfera.Admin <- NULL

srag$Esfera.Admin[srag$Esfera.administrativa.da.unidade.notificante == "01 - Administra??o P?blica Federal" | 
                    srag$Esfera.administrativa.da.unidade.notificante == "03 - Administra??o P?blica Municipal" |
                    srag$Esfera.administrativa.da.unidade.notificante == "02 - Administra??o P?blica Estadual ou Distrito Federal"] <- "P?blico"

srag$Esfera.Admin[srag$Esfera.administrativa.da.unidade.notificante == "06 - Entidades Empresariais" ] <- "Privado"

srag$Esfera.Admin[srag$Esfera.administrativa.da.unidade.notificante == "07 - Entidades sem Fins Lucrativos"] <- "Entidades sem Fins Lucrativos"

srag$Esfera.Admin <- factor(srag$Esfera.Admin, levels = c("Entidades sem Fins Lucrativos", "Privado", "P?blico", "Ignorado"))

srag$Esfera.Admin[srag$Esfera.Admin == ""] <- "Ignorado"
srag$Esfera.Admin[is.na(srag$Esfera.Admin)] <- "Ignorado"

srag$Esfera.Admin <- as.factor(srag$Esfera.Admin)
levels(srag$Esfera.Admin)
summary(srag$Esfera.Admin)



####### uso Sintomas...dispneia - colocando "" em ignorado
table(srag$Sintomas...dispneia)
srag$Sintomas...dispneia[srag$Sintomas...dispneia == " "] <- "Ignorado"
table(srag$Sintomas...dispneia)
srag$Sintomas...dispneia <- factor(srag$Sintomas...dispneia)

####### uso Sintomas...desconforto.respiratório - colocando "" em ignorado
levels(srag$Sintomas...desconforto.respiratório)
srag$Sintomas...desconforto.respiratório[srag$Sintomas...desconforto.respiratório == " "] <- "Ignorado"
table(srag$Sintomas...desconforto.respiratório)
srag$Sintomas...desconforto.respiratório <- factor(srag$Sintomas...desconforto.respiratório)

####### uso Sintomas...satura??o.O2.95. - colocando "" em ignorado
levels(srag$Sintomas...saturação.O2.95.)
srag$Sintomas...saturação.O2.95.[srag$Sintomas...saturação.O2.95. == " "] <- "Ignorado"
table(srag$Sintomas...saturação.O2.95.)
srag$Sintomas...saturação.O2.95. <- factor(srag$Sintomas...saturação.O2.95.)

####### UTI - colocando "" em ignorado
levels(srag$Internado.em.UTI)
srag$Internado.em.UTI[srag$Internado.em.UTI == " "] <- "Ignorado"
table(srag$Internado.em.UTI)
srag$Internado.em.UTI <- factor(srag$Internado.em.UTI)

srag$num.comorbidades
names(srag)


####### Tabela geral

srag.tabela <-  srag %>%
  select(Desfecho,
         Sexo, Raça.cor,
         fx_etaria,
         Esfera.Admin,
         Internado.em.UTI, 
         Uso.de.suporte.ventilatório,
         num.comorbidades.cat,
         Sintomas...dispneia,
         Sintomas...desconforto.respiratório,                          
         Sintomas...saturação.O2.95. )

srag.tabela %>%
  tbl_summary( label = c(Raça.cor ~ "Raça/cor", 
                         Internado.em.UTI ~ "Internação em UTI", 
                         Uso.de.suporte.ventilatório ~ "Uso de suporte ventilatório",
                         fx_etaria ~"Faixa etária",
                         Esfera.Admin ~ "Instituição notificadora",
                         Sintomas...dispneia ~ "Dispneia",
                         num.comorbidades.cat ~ "número de comorbidades",
                         Sintomas...desconforto.respiratório ~ "Desconforto Respiratório ",
                         Sintomas...saturação.O2.95.~ "Saturação O2 < 95%"
  ),
  digits = all_categorical() ~ 1) %>%
  modify_header(label ~ "**Características**") %>%
  bold_labels()  


####### Tabela geral 2x2, por desfecho

srag.tabela %>%
  tbl_summary( by = Desfecho, label = c(Raça.cor ~ "Raça/cor", 
                                        Internado.em.UTI ~ "Internação em UTI", 
                                        Uso.de.suporte.ventilatório ~ "Uso de suporte ventilatório",
                                        fx_etaria ~"Faixa etária",
                                        Esfera.Admin ~ "Instituição notificadora",
                                        Sintomas...dispneia ~ "Dispneia",
                                        num.comorbidades.cat ~"número de comorbidades",
                                        Sintomas...desconforto.respiratório ~ "Desconforto Respiratório ",
                                        Sintomas...saturação.O2.95.~ "Saturação O2 < 95%"
  ),
  percent = "row",
  digits = all_categorical() ~ 1) %>%
  modify_header(label ~ "**Características**") %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Desfecho**") %>%
  bold_labels()   


############### VER AS COMORBIDADES/FATORES DE RISCO EM RELAÇÃO A COVID-19 ################
srag.tabela2 <-  srag %>%
  select(Fatores.de.risco...puérpera,                                
         Fatores.de.risco...doença.cardiovascular.crônica,             
         Fatores.de.risco...doença.hematológica.crônica,               
         Fatores.de.risco...síndrome.de.Down,                          
         Fatores.de.risco...doença.hepática.crônica,                   
         Fatores.de.risco...asma,                                      
         Fatores.de.risco...diabetes.mellitus,                         
         Fatores.de.risco...doença.neurológica.crônica,                
         Fatores.de.risco...outra.pneumopatia.crônica,                 
         Fatores.de.risco...imunodeficiência.imunodepressão,           
         Fatores.de.risco...doença.renal.crônica,                      
         Fatores.de.risco...obesidade,
         Fatores.de.risco...tem.outros,
         Fatores.de.risco...todos,
         Fatores.de.risco...outros)


srag.tabela2 %>%
  tbl_summary( label = c(Fatores.de.risco...puérpera ~ "puérpera",                                
                         Fatores.de.risco...doença.cardiovascular.crônica ~"doença cardio",             
                         Fatores.de.risco...doença.hematológica.crônica ~ "doença hemato",               
                         Fatores.de.risco...síndrome.de.Down ~ "sindrome",                          
                         Fatores.de.risco...doença.hepática.crônica ~"doença hepatica",                   
                         Fatores.de.risco...asma ~ "asma",                                      
                         Fatores.de.risco...diabetes.mellitus ~ "mellitus",                         
                         Fatores.de.risco...doença.neurológica.crônica ~ "neurológica",                
                         Fatores.de.risco...outra.pneumopatia.crônica ~ "pneumopatia crônica",                 
                         Fatores.de.risco...imunodeficiência.imunodepressão ~ "imunodeficiencia",           
                         Fatores.de.risco...doença.renal.crônica ~ "renal",                      
                         Fatores.de.risco...obesidade ~ "oesidade",
                         Fatores.de.risco...tem.outros ~ "tem outros",
                         Fatores.de.risco...todos ~ "todos",
                         Fatores.de.risco...outros ~ "outros"),
  digits = all_categorical() ~ 1) %>%
  modify_header(label ~ "**Características**") %>%
  bold_labels()  

#############################################################

############## graficos ##############

### Semana epidemioligica dos primeiros sintomas

tab.SEsint.casos <- srag %>%
  filter(Semana.epidemiológica.dos.primeiros.sintomas != "202117")%>%
  group_by(Semana.epidemiológica.dos.primeiros.sintomas) %>%
  tally

tab.SEsint.casos$tipo <- "Casos"

tab.SEsint.ob <- srag %>%
  filter(Semana.epidemiológica.dos.primeiros.sintomas != "202117" & Desfecho == "Óbito por SRAG")%>%
  group_by(Semana.epidemiológica.dos.primeiros.sintomas) %>%
  tally

tab.SEsint.ob$tipo <- "Óbito"

tab.SEsint <- rbind(tab.SEsint.casos, tab.SEsint.ob)

tab.SEsint$SE <- as.factor(tab.SEsint$Semana.epidemiológica.dos.primeiros.sintomas) 

ggplot(tab.SEsint) +
  aes(x = SE, fill = tipo, weight = n) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Semana epidemiológica dos 1ºs sintomas", y="Frequ?ncia", fill = "Tipo")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


################ Desfecho

Tab.desfecho <- srag %>%
  group_by(Desfecho) %>%
  tally    

#colocar procentagem

Tab.desfecho$porcent <- round((100 * Tab.desfecho$n / sum(Tab.desfecho$n)),1)


############ grafico de desfecho em porcentagem

ggplot(Tab.desfecho, aes(x= Desfecho, y= porcent)) +
  geom_bar(stat="identity", fill= "steelblue") +
  labs( y = "%", x = "Desfecho", caption = "Fonte: Fonte dos dados:SIVEP-Gripe")+ 
  ylim(0 , max(Tab.desfecho$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), vjust= - 0.2, size = 4) +
  theme_classic(base_size = 16) 


################# Casos de SRAG segundo cor da pele

Tab.raca <- srag %>%
  group_by(Raça.cor) %>%
  filter(Raça.cor != "Amarela") %>%
  filter(Raça.cor != "Indígena") %>%
  filter(Raça.cor != "Ignorado") %>%
  tally 

#colocar procentagem

Tab.raca$porcent <- round((100 * Tab.raca$n / sum(Tab.raca$n)),1)


################# grafico de raca em porcentagem
ggplot(Tab.raca, aes(x= reorder(Raça.cor, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "darkslategray3") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Raça ou cor da pele", title= "Figura 3: Casos de SRAG por COVID-19 segundo a cor de pele", caption = "Fonte: - SIVEP-Gripe")+ 
  ylim(0 , max(Tab.raca$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 



################# Casos de SRAG segundo num.comorbidades.cat

Tab.comorb <- srag %>%
  group_by(num.comorbidades.cat) %>%
  tally    

#colocar procentagem

Tab.comorb$porcent <- round((100 * Tab.comorb$n / sum(Tab.comorb$n)),1)


### grafico de num comorbidades categorico em porcentagem

ggplot(Tab.comorb, aes(x= num.comorbidades.cat, y= porcent)) +
  geom_bar(stat="identity", fill= "chocolate2") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Nº de comorbidades", 
        title= "Figura 2: Distribuição de número de comorbidades",
        subtitle = "ERJ, 01/03/2020 a 10/10/2021",
        caption = "Fonte: SIVEP-GRIPE")+ 
  ylim(0 , max(Tab.comorb$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 




################# Casos de SRAG segundo internacao UTI

Tab.uti <- srag %>%
  group_by(Internado.em.UTI) %>%
  tally    

#colocar procentagem

Tab.uti$porcent <- round((100 * Tab.uti$n / sum(Tab.uti$n)),1)


################# grafico de SRAG segundo internacao UTI

ggplot(Tab.uti, aes(x= reorder(Internado.em.UTI, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "lightskyblue") +
  labs( y = "%", x = "Internação em UTI", title= "Figura 4: Distribuição dos casos de SRAG com COVID-19 segundo internações em UTI", 
        subtitle = "ERJ, 01/03/2020 a 10/10/2021", caption = "Fonte: Fonte dos dados: SIVEP-Gripe")+ 
  ylim(0 , max(Tab.uti$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.1, size = 4) +
  theme_classic(base_size = 14) 



############# Casos de SRAG segundo uso de suporte ventilatorio

Tab.suporte <- srag %>%
  group_by(Uso.de.suporte.ventilatório) %>%
  tally    

#colocar procentagem

Tab.suporte$porcent <- round((100 * Tab.suporte$n / sum(Tab.suporte$n)),1)


################# grafico de raca em porcentagem

ggplot(Tab.suporte, aes(x= reorder(Uso.de.suporte.ventilatório, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "mediumaquamarine") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Uso de suporte ventilatório", title= "Figura 5: Distribuição dos casos de SRAG segundo o uso de suporte ventilatorio", caption = "Fonte: SIVEP-GRIPE")+ 
  ylim(0 , max(Tab.suporte$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 

################# Casos de SRAG segundo dispneia

Tab.dispneia <- srag %>%
  group_by(Sintomas...dispneia) %>%
  tally    

#colocar procentagem

Tab.dispneia$porcent <- round((100 * Tab.dispneia$n / sum(Tab.dispneia$n)),1)


################# grafico de raca em porcentagem

ggplot(Tab.dispneia, aes(x= reorder(Sintomas...dispneia, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "steelblue") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Dispn?ia", caption = "Fonte: SIVEP-GRIPE")+ 
  ylim(0 , max(Tab.dispneia$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 

################# Casos de SRAG segundo desconforto respiratorio

Tab.desconforto <- srag %>%
  group_by(Sintomas...desconforto.respirat?rio) %>%
  tally    

#colocar procentagem

Tab.desconforto$porcent <- round((100 * Tab.desconforto$n / sum(Tab.desconforto$n)),1)


### grafico de raca em porcentagem

ggplot(Tab.desconforto, aes(x= reorder(Sintomas...desconforto.respirat?rio, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "steelblue") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Desconforto respirat?rio", caption = "Fonte: SIVEP-GRIPE")+ 
  ylim(0 , max(Tab.desconforto$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 


################# Casos de SRAG segundo saturacao


Tab.saturacao <- srag %>%
  group_by(Sintomas...satura??o.O2.95.) %>%
  tally    

#colocar procentagem

Tab.saturacao$porcent <- round((100 * Tab.saturacao$n / sum(Tab.saturacao$n)),1)


### grafico de raca em porcentagem

ggplot(Tab.saturacao, aes(x= reorder(Sintomas...satura??o.O2.95., -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "steelblue") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "%", x = "Satura??o O2 < 95%", caption = "Fonte: SIVEP-GRIPE")+ 
  ylim(0 , max(Tab.saturacao$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 


# gráfico de linhas data 1º sint casos 

srag <- srag %>%
  mutate(Data.dos.primeiros.sintomas = as.Date(Data.dos.primeiros.sintomas, format = "%d/%m/%Y"))

srag$Data.dos.primeiros.sintomas <- factor(srag$Data.dos.primeiros.sintomas)


tab.1sint.casos <- srag %>%
  group_by(Data = Data.dos.primeiros.sintomas, .drop=FALSE) %>%
  tally

tab.1sint.casos$tipo <- "Casos"

tab.1sint.ob <- srag %>%
  filter(Desfecho == "?bito por SRAG")%>%
  group_by(Data = Data.dos.primeiros.sintomas, .drop=FALSE) %>%
  tally

tab.1sint.ob$tipo <- "?bito"


tab.1sint <- rbind(tab.1sint.casos, tab.1sint.ob)
