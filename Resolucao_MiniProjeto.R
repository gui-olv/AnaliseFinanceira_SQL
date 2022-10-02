#Imporatando Dados


#setwd("")
df<-read.csv("dataset.csv")

#*******************************************************************************************#
# --------------------------- ANÁLISE EXPLORATÓRIA -----------------------------------------#
#*******************************************************************************************#

library(dplyr)

#Dimensões
dim(df)

#Visualização de Dados
View(df)

#Tipos de dados
str(df)

#Sumário
summary(df)


#Verificando Valores Ausentes

sum(!complete.cases(df))

colSums(is.na(df))

#Observando registro com valor ausente

df[is.na(df$RACE)==TRUE,]


#Eliminando Registro NA
df<-na.omit(df)

dim(df)

#Renomeando Colunas

#Renomeando Colunas
myColumns<-c()
myColumns[1] <- "Idade"
myColumns[2] <- "Mulher"
myColumns[3] <- "Tempo_Intern"
myColumns[4] <- "Raca"
myColumns[5] <- "Custo_Intern"
myColumns[6] <- "Grupo_Diagnostico"

# Verifica o resultado
myColumns

# Atribui os novos nomes de colunas ao dataframe
colnames(df) <- myColumns
rm(myColumns)
colnames(df) 

#*******************************************************************************#
#Biblioteca SQLdf

if (!require(sqldf))
  install.packages('sqldf')

library(sqldf)

#teste
sqldf("select * from df limit 5")

##############################
#      Etapa 1 - QUESTÕES    #
##############################


#1- Quantas raças estão representadas no dataset?
sqldf("select Raca,count(*) as QTD 
      from df
      group by Raca")
#***************************************
#
#***************************************
#2- Qual a idade média dos pacientes?

sqldf("Select avg(Idade) as Media_Idade 
      from df")
#***************************************
#
#***************************************
#3- Qual a moda da idade dos pacientes?
sqldf("Select Idade
      from(select Idade,count(*) as QTD_Idade
            from df
            group by Idade
            order by QTD_Idade desc) 
      limit 1")
#***************************************
# 
#***************************************

#4- Qual a variância da coluna idade?
sqldf("Select variance(Idade) as Var_Idade
      from df")
#***************************************
#
#***************************************

#5- Qual o gasto total com internações hospitalares por idade?
sqldf("select Idade,sum(Custo_Intern) as Custo_Internacoes 
      from df
      group by Idade")
#***************************************
#
#***************************************
#6- Qual idade gera o maior gasto total com internações hospitalares?
sqldf("select Idade,max(Custo_Internacoes) as Custo
      from(select Idade,sum(Custo_Intern) as Custo_Internacoes
          from df
          group by Idade)
      ")

#***************************************
#
#***************************************
#7- Qual o gasto total com internações hospitalares por gênero?
#Estamos partindo da premissa que os valores 
#0 = Feminino
#1 = Masculino 

sqldf("select Case Mulher
              WHEN 0 THEN 'Feminino'
              WHEN 1 THEN 'Masculino'
              end as Sexo,Custo_Internacoes
      from (select Mulher,sum(Custo_Intern) as Custo_Internacoes
            from df
            group by Mulher)        
      ")


#***************************************
#
#***************************************
#8- Qual a média de gasto com internações hospitalares por raça do paciente?

sqldf("select Raca,AVG(Custo_Intern) as Custo_Medio
            from df
            group by Raca       
      ")

#***************************************
#
#***************************************
#9- Para pacientes acima de 10 anos, qual a média de gasto total com internações 
#hospitalares?

sqldf("select Idade,avg(Custo_Intern) as 'Custo_Medio'
      from df
      where Idade > 10
      group by Idade
      ")

#***************************************
#
#***************************************
#10- Considerando o item anterior, qual idade tem média de gastos superior a 3000?
sqldf("select Idade, avg(Custo_Intern) as 'Custo_Medio'
      from df
      where idade >10
      group by Idade
      having Custo_Medio > 3000
      ")


##############################
#      Etapa 2 - QUESTÕES    #
##############################

#função Classificação faixa etária
classificar_idade<-function(valor){
  if(valor<6){
    return("0-5")
  }
  else if(valor<11){
    return("6-10")
  }
  else if(valor<16){
    return("11-15")
  }else
    return("16-17")
}

#ADD Coluna Faixa Etária
df$Faixa<-unlist(lapply(df$Idade,classificar_idade))
df$Faixa<-factor(df$Faixa,levels = c("0-5", "6-10", "11-15", "16-17"), 
                 ordered = TRUE)

str(df)
levels(df$Faixa)

#******************************************************************************
#1- Qual a distribuição da idade dos pacientes que frequentam o hospital?
#******************************************************************************

table(df$Idade)

hist(df$Idade, labels = T, ylim = c(0,350), breaks = 4, main = "Distribuição Idade",col = c('pink'))


#******************************************************************************
#2- Qual faixa etária tem o maior gasto total no hospital
#******************************************************************************
resumo<-df%>%
  group_by(Faixa)%>%summarise(Total_Internacao = sum(Custo_Intern))

arrange(resumo,desc(Total_Internacao))

barplot(resumo$Total_Internacao,
        main = "Custo de Internação por Faixa Etária",
        xlab = "Faixa Etária",
        ylab = "Custo",
        names.arg = resumo$Faixa,
        col = "pink")
rm(resumo)

#*#*****************************************************************************
#3- Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
#******************************************************************************
hist(df$Grupo_Diagnostico, 
     labels = T, 
     breaks = 4,
     main = "Grupo de Diagnóstico",
     col = 'pink')

table(df$Grupo_Diagnostico)

df %>%
  mutate(classeDiagnóstico = case_when(
    Grupo_Diagnostico <= 200 ~ '<200',
    Grupo_Diagnostico >200 & Grupo_Diagnostico <=400 ~ '201-400',
    Grupo_Diagnostico >400 & Grupo_Diagnostico <=600 ~ '401-600',
    Grupo_Diagnostico >600 & Grupo_Diagnostico <=800 ~ '601-800',
    Grupo_Diagnostico >800  ~ '>800'))%>%
  group_by(classeDiagnóstico)%>%
  summarise(Custo_GrupoDiag = sum(Custo_Intern))%>%
  arrange(desc(Custo_GrupoDiag))


#*#*****************************************************************************
#4- A raça do paciente tem relação com o total gasto em internações no hospital?
#******************************************************************************

# A ANOVA (do inglês: Análise de Variância) é uma técnica estatística que pode 
#ser aplicada para descobrir se há correlação quando a variável dependente é 
# quantitativa e a independente é categórica.
#https://medium.com/dos-dados-%C3%A0-ci%C3%AAncia/correla%C3%A7%C3%A3o-de-vari%C3%A1veis-parte-1-categ%C3%B3ricas-x-quantitativa-3d0bdd73bd30



# Hipóteses:
# H0: Não há efeito de RACE em TOTCHG.
# H1: Há efeito de RACE em TOTCHG.



str(df)
summary(as.factor(df$Raca))

#Teste Anova

var_dep = df$Custo_Intern
var_indep = df$Raca

anova_teste1<-aov(var_dep~var_indep)
summary(anova_teste)

#Valor-p é maior que 0.05. Falhamos em rejeitar o H0
#Raça do paciente não influencia no gasto total com internações


#*#*****************************************************************************
#5- A combinação de idade e gênero dos pacientes influencia no gasto total 
#em internações no hospital?
#******************************************************************************

# Hipóteses:
# H0: Não há efeito de AGE e FEMALE em TOTCHG.
# H1: Há efeito de AGE e FEMALE em TOTCHG.
var_indep2 = df$Mulher
var_indep1 = df$Idade
var_dep = df$Custo_Intern

anova_teste2<-aov(var_dep~var_indep1+var_indep2)
anova_teste3<-aov(var_dep~var_indep2+var_indep1)
summary(anova_teste2)
summary(anova_teste3)


# Resposta da Pergunta 5:
# Em ambos os casos o valor-p é menor que 0.05. Rejeitamos a hipótese nula. 
# Há um efeito significativo da idade e do gênero nos custos hospitalares.

#*#*****************************************************************************
#6- Como o tempo de permanência é o fator crucial para pacientes internados, 
#desejamos descobrir se o tempo de permanência pode ser previsto 
#a partir de idade, gênero e raça
#******************************************************************************

#H0: Não há relação linear entre as variaveis dependentes e independentes
#H1: Há relação linear entre variáveis  

model<-lm(Tempo_Intern~Idade+Mulher+Raca,data = df)
summary(model)

#Valor-p > 0,05. Falhamos em Rejeitar H0.
#Tempo de internação não pode ser previsto a partir das variaveis independentes. 

#*#******************************************************************************
#7- Quais variável têm maior impacto nos custos de internação hospitalar
#******************************************************************************


#Eliminando a coluna Faixa, crianda anteriormente

df$Faixa<-NULL

head(df)
str(df)

#Hipóteses

#  H0: Não há relação linear entre variáveis dependente e independentes.
#  H1: Há relação linear entre variáveis dependente e independentes.


#Criando modelo de Regressão e avalaliando seus resultados 
#Modelo  (Usando todos os atributos)
model_1<-lm(Custo_Intern ~ ., data = df)
summary(model_1)
#******************************************************************

#Modelo 2 (Eliminando Atributo Raça)
model_2<-lm(Custo_Intern ~Idade+Mulher+Tempo_Intern+Grupo_Diagnostico , data = df)
summary(model_2)
#******************************************************************
#*
#*
#Modelo 3 (Eliminando Atributo Raça e Mulher
model_3<-lm(Custo_Intern ~Tempo_Intern+Idade+Grupo_Diagnostico , data = df)
summary(model_3)
#******************************************************************

# Conclusão:

# Como é evidente nos vários modelos acima, os custos dos cuidados de saúde dependem 
# da idade, do tempo de permanência e do grupo de diagnóstico.
# Essas são as 3 variáveis mais relevantes para explicar e prever o gasto com 
# internações hospitalares.




