#Estatistica tradicional x machine learning
#Autora: CDM.
#Ano: 2023

set.seed(123)

rm(list = ls(all=T))

usethis::use_git_config(
  user.name = "Caroline Modesto",
  user.email = "carolinedias483@gmail.com")

usethis::create_github_token()

usethis::edit_r_environ()

usethis::git_sitrep()

usethis::use_git()

# Carregando e instalando pacotes -----------------------------------------

if (!require("pacman")) install.packages("pacman");pacman::p_load(caret, randomForest, GGally, 
                                                                  ggcorrplot, prettydoc, ggplot2, 
                                                                  lubridate, data.table, magrittr, 
                                                                  stringr, dplyr, tidyr, forcats, psych, car, MASS, 
                                                                  QuantPsyc, smotefamily,themis, ROCR,VGAM, glmtoolbox, pROC)


# Setando diretorio -------------------------------------------------------

getwd()
list.dirs()
setwd("./DATASUS/EMR/")
list.files()

# Importando os dados -----------------------------------------------------


SaudeMental <- rio::import("SaudeMental.csv")



# Categorizacao -----------------------------------------------------------


#idade:
SaudeMental <- SaudeMental %>%
 mutate(Idade = case_when(C008 >= 15 & C008 <= 21 ~ "1", #ate 21
                             C008 >= 22 & C008 <= 29 ~ "0")) #cima de 21


# Novo dataset ------------------------------------------------------------

saude <- SaudeMental %>% 
  dplyr::select(Idade, C006, Bipolar, J001, Q11007, Q11009, Q115, Q11405, Q11406, Q11407) 

colnames(saude)

# Renomeando variaveis para melhor visualizacao ---------------------------

saude <- saude %>% 
  dplyr::rename("Sexo" = "C006",
         "Percep_Saude"  = "J001",
         "esquiso" = "Q11007",
         "TOC" = "Q11009",
         "limitacao" = "Q115",
         "FazPsi" = "Q11405",
         "medicamento" = "Q11406",
         "Praticas_integrativas" = "Q11407")  #cardio = J00402

colnames(saude)


# One-hot enconding -------------------------------------------------------

#Variaveis com mais de dois niveis atrapalham o modelo portanto devem ser transformadas em dummies. 

# saude <- saude %>%
# dplyr::mutate(limitacao = ifelse(limitacao == 1, 1,0),
#               Percep_Saude = ifelse(Percep_Saude == 1, 1,0),
#               esquiso = ifelse(esquiso == 1, 1,0),
#               TOC = ifelse(TOC == 1, 1,0),
#               FazPsi = ifelse(FazPsi == 1, 1,0),
#               medicamento = ifelse(medicamento == 1, 1,0),
#               Praticas_integrativas = ifelse(Praticas_integrativas == 1, 1,0),
#               Sexo = ifelse(Sexo == 1, 1,0))


#write.csv2(x = saude, file = "saude.csv")


# transformar em fator para a modelagem -----------------------------------

fatores <- c("Bipolar","Sexo", "Percep_Saude",
             "esquiso" , "TOC", "limitacao",
             "FazPsi", "medicamento", "Praticas_integrativas", "Idade")

saude[,fatores] = lapply(saude[fatores], as.factor)

str(saude)
set.seed(123)
# Atribuindo niveis -------------------------------------------------------

# Categoria bipolar como referencia

levels(saude$Bipolar)
saude$Bipolar <- relevel(saude$Bipolar, ref = "1")
str(saude)

#col_relevels <- c(1:10) #colunas

#estas aqui sao as referencias atribuidas

# niveis <- c("1","1","1","1","1","1","1","1","1", "1")
# 
# for (col in col_relevels) {
#   if (is.factor(saude[[col]])) {
#     for (i in 1:length(niveis)) {
#       saude[[col]] <- relevel(saude[[col]], ref = niveis[i])}}}


# Aplicando a tecnica SmoteNC ---------------------------------------------

saudeSmote <- saude

sssmote = smotenc(saudeSmote[,1:10], var = "Bipolar", over_ratio = 0.8)

summary(sssmote) #1 = 724 e 0 = 906
summary(saude) 
# construcao do modelo apos balanceamento ---------------------------------

set.seed(123)

str(sssmote)

set.seed(123)

ML2 <- glm(sssmote$Bipolar ~ sssmote$Idade + sssmote$Percep_Saude + 
             sssmote$esquiso + sssmote$TOC + sssmote$Praticas_integrativas + sssmote$medicamento + 
             sssmote$limitacao + sssmote$FazPsi + sssmote$Sexo, family = binomial(link = 'logit'))
 
summary(ML2)
# plot(ML2, which = 5)
# summary(stdres(ML2))
# #pairs.panels(saudeSmote)
# summary(ML2, exp(F))


# Multicolinearidade ------------------------------------------------------

#Valores precisam estar abaixo de 10.

vif(ML2)

# Teste de Wald -----------------------------------------------------------

Anova(ML2, type = "II", test = "Wald")


#MELHOR CONFIGURACAO DE ACORDO COM O CRITERIO AIC -------------------------

aic = MASS::stepAIC(ML2, trace = T)


# modelo com base no criterio AIC -----------------------------------------
set.seed(123)

ML3 <- glm(sssmote$Bipolar ~ sssmote$Percep_Saude + sssmote$esquiso + 
             sssmote$TOC + sssmote$Praticas_integrativas + sssmote$limitacao + 
             sssmote$FazPsi + sssmote$Sexo + sssmote$Idade, family = binomial(link = 'logit'))

summary(ML3, exp(F))


# Teste de Hosmer-Lemeshow ------------------------------------------------

hltest(ML3) # ajuste de coavariavel ao modelo 

# Razao de chance ---------------------------------------------------------
set.seed(123)

exp(cbind(OR = coef(ML3), confint(ML3)))

# calculando previsoes ----------------------------------------------------
set.seed(123)

glm_probs = data.frame(probs = predict(ML3, type = "response"))

pred = glm_probs %>% 
  mutate(pred = ifelse(probs>.5, 1,0))

pred <- factor(pred[,2], labels = levels(as.factor(sssmote$Bipolar)))



###########################################################################
# ---------------------- Ava. desempenho do modelo ----------------------
###########################################################################
# Voce pode identificar o poder de predicao do seu modelo atraves de:
# 2. Matriz de confusao
# 3. acuracia
# 4. sensibilidade
# 5. especificidade
# 6. curva de ROC

set.seed(123)

# Matriz de confusao ------------------------------------------------------
mod.confusion1 <- caret::confusionMatrix(as.factor(pred), as.factor(sssmote$Bipolar))
mod.confusion1

matrixML <- as.data.frame(mod.confusion1$table)

# Acuracia ----------------------------------------------------------------
mod.accuracy1 <- round(as.numeric(mod.confusion1$overall[1]),3) #0.71


# Sensibilidade -----------------------------------------------------------
mod.sensitivity1 <- round(as.numeric(mod.confusion1$byClass[1]),3) #0.62


# Especificidade ----------------------------------------------------------
mod.specificity1 <- round(as.numeric(mod.confusion1$byClass[2]),3) #0.78


# Curva de ROC ------------------------------------------------------------
p_load(pROC)
set.seed(123)

pred1 <- predict(ML3, type = "response")
perf = prediction(pred1, sssmote$Bipolar)
auc = performance(perf, "auc")
auc
pred3 = performance(perf, "tpr", "fpr")
set.seed(123)

#roc grafico 1 com apenas a curva
roc1 = plot.roc(sssmote$Bipolar, fitted(ML3))
set.seed(123)

#roc grafico 2 com a curva e detalhes 
plot( roc1 ,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grud=c(0.1,0.2) ,
         grid.col=c("green","red") ,
         max.auc.polygon=TRUE,
         auc.polygon.col= "lightgreen",
         print.thres=TRUE)



###########################################################################
# Random Forest -----------------------------------------------------------
###########################################################################

saude2 <- sssmote
set.seed(123)
saude2$random <- sample(0:1, size = nrow(saude2), replace = T , prob = c(0.3,0.7))

train <- filter(saude2,random==1)
test <- filter(saude2,random==0)

saude2$random <- NULL

# sssmote2 = smotenc(train[,1:10], var = "Bipolar", over_ratio = 0.8)

# sssmote2 <- upSample(x = train[,1:10], y = train$Bipolar)
set.seed(123)
test <-  test %>% 
  dplyr::select("Bipolar","Idade", "Percep_Saude" ,
                "esquiso" , "TOC", "limitacao",
                "FazPsi", "Praticas_integrativas", "Sexo", "medicamento")


# Ajuste dos parametros ---------------------------------------------------
library(caret)

#Ajustar manualmente mtry e ntree

#mtry: numero de variaveis
#ntree: numero de arvores
set.seed(123)
customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                 label = c("mtry", "ntree"))

customRF$grid <-  function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
    randomForest(x, y, 
                 mtry = param$mtry, 
                 ntree=param$ntree)
}

#predizer classe
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
predict(modelFit, newdata)
set.seed(123)
#predizer probabilidade
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
predict(modelFit, newdata, type = "prob")
set.seed(123)
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

set.seed(123)
#validacao cruzada
control <- trainControl(method="cv",
                        number=10,
                        repeats=3)

grid <- expand.grid(.mtry = c(1:10),
                    .ntree = c(200, 500, 1000))
set.seed(123)
rfFit <- train(Bipolar ~.,
               method=customRF,
               tuneGrid = grid,
               trControl = control,
               metric = "Accuracy",
               data = train)

rfFit

plot(rfFit)

plot(rfFit$finalModel) 
legend("topright", colnames(rfFit$finalModel$err.rate),
       col = 1:3,
       cex = 0.8,
       fill = 1:3,
       bty = "n")


set.seed(123)
oob.errV <- data.frame(Trees = rep(1:nrow(rfFit$finalModel$err.rate), 3),
                      Type = rep(c("OOB","Não bipolar", "Bipolar"), 
                                 each = nrow(rfFit$finalModel$err.rate)),
                      Error = c(rfFit$finalModel$err.rate[,"OOB"],rfFit$finalModel$err.rate[,"0"], rfFit$finalModel$err.rate[,"1"]))


ggplot2::ggplot(data = oob.errV, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type)) +
  labs(x = "Arvores", y = "Taxa de erro", color = "Grupo") +
    theme_classic()




# Importancia das variaveis -----------------------------------------------


set.seed(123)
rfFit <- randomForest(Bipolar ~ Idade + Sexo + Percep_Saude + 
                        esquiso + TOC + limitacao +
                        FazPsi + medicamento + Praticas_integrativas, train, importance = TRUE, ntree = 500)

rfFit


# Importancia -------------------------------------------------------------

set.seed(123)
varImpPlot(rfFit, main = "Importancia das variaveis")



# Qualidade de predicao --------------------------------------------------

set.seed(123)
predictions.rfFit <- predict(rfFit, newdata = test[,c(1:10)])

confusionMatrix(predictions.rfFit, as.factor(test$Bipolar))

mod.confusion <- caret::confusionMatrix(predictions.rfFit, test$Bipolar, positive = NULL)

mod.accuracy <- round(as.numeric(mod.confusion$overall[1]),3)
set.seed(123)
predRF <- predict(rfFit, type = "prob")
perfRF <- prediction(predRF[,2], train$Bipolar)
aucRF <- performance(perfRF, "aucRF")
aucRF

pred33 <- performance(perfRF, "tpr", "fpr")
plot(pred33, main = "Roc Curve Random Forest", col = 2, lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")

set.seed(123)
oob.err <- data.frame(Trees = rep(1:nrow(rfFit$err.rate), 3),
                      Type = rep(c("OOB", "Bipolar", "Não bipolar"), 
                                 each = nrow(rfFit$err.rate)),
                      Error = c(rfFit$err.rate[,"OOB"], rfFit$err.rate[,"0"], rfFit$err.rate[,"1"]))


ggplot2::ggplot(data = oob.err, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type)) +
  labs(x = "Arvores", y = "Taxa de erro", color = "Grupo")


set.seed(123)
#roc grafico 1 com apenas a curva
roc2 = plot.roc(test$Bipolar, fitted(rfFit))

#roc grafico 2 com a curva e detalhes
plot( roc2,
      print.auc=TRUE,
      auc.polygon=TRUE,
      grud=c(0.1,0.2) ,
      grid.col=c("green","red"),
      max.auc.polygon=TRUE,
      auc.polygon.col= "lightgreen",
      print.thres=TRUE)



# Graficos 1 --------------------------------------------------------------

bipol <- as.data.frame(table(saude$Bipolar))

bipol$perc <- round(bipol$Freq/sum(bipol$Freq)*100,2)

grafico <- ggplot(bipol, aes(x = Var1, y = perc, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Categoria",
       y = "Percentual") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(fill = "Categoria") +
  theme_classic()

print(grafico)


# Grafico 2 ---------------------------------------------------------------

bipols <- as.data.frame(table(sssmote$Bipolar))

bipols$perc <- bipols$Freq/sum(bipols$Freq)*100

grafico2 <- ggplot(bipols, aes(x = Var1, y = perc, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Categoria",
       y = "Percentual") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(fill = "Categoria") +
  theme_classic()

print(grafico2)
#p_load(gridExtra)

grid.arrange(grafico, grafico2, ncol = 2)

