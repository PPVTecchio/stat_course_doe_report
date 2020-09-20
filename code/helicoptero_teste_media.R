## Modelagem

# Utilizamos o script helicoptero_cria_experimento_modelagem.R para criar um experimento 
# e depois importamos os dados aqui.

#importa a ordem dos experimentos
library(readxl)
helicopterodesign <- read_excel("Documents/CIMATEC/Aulas/Estatistica/HelicopteroGrupo/Stat_DOE_Helicopter.xlsx", 
                                sheet = "ExperimentoModelagem")
helicopterodesign

#importa os dados salvos de tempo
dados_helicopterdesign <- read_excel("Documents/CIMATEC/Aulas/Estatistica/HelicopteroGrupo/Stat_DOE_Helicopter.xlsx", 
                                     sheet = "DadosModelagem")
dados_helicopterdesign
#computa a media das tomadas de tempo de cada experimento
t1 <- rowMeans(cbind(dados_helicopterdesign$Teste1_Mateus,dados_helicopterdesign$Teste1_Pedro))
t1
t2 <- rowMeans(cbind(dados_helicopterdesign$Teste2_Mateus,dados_helicopterdesign$Teste2_Pedro))
t2
t3 <- rowMeans(cbind(dados_helicopterdesign$Teste3_Mateus,dados_helicopterdesign$Teste3_Pedro))
t3
#replica os dados de ordenamento do experimento
helicoptero <- rbind(helicopterodesign,helicopterodesign,helicopterodesign)
helicoptero
#insere os tempos nos dados replicados
helicoptero$Tempo <- c(t1,t2,t3)
helicoptero

#cria o modelo de primeira ordem
helicoptero.model1ordem <- lm(Tempo ~ (.), data = helicoptero)
summary(helicoptero.model1ordem)
confint(helicoptero.model1ordem)
par(mfrow = c(2,2))
plot(helicoptero.model1ordem)

#cria o modelo de segunda ordem
helicoptero.model2ordem <- lm(Tempo ~ (.)^2, data = helicoptero)
summary(helicoptero.model2ordem)
confint(helicoptero.model2ordem)
par(mfrow = c(2,2))
plot(helicoptero.model2ordem)

## Modelo considerado é de primeira ordem!
#modelo considerando apenas altura e clipe
helicoptero.model <- lm(Tempo ~ Altura + Clipe, data = helicoptero)
summary(helicoptero.model)
confint(helicoptero.model2ordem)
par(mfrow = c(2,2))
plot(helicoptero.model2ordem)

## Testes do modelo

# Utilizamos o script helicoptero_cria_experimento_teste_modelo.R para criar um experimento 
# e depois importamos os dados aqui.

#importa a ordem dos experimentos
helicopteromodeltestdesign <- read_excel("Documents/CIMATEC/Aulas/Estatistica/HelicopteroGrupo/Stat_DOE_Helicopter.xlsx", 
                                         sheet = "ExperimentoTesteModelo")
helicopteromodeltestdesign

#importa os dados salvos de tempo
dados_helicopteromodeltest <- read_excel("Documents/CIMATEC/Aulas/Estatistica/HelicopteroGrupo/Stat_DOE_Helicopter.xlsx", 
                                         sheet = "DadosTesteModelo")
dados_helicopteromodeltest

tm1 <- rowMeans(cbind(dados_helicopteromodeltest$Teste1_Mateus,dados_helicopteromodeltest$Teste1_Pedro))
tm1
tm2 <- rowMeans(cbind(dados_helicopteromodeltest$Teste2_Mateus,dados_helicopteromodeltest$Teste2_Pedro))
tm2
tm3 <- rowMeans(cbind(dados_helicopteromodeltest$Teste3_Mateus,dados_helicopteromodeltest$Teste3_Pedro))
tm3

tm <- rowMeans(cbind(tm1,tm2,tm3))
tm

#valores modelo
model <- predict(helicoptero.model, newdata = helicopteromodeltestdesign)
model

#teste de correlação entre a predição do modelo e os resultados experimentais
cor.test(model, tm)
cor(model,tm)

data <- data.frame(model = model, tm = tm)

library("ggpubr")
ggscatter(data, x = "model", y = "tm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Modelo", ylab = "Experimento", xlim = c(0.5,2), ylim = c(0.5,2))


