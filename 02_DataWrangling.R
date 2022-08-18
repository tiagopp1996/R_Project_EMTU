#load Data
data = read.table("./Bases EMTU/CaCEMTU.csv", header=TRUE, sep=";")

#Separa Mes e Ano
nMes=as.integer(stringr::str_extract(data[,1], "^.{2}"))
nAno=as.integer(str_sub(data$Mes,4,9))
anomes=as.integer(str_c(str_sub(data$Mes,4,9),stringr::str_extract(data[,1], "^.{2}")))

#Drop Mes
names(data)[1]<-"MesAno"

#append int no dataframe
data['Mes']<-nMes
data['Ano']<-nAno
data['Anomes']<-anomes
names(data)[2]<-"Regiao"

#Organiza Colunas
data = data %>% select(Regiao,Empresa,Manifestacao,Item,Mes,Ano,MesAno,Anomes,Quantidade)
#Sumariza dados
summary(data)


########################################Analise voltada a Manifesta??es############################


#Soma Acumulando por Manifestacao,Mes e Ano
#Acumula toda a bases
Acum_Manifestacao_Anomes_Full = filter(data,Anomes<202208) %>% group_by(Manifestacao,MesAno,Anomes) %>% 
  summarise(S_qtd= sum(Quantidade)) %>% arrange(Anomes)
Acum_Manifestacao_Anomes_Full['Anomes_int']<-Acum_Manifestacao_Anomes_Full$Anomes
Acum_Manifestacao_Anomes_Full['Anomes']<-factor(Acum_Manifestacao_Anomes_Full$Anomes)
head(Acum_Manifestacao_Anomes_Full)

summary(Acum_Manifestacao_Anomes_Full)

#Descobre as Macro Regioes
data$Manifestacao %>% unique

#Valores totais por Manifestacao
summary(filter(Acum_Manifestacao_Anomes_Full,Manifestacao=="Reclamação"))
summary(filter(Acum_Manifestacao_Anomes_Full,Manifestacao=="Elogio"))
summary(filter(Acum_Manifestacao_Anomes_Full,Manifestacao=="Informação"))
summary(filter(Acum_Manifestacao_Anomes_Full,Manifestacao=="Sugestão"))
summary(filter(Acum_Manifestacao_Anomes_Full,Manifestacao=="Denúncia"))

#Total Manifestacao
ggplot(data=filter(Acum_Manifestacao_Anomes_Full,Anomes_int < 202208),
       aes(x=Anomes, y=S_qtd,group=Manifestacao, color=Manifestacao)) +
  geom_line()

#Analise de Informa??es Isolando Maiores volumes
ggplot(data=filter(Acum_Manifestacao_Anomes_Full,Anomes_int < 202208 & Manifestacao!="Reclamação"
                   & Manifestacao!="Informação"),
       aes(x=Anomes, y=S_qtd,group=Manifestacao, color=Manifestacao)) +
  geom_line()

#Acumula Anterior a 2020
ggplot(data=filter(Acum_Manifestacao_Anomes_Full,Anomes_int < 202001),
       aes(x=Anomes, y=S_qtd,group=Manifestacao, color=Manifestacao)) +
  geom_line()

#Acumula 2020 em diante
ggplot(data=filter(Acum_Manifestacao_Anomes_Full,Anomes_int >= 202001 & Anomes_int < 202208),
       aes(x=Anomes, y=S_qtd,group=Manifestacao, color=Manifestacao)) +
  geom_line()

#Conclusão até o momento:As Manifestações possuem tendencias parecidas no comportamento mas 
#com aplitudes muito diversas Sendo que os volumes agrupados por manifestações estão concentrados
#em Reclamações e Informações assuntos que são direcionados as macro regiões

########################################Analise voltada a Macro Regioes############################

#Soma Acumulando por Regiao,Mes e Ano
#Filtra > 2020
Acum_Regiao_Anomes = filter(data,Anomes>=202001 & Anomes<202208) %>% group_by(Regiao,MesAno,Anomes) %>% 
  summarise(S_qtd= sum(Quantidade)) %>% arrange(Anomes) 
Acum_Regiao_Anomes['Anomes_int']<-Acum_Regiao_Anomes$Anomes
Acum_Regiao_Anomes['Anomes']<-factor(Acum_Regiao_Anomes$Anomes)
head(Acum_Regiao_Anomes)

summary(Acum_Regiao_Anomes)


#Acumula toda a bases
Acum_Regiao_Anomes_Full = filter(data,Anomes<202208) %>% group_by(Regiao,MesAno,Anomes) %>% 
  summarise(S_qtd= sum(Quantidade)) %>% arrange(Anomes)
Acum_Regiao_Anomes_Full['Anomes_int']<-Acum_Regiao_Anomes_Full$Anomes
Acum_Regiao_Anomes_Full['Anomes']<-factor(Acum_Regiao_Anomes_Full$Anomes)
head(Acum_Regiao_Anomes_Full)

summary(Acum_Regiao_Anomes_Full)

#Descobre as Macro Regioes
data$Regiao %>% unique

#Base Total
ggplot(data=Acum_Regiao_Anomes_Full, aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()

ggplot(data=filter(Acum_Regiao_Anomes_Full,Regiao !="RMSP"),
       aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()


#Acumula Anterior a 2020
ggplot(data=filter(Acum_Regiao_Anomes_Full,Anomes_int < 202001),
       aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()

ggplot(data=filter(Acum_Regiao_Anomes_Full,Anomes_int < 202001 & Regiao !="RMSP"),
       aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()




#Acumula 2020 em diante
ggplot(data=Acum_Regiao_Anomes, aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()

ggplot(data=filter(Acum_Regiao_Anomes,Regiao !="RMSP"),
       aes(x=Anomes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()



#Conclusão até o momento: A Pandemia causou uma suavização na curva do ano de 2021 
# mas sem grandes modicaõees na tendencia e sazonalidade
# IMPORTANTE cada empresa possui uma curva diferente, necessario um forecast por empresa


AcumRMBS <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMBS' & Anomes_int<=202112)
AcumRMBS <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMBS')
AcumRMC<- filter(Acum_Regiao_Anomes_Full,Regiao=='RMC')
AcumRMS <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMS')
AcumRMSP <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMSP')
AcumRMVPLN <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMVPLN')

tsRMBS <- ts(AcumRMBS$S_qtd,frequency = 12, start = c(2017,1))


# Decomposição
autoplot(decompose(tsRMBS)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))

#autoarima com sazonalidade
forecast_arima_tsRMBS <- auto.arima(tsRMBS, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
plot(tsRMBS)
forecast_arima_tsRMBS = forecast(forecast_arima_tsRMBS, h=7)
plot(forecast_arima_tsRMBS)


#Forecast Holt-Winter
forecast_hw_tsRBS <- hw(tsRMBS, seasonal="additive", h=7)
summary(forecast_hw_tsRBS) # avaliando os resultados 
plot(tsRMBS)
plot(forecast_hw_tsRBS) # plotando os resultados

#Analise AIC
forecast_arima_tsRMBS['model']
forecast_hw_tsRBS['model']


#Analise resultados comparados entre dados, fitted e forecast
autoplot(tsRMBS,series=" Historical data") +
  autolayer(forecast_hw$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima$fitted, series="Arima fitted") +
  autolayer(forecast_arima, series="Arima Forecast") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))

autoplot(tsRMBS,series=" Historical data") +
  autolayer(forecast_hw$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima$fitted, series="Arima fitted") +
  autolayer(forecast_hw, series="Holt-Winter Forecast") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))


