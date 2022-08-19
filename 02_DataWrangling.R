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


#Filtra base até 202112
AcumRMBS <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMBS' & Anomes_int<=202112)
AcumRMC<- filter(Acum_Regiao_Anomes_Full,Regiao=='RMC' & Anomes_int<=202112)
AcumRMS <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMS' & Anomes_int<=202112)
AcumRMSP <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMSP' & Anomes_int<=202112)
AcumRMVPLN <- filter(Acum_Regiao_Anomes_Full,Regiao=='RMVPLN' & Anomes_int<=202112)

#Aplica Serie Temporal
tsRMBS <- ts(AcumRMBS$S_qtd,frequency = 12, start = c(2017,1))
tsRMC <- ts(AcumRMC$S_qtd,frequency = 12, start = c(2017,1))
tsRMS <- ts(AcumRMS$S_qtd,frequency = 12, start = c(2017,1))
tsRMSP <- ts(AcumRMSP$S_qtd,frequency = 12, start = c(2017,1))
tsRMVPLN <- ts(AcumRMVPLN$S_qtd,frequency = 12, start = c(2017,1))

# Decomposição
autoplot(decompose(tsRMBS)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))
autoplot(decompose(tsRMC)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))
autoplot(decompose(tsRMS)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))
autoplot(decompose(tsRMSP)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))
autoplot(decompose(tsRMVPLN)) + ggtitle("Decomposition of the series") + 
  theme(plot.title = element_text(size=8))



#autoarima com sazonalidade
forecast_arima_tsRMBS <- auto.arima(tsRMBS, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
forecast_arima_tsRMC <- auto.arima(tsRMC, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
forecast_arima_tsRMS <- auto.arima(tsRMS, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
forecast_arima_tsRMSP <- auto.arima(tsRMSP, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
forecast_arima_tsRMVPLN <- auto.arima(tsRMVPLN, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)



#forecast 7 meses de 2022 usando arima
forecast_arima_tsRMBS = forecast(forecast_arima_tsRMBS, h=7)
forecast_arima_tsRMC = forecast(forecast_arima_tsRMC, h=7)
forecast_arima_tsRMS = forecast(forecast_arima_tsRMS, h=7)
forecast_arima_tsRMSP = forecast(forecast_arima_tsRMSP, h=7)
forecast_arima_tsRMVPLN = forecast(forecast_arima_tsRMVPLN, h=7)


#Plots Comparativos Arima
plot(tsRMBS)
plot(forecast_arima_tsRMBS)
plot(tsRMC)
plot(forecast_arima_tsRMC)
plot(tsRMS)
plot(forecast_arima_tsRMS)
plot(tsRMSP)
plot(forecast_arima_tsRMSP)
plot(tsRMVPLN)
plot(forecast_arima_tsRMVPLN)


#Forecast Holt-Winter
forecast_hw_tsRMBS <- hw(tsRMBS, seasonal="additive", h=7)
forecast_hw_tsRMC <- hw(tsRMC, seasonal="additive", h=7)
forecast_hw_tsRMS <- hw(tsRMS, seasonal="additive", h=7)
forecast_hw_tsRMSP <- hw(tsRMSP, seasonal="additive", h=7)
forecast_hw_tsRMVPLN <- hw(tsRMVPLN, seasonal="additive", h=7)




#Avaliando Resultados
summary(forecast_hw_tsRMBS)  
summary(forecast_hw_tsRMC)  
summary(forecast_hw_tsRMS)  
summary(forecast_hw_tsRMSP)  
summary(forecast_hw_tsRMVPLN)  



#Plots Comparativos Holt Winter
plot(tsRMBS)
plot(forecast_hw_tsRMBS) 
plot(tsRMC)
plot(forecast_hw_tsRMC) 
plot(tsRMS)
plot(forecast_hw_tsRMS) 
plot(tsRMSP)
plot(forecast_hw_tsRMSP) 
plot(tsRMVPLN)
plot(forecast_hw_tsRMVPLN) 


#Analise AIC
forecast_hw_tsRMBS['model']
forecast_arima_tsRMBS$aic
forecast_hw_tsRMC['model']
forecast_arima_tsRMC$aic
forecast_hw_tsRMS['model']
forecast_arima_tsRMS$aic
forecast_hw_tsRMSP['model']
forecast_arima_tsRMSP$aic
forecast_hw_tsRMVPLN['model']
forecast_arima_tsRMVPLN$aic


#Analise resultados comparados entre dados, fitted e forecast
autoplot(tsRMBS,series=" Historical data") +
  autolayer(forecast_hw_tsRMBS$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima_tsRMBS$fitted, series="Arima fitted") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))
autoplot(tsRMC,series=" Historical data") +
  autolayer(forecast_hw_tsRMC$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima_tsRMC$fitted, series="Arima fitted") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))
autoplot(tsRMS,series=" Historical data") +
  autolayer(forecast_hw_tsRMS$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima_tsRMS$fitted, series="Arima fitted") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))
autoplot(tsRMSP,series=" Historical data") +
  autolayer(forecast_hw_tsRMSP$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima_tsRMSP$fitted, series="Arima fitted") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))
autoplot(tsRMVPLN,series=" Historical data") +
  autolayer(forecast_hw_tsRMVPLN$fitted, series="Holt-Winter fitted") +
  autolayer(forecast_arima_tsRMVPLN$fitted, series="Arima fitted") +
  ggtitle("Modelos") +
  theme(plot.title = element_text(size=8))


#Trandformação Dataframe Forecast HW
df_tsRMBS_HW=data.frame(Y=as.matrix(forecast_hw_tsRMBS$fitted), date=time(forecast_hw_tsRMBS$x))
df_tsRMBS_HW['ano']=str_sub(as.character(df_tsRMBS_HW$date),1,4)
names(df_tsRMBS_HW)[1]<-"Fitted_HW"
df_tsRMBS_HW <- df_tsRMBS_HW %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMBS_HW <- df_tsRMBS_HW %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_HW,Anomes_int,anomes)

df_tsRMC_HW=data.frame(Y=as.matrix(forecast_hw_tsRMC$fitted), date=time(forecast_hw_tsRMC$x))
df_tsRMC_HW['ano']=str_sub(as.character(df_tsRMC_HW$date),1,4)
names(df_tsRMC_HW)[1]<-"Fitted_HW"
df_tsRMC_HW <- df_tsRMC_HW %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMC_HW <- df_tsRMC_HW %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_HW,Anomes_int,anomes)

df_tsRMS_HW=data.frame(Y=as.matrix(forecast_hw_tsRMS$fitted), date=time(forecast_hw_tsRMS$x))
df_tsRMS_HW['ano']=str_sub(as.character(df_tsRMS_HW$date),1,4)
names(df_tsRMS_HW)[1]<-"Fitted_HW"
df_tsRMS_HW <- df_tsRMS_HW %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMS_HW <- df_tsRMS_HW %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_HW,Anomes_int,anomes)

df_tsRMSP_HW=data.frame(Y=as.matrix(forecast_hw_tsRMSP$fitted), date=time(forecast_hw_tsRMSP$x))
df_tsRMSP_HW['ano']=str_sub(as.character(df_tsRMSP_HW$date),1,4)
names(df_tsRMSP_HW)[1]<-"Fitted_HW"
df_tsRMSP_HW <- df_tsRMSP_HW %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMSP_HW <- df_tsRMSP_HW %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_HW,Anomes_int,anomes)

df_tsRMVPLN_HW=data.frame(Y=as.matrix(forecast_hw_tsRMVPLN$fitted), date=time(forecast_hw_tsRMVPLN$x))
df_tsRMVPLN_HW['ano']=str_sub(as.character(df_tsRMVPLN_HW$date),1,4)
names(df_tsRMVPLN_HW)[1]<-"Fitted_HW"
df_tsRMVPLN_HW <- df_tsRMVPLN_HW %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMVPLN_HW <- df_tsRMVPLN_HW %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_HW,Anomes_int,anomes)


#Trandformação Dataframe Forecast Arima
df_tsRMBS_AR=data.frame(Y=as.matrix(forecast_arima_tsRMBS$fitted), date=time(forecast_arima_tsRMBS$x))
df_tsRMBS_AR['ano']=str_sub(as.character(df_tsRMBS_AR$date),1,4)
names(df_tsRMBS_AR)[1]<-"Fitted_AR"
df_tsRMBS_AR <- df_tsRMBS_AR %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMBS_AR <- df_tsRMBS_AR %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_AR,Anomes_int,anomes)

df_tsRMC_AR=data.frame(Y=as.matrix(forecast_arima_tsRMC$fitted), date=time(forecast_arima_tsRMC$x))
df_tsRMC_AR['ano']=str_sub(as.character(df_tsRMC_AR$date),1,4)
names(df_tsRMC_AR)[1]<-"Fitted_AR"
df_tsRMC_AR <- df_tsRMC_AR %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMC_AR <- df_tsRMC_AR %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_AR,Anomes_int,anomes)

df_tsRMS_AR=data.frame(Y=as.matrix(forecast_arima_tsRMS$fitted), date=time(forecast_arima_tsRMS$x))
df_tsRMS_AR['ano']=str_sub(as.character(df_tsRMS_AR$date),1,4)
names(df_tsRMS_AR)[1]<-"Fitted_AR"
df_tsRMS_AR <- df_tsRMS_AR %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMS_AR <- df_tsRMS_AR %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_AR,Anomes_int,anomes)

df_tsRMSP_AR=data.frame(Y=as.matrix(forecast_arima_tsRMSP$fitted), date=time(forecast_arima_tsRMSP$x))
df_tsRMSP_AR['ano']=str_sub(as.character(df_tsRMSP_AR$date),1,4)
names(df_tsRMSP_AR)[1]<-"Fitted_AR"
df_tsRMSP_AR <- df_tsRMSP_AR %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMSP_AR <- df_tsRMSP_AR %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_AR,Anomes_int,anomes)

df_tsRMVPLN_AR=data.frame(Y=as.matrix(forecast_arima_tsRMVPLN$fitted), date=time(forecast_arima_tsRMVPLN$x))
df_tsRMVPLN_AR['ano']=str_sub(as.character(df_tsRMVPLN_AR$date),1,4)
names(df_tsRMVPLN_AR)[1]<-"Fitted_AR"
df_tsRMVPLN_AR <- df_tsRMVPLN_AR %>%
  group_by(ano) %>%
  mutate(mes_int = 1:n())
df_tsRMVPLN_AR <- df_tsRMVPLN_AR %>%
  mutate(mes=str_pad(mes_int, 2, pad = "0")) %>%
  mutate(Anomes_int=as.integer(str_c(ano,mes))) %>%
  mutate(anomes= str_c(ano,mes)) %>%
  select(Fitted_AR,Anomes_int,anomes)



#Join e calculo de distancia
AcumRMBS<-inner_join(AcumRMBS,df_tsRMBS_AR,by='Anomes_int') 
AcumRMBS<-inner_join(AcumRMBS,df_tsRMBS_HW,by='Anomes_int')
AcumRMBS['Dist_HW']<- sqrt((AcumRMBS$S_qtd^2) -(2*AcumRMBS$S_qtd*AcumRMBS$Fitted_HW)   +  (AcumRMBS$Fitted_HW^2))
AcumRMBS['Dist_AR']<- sqrt((AcumRMBS$S_qtd^2) -(2*AcumRMBS$S_qtd*AcumRMBS$Fitted_AR)   +  (AcumRMBS$Fitted_AR^2))
AcumRMBS['M_modelo']<- factor(ifelse(AcumRMBS$Dist_HW < AcumRMBS$Dist_AR, 'HW','AR'))

AcumRMC<-inner_join(AcumRMC,df_tsRMC_AR,by='Anomes_int') 
AcumRMC<-inner_join(AcumRMC,df_tsRMC_HW,by='Anomes_int')
AcumRMC['Dist_HW']<- sqrt((AcumRMC$S_qtd^2) -(2*AcumRMC$S_qtd*AcumRMC$Fitted_HW)   +  (AcumRMC$Fitted_HW^2))
AcumRMC['Dist_AR']<- sqrt((AcumRMC$S_qtd^2) -(2*AcumRMC$S_qtd*AcumRMC$Fitted_AR)   +  (AcumRMC$Fitted_AR^2))
AcumRMC['M_modelo']<- factor(ifelse(AcumRMC$Dist_HW < AcumRMC$Dist_AR, 'HW','AR'))

AcumRMS<-inner_join(AcumRMS,df_tsRMS_AR,by='Anomes_int') 
AcumRMS<-inner_join(AcumRMS,df_tsRMS_HW,by='Anomes_int')
AcumRMS['Dist_HW']<- sqrt((AcumRMS$S_qtd^2) -(2*AcumRMS$S_qtd*AcumRMS$Fitted_HW)   +  (AcumRMS$Fitted_HW^2))
AcumRMS['Dist_AR']<- sqrt((AcumRMS$S_qtd^2) -(2*AcumRMS$S_qtd*AcumRMS$Fitted_AR)   +  (AcumRMS$Fitted_AR^2))
AcumRMS['M_modelo']<- factor(ifelse(AcumRMS$Dist_HW < AcumRMS$Dist_AR, 'HW','AR'))

AcumRMSP<-inner_join(AcumRMSP,df_tsRMSP_AR,by='Anomes_int') 
AcumRMSP<-inner_join(AcumRMSP,df_tsRMSP_HW,by='Anomes_int')
AcumRMSP['Dist_HW']<- sqrt((AcumRMSP$S_qtd^2) -(2*AcumRMSP$S_qtd*AcumRMSP$Fitted_HW)   +  (AcumRMSP$Fitted_HW^2))
AcumRMSP['Dist_AR']<- sqrt((AcumRMSP$S_qtd^2) -(2*AcumRMSP$S_qtd*AcumRMSP$Fitted_AR)   +  (AcumRMSP$Fitted_AR^2))
AcumRMSP['M_modelo']<- factor(ifelse(AcumRMSP$Dist_HW < AcumRMSP$Dist_AR, 'HW','AR'))

AcumRMVPLN<-inner_join(AcumRMVPLN,df_tsRMVPLN_AR,by='Anomes_int') 
AcumRMVPLN<-inner_join(AcumRMVPLN,df_tsRMVPLN_HW,by='Anomes_int')
AcumRMVPLN['Dist_HW']<- sqrt((AcumRMVPLN$S_qtd^2) -(2*AcumRMVPLN$S_qtd*AcumRMVPLN$Fitted_HW)   +  (AcumRMVPLN$Fitted_HW^2))
AcumRMVPLN['Dist_AR']<- sqrt((AcumRMVPLN$S_qtd^2) -(2*AcumRMVPLN$S_qtd*AcumRMVPLN$Fitted_AR)   +  (AcumRMVPLN$Fitted_AR^2))
AcumRMVPLN['M_modelo']<- factor(ifelse(AcumRMVPLN$Dist_HW < AcumRMVPLN$Dist_AR, 'HW','AR'))


#Analise Final e escolha de modelos
summary(AcumRMBS)
summary(AcumRMC)
summary(AcumRMS)
summary(AcumRMSP)
summary(AcumRMVPLN)

