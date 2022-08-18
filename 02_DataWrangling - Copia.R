#load Data
data = read.table("./Bases EMTU/CaCEMTU.csv", header=TRUE, sep=";")

#Separa Mes e Ano
nMes=as.integer(stringr::str_extract(data[,1], "^.{2}"))
nAno=as.integer(str_sub(data$Mes,4,9))

#Drop Mes
data = data %>% select(Região,Empresa,Manifestacao,Item,Quantidade)

#append int no dataframe
data['Mes']<-nMes
data['Ano']<-nAno
names(data)[1]<-"Regiao"

#Organiza Colunas
data = data %>% select(Regiao,Empresa,Manifestacao,Item,Mes,Ano,Quantidade)

#Sumariza dados
summary(data)

#Soma Acumulando por Regiao,Empresa, Mes e Ano
Acum_Empresa = data %>% group_by(Regiao,Mes,Ano) %>% summarise(S_qtd= sum(Quantidade)) 


#Soma Acumulando por  Mes e Ano
Acum_Empresa_AnoMes = data %>% group_by(Mes,Ano) %>% summarise(S_qtd= sum(Quantidade)) 

#Descobre as Macro Regiões
data$Regiao %>% unique

ggplot(data=filter(Acum_Empresa,Ano==2021), aes(x=Mes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()

ggplot(data=filter(Acum_Empresa,Ano==2021 && Regiao !="RMSP"), aes(x=Mes, y=S_qtd,group=Regiao, color=Regiao)) +
  geom_line()

ggplot(data=Acum_Empresa_AnoMes, aes(x=Mes, y=S_qtd,group=1)) +
  geom_line()

