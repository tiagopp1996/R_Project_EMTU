##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("MASS","neuralnet","ISLR","mlbench","neuralnet","rpart","datasets"
             ,"forecast","fpp2","tseries","patchwork", "DataCombine", "TTR", "dplyr"
             ,"reshape2","tidyverse","PerformanceAnalytics")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

