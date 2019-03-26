library (ggplot2)
library (quadprog)
library (MASS)
library (Rblpapi)
library(corrplot)
library(RColorBrewer)
blpConnect()


pathOutput = "K://Users/SUFIX/Leo/Git/R_Corr/"
pathInput = pathOutput

#===========================================================================
# Cores
#===========================================================================

lista_cores = c(
  rgb(255,153,102, maxColorValue=255), # Laranja Claro
  rgb(102,153,255, maxColorValue=255), # Azul
  rgb(151,153,155, maxColorValue=255), # Cinza Escuro
  rgb(4,37,90, maxColorValue=255), # Azul Escuro
  rgb(183,183,183, maxColorValue=255), # Cinza 
  rgb(211,211,211, maxColorValue=255), # Cinza Claro
  rgb(220,107,47, maxColorValue=255), # Laranja 
  rgb(0,153,204, maxColorValue=255), # Azul 2
  rgb(153,204,255, maxColorValue=255), # Azul 2
  rgb(204,236,255, maxColorValue=255)) # Azul 2

names(lista_cores)=lista_cores

#===========================================================================
# Pega dados
#===========================================================================

lista_fundos = read_csv(paste(pathInput,"Fundos_Cov.txt",sep=""))
lista_fundos_n = nrow(lista_fundos)

data0 = as.Date("2015-01-01")
dataN = Sys.Date()-2
sDatas=seq(data0, dataN,by="day") # Maximum time span for dates
nDatas = length(sDatas)

bdhOpt = c("nonTradingDayFillOption" = "ALL_CALENDAR_DAYS", "nonTradingDayFillMethod" = "PREVIOUS_VALUE")
pos0_series = rep(1, lista_fundos_n)
serieRaw=matrix(0,ncol=lista_fundos_n,nrow=nDatas)

for(i in 1:lista_fundos_n) {
  # Open file removing any bizar case without valeu withing the series
  date_i = data0
  serieI= bdh(as.character(lista_fundos$Ticker[i]),"PX_LAST",date_i,dataN,options=bdhOpt)
  serieI=serieI[!is.na(serieI[,2]),]
  
  # Send data with vector with all dates
  validos = is.element(sDatas,serieI$date)
  serieRaw[validos,i]=serieI[,2]
}

serieRaw[serieRaw==0]=NA

#==================================================
# Matriz de correlação
#==================================================

# Week definitions
retWin = 7
rangeWin = seq((nDatas-1)%%retWin+1,nDatas,retWin)
rangeWin = tail(rangeWin,156)
nW = length(rangeWin)

# Adjust for CDI
retWin = log(serieRaw[rangeWin[-1],]/serieRaw[rangeWin[-nW],])
retWin = retWin[,-c(1)] - retWin[,1]
colnames(retWin)=lista_fundos$Name[-c(1)]

retWin[is.nan(retWin)]=NA

M = cor(retWin,use="pairwise.complete.obs",method = "spearman")

tcol_aux = rep(lista_cores[3],dim(M)[1])
X = corrplot(M, order = "hclust", addrect = 3)
png(height=1200, width=1200, pointsize=25, filename=paste(pathOutput, "Cor.png", sep = ""))
tcol_aux[colnames(X) %in% c("IMA B","IRF-M","Ibov","HF Index","Carry Real")]=lista_cores[8]
tcol_aux[colnames(X) %in% c("Apollo","SulAmerica Tatico","SulAmerica Endurance","SulAmerica Evolution")]=lista_cores[7]
corrplot(M, order = "hclust", addrect = 3,tl.col=tcol_aux)
dev.off()


