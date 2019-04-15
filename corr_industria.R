library (ggplot2)
library (quadprog)
library (MASS)
library (Rblpapi)
library(corrplot)
library(RColorBrewer)
blpConnect()


pathOutput = "K://Users/SUFIX/Leo/Git/R_Correl/"
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

data0 = as.Date("2013-01-01")
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
#rangeWin = tail(rangeWin,156)
nW = length(rangeWin)

# Adjust for CDI
retWin = log(serieRaw[rangeWin[-1],]/serieRaw[rangeWin[-nW],])
retWin = retWin[,-c(1)] - retWin[,1]
colnames(retWin)=lista_fundos$Name[-c(1)]

retWin[is.nan(retWin)]=NA

M = cor(tail(retWin,156),use="pairwise.complete.obs",method = "spearman")

tcol_aux = rep(lista_cores[3],dim(M)[1])
X = corrplot(M, order = "hclust", addrect = 4)
png(height=1200, width=1200, pointsize=25, filename=paste(pathOutput, "Cor.png", sep = ""))
tcol_aux[colnames(X) %in% c("IMA B","IRF-M","Ibov","HF Index","Carry Real")]=lista_cores[8]
tcol_aux[colnames(X) %in% c("Apollo","SulAmerica Tatico","SulAmerica Endurance","SulAmerica Evolution")]=lista_cores[7]
corrplot(M, order = "hclust", addrect = 4,tl.col=tcol_aux)
dev.off()

#==================================================
# Plot industria vs. bench
#==================================================

ret_ind = retWin[,colnames(retWin) =="HF Index"]
ret_ben = retWin[,colnames(retWin) %in% c("IRF-M","Ibov")]
n_row = length(ret_ind)

plot_data = tibble()

# Long term
for (i in 1:(dim(ret_ben)[2])) {
  
  ret_i = rlm(ret_ind~ret_ben[,i])
  ret_i_info = summary(ret_i)
  print(ret_i_info$coefficient[2,3])
  
  valid = order(abs(ret_i$residuals))[1:round(0.9*n_row,0)]
  size_aux = rep("Outlier",n_row)
  size_aux[valid]="Normal"
  
  ret_i = summary(lm(ret_ind[valid]~ret_ben[valid,i]))
  name_i = paste(colnames(ret_ben)[i]," 2013 -  (",round(ret_i$r.squared,2),")",sep="")

  plot_data = bind_rows(plot_data,
                        tibble(
                          HF = ret_ind,
                          bench = ret_ben[,i],
                          nome = name_i,
                          size_pts = size_aux
                        ))
  
  
}

# Short term
n_row = 104
ret_ind = tail(ret_ind,n_row)
ret_ben = tail(ret_ben,n_row)

for (i in 1:(dim(ret_ben)[2])) {

  ret_i = rlm(ret_ind~ret_ben[,i])
  ret_i_info = summary(ret_i)
  print(ret_i_info$coefficient[2,3])
  
  valid = order(abs(ret_i$residuals))[1:round(0.9*n_row,0)]
  size_aux = rep("Outlier",n_row)
  size_aux[valid]="Normal"
  
  ret_i = summary(lm(ret_ind[valid]~ret_ben[valid,i]))
  name_i = paste(colnames(ret_ben)[i]," ",n_row,"w (",round(ret_i$r.squared,2),")",sep="")
  
  plot_data = bind_rows(plot_data,
                        tibble(
                          HF = ret_ind,
                          bench = ret_ben[,i],
                          nome = name_i,
                          size_pts = size_aux
                        ))
  
  
}

size_aux = c(1,2)
names(size_aux)=c("Outlier","Normal")
col_aux = c(lista_cores[1],lista_cores[7])
names(col_aux)=c("Outlier","Normal")

ggplot(data = plot_data, aes(x = bench, y = HF)) + 
  facet_wrap(~nome,nrow=2,scales="free_x")+
  geom_point(aes(size = size_pts,colour=size_pts))+ 
  geom_smooth(method = "rlm", se = FALSE,colour = lista_cores[2])+
  scale_size_manual(name = NULL, values = size_aux)+
  scale_colour_manual(name = NULL, values = col_aux)+
  xlab("Fator de risco")+
  ylab("Índice de Hedge Fund ANBIMA")+
  theme(legend.position = c(0.89, 0.09))
 
ggsave(paste(pathOutput,"Beta.png", sep = ""),width=14,height=14,units="cm")

size_aux = c(0.5,1)
names(size_aux)=c("Outlier","Normal")

ggplot(data = plot_data %>% filter(substr(plot_data$nome,1,8)=="Ibov 104"), aes(x = 100*bench, y = 100*HF)) + 
  geom_point(aes(size = size_pts,colour=size_pts))+ 
  geom_smooth(method = "rlm", se = FALSE,colour = lista_cores[2])+
  scale_size_manual(name = NULL, values = size_aux)+
  scale_colour_manual(name = NULL, values = col_aux)+
  xlab("IBOVESPA")+
  ylab("Índice de Hedge\nFund ANBIMA")+
  theme(legend.position = c(0.85, 0.3))

ggsave(paste(pathOutput,"Beta2.png", sep = ""),width=10,height=5,units="cm")

