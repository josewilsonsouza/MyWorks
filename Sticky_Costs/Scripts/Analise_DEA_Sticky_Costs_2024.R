library(reshape2)
library(dplyr)
library(Benchmarking)
library(writexl)
library(readxl)


data <- read_excel("Dados/PAINEL_GERAL.xlsx", sheet = "DADOS")
data$MÊS <- as.numeric(gsub("/",'',data$MÊS))

dfemp_trim <- function(df,ano,trim){
  filtro <- df %>%
    filter(MÊS == trim & `ANO` == ano)
  return(filtro)
}

fun_delNA <- function(df, ano, trim){
  df_ano_trim = dfemp_trim(df, ano, trim)
  df_ano_trim <- df_ano_trim[,c('EMPRESA','ANO','MÊS','IN','E&M','AI','MO','RLV')]
  df_ano_trim <- na.omit(df_ano_trim)
  return(df_ano_trim)
}

anos <- 2010:2019

df_completo = data.frame()
for (ano in anos) {
  for (trim in c(3,6,9,12)) {
    df <- fun_delNA(data,ano,trim)
    x <- as.matrix(with(df, cbind(IN,`E&M`,AI,MO)))
    y <- as.matrix(with(df, RLV))
    
    crs_in <- dea(x,y, RTS="crs", ORIENTATION = "in")
    vrs_in <- dea(x,y, RTS="vrs", ORIENTATION = "in")
    
    dc <- mutate(df, crs_ins=crs_in$eff, vrs_ins=vrs_in$eff)
    df_completo <- bind_rows(df_completo, dc)
  }
}


dea.plot(x,y,RTS="vrs",ORIENTATION="in-out",txt=rownames(x))
dea.plot(x,y,RTS="drs",ORIENTATION="in-out",add=TRUE,lty="dashed",lwd=2)
dea.plot(x,y,RTS="crs",ORIENTATION="in-out",add=TRUE,lty="dotted")
dea.plot(x,y,RTS="fdh",ORIENTATION="in-out",txt=rownames(x),main="fdh")
dea.plot(x,y,RTS="irs",ORIENTATION="in-out",txt=TRUE,main="irs")
dea.plot(x,y,RTS="irs2",ORIENTATION="in-out",txt=rownames(x),main="irs2")
dea.plot(x,y,RTS="add",ORIENTATION="in-out",txt=rownames(x),main="add")

View(df_completo)

#write_xlsx(df_completo, "ARTIGO - STICKY COSTS - BASE//Analise_DEA_2024.xlsx")
