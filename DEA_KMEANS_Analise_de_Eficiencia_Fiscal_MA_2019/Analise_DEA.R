#install.packages(c("Benchmarking", "deaR"))
#install.packages("writexl")
#install.packages("readxl")
#install.packages('dplyr')
#install.packages('nonparaeff')

library(dplyr)
library(Benchmarking)
library(writexl)
library(readxl)

data <- read_excel("Dados.xlsx", sheet = "DADOS")
df = data[, c('CODIGO', 'MUNICIPIO', 'POP', 'VAI_pc', 'VAS_pc', 'VAA_pc', 'ARR_pc','FPM_pc',
              'PIB_pc','GD','GAP')]

x <- as.matrix(with(df, cbind(VAA_pc, VAI_pc, VAS_pc, PIB_pc)))
y <- as.matrix(with(df, ARR_pc))

cin <- dea(x,y, RTS="crs", ORIENTATION = "in")
cou <- dea(x,y, RTS="crs", ORIENTATION = "out")
vin <- dea(x,y, RTS="vrs", ORIENTATION = "in")
vou <- dea(x,y, RTS="vrs", ORIENTATION = "out")

res_ef <- mutate(df,
                 cin=cin$eff,
                 cou=cou$eff, 
                 vin=vin$eff,
                 vou=vou$eff)


View(res_ef)

write_xlsx(res_ef, "Resultados/Analise_DEA.xlsx")