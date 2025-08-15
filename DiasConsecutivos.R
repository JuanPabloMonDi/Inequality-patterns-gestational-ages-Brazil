library(sf)
library(ggplot2)
library(geobr)
library(arrow); library(dplyr) ; library(geobr) ; library(stringr) ; library(roll) ; library(Kendall) ; library(trend) ; library(ggplot2) ; library(RColorBrewer) ; library(mltools) ; library(data.table) ; library(sf) ; library(ResourceSelection)

# Define el umbral (por ejemplo 30 grados)
umbral <- 0

detectar_exposicion_dias_info <- function(x, dias){
  r <- (x > 0)  # Vector lógico de exposición
  resultado <- rep(0, length(x))  # Para marcar los días donde empieza la secuencia
  contador <- 0  # Cuenta cuántas veces ocurre
  i<-1
  while (i <= (length(x) - dias)) {
    if (all(r[i:(i+2)])){
      resultado[i] <- 1
      contador <- contador + 1
      i<- i+dias  # Salta al día después de la secuencia para evitar solapamientos
      }else{
        i<-i+1
      }
    }
  
  # Clasificación final según cantidad de secuencias
  clasificacion <- ifelse(contador == 0, "0",
                          ifelse(contador == 1, "1",
                                 ifelse(contador == 2, "2", "3 ou mais")))  # 3 significa "3 o más"
  
  return(list(clase = clasificacion, conteo = contador, dias_inicio = resultado,dias=dias))
}

detectar_si<-function(x,dias){
  b<-detectar_exposicion_dias_info(x,dias)
  b<-b$clase
  return(b)
}

contagem<-data.frame('CD_MUN'=bulbo_seco_municipal$CD_MUN)
for (n in c(2,3,5,7,10)){
  contagem_bulbo_seco <- apply(bulbo_seco_municipal[,-1],1, function(x) detectar_exposicion_dias_info(x)$clase)
  contagem_bulbo_umido <- apply(bulbo_umido_municipal[,-1],1, function(x) detectar_exposicion_dias_info(x)$clase)
  a <- data.frame("CD_MUN" = bulbo_seco_municipal$CD_MUN,
                  "seco" = contagem_bulbo_seco,
                  "umido" = contagem_bulbo_umido)
  colnames(a)[2:3] <- c(paste0("seco", n), paste0("umido", n))
  contagem<-merge(contagem,a,by="CD_MUN")
}

ano=2016

janela_tempo<-function(n_dias, ano,
                       bulbo_seco_muni=bulbo_seco_municipal, 
                       bulbo_umido_muni=bulbo_umido_municipal){
  b <- bulbo_seco_muni[, c("CD_MUN", colnames(bulbo_seco_muni)[substr(colnames(bulbo_seco_muni), 1, 4) == ano])]
  c <- bulbo_umido_muni[, c("CD_MUN", colnames(bulbo_umido_muni)[substr(colnames(bulbo_umido_muni), 1, 4) == ano])]
  contagem<-data.frame('CD_MUN'=b$CD_MUN)
  for (n in n_dias){
    contagem_bulbo_seco <- apply(b[,-1],1,detectar_si,dias=n)
    contagem_bulbo_umido <- apply(c[,-1],1,detectar_si,dias=n)  
    a <- data.frame("CD_MUN" = b$CD_MUN,
                    "seco" = contagem_bulbo_seco,
                    "umido" = contagem_bulbo_umido)
    colnames(a)[2:3] <- c(paste0("seco", n), paste0("umido", n))
    contagem<-merge(contagem,a,by="CD_MUN")
  }
  return(contagem)
}

contagem2016<-janela_tempo(n_dias = c(3),ano = "2016")
contagem2023<-janela_tempo(n_dias = c(3),ano = "2023")

estado_mapa<- read_state(code_state = "all", year = 2020, showProgress = F)
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = F) 
colnames(contagem2016)[1]<-"code_muni"
colnames(contagem2023)[1]<-"code_muni"

cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
#estado_mapa$code[municipios_koppen$IBGE.Code %in% capitais]<- as.numeric( paste0( substr(municipios_koppen$IBGE.Code[municipios_koppen$IBGE.Code %in% capitais], 1, 2)  , "0000") ) 

mapas2016<-merge(contagem2016, cidade_mapa, by="code_muni")
mapas2023<-merge(contagem2023, cidade_mapa, by="code_muni")
# Asegúrate de que sea un objeto sf
mapa2016 <- st_as_sf(mapas2016)
mapa2023 <- st_as_sf(mapas2023)
# Graficar
ggplot(data=mapa2016) +
  geom_sf(aes(fill = seco3), color=NA) +  # municipalities colored
  geom_sf(data = estado_mapa, fill = NA, color = "black", lwd = 0.3) +  # state outlines
  #scale_fill_brewer(type = "div", palette = 4, na.translate = FALSE) +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() 

ggplot(data=mapa2016) +
  geom_sf(aes(fill = umido3), color=NA) +  # municipalities colored
  geom_sf(data = estado_mapa, fill = NA, color = "black", lwd = 0.3) +  # state outlines
  #scale_fill_brewer(type = "div", palette = 4, na.translate = FALSE) +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() 

ggplot(data = mapa2023) +
  geom_sf(aes(fill = seco3), color=NA) +  # municipalities colored
  geom_sf(data = estado_mapa, fill = NA, color = "black", lwd = 0.3) +  # state outlines
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal()

ggplot(data=mapa2023) +
  geom_sf(aes(fill = umido3), color=NA) +  # municipalities colored
  geom_sf(data = estado_mapa, fill = NA, color = "black", lwd = 0.3) +  # state outlines
  #scale_fill_brewer(type = "div", palette = 4, na.translate = FALSE) +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() 



## Carregar os dados do anoo!!!!!! Pelo momento é separado porque é pessado


data_limite <- as.Date(colnames(bulbo_seco_municipal)[ncol(bulbo_seco_municipal)])

contar_ocurrencias_rango <- function(codmun, concepcao, nasc, tabla_b, fecha_limite) {
  fecha_fin <- min(nasc, fecha_limite)
  
  if (concepcao > fecha_fin) return(0)
  
  fechas_rango <- format(seq(concepcao, fecha_fin, by = "day"), "%Y-%m-%d")
  
  fila_b <- tabla_b[tabla_b$CD_MUN == codmun, ]
  if (nrow(fila_b) == 0) return(NA)  # Municipio no encontrado en tabla B
  
  fechas_validas <- fechas_rango[fechas_rango %in% colnames(fila_b)]
  
  sum(as.numeric(fila_b[1, fechas_validas]), na.rm = TRUE)
}

ocurrencias_vector <- numeric(nrow(dados_col))

for (i in seq_len(nrow(dados_col))) {
  codmun     <- dados_col$CODMUNRES[i]
  concepcao  <- dados_col$CONCEPCAO[i]
  nasc       <- dados_col$DTNASC_R[i]
  
  ocurrencias_vector[i] <- contar_ocurrencias_rango(
    codmun, concepcao, nasc,
    bulbo_seco_municipal, data_limite
  )
  print(i/nrow(dados_col))
}

dados_col2 <- dados_col[1:i, ]

dados_col2$ocurrencias <- ocurrencias_vector[1:i]

#dados_col$ocurrencias <- ocurrencias_vector