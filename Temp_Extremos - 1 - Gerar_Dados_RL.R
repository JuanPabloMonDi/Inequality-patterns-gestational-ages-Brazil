########################################################
###        Organizando os Dados  --  SINASC
###           Condicao Seca, Umida e Tw
#######################################################

### Pacotes
rm(list = ls()) ; options(scipen = 999) ; library(arrow); library(dplyr) ; library(geobr) ; library(stringr) ; library(roll) ; library(Kendall) ; library(trend) ; library(ggplot2) ; library(RColorBrewer) ; library(mltools) ; library(data.table) ; setwd("~/Dropbox/Unicamp - Cristiano/") 


########################################################################
#if (TRUE) {  my_data<-2022 ;  # Loop "for" para gerar varios anos
#rm(list = ls())  ; #for (my_data in 2012:2023){ cat(paste0("Ano: ",my_data,"\n")) #
my_data<- 2022

###  Dados 
SINASC<- read.csv(file = paste0("~/Documentos/opendatasus/SINASC_",my_data,".csv"), header = T, sep = ";") # Dados do SINASC
load(file = "~/Dropbox/Unicamp - Cristiano/peso_UF_Micro.RData") # Tabela de referencia com os valores de baixo peso por sexo e semana gestacional
microrregiao_dtb<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/Climaterna/Programas/DTB_2022.csv", header = T, sep = ";", colClasses = c("character") )
estado_mapa<- read_state(code_state = "all", year = 2020, showProgress = F)
microrregiao_mapa <- read_micro_region(code_micro="all", year=2020, showProgress = F) ; microrregiao_mapa<- microrregiao_mapa[ !(microrregiao_mapa$name_micro == "Fernando De Noronha"), ]
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = F) 


### Algumas Colunas do SINASC para Utilizar no Modelo
# Variaveis efetivamente utilizadas
colunas_utilizadas<- c("CODMUNRES", 'DTNASC', 'GESTACAO', 'PESO','PARIDADE', 'PARTO','SEXO','RACACOR','ESCMAE','SEMAGESTAC','CONSULTAS','ESTCIVMAE','IDADEMAE', 'GRAVIDEZ')
dados_col<- select(SINASC, c(colunas_utilizadas) ) ; rm(SINASC) # removendo o sinasc, liberar memoria


### Joining classes
# df['PARTO'] = df['PARTO'].replace(1, 0) # 1 to 0 - Vaginal delivery is ref.
dados_col$PARTO[dados_col$PARTO == 1 ]<- 0
#df['PARTO'] = df['PARTO'].replace(2, 1) # 2 to 1 -
dados_col$PARTO[dados_col$PARTO == 2 ]<- 1
#df['ESCMAE'] = df['ESCMAE'].replace(2, 1) # Escolaridade 2 agrupada na classe 1.
dados_col$ESCMAE[dados_col$ESCMAE == 2 ]<- 1
#df['ESCMAE'] = df['ESCMAE'].replace(3, 1) # Escolaridade 3 agrupada na classe 1.
dados_col$ESCMAE[dados_col$ESCMAE == 3 ]<- 1
#df['ESTCIVMAE'] = df['ESTCIVMAE'].replace(5, 2) # Estado civil 5 (união estável) agrupada na classe 2 (casado).
dados_col$ESTCIVMAE[dados_col$ESTCIVMAE == 5 ]<- 2
#df['ESTCIVMAE'] = df['ESTCIVMAE'].replace(4, 1) # Estado civil 4 (separada) agrupada na classe 1 (solteira).
dados_col$ESTCIVMAE[dados_col$ESTCIVMAE == 4 ]<- 1




###  Formatando dados
if (my_data == 2014){ dados_col$SEXO<- as.numeric(dados_col$SEXO=="M") + 2*as.numeric(dados_col$SEXO=="F")  }
dados_col<- dados_col[(!(is.na(dados_col$PESO))), ]  ; dados_col<- dados_col[(!(is.na(dados_col$SEMAGESTAC))), ] ; dados_col<- dados_col[(!(is.na(dados_col$CODMUNRES))), ] ; dados_col<- dados_col[(!(is.na(dados_col$DTNASC))), ] ; 
dados_col<- dados_col[ (!( dados_col$CODMUNRES <= 110000  )) & (!( dados_col$CODMUNRES > 530010  )),   ] # CODMUNRES Mal inseridos

#  Passando datas para o formato no R e Obtendo o dia/periodo da concepcao
aux_data<- paste0( c(str_sub(string = as.character( dados_col$DTNASC), start = 1, end = -7)), "-", 
                   c(str_sub(string = as.character( dados_col$DTNASC), start = -6, end = -5)), "-",
                   c(str_sub(string = as.character( dados_col$DTNASC), start = -4, end = -1))  ) ; aux_data<-  as.Date(x = aux_data, format = "%d-%m-%Y")
dados_col$CONCEPCAO<- aux_data - dados_col$SEMAGESTAC*7 # Data da Concepcao
dados_col$DTNASC_R<- aux_data ; dados_col$MES_R<- as.numeric( format(x =as.Date(  aux_data ), format = "%m") )  

# Adicionando as capitais no aux_dtb  como cidades "0000". No SINASC aparece mais de 480000 nesse formato
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
cidade_mapa$code_muni[cidade_mapa$code_muni %in% capitais]<- as.numeric( paste0( substr(cidade_mapa$code_muni[cidade_mapa$code_muni %in% capitais], 1, 2)  , "0000") ) 
dados_col$CODMUNRES[dados_col$CODMUNRES %in% capitais]<- as.numeric( paste0( substr(dados_col$CODMUNRES[dados_col$CODMUNRES %in% capitais], 1, 2)  , "0000") ) 


### Adicionando o codigo da microrregiao
aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                 data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = as.numeric( paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )  )
aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)
dados_col<-  left_join(dados_col, aux_dtb[, c("Nome_UF","Nome_Microrregião", "Nome_Município", "code_micro", "codmun")], by=c("CODMUNRES"="codmun"))


### Baixo Peso - Adicionando a comparacao com o peso (10% menores) nos dados - Dados da  "WHO"
aux.semana<- dados_col$SEMAGESTAC ; aux.semana[aux.semana>40]<- 40 ; aux.semana[aux.semana < 14]<- 14 
peso_10<- (pesos_mm$Male_10[(aux.semana - 13)])*(dados_col$SEXO == 1) + (pesos_mm$Female_10[(aux.semana - 13)])*(dados_col$SEXO == 2)
dados_col$PESO_10<- peso_10 ; dados_col$BAIXO_PESO<- as.numeric(peso_10 >= dados_col$PESO)


###  Prematuridade - Nascimento antes de completar 37 semanas. Separando por faixas
dados_col$NASCIMENTOS<- 1
dados_col$EXTREMO_PREMATURO<-   as.numeric(  dados_col$SEMAGESTAC <  28)
dados_col$MUITO_PREMATURO<-     as.numeric( (dados_col$SEMAGESTAC >= 28) & (dados_col$SEMAGESTAC < 32) )
dados_col$MODERADO_PREMATURO<-  as.numeric( (dados_col$SEMAGESTAC >= 32) & (dados_col$SEMAGESTAC < 34) )
dados_col$TARDIO_PREMATURO<-    as.numeric( (dados_col$SEMAGESTAC >= 34) & (dados_col$SEMAGESTAC < 37) )
dados_col$PREMATURO<-           as.numeric(  dados_col$SEMAGESTAC <  37)
dados_col$A_TERMO<-             as.numeric(  dados_col$SEMAGESTAC >=  37)




########
###  Subnotificacoes
#corrigir<- TRUE ; if (corrigir){ 
#cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )

# Carregando os dados; 2019 e 2020 estao com 7 digitos =(  ; 
if ( (my_data < 2015) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',2015,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }
if ( (my_data > 2022) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',2022,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }
if ( (my_data >= 2015) & (my_data <= 2022) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',my_data,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }

#if (my_data>= 2019){ eval(parse(text = paste0('  sub_nasc_', my_data, '$CODMUNRES<- as.numeric( substr(x = as.character(sub_nasc_', my_data, '$CODMUNRES), start = 1, stop = 6) )  ') )) }
sub_nasc$CODMUNRES<- as.numeric( substr(x = as.character(sub_nasc$CODMUNRES), start = 1, stop = 6) )  ;#sub_nasc$Sub_MS<- sub_nasc$Sub_MS/100 ; sub_nasc$Sub_IBGE<- sub_nasc$Sub_IBGE/100
sub_nasc$CODMUNRES[sub_nasc$CODMUNRES %in% capitais]<- as.numeric( paste0( substr(sub_nasc$CODMUNRES[sub_nasc$CODMUNRES %in% capitais], 1, 2)  , "0000") ) 

dados_col<- left_join(dados_col, sub_nasc[,c("CODMUNRES","Sub_MS")], by=c("CODMUNRES"="CODMUNRES"))  
sum(is.na(dados_col$Sub_MS  )) ; dados_col$Sub_MS<-  dados_col$Sub_MS/100 # Confirmando se tem NA
#save(list = c("dados_col" ), file = paste0("~/Downloads/dados_col_SINASC_",my_data,".RData") ) ;  # save(list = c("dados_col","cidade_mapa", "capitais", "microrregiao_dtb" ), file = paste0("~/Downloads/dados_col_SINASC_",my_data,".RData") )
#rm(list = ls()) }
#}


###  Removendo os NA de todas as Variaveis
dim(dados_col) ; dados_col<- dados_col[rowSums(is.na(dados_col) ) == 0, ] ; dim(dados_col)


### One-hot enconding
#define one-hot encoding function and  #perform one-hot encoding on data frame
#library(caret) ; dummy <- dummyVars(" ~ .", data= aux_dados, sep = "_") ;final_df <- data.frame(predict(dummy, newdata=aux_dados))
aux_dados<- dados_col[,c("SEXO", "RACACOR", "ESCMAE", "GRAVIDEZ", "CONSULTAS", "ESTCIVMAE" )] ; aux_dados[] <- lapply( aux_dados, factor) ; aux_dados <- one_hot(as.data.table( aux_dados) )

### Creating a dummy for mother age
#df["IDADE_CAT"] = np.where(df['IDADEMAE'] < 19, '10-18', np.where(df['IDADEMAE'] < 34, '19-34','35+' ))
aux_dados$IDADEMAE_18<- as.numeric(dados_col$IDADEMAE <= 18) ; aux_dados$IDADEMAE_34<- as.numeric(dados_col$IDADEMAE <= 34) ; aux_dados$IDADEMAE_35mais<- as.numeric(dados_col$IDADEMAE > 34)
dim(aux_dados) ; dim(dados_col)

dados_col<- cbind(dados_col, aux_dados)
#save(list = c("my_data","dados_col"), file = paste0("~/Downloads/dados_col_SINASC-Propensity_Score (",my_data,").RData")) 
#}





########################################################################
########  Analisando os Efeitos de Temperaura
########  Calculando Condicao Seca e Umida, Alemda Temperatura de Bulbo
## Testando Exposicao de 3, 7 e 30 dias 
#rm(list = ls()) ; my_data<- 2022 ; load(file = paste0("~/Downloads/dados_col_SINASC-Propensity_Score (",my_data,").RData"))

# Definicao de Bulbo Seco
#sum((Tmax_municipal[,-1] >= 35 & RH_municipal[,-1] <= 30) != bulbo_seco_municipal[,-1], na.rm = T)
# Definicao de Bulbo Umido
#sum((Tmax_municipal[,-1] >= 35 & RH_municipal[,-1] >= 50) != bulbo_seco_municipal[,-1], na.rm = T)

###  Dados de Temperaturas Gerados pelos Arquivos Parquet.
###  Forma como gerei esta no final deste arquivo, como "Etapa Zero"
load(file = "~/Documentos/opendatasus/dados_col_SINASC/Tmax_Tmin_maximas_RH_Bulbos_municipal(nomes).RData") 
CD_mun<- bulbo_seco_municipal$CD_MUN ; rm(list = c("RH_municipal" ,"Tmax_municipal","Tmin_municipal")) ; gc()


###  Calculnando a Exposicao de Bulbo Seco, Bulbo Umido, 
###  Temperatura do Bulbo Media (Conceito  do Clima)  e a Temperatura do Bulbo Maxima (Conceito  do Clima) 
#  Bulbo Seco
exp_bulbo_seco_3<-t(apply(X = bulbo_seco_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 3  )) ; exp_bulbo_seco_3<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_seco_3 )
exp_bulbo_seco_7<-t(apply(X = bulbo_seco_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 7  )) ; exp_bulbo_seco_7<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_seco_7 )
exp_bulbo_seco_30<-t(apply(X = bulbo_seco_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 30  )) ; exp_bulbo_seco_30<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_seco_30 )

#  Bulbo Umido
exp_bulbo_umido_3<-t(apply(X = bulbo_umido_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 3  )) ; exp_bulbo_umido_3<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_umido_3 )
exp_bulbo_umido_7<-t(apply(X = bulbo_umido_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 7  )) ; exp_bulbo_umido_7<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_umido_7 )
exp_bulbo_umido_30<-t(apply(X = bulbo_umido_municipal[,-1],MARGIN = 1, FUN = roll_sum, width = 30  )) ; exp_bulbo_umido_30<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_bulbo_umido_30 )
rm(list = c("bulbo_seco_municipal")) ; gc()

#  Temperatura do Bulbo Media
exp_Tw_mean_3<- t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_mean, width = 3  ))  ; exp_Tw_mean_3<-  cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_mean_3 )
exp_Tw_mean_7<- t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_mean, width = 7  ))  ; exp_Tw_mean_7<-  cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_mean_7 )
exp_Tw_mean_30<-t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_mean, width = 30  )) ; exp_Tw_mean_30<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_mean_30 )

#  Temperatura do Bulbo Maxima
exp_Tw_max_3<- t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_max, width = 3  ))  ; exp_Tw_max_3<-  cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_max_3 )
exp_Tw_max_7<- t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_max, width = 7  ))  ; exp_Tw_max_7<-  cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_max_7 )
exp_Tw_max_30<-t(apply(X = bulbo_temp_municipal[,-1],MARGIN = 1, FUN = roll_max, width = 30  )) ; exp_Tw_max_30<- cbind(data.frame(CD_MUN = bulbo_umido_municipal$CD_MUN), exp_Tw_max_30 )
rm(list = c("bulbo_temp_municipal", "bulbo_umido_municipal" )) ; gc()

#
todos_dias_sinasc<- levels(as.factor(dados_col$DTNASC_R))
dados_col$condicao_seco_3dias<-NA  ; dados_col$condicao_seco_7dias<-NA  ; dados_col$condicao_seco_30dias<-NA
dados_col$condicao_umido_3dias<-NA ; dados_col$condicao_umido_7dias<-NA ; dados_col$condicao_umido_30dias<-NA
dados_col$Tw_mean_3dias<-NA ; dados_col$Tw_mean_7dias<-NA ; dados_col$Tw_mean_30dias<-NA
dados_col$Tw_max_3dias<-NA ; dados_col$Tw_max_7dias<-NA ; dados_col$Tw_max_30dias<-NA


###  Organizando os Resultados
gc() ; for (i_dd in 1:length(todos_dias_sinasc)  ) {  cat(paste0( "Dia: ",i_dd,"\n" ) )
  aux_bulbo_cd<- data.frame( CD_MUN = CD_mun,  
                             condicao_seco_3dias   = exp_bulbo_seco_3[    , (names(exp_bulbo_seco_3)   == todos_dias_sinasc[i_dd]) ],
                             condicao_seco_7dias   = exp_bulbo_seco_7[    , (names(exp_bulbo_seco_7)   == todos_dias_sinasc[i_dd]) ],
                             condicao_seco_30dias  = exp_bulbo_seco_30[   , (names(exp_bulbo_seco_30)  == todos_dias_sinasc[i_dd]) ],
                             condicao_umido_3dias  = exp_bulbo_umido_3[   , (names(exp_bulbo_umido_3)  == todos_dias_sinasc[i_dd]) ],
                             condicao_umido_7dias  = exp_bulbo_umido_7[   , (names(exp_bulbo_umido_7)  == todos_dias_sinasc[i_dd]) ],
                             condicao_umido_30dias = exp_bulbo_umido_30[  , (names(exp_bulbo_umido_30) == todos_dias_sinasc[i_dd]) ],
                             
                             Tw_mean_3dias  = exp_Tw_mean_3[   , (names(exp_Tw_mean_3)  == todos_dias_sinasc[i_dd]) ],
                             Tw_mean_7dias  = exp_Tw_mean_7[   , (names(exp_Tw_mean_7)  == todos_dias_sinasc[i_dd]) ],
                             Tw_mean_30dias = exp_Tw_mean_30[  , (names(exp_Tw_mean_30) == todos_dias_sinasc[i_dd]) ],
                             Tw_max_3dias   = exp_Tw_max_3[    , (names(exp_Tw_max_3)  == todos_dias_sinasc[i_dd]) ],
                             Tw_max_7dias   = exp_Tw_max_7[    , (names(exp_Tw_max_7)  == todos_dias_sinasc[i_dd]) ],
                             Tw_max_30dias  = exp_Tw_max_30[   , (names(exp_Tw_max_30) == todos_dias_sinasc[i_dd]) ])
  
  
  aux_cd<- dados_col[ dados_col$DTNASC_R == todos_dias_sinasc[i_dd], c("CODMUNRES", "DTNASC_R") ]
  aux_cd<- left_join(x = aux_cd, y =   aux_bulbo_cd, by=c("CODMUNRES"="CD_MUN") )
  dados_col[ dados_col$DTNASC_R == todos_dias_sinasc[i_dd], c("condicao_seco_3dias", "condicao_seco_7dias", "condicao_seco_30dias", "condicao_umido_3dias", "condicao_umido_7dias", "condicao_umido_30dias",   "Tw_mean_3dias","Tw_mean_7dias","Tw_mean_30dias",  "Tw_max_3dias", "Tw_max_7dias", "Tw_max_30dias")  ]<- aux_cd[, c("condicao_seco_3dias", "condicao_seco_7dias", "condicao_seco_30dias", "condicao_umido_3dias", "condicao_umido_7dias", "condicao_umido_30dias",   "Tw_mean_3dias","Tw_mean_7dias","Tw_mean_30dias",  "Tw_max_3dias", "Tw_max_7dias", "Tw_max_30dias") ]
} ; 

dim(dados_col) ; dados_col<- dados_col[rowSums(is.na(dados_col) ) == 0, ] ; dim(dados_col)  # Removendo os NA
save(list = c("my_data","dados_col"), file = paste0("~/Downloads/dados_col_SINASC_Condicao_Tw_-Propensity_Score (",my_data,").RData")) 
#write.table(dados_col, file = "~/Downloads/dados-SINASC_Condicao_Tw.csv",  dec = ",", sep = ";", row.names = T)  
gc()
#}
#










################################################################################
################################################################################
################################################################################

################################################################
###        Etapa Zero - Organizando os Dados de Temperatura
###              a Partir dos Arquivo Parquet
################################################################


#####################################################################################
###        Gerando o Arquivo:  Tmax_Tmin_maximas_RH_Bulbos_municipal(nomes).RData
##           Utilizado Anteriormente
#####################################################################################

##### Bulbo Umido:  Tmax>= 35°C, RH (umidade rel) >= 50%
##### Bulbo Sexo:   Tmax>= 35°C, RH (umidade rel) <= 30%
#####
###  Calculo do Bulbo Umido  --  Seguindo o Artigo - Referencia do Clima
#rm(list = ls()) ;  ano_parquet<- 2023 ; aux_parquet<- read_parquet(file =  paste0("~/Documentos/clima/BR-DWGD_",ano_parquet,".parquet"),  col_types = "string")
#bulbo_umido<- (aux_parquet$TMAX_mean)*atan( 0.151977*( aux_parquet$RH_mean +  8.313659)^(1/2) ) +  atan(aux_parquet$TMAX_mean + aux_parquet$RH_mean) - 
#  atan(aux_parquet$RH_mean - 1.676331) + 0.00391838*(( aux_parquet$RH_mean )^(3/2))* atan(0.023101*(aux_parquet$RH_mean) ) -4.686035
#fun_buldo_umido<- function(x){ temp<- x[,1] ; RH<- x[,2]
#return(  (temp)*atan( 0.151977*( RH +  8.313659)^(1/2) ) +  atan(temp + RH) - atan(RH - 1.676331) + 0.00391838*(( RH )^(3/2))* atan(0.023101*(RH) ) -4.686035 ) }
#fun_buldo_umido(matrix(data = c(20, 50), ncol = 2)) # Testando o valor. Bate com o do artigo


#rm(list = ls()) ; my_data<- 2023 ; load(file = paste0("~/Downloads/dados_col_SINASC_",my_data,".RData"))
rm(list = ls()) ;  ano_parquet<- 2010 ; aux_parquet<- read_parquet(file =  paste0("~/Documentos/clima/BR-DWGD_",ano_parquet,".parquet"),  col_types = "string")
## 2020, cidade 4300001 e 4300002  deu erro
cd_muni<- as.numeric( levels(as.factor(aux_parquet$code_muni)) ) ; 
cd_muni<- cd_muni[cd_muni != 4300001] ; cd_muni<- cd_muni[cd_muni != 4300002] ; cd_muni<- cd_muni[cd_muni != 2605459] ; cd_muni<- cd_muni[cd_muni != 4202453]


# Funcao para Calcular a Temperatura do Bulbo Umido ; Contagem dos Bulbos como Tmax>= 35°C e Umidade >= 50% para o Umido e <= 30% para o Seco
fun_buldo_umido<- function(x){ temp<- x[,1] ; RH<- x[,2]  # Primeira coluna temperatura ; Segunda Coluna Umidade
aux_bulbo<-   (temp)*atan( 0.151977*( RH +  8.313659)^(1/2) ) +  atan(temp + RH) - atan(RH - 1.676331) + 0.00391838*(( RH )^(3/2))* atan(0.023101*(RH) ) -4.686035 ; names(aux_bulbo)<- c("bulbo_temp") ;  return( aux_bulbo ) }
#fun_buldo_umido(matrix(data = c(20, 50), ncol = 2)) # Testando o valor. Bate com o do artigo

#aux_parquet$bulbo_seco<-  (aux_parquet$TMAX_max >= 35 ) * (aux_parquet$RH_min <= 30 )
#aux_parquet$bulbo_umido<- (aux_parquet$TMAX_max >= 35 ) * (aux_parquet$RH_max >= 50 )
#aux_parquet$bulbo_temp<-  fun_buldo_umido(aux_parquet[,c("TMAX_max", "RH_mean")] )[,1]

Tmax_municipal<- data.frame(CD_MUN = cd_muni)  ; Tmin_municipal<- data.frame(CD_MUN = cd_muni)  ; RH_municipal  <- data.frame(CD_MUN = cd_muni) 
bulbo_seco_municipal  <- data.frame(CD_MUN = cd_muni)  ; bulbo_umido_municipal <- data.frame(CD_MUN = cd_muni)  ; bulbo_temp_municipal  <- data.frame(CD_MUN = cd_muni) 

for (ano_parquet in 2010:2023) { 
  aux_parquet<- read_parquet(file =  paste0("~/Documentos/clima/BR-DWGD_",ano_parquet,".parquet"),  col_types = "string")
  aux_parquet$bulbo_seco<-  (aux_parquet$TMAX_max >= 35 ) * (aux_parquet$RH_min <= 30 )
  aux_parquet$bulbo_umido<- (aux_parquet$TMAX_max >= 35 ) * (aux_parquet$RH_max >= 50 )
  aux_parquet$bulbo_temp<-  fun_buldo_umido(aux_parquet[,c("TMAX_max", "RH_mean")] )[,1]
  
  aux_temp_max<-  t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("TMAX_max")]  ))  ; aux_temp_max_ano<- aux_temp_max
  aux_temp_min<-  t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("TMIN_max")]  ))  ; aux_temp_min_ano<- aux_temp_min
  aux_RH_mean<-   t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("RH_mean")]  ))   ; aux_RH_mean_ano<-  aux_RH_mean
  
  aux_bulbo_seco<-  t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("bulbo_seco")]  ))  ; aux_bulbo_seco_ano  <- aux_bulbo_seco
  aux_bulbo_umido<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("bulbo_umido")] ))  ; aux_bulbo_umido_ano <- aux_bulbo_umido
  aux_bulbo_temp<-  t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[1], c("TMIN_max")]    ))  ; aux_bulbo_temp_ano  <- aux_bulbo_temp
  
  for ( ii in 2:length(cd_muni) ) { cat(paste0("Ano: ",ano_parquet,", cidade:",ii," \n")) 
    
    aux_temp_max<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("TMAX_max")]  ))  ; 
    aux_temp_max_ano<- rbind(aux_temp_max_ano, aux_temp_max)
    
    aux_temp_min<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("TMIN_max")]  ))  ; 
    aux_temp_min_ano<- rbind(aux_temp_min_ano, aux_temp_min)
    
    aux_RH_mean<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("RH_mean")]  ))  ; 
    aux_RH_mean_ano<- rbind(aux_RH_mean_ano, aux_RH_mean)
    
    aux_bulbo_seco<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("bulbo_seco")]  ))  ; 
    aux_bulbo_seco_ano<- rbind(aux_bulbo_seco_ano, aux_bulbo_seco)
    
    aux_bulbo_umido<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("bulbo_umido")]  ))  ; 
    aux_bulbo_umido_ano<- rbind(aux_bulbo_umido_ano, aux_bulbo_umido)
    
    aux_bulbo_temp<- t(data.frame(aux_parquet[ aux_parquet$code_muni == cd_muni[ii], c("bulbo_temp")]  ))  ; 
    aux_bulbo_temp_ano<- rbind(aux_bulbo_temp_ano, aux_bulbo_temp)
    
  } ; Tmax_municipal<-cbind(Tmax_municipal,aux_temp_max_ano) ; Tmin_municipal<-cbind(Tmin_municipal,aux_temp_min_ano) ;  RH_municipal<-cbind(RH_municipal,aux_RH_mean_ano)   ; 
  bulbo_seco_municipal<-cbind(bulbo_seco_municipal,aux_bulbo_seco_ano)   ; bulbo_umido_municipal<-cbind(bulbo_umido_municipal,aux_bulbo_umido_ano)  ; bulbo_temp_municipal<-cbind(bulbo_temp_municipal,aux_bulbo_temp_ano)    }

# 00:47 minutos Rodando

####  Adicionando as datas e Salvando os Dados

#dim(Tmax_municipal) ; dim(Tmin_municipal) ; dim(RH_municipal) ; dim(bulbo_seco_municipal) ; dim(bulbo_umido_municipal) ; dim(bulbo_temp_municipal)
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
ab_cd_mun<- as.numeric( substr(x = as.character(Tmax_municipal$CD_MUN), start = 1, stop = 6) ) ; ab_cd_mun[ab_cd_mun %in% capitais]<- as.numeric( paste0( substr(ab_cd_mun[ab_cd_mun %in% capitais], 1, 2)  , "0000") ) 
#lll<- sample(1:nrow(Tmax_municipal), 10) ; ab_cd_mun[lll] ; Tmax_municipal$CD_MUN[lll]

Tmax_municipal$CD_MUN<- ab_cd_mun ; Tmin_municipal$CD_MUN<- ab_cd_mun ; RH_municipal$CD_MUN<- ab_cd_mun
bulbo_seco_municipal$CD_MUN<- ab_cd_mun ; bulbo_umido_municipal$CD_MUN<- ab_cd_mun ; bulbo_temp_municipal$CD_MUN<- ab_cd_mun

todas_datas<- as.Date(x = "2010-01-01", format = "%Y-%m-%d") +1:(365*14 +3) -1 ; head(todas_datas) ; tail(todas_datas) ; todas_datas<- as.character( todas_datas )
names(Tmax_municipal)<- c("CD_MUN", todas_datas) ; names(Tmin_municipal)<- c("CD_MUN", todas_datas) ; names(RH_municipal)<- c("CD_MUN", todas_datas) ; 
names(bulbo_seco_municipal)<- c("CD_MUN", todas_datas) ; names(bulbo_umido_municipal)<- c("CD_MUN", todas_datas) ; names(bulbo_temp_municipal)<- c("CD_MUN", todas_datas)

save.image(file = "~/Downloads/Tudo_Tmax_Tmin_maximas_RH_Bulbos_municipal(nomes).RData") ; 
save(list = c("Tmax_municipal", "Tmin_municipal", "RH_municipal", "bulbo_seco_municipal", "bulbo_umido_municipal", "bulbo_temp_municipal"), file = "~/Downloads/Tmax_Tmin_maximas_RH_Bulbos_municipal(nomes).RData")
#}


