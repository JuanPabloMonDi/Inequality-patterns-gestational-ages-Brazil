#############################################################################
##############         Programa - Teste Climaterna             ##############
#############################################################################



###############################################################################################
######### 
################       PARTE II : PREMATURIDADE
######### 
###############################################################################################

#########################################
###        Organizando os Dados
#########################################

###  Pacotes e Diretorio Raiz
rm(list = ls()) ;  library(arrow); library(dplyr) ; library(geobr) ; library(stringr) ; library(roll) ; library(Kendall) ; library(trend) ; library(ggplot2) ; library(RColorBrewer) ; library(tidyr); library(spdep) ;library(sf) ; library(sfdep); library(ggpubr);  setwd("~/Dropbox/Unicamp - Cristiano/Climaterna/")

# Escolhendo o Ano de Analise do SINASC
gc(); my_data<- 2021 ; 

# Dados do SINASC e informacoes das cidades, microrregioes e estados
SINASC<- read.csv(file = paste0("~/Documentos/opendatasus/SINASC_",my_data,".csv"), header = T, sep = ";") # Dados do SINASC
microrregiao_dtb<- read.csv(file = "Programas/DTB_2022.csv", header = T, sep = ";", colClasses = c("character") )
estado_mapa<- read_state(code_state = "all", year = 2020, showProgress = T)
microrregiao_mapa <- read_micro_region(code_micro="all", year=2020, showProgress = T) ; microrregiao_mapa<- microrregiao_mapa[ !(microrregiao_mapa$name_micro == "Fernando De Noronha"), ]
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = F) 

###  Baixo Peso pelo Intergrowth
load(file = "~/Dropbox/Unicamp - Cristiano/SINASC/PIG - WHO Intergrow/peso_WHO_Intergrowth_UF_Micro.RData") ; 


### Algumas Colunas - Semana Gestacional e Data_Nascimento
#colunas_utilizadas<- c("SEXO","PESO","CODMUNRES", "SEMAGESTAC", "DTNASC") 
colunas_utilizadas<- c("SEXO", "PESO", "SEMAGESTAC","CODMUNRES", "DTNASC", "PARTO", "IDADEMAE", "ESCMAE", "ESCMAE2010", "CONSULTAS", "CONSPRENAT", "RACACORMAE") 
dados_col<- select(SINASC, c(colunas_utilizadas) ) ; rm(SINASC) # removendo o sinasc, liberar memoria


###  Formatando dados
if (my_data == 2014){ dados_col$SEXO<- as.numeric(dados_col$SEXO=="M") + 2*as.numeric(dados_col$SEXO=="F")  }
dados_col<- dados_col[(!(is.na(dados_col$PESO))), ]  ; 
dados_col<- dados_col[(!(is.na(dados_col$SEXO))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$CODMUNRES))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$SEMAGESTAC))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$DTNASC))), ]  # sum((is.na(dados_col$DTNASC))) Zero em 2023
dados_col<- dados_col[ (!( dados_col$CODMUNRES <= 110000  )) & (!( dados_col$CODMUNRES > 530010  )),   ] # CODMUNRES Mal inseridos - 3 em 2023 # sum(( ( dados_col$CODMUNRES <= 110000  )) | ( ( dados_col$CODMUNRES > 530010  )))



# Como sugerido, filtrando somente para o intervalo entre 24 e 42 semanas gestacionais
#(1-sum((dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42)) /nrow(dados_col))*100 # Removendo 0.85
dados_col<- dados_col[ (dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42) , ] 

#  Passando datas para o formato no R e Obtendo o dia/periodo da concepcao
aux_data<- paste0( c(str_sub(string = as.character( dados_col$DTNASC), start = 1, end = -7)), "-", 
                   c(str_sub(string = as.character( dados_col$DTNASC), start = -6, end = -5)), "-",
                   c(str_sub(string = as.character( dados_col$DTNASC), start = -4, end = -1))  ) ; aux_data<-  as.Date(x = aux_data, format = "%d-%m-%Y")
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
cat(paste0(" Tabela Intergrowth."))
pesos_mm<- pesos_intergrowth
aux.semana<- dados_col$SEMAGESTAC ; aux.semana[aux.semana>40]<- 40 ; aux.semana[aux.semana < 14]<- 14 
peso_10<- (pesos_mm$Male_10[(aux.semana - 23)])*(dados_col$SEXO == 1) + (pesos_mm$Female_10[(aux.semana - 23)])*(dados_col$SEXO == 2)
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
# Carregando os dados; 2019 e 2020 estao com 7 digitos =(  ; 
if ( (my_data < 2015) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',2015,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }
if ( (my_data > 2022) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',2022,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }
if ( (my_data >= 2015) & (my_data <= 2022) ) { eval(parse(text = paste0('  sub_nasc<- read.csv(file = "~/Dropbox/Unicamp - Cristiano/SINASC/Subnotitificacao/Sub_nascidos_',my_data,'.csv", header = T, sep = ";", dec = ",")   ')  ))  }

sub_nasc$CODMUNRES<- as.numeric( substr(x = as.character(sub_nasc$CODMUNRES), start = 1, stop = 6) )  
sub_nasc$CODMUNRES[sub_nasc$CODMUNRES %in% capitais]<- as.numeric( paste0( substr(sub_nasc$CODMUNRES[sub_nasc$CODMUNRES %in% capitais], 1, 2)  , "0000") ) 

# Adicionando os dados de Subnotificacao aos dados do SINASC
dados_col<- left_join(dados_col, sub_nasc[,c("CODMUNRES","Sub_MS")], by=c("CODMUNRES"="CODMUNRES")) ; dados_col$Sub_MS<-  dados_col$Sub_MS/100 
sum(is.na(dados_col$Sub_MS  )) # Confirmando se tem NA

# Corrigindo os Nascimentos pelas Subnotificacoes
dados_col$NASCIMENTOS<- 1 ; dados_col$NASCIMENTOS<- dados_col$NASCIMENTOS + (dados_col$Sub_MS)


### Agregando as informacoes por Microrregiao e Mes
dados_agregado_micro<- aggregate(cbind(NASCIMENTOS, A_TERMO, PREMATURO, TARDIO_PREMATURO, MODERADO_PREMATURO, MUITO_PREMATURO,  
                                       EXTREMO_PREMATURO, BAIXO_PESO ) ~MES_R + code_micro , data = dados_col, FUN = sum, na.rm = TRUE)

# Retirando Fernando de Noronha e Uniformizando os codifos: 26019 # Fernando de Noronha
aux_micro_dados<- as.numeric( levels( as.factor( dados_agregado_micro$code_micro))) ; aux_micro_dados<- aux_micro_dados[aux_micro_dados !=  26019]

# Filtrando por Microrregiao e organizando os meses de 1 ate 12
nascimentos<- data.frame(code_micro = aux_micro_dados) ; extremo_prem<- data.frame(code_micro = aux_micro_dados) ; muito_prem<- data.frame(code_micro = aux_micro_dados) ; moderado_prem<- data.frame(code_micro = aux_micro_dados) ; tardios_prem<- data.frame(code_micro = aux_micro_dados) ; prematuros<- data.frame(code_micro = aux_micro_dados) ; a_termos<- data.frame(code_micro = aux_micro_dados) ; 
baixo_peso<- data.frame(code_micro = aux_micro_dados) ; mortos<- data.frame(code_micro = aux_micro_dados) ;   for (dfe in 1:length(aux_micro_dados) ) {
  aux_agregados<- left_join(data_frame(MES_R = 1:12) , dados_agregado_micro[dados_agregado_micro$code_micro == aux_micro_dados[dfe],] , by=c("MES_R"="MES_R") ) 
  
  nascimentos[dfe,2:13]<-        t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("NASCIMENTOS")])  
  extremo_prem[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("EXTREMO_PREMATURO")])
  muito_prem[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("MUITO_PREMATURO")])
  moderado_prem[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("MODERADO_PREMATURO")])
  tardios_prem[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("TARDIO_PREMATURO")])
  prematuros[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("PREMATURO")])
  a_termos[dfe,2:13]<- t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("A_TERMO")])
  baixo_peso[dfe,2:13]<-         t(aux_agregados[aux_agregados$code_micro == aux_micro_dados[dfe],c("BAIXO_PESO")])
  
  # No caso de Obitos Fetais, pode ocorrer que nao houve obitos numa determinada microrregiao
  #if (nrow(obitos_fetais[obitos_fetais$code_micro == aux_micro_dados[dfe], ]) != 0) {
  #  mortos[dfe,1:13]<- obitos_fetais[obitos_fetais$code_micro == aux_micro_dados[dfe], ] }else{ mortos[dfe,2:13]<- 0 }
} ; 
nascimentos[ is.na(nascimentos)]<-0 ; extremo_prem[ is.na(extremo_prem)]<-0 ; muito_prem[ is.na(muito_prem)]<-0 ; moderado_prem[ is.na(moderado_prem)]<-0 ; tardios_prem[ is.na(tardios_prem)]<-0 ; prematuros[ is.na(prematuros)]<-0  ; a_termos[ is.na(a_termos)]<-0 ; baixo_peso[ is.na(baixo_peso)]<-0  ; mortos[ is.na(mortos)]<-0
names(nascimentos)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ; names(extremo_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ; names(moderado_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ;  names(tardios_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ; names(prematuros)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ; names(a_termos)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ;  names(baixo_peso)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12)) ; #names(mortos)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))


###  Calculando os Indices, dividindo pelos nascimentos corrigidos pelas subnotificacoes
indice_extremo_prem<-  extremo_prem/(nascimentos )*100  ; indice_extremo_prem$code_micro<-  nascimentos$code_micro ; names(indice_extremo_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_muito_prem<-    muito_prem/(nascimentos )*100    ; indice_muito_prem$code_micro<-    nascimentos$code_micro ; names(indice_muito_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_moderado_prem<- moderado_prem/(nascimentos )*100 ; indice_moderado_prem$code_micro<- nascimentos$code_micro ; names(indice_moderado_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_tardios_prem<-  tardios_prem/(nascimentos )*100  ; indice_tardios_prem$code_micro<- nascimentos$code_micro ; names(indice_tardios_prem)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_prematuros<-    prematuros/(nascimentos )*100    ; indice_prematuros$code_micro<- nascimentos$code_micro ; names(indice_prematuros)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_a_termos<-      a_termos/(nascimentos )*100      ; indice_a_termos$code_micro<- nascimentos$code_micro ; names(indice_a_termos)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))
indice_baixo_peso<-    baixo_peso/(nascimentos )*100    ; indice_baixo_peso$code_micro<- nascimentos$code_micro ;  names(indice_baixo_peso)<- c("code_micro", paste0("mes_0",1:9), paste0("mes_",10:12))


###  Salvando Baixo Peso
#write.table( indice_baixo_peso, file = paste0("~/Downloads/Baixo Peso/Indice_PIG_mes_micro_",my_data,".csv"),  dec = ".", sep = ";", row.names = F)  
#