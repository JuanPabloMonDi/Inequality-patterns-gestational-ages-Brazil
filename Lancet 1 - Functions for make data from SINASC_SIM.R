###################################################################################
##############         Program for aggregate and correct the         ##############
##############        SIM and SINASC data for subnotification  ####################
###################################################################################

######### 
################    Function for make analisys for each year 
#########
# Data Public (SINASC and SIM): https://opendatasus.saude.gov.br/
# Subnotification: https://ftp.ibge.gov.br/Estatisticas_Vitais/Estimativas_sub_registro_nascimentos/2018/xlsx/

### Library
rm(list = ls()) ;  library(dplyr) ; library(geobr)

###########################################################################################################################
###########################################################################################################################
#########  1 -- Make a function for create the data for SGA, for each year and reference table: Intergrowth or WHO
# Execute the function changing the path of the files


funcao_baixo_peso_cidade<- function(my_data, referencia_pig){
#Some csv files are separated by ";" and others by ","
if (my_data %in% c(2012,2013)){sep=","}else{sep=";"}
### Change the path of SINASC file in .csv
my_data<- paste0("data/SINASC_",my_data,".csv")  


### Reading RData with: SGA (percentil 10) from Intergrowth and WHO, and municipality information
cat(paste0("\n Loading data from SINASC: "))
load(file = "data/peso_WHO_Intergrowth_UF_Micro.RData") ; 

# Considering as a reference for weeks 41 and 42 the weights of week 40 for the WHO table
if ( sum(pesos_who$Gestational_Week_WHO == 42) == 0 ) { pesos_who<- rbind(pesos_who, 
      c(41, pesos_who$Female_10[nrow(pesos_who)] , pesos_who$Male_10[nrow(pesos_who)]), 
      c(42, pesos_who$Female_10[nrow(pesos_who)] , pesos_who$Male_10[nrow(pesos_who)]) )  }

# Considering as only above 24 weeks
if ( sum(pesos_who$Gestational_Week_WHO <24) > 0 ){ pesos_who<- pesos_who[pesos_who$Gestational_Week_WHO>=24,] }
# Therefore, the WHO and Intergrowth data are standardized for 24-42 gestational weeks.
  

### Reading data from SINASC
cat(paste0("\n Loading data from SINASC, with year: ", my_data))
SINASC<- read.csv(file = my_data, header = T, sep = sep)
#Some tables have column names in lower letters
colnames(SINASC)=toupper(colnames(SINASC))
ano_dado<- as.character( SINASC$DTNASC[ !is.na(SINASC$DTNASC) ][1] ) ;
ano_dado<-as.numeric( substr(x = ano_dado, start = nchar(ano_dado)-3, stop = nchar(ano_dado) ) )


###  Selecting the data used
colunas_utilizadas<- c("SEXO", "PESO", "SEMAGESTAC","CODMUNRES") 
dados_col<- select(SINASC, all_of(c(colunas_utilizadas)) ) ;

# In 2014, the Sex data is presented as letters. Standardize
if (ano_dado == 2014){ dados_col$SEXO<- as.numeric(dados_col$SEXO=="M") + 2*as.numeric(dados_col$SEXO=="F")  } ; dados_col<- dados_col[((dados_col$SEXO==1)|(dados_col$SEXO==2)),] 

# Removing NA from data
dados_col<- dados_col[(!(is.na(dados_col$PESO))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$SEXO))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$CODMUNRES))), ] 
dados_col<- dados_col[(!(is.na(dados_col$SEMAGESTAC))), ] 

# Filtering only for the interval between 24 and 42 gestational weeks
#(1-sum((dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42)) /nrow(dados_col))*100 # Checking
dados_col<- dados_col[ (dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42) , ] 


### SGA - Calculating the SGA for the Intergrowth or WHO Reference Table
cat(paste0("\n Calculating the SGA for: ")) # Choosing between the WHO and Intergrowth tables
if (tolower(referencia_pig) == "intergrowth") { pesos_mm<- pesos_intergrowth ; cat(paste0(" Intergrowth Table.")) }
if (tolower(referencia_pig) == "who") { pesos_mm<- pesos_who ; cat(paste0(" WHO Table.")) }

aux.semana<- dados_col$SEMAGESTAC # Use the first week of gestational week as pointer in the vector: it starts at 24, therefore -23
peso_10<- (pesos_mm$Male_10[(aux.semana - 23)])*(dados_col$SEXO == 1) + (pesos_mm$Female_10[(aux.semana - 23)])*(dados_col$SEXO == 2)
dados_pesos<- cbind(data.frame(PESO_10 = peso_10, BAIXO_PESO =  as.numeric(peso_10 >= dados_col$PESO)  ), dados_col )
#mean(dados_pesos$BAIXO_PESO) # Checking


### Adding IBGE information: DTB - Brazilian Territorial Division
aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                 data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )

# Adding capitals to aux_dtb as "0000" cities. SINASC shows more than 480,000 in this format
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)




#####################################   Analyzing Data at the City Level

# Data has capitals like "0000" which causes duplicate codes. Unifying all those ending in "0000" into the current codes
dados_pesos_cid<- dados_pesos ; for (pp in 1:nrow(aux_dtb_0000) ) { 
dados_pesos_cid$CODMUNRES[ dados_pesos_cid$CODMUNRES == aux_dtb_0000$codmun[pp] ]<- as.numeric( substr(x = aux_dtb_0000$Código.Município.Completo[pp], start = 1, stop = 6) ) }

# Removing poorly inserted cities.
#sum( (( dados_pesos$CODMUNRES <= 110000  )) & (( dados_pesos$CODMUNRES > 530010  )) ) # Checking
dados_pesos_cid<- dados_pesos_cid[ (!( dados_pesos_cid$CODMUNRES <= 110000  )) & (!( dados_pesos_cid$CODMUNRES > 530010  )),   ] 

# SGA for City
cat(paste0("\n Calculating SGA by City."))
cidade_nascido<- NULL ; cidade_baixo_peso<- NULL ;  cidade_todos<- as.numeric( levels( as.factor(dados_pesos_cid$CODMUNRES) ) );  
for (kk in 1:( length(cidade_todos) ) ) {   aux_cidade<-  dados_pesos_cid[( dados_pesos_cid$CODMUNRES == cidade_todos[kk]),  ]
  cidade_nascido[kk]<- nrow(aux_cidade) ; cidade_baixo_peso[kk]<- sum(aux_cidade$BAIXO_PESO) } ; baixo_peso_cidade<- cbind(code_cidade = as.numeric(cidade_todos) , data.frame( Nascidos = cidade_nascido, Baixo_Peso = cidade_baixo_peso, Porcentagem = round((cidade_baixo_peso/cidade_nascido)*100, 2)  )  )

# Aggregating information
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
mapa_join_cidade<- left_join(cidade_mapa, baixo_peso_cidade, by=c("code_muni"="code_cidade")) 

return(mapa_join_cidade)
}






###########################################################################################################################
###########################################################################################################################
#########  1 - Creating a Function to Generate Infant Mortality per Year
#########

funcao_mortalidade_cidade<- function(my_data, peso_UF_micro = "data/peso_UF_Micro.RData"){

### Change the path of SINASC file in .csv
my_data<- paste0("data/Mortalidade_Geral_",my_data,".csv")  


### Reading SIM data
load(file = "data/peso_WHO_Intergrowth_UF_Micro.RData")
#load(file = peso_UF_micro) ;
cat(paste0("\n Loading Data: ", my_data))
SIM<- read.csv(file = my_data, header = T, sep = ";")  

  
###  Filtering Deaths up to 1 year old
SIM_1ano<- filter(SIM,   IDADE < 401 & (!is.na(SIM$CODMUNRES)) )  ; SIM_1ano<- data.frame( CODMUNRES = SIM_1ano[, c( "CODMUNRES")   ], UF = as.numeric( substr(x =  as.character( SIM_1ano[, c( "CODMUNRES")   ] ), start = 1, stop = 2) ) )


### Adding capitals to aux_dtb as "0000" cities
aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                 data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )

capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)


###  Adding Microregion Codes
sim_micro<- left_join(SIM_1ano, aux_dtb[, c("Nome_UF","Nome_Microrregião", "Nome_Município", "code_micro", "codmun")], by=c("CODMUNRES"="codmun")) ; sim_micro$code_micro<- as.numeric( sim_micro$code_micro)


### Removing missing and typing errors of city's code
sim_micro<- sim_micro[ !is.na(sim_micro$code_micro), ]
sim_micro<- sim_micro[ (sim_micro$CODMUNRES>= 110000) & (sim_micro$CODMUNRES<= 530010)  , ]


#####################################   Analyzing Data at the City Level

### Data has capitals like "0000" which causes duplicate codes. Unifying all those ending in "0000" into the current codes
cat(paste0("\n Calculating Mortality by City."))
sim_micro_cid<- sim_micro ; for (pp in 1:nrow(aux_dtb_0000) ) { 
  sim_micro_cid$CODMUNRES[ sim_micro_cid$CODMUNRES == aux_dtb_0000$codmun[pp] ]<- as.numeric( substr(x = aux_dtb_0000$Código.Município.Completo[pp], start = 1, stop = 6) ) }
  
  
### Calculating Death by State
cidade_sim<- NULL ; cidade_todos<- as.numeric( levels( as.factor(sim_micro_cid$CODMUNRES) ) ) ; for( kk in 1:length(cidade_todos) ){
  cidade_sim[kk]<- sum( sim_micro_cid$CODMUNRES == cidade_todos[kk]) }

obito_cidade<- cbind(CODMUNRES = cidade_todos, data.frame( Obitos_1ano = cidade_sim ) )

# Adding map information
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
mapa_join_cidade<- left_join(cidade_mapa, obito_cidade, by=c("code_muni"="CODMUNRES")) ; 

return(mapa_join_cidade)
}






#########
####################    Generate SGA for Each Year
#########
resul_baixo_peso_cidade<- list() ; Sys.time() 

###  Calculates each year and saves it in a list 
# Choosing between the WHO and Intergrowth reference tables

#referencia_pig<- "who"
referencia_pig<- "intergrowth"

ano_dado<- 2022; resul_baixo_peso_cidade$ano_2022<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2021; resul_baixo_peso_cidade$ano_2021<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2020; resul_baixo_peso_cidade$ano_2020<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2019; resul_baixo_peso_cidade$ano_2019<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2018; resul_baixo_peso_cidade$ano_2018<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2017; resul_baixo_peso_cidade$ano_2017<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2016; resul_baixo_peso_cidade$ano_2016<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2015; resul_baixo_peso_cidade$ano_2015<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2014; resul_baixo_peso_cidade$ano_2014<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2013; resul_baixo_peso_cidade$ano_2013<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
ano_dado<- 2012; resul_baixo_peso_cidade$ano_2012<- funcao_baixo_peso_cidade(my_data = ano_dado, referencia_pig = referencia_pig )
Sys.time() ;




#########
######################    Generate Mortality for Each Year
#########

resul_mortalidade_cidade<- list() ; Sys.time() 

###  Calculates each year and saves it in a list

ano_dado<- 2022; resul_mortalidade_cidade$ano_2022<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2021; resul_mortalidade_cidade$ano_2021<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2020; resul_mortalidade_cidade$ano_2020<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2019; resul_mortalidade_cidade$ano_2019<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2018; resul_mortalidade_cidade$ano_2018<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2017; resul_mortalidade_cidade$ano_2017<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2016; resul_mortalidade_cidade$ano_2016<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2015; resul_mortalidade_cidade$ano_2015<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2014; resul_mortalidade_cidade$ano_2014<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2013; resul_mortalidade_cidade$ano_2013<- funcao_mortalidade_cidade(my_data = ano_dado )
ano_dado<- 2012; resul_mortalidade_cidade$ano_2012<- funcao_mortalidade_cidade(my_data = ano_dado )

rm(funcao_baixo_peso_cidade) ; rm(funcao_mortalidade_cidade)
Sys.time() ; save.image(file = paste0("resul_peso_",referencia_pig,"_mort_cidade.RData"))






###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################






######### 
################    As it takes a long time to run every year and takes up a lot of memory, saved in .RData
################                   and then I perform the other analyses
#########



###  Packages and set work dir (change for your path)
rm(list = ls()) ; 
#setwd(dir = "~/") ;
library(ggplot2) ; library(ggspatial) ; library(spdep) ; library(dplyr) ; library(tmap) ; library(biscale) ; library(cowplot) ; library(geobr) ; library(RColorBrewer) ; library(stringr) ; library(ggpubr) 


### Carregando Dados do Baixo Peso: Escolher entre WHO e Intergrowth

# Loading SGA RData save for WHO or Intergrowth table
load(file = "data/resul_peso_who_mort_cidade.RData")
# Loading SGA RData save for WHO or Intergrowth table
#load(file = "data/resul_peso_intergrowth_mort_cidade.RData")             

# Saving data in a list, to be able to aggregate state, micro and city
resul_baixo_peso<- list() ; resul_baixo_peso$cidade<-resul_baixo_peso_cidade 


### Loading Data of Mortality
load(file = "data/resul_mortalidade.RData") ; 

# NA in Mortality, where it should be zero. 2019 (41008); 2020 (41035 and 51015). Correcting
resul_mortalidade$micro$ano_2019$Obitos_1ano[is.na(resul_mortalidade$micro$ano_2019$Obitos_1ano)]<- 0 
resul_mortalidade$micro$ano_2020$Obitos_1ano[is.na(resul_mortalidade$micro$ano_2020$Obitos_1ano)]<- 0


###  Subnotifications must be made
corrigir<- TRUE ; if (corrigir == TRUE){ 
  cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
  
  
  ### Loading data; 2019 and 2020 have 7 digits =( ;
  for (aaa in 2015:2020) {  
    eval(parse(text = paste0('  sub_nasc_', aaa, '<- read.csv(file = "data/Subnotification/Sub_nascidos_',aaa,'.csv", header = T, sep = ";", dec = ",")   ')  ))  
    if (aaa>= 2019){ eval(parse(text = paste0('  sub_nasc_', aaa, '$CODMUNRES<- as.numeric( substr(x = as.character(sub_nasc_', aaa, '$CODMUNRES), start = 1, stop = 6) )  ') )) }
    eval(parse(text = paste0('  sub_nasc_', aaa, '<- left_join(cidade_mapa, sub_nasc_', aaa, '[,c("CODMUNRES","Sub_MS")], by=c("code_muni"="CODMUNRES"))  ')  ))  
    
    eval(parse(text = paste0('  sub_obit_', aaa, '<- read.csv(file = "data/Subnotification/Sub_obitos_', aaa, '.csv", header = T, sep = ";", dec = ",")  ')  ))  
    if (aaa>= 2019){ eval(parse(text = paste0('  sub_obit_', aaa, '$CODMUNRES<- as.numeric( substr(x = as.character(sub_obit_', aaa, '$CODMUNRES), start = 1, stop = 6) )  ') ))   } 
    eval(parse(text = paste0('  sub_obit_', aaa, '<- left_join(cidade_mapa, sub_obit_', aaa, '[,c("CODMUNRES","Sub_MS")], by=c("code_muni"="CODMUNRES"))   ')  ))  
  } #; sub_obit_2019$Sub_MS<- as.numeric( sub_obit_2019$Sub_MS ) ; rm(aaa) # Obits 2019 reading as character =( ; Figueirao-MS 500390 with dash
  
  # Checking for missing values. Complete match
  tt_na<-NULL ; for (bbb in 1:length(2015:2020) ) { tt_na[bbb]<-  eval(parse(text = paste0(' sum(is.na(sub_nasc_', 2014+bbb, '$Sub_MS)) ') ))
  tt_na[bbb+6]<-  eval(parse(text = paste0(' sum(is.na(sub_obit_', 2014+bbb, '$Sub_MS)) ') )) }  ; tt_na ; rm(bbb)
  
  # Assuming the years that are not in the data
  sub_nasc_2012<- sub_nasc_2013<- sub_nasc_2014<- sub_nasc_2015  ;  sub_obit_2012<-sub_obit_2013<- sub_obit_2014<- sub_obit_2015 ; sub_nasc_2021<- sub_nasc_2022<- sub_nasc_2020 ; sub_obit_2021<- sub_obit_2022<- sub_obit_2020
  
  
  ### Correcting and Standardizing City Information for each year
  for (ccc in 2012:2022) {
    eval(parse(text = paste0(' resul_baixo_peso$cidade$ano_', ccc, '$Nascidos<- round( (resul_baixo_peso$cidade$ano_', ccc, '$Nascidos) * (1 + ((sub_nasc_', ccc, '$Sub_MS)/100) ) ) ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$cidade$ano_', ccc, '$Baixo_Peso<- round( (resul_baixo_peso$cidade$ano_', ccc, '$Baixo_Peso) * (1 + ((sub_nasc_', ccc, '$Sub_MS)/100) )   ) ')  ))
    eval(parse(text = paste0(' resul_mortalidade$cidade$ano_', ccc, '$Obitos_1ano<- round( (resul_mortalidade$cidade$ano_', ccc, '$Obitos_1ano) * (1 + ((sub_obit_', ccc, '$Sub_MS)/100) ) )   ')  )) }
  
  # Organizing microregions (numbering and names)
  microrregiao_mapa <- read_micro_region(code_micro="all", year=2020, showProgress = T) 
  microrregiao_dtb<- read.csv(file = "Data/Subnotification/DTB_2022.csv", header = T, sep = ";", colClasses = c("character") )
  aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                   data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = as.numeric( paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )  )

  # Adding capitals to aux_dtb as "0000" cities
  capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
               Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
  aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)
  
  aux_correcao<-  left_join(resul_baixo_peso$cidade$ano_2022, aux_dtb[, c("Nome_UF","Nome_Microrregião", "Nome_Município", "code_micro", "codmun")], by=c("code_muni"="codmun"))
  aux_micro1<- microrregiao_mapa$code_micro   ; 
  
  # Organizing the States
  estado_mapa <- read_state(code_state="all", year=2020, showProgress = T) ; estado_mapa$code_state<- as.numeric(estado_mapa$code_state) 
  aux_estado1<- estado_mapa$code_state
  
  # Adding information from microregions and states, for each year, to the data
  for (ccc in 2012:2022) {
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_', ccc, '<- microrregiao_mapa ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_', ccc, '<- estado_mapa ')  ))  }  
  
  
  ### Adding cities to correct microregions
  sum(resul_baixo_peso$cidade$ano_2022$code_muni != aux_correcao$code_muni) # Conferindo a ordem das cidades e micro (sempre seguindo a ordem do "read_micro")
  for (eee in 2012:2022) { for(ddd in 1:length(aux_micro1) ){
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Nascidos[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Nascidos[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Baixo_Peso[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Baixo_Peso[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_mortalidade$micro$ano_' ,eee, '$Obitos_1ano[ddd]<-  sum(resul_mortalidade$cidade$ano_' ,eee, '$Obitos_1ano[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
  } ; eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Porcentagem<- (resul_baixo_peso$micro$ano_' ,eee, '$Baixo_Peso)/(resul_baixo_peso$micro$ano_' ,eee, '$Nascidos)*100  ')  ))  }
  
  # Checking the sum by microregions
  #set.seed(1234); conferindo_micro<- sample(aux_correcao$code_micro, 1)
  #st_drop_geometry( resul_baixo_peso$micro$ano_2022[ resul_baixo_peso$micro$ano_2022$code_micro == conferindo_micro, c("Nascidos","Baixo_Peso")])
  #colSums( st_drop_geometry( resul_baixo_peso$cidade$ano_2022[aux_correcao$code_micro == conferindo_micro,c("Nascidos","Baixo_Peso")] ) )
  
  
  ### SUM cities to states
  sum(resul_baixo_peso$cidade$ano_2022$code_muni != aux_correcao$code_muni) # Conferindo a ordem das cidades e micro (sempre seguindo a ordem do "read_micro")
  for (eee in 2012:2022) { for(ddd in 1:length(aux_estado1) ){
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Nascidos[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Nascidos[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Baixo_Peso[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Baixo_Peso[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_mortalidade$estado$ano_' ,eee, '$Obitos_1ano[ddd]<-  sum(resul_mortalidade$cidade$ano_' ,eee, '$Obitos_1ano[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
  } ; eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Porcentagem<- (resul_baixo_peso$estado$ano_' ,eee, '$Baixo_Peso)/(resul_baixo_peso$estado$ano_' ,eee, '$Nascidos)*100  ')  ))  }
  
  # Checking the sums by state
  #dados_agregado_estado<- aggregate(cbind(Nascidos, Baixo_Peso ) ~ code_state , data = resul_baixo_peso$cidade$ano_2021, FUN = sum, na.rm = TRUE)
  #sum(dados_agregado_estado$Nascidos != resul_baixo_peso$estado$ano_2021$Nascidos)
  #sum(dados_agregado_estado$Baixo_Peso != resul_baixo_peso$estado$ano_2021$Baixo_Peso)
  
}




### Elements in the same order. I used read_micro_region (geobr) as a base and added the information via leftjoin
### Checking once again that everyone is in the same way, following the order of read_micro_region =D

aux_ordem<- NULL ; for (bb in 1:11) { 
  aux_ordem[bb]<-       sum( resul_baixo_peso$micro$ano_2022$code_micro == resul_baixo_peso$micro[[bb]]$code_micro ) 
  aux_ordem[bb +11 ]<-  sum( resul_baixo_peso$micro$ano_2022$code_micro == resul_mortalidade$micro[[bb]]$code_micro ) } ; aux_ordem

aux_ordem<- NULL ; for (bb in 1:11) { 
  aux_ordem[bb]<-       sum( resul_baixo_peso$cidade$ano_2022$code_muni == resul_baixo_peso$cidade[[bb]]$code_muni ) 
  aux_ordem[bb +11 ]<-  sum( resul_baixo_peso$cidade$ano_2022$code_muni == resul_mortalidade$cidade[[bb]]$code_muni ) } ; aux_ordem


####   Infant Mortality Rate - Professor Everton's Suggestion
# Numerator = (  Death 2020 < 1year + Death 2021 < 1year + Death 2022 < 1year) / 3
# Denominator = ( Born 2020 * Born 2022)^(1/2)

# 3-Year Window - Order going from 2022 - 2014 (descending)
aux_mortalidade<- data.frame(NULL) ; for (mm in 1:9) {  
  aux_mortalidade[1:558 ,mm]<- ((resul_mortalidade$micro[[mm]]$Obitos_1ano + resul_mortalidade$micro[[mm +1]]$Obitos_1ano +resul_mortalidade$micro[[mm +2]]$Obitos_1ano 
  )/3)/(  ((resul_baixo_peso$micro[[mm]]$Nascidos)^(1/2)) * ((resul_baixo_peso$micro[[mm+2]]$Nascidos)^(1/2))   )*1000  } ; names(aux_mortalidade)<- paste0("Ano_",2022:2014) ; aux_mortalidade<- aux_mortalidade[,9:1]

# The order of cbind influences. Changing the order stops being "sf"
#View(resul_baixo_peso$micro$ano_2022[,c(7,8,9)]) # Checking
tx_mortalidade<- cbind(resul_baixo_peso$micro$ano_2022[,c(-7,-8,-9)], aux_mortalidade )


####   Rate of SGA
# Numerator = (  SGA 2020 + SGA 2021 + SGA 2022 ) / 3
# Denomeinator = ( Born 2020 + Born 2021 + Born 2022) / 3

aux_baixo_peso_br<- data.frame(NULL) ;  for( mm in 1:9){  # 3-Year Window - Order going from 2022 - 2014 (descending)
  aux_baixo_peso_br[1:558, mm]<-  ( resul_baixo_peso$micro[[mm]]$Baixo_Peso + resul_baixo_peso$micro[[mm +1]]$Baixo_Peso + resul_baixo_peso$micro[[mm +2]]$Baixo_Peso
  )/(   resul_baixo_peso$micro[[mm]]$Nascidos + resul_baixo_peso$micro[[mm +1]]$Nascidos + resul_baixo_peso$micro[[mm +2]]$Nascidos   )*100   } ; names(aux_baixo_peso_br)<- paste0("Ano_",2022:2014) ;  aux_baixo_peso_br<- aux_baixo_peso_br[, 9:1]

# The order of cbind influences. Changing the order stops being "sf"
#View(resul_baixo_peso$micro$ano_2022[,c(7,8,9)])
baixo_peso_br<- cbind(resul_baixo_peso$micro$ano_2022[,c(-7,-8,-9)], aux_baixo_peso_br)


save(list = c("referencia_pig", "resul_baixo_peso", "resul_mortalidade" , "baixo_peso_br", "tx_mortalidade", "estado_mapa"), file = paste0("data/Baixo_Peso_Mortalidade_",referencia_pig,".RData"))
save(list = c("referencia_pig", "baixo_peso_br", "tx_mortalidade", "estado_mapa"), file = paste0("data/Taxa_Baixo_Peso_Mortalidade_",referencia_pig,".RData"))


