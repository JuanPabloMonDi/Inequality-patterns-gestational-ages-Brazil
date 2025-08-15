#########################################################################
##############         Programa - SIM e SINASC             ##############
#########################################################################


######### 
################    Funcoes para o Mapa do Baixo Peso no Brasil  --  14/08/2023  
#########
# https://opendatasus.saude.gov.br/
# https://ftp.ibge.gov.br/Estatisticas_Vitais/Estimativas_sub_registro_nascimentos/2018/xlsx/

rm(list = ls()) ;  library(dplyr) ; library(geobr)

###########################################################################################################################
###########################################################################################################################
#########  Criando Funcao para Gerar o Baixo Peso por ano
# Para rodar a funcao completa, execute essa parte
# my_data<- 2022

funcao_baixo_peso_cidade<- function(my_data, referencia_pig){

### Pasta Aonde esta os arquivos do SINASC
my_data<- paste0("~/Documentos/opendatasus/SINASC_",my_data,".csv")  


### Lendo dados dos Pesos por semana Gestacional (WHO e Intergrowth), UF e codigos do IBGE, Municipios por microrregiao  e SINASC
cat(paste0("\n Carregando os dados PIG: "))
load(file = "~/Dropbox/Unicamp - Cristiano/SINASC/PIG - WHO Intergrow/peso_WHO_Intergrowth_UF_Micro.RData") ; 

# Como Sugerido, considerando como referencia para as semanas 41 e 42 os pesos da semana 40 para a tabela da WHO
if ( sum(pesos_who$Gestational_Week_WHO == 42) == 0 ) { pesos_who<- rbind(pesos_who, 
      c(41, pesos_who$Female_10[nrow(pesos_who)] , pesos_who$Male_10[nrow(pesos_who)]), 
      c(42, pesos_who$Female_10[nrow(pesos_who)] , pesos_who$Male_10[nrow(pesos_who)]) )  }

# Como Sugerido, considerando como somente acima de 24 semanas
if ( sum(pesos_who$Gestational_Week_WHO <24) > 0 ){ pesos_who<- pesos_who[pesos_who$Gestational_Week_WHO>=24,] }
# Com isso os dados da WHO e da Intergrowth estao padronizados para 24-42 semanas gestacionais
  

### Lendo dados do SINASC
cat(paste0("\n Carregando os dados: ", my_data))
SINASC<- read.csv(file = my_data, header = T, sep = ";")
ano_dado<- as.character( SINASC$DTNASC[ !is.na(SINASC$DTNASC) ][1] ) ; ano_dado<-as.numeric( substr(x = ano_dado, start = nchar(ano_dado)-3, stop = nchar(ano_dado) ) )


###  Selecionando os dados utilizados
colunas_utilizadas<- c("SEXO", "PESO", "SEMAGESTAC","CODMUNRES") 
dados_col<- select(SINASC, c(colunas_utilizadas) ) ;

# No ano de 2014, os dados do Sexo estao como letras. Padronizar
if (ano_dado == 2014){ dados_col$SEXO<- as.numeric(dados_col$SEXO=="M") + 2*as.numeric(dados_col$SEXO=="F")  } ; dados_col<- dados_col[((dados_col$SEXO==1)|(dados_col$SEXO==2)),] 

# Removendo os NA dos dados
dados_col<- dados_col[(!(is.na(dados_col$PESO))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$SEXO))), ] ; 
dados_col<- dados_col[(!(is.na(dados_col$CODMUNRES))), ] 
dados_col<- dados_col[(!(is.na(dados_col$SEMAGESTAC))), ] 

# Como sugerido, filtrando somente para o intervalo entre 24 e 42 semanas gestacionais
#(1-sum((dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42)) /nrow(dados_col))*100 # Removendo 0.85
dados_col<- dados_col[ (dados_col$SEMAGESTAC >= 24) & (dados_col$SEMAGESTAC <= 42) , ] 


### Baixo Peso - Adicionando a comparacao com o peso (10% menores) nos dados - Dados da  "WHO"
cat(paste0("\n Calculando o Baixo Peso:  ")) # Escolhendo entre as tabelas da WHO e da Intergrowth
if (tolower(referencia_pig) == "intergrowth") { pesos_mm<- pesos_intergrowth ; cat(paste0(" Tabela Intergrowth.")) }
if (tolower(referencia_pig) == "who") { pesos_mm<- pesos_who ; cat(paste0(" Tabela WHO.")) }

aux.semana<- dados_col$SEMAGESTAC # Colocar como ponteiro da tabela a primeira semana: comeca em 24, logo -23
peso_10<- (pesos_mm$Male_10[(aux.semana - 23)])*(dados_col$SEXO == 1) + (pesos_mm$Female_10[(aux.semana - 23)])*(dados_col$SEXO == 2)
dados_pesos<- cbind(data.frame(PESO_10 = peso_10, BAIXO_PESO =  as.numeric(peso_10 >= dados_col$PESO)  ), dados_col )
#mean(dados_pesos$BAIXO_PESO)


### Adicionando as informacoes do IBGE: DTB - Divisao Territorial Brasileira
aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                 data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )

# Adicionando as capitais no aux_dtb  como cidades "0000". No SINASC aparece mais de 480000 nesse formato
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)




#####################################   Analisando os Dados no Nivel das Cidades

# Dados tem capitais como "0000" o que causa duplicidade nos codigos. Unificando todos os com final "0000" nos codigos atuais
dados_pesos_cid<- dados_pesos ; for (pp in 1:nrow(aux_dtb_0000) ) { 
dados_pesos_cid$CODMUNRES[ dados_pesos_cid$CODMUNRES == aux_dtb_0000$codmun[pp] ]<- as.numeric( substr(x = aux_dtb_0000$Código.Município.Completo[pp], start = 1, stop = 6) ) }

# Removendo cidades mal inseridas. sum( (( dados_pesos$CODMUNRES <= 110000  )) & (( dados_pesos$CODMUNRES > 530010  )) )
dados_pesos_cid<- dados_pesos_cid[ (!( dados_pesos_cid$CODMUNRES <= 110000  )) & (!( dados_pesos_cid$CODMUNRES > 530010  )),   ] 

# Baixo Peso por Cidade
cat(paste0("\n Calculando o Baixo Peso por Cidade."))
cidade_nascido<- NULL ; cidade_baixo_peso<- NULL ;  cidade_todos<- as.numeric( levels( as.factor(dados_pesos_cid$CODMUNRES) ) );  
for (kk in 1:( length(cidade_todos) ) ) {   aux_cidade<-  dados_pesos_cid[( dados_pesos_cid$CODMUNRES == cidade_todos[kk]),  ]
  cidade_nascido[kk]<- nrow(aux_cidade) ; cidade_baixo_peso[kk]<- sum(aux_cidade$BAIXO_PESO) } ; baixo_peso_cidade<- cbind(code_cidade = as.numeric(cidade_todos) , data.frame( Nascidos = cidade_nascido, Baixo_Peso = cidade_baixo_peso, Porcentagem = round((cidade_baixo_peso/cidade_nascido)*100, 2)  )  )

# Gerando mapa e agregando a informacao
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
mapa_join_cidade<- left_join(cidade_mapa, baixo_peso_cidade, by=c("code_muni"="code_cidade")) 

return(mapa_join_cidade)
}






###########################################################################################################################
###########################################################################################################################
#########  Criando Funcao para Gerar a Mortalidade Infantil por Ano

funcao_mortalidade_cidade<- function(my_data, peso_UF_micro = "~/Dropbox/Unicamp - Cristiano/SINASC/peso_UF_Micro.RData"){

### Evitando erros iniciais
my_data<- paste0("~/Documentos/opendatasus/Mortalidade_Geral_",my_data,".csv")  # Facilitar, depois eu tiro


### Lendo dados dos SIM
load(file = peso_UF_micro) ; cat(paste0("\n Carregando os dados: ", my_data))
SIM<- read.csv(file = my_data, header = T, sep = ";")  

  
###  Filtrando Obitos com ate 1 ano
SIM_1ano<- filter(SIM,   IDADE < 401 & (!is.na(SIM$CODMUNRES)) )  ; SIM_1ano<- data.frame( CODMUNRES = SIM_1ano[, c( "CODMUNRES")   ], UF = as.numeric( substr(x =  as.character( SIM_1ano[, c( "CODMUNRES")   ] ), start = 1, stop = 2) ) )


### Adicionando as informacoes do IBGE: DTB - Divisao Territorial Brasileira
aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                 data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )

# Adicionando as capitais no aux_dtb  como cidades "0000". No SINASC aparece mais de 480000 nesse formato
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
             Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)


###  Adicionando os Codigos das Microrregioes
sim_micro<- left_join(SIM_1ano, aux_dtb[, c("Nome_UF","Nome_Microrregião", "Nome_Município", "code_micro", "codmun")], by=c("CODMUNRES"="codmun")) ; sim_micro$code_micro<- as.numeric( sim_micro$code_micro)


### Removendo os dados faltantes e com problemas
sim_micro<- sim_micro[ !is.na(sim_micro$code_micro), ]
sim_micro<- sim_micro[ (sim_micro$CODMUNRES>= 110000) & (sim_micro$CODMUNRES<= 530010)  , ]


#####################################   Analisando os Dados no Nivel das Cidades

### Dados tem capitais como "0000" o que causa duplicidade nos codigos. Unificando todos os com final "0000" nos codigos atuais
cat(paste0("\n Calculando Mortalidade por Cidade."))
sim_micro_cid<- sim_micro ; for (pp in 1:nrow(aux_dtb_0000) ) { 
  sim_micro_cid$CODMUNRES[ sim_micro_cid$CODMUNRES == aux_dtb_0000$codmun[pp] ]<- as.numeric( substr(x = aux_dtb_0000$Código.Município.Completo[pp], start = 1, stop = 6) ) }
  
  
### Calculando o Obito por Estado
cidade_sim<- NULL ; cidade_todos<- as.numeric( levels( as.factor(sim_micro_cid$CODMUNRES) ) ) ; for( kk in 1:length(cidade_todos) ){
  cidade_sim[kk]<- sum( sim_micro_cid$CODMUNRES == cidade_todos[kk]) }

obito_cidade<- cbind(CODMUNRES = cidade_todos, data.frame( Obitos_1ano = cidade_sim ) )

# Adicionando informacoes do mapa
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
mapa_join_cidade<- left_join(cidade_mapa, obito_cidade, by=c("code_muni"="CODMUNRES")) ; 

return(mapa_join_cidade)
}






#########
####################    Baixo Peso (Lista para usar depois  --   01 hora )
#########
resul_baixo_peso_cidade<- list() ; Sys.time() 

###  Calcula cada ano e salva em uma lista
# Escolhendo entre as tabelas de referencia da WHO e da Intergrowth

referencia_pig<- "who"
#referencia_pig<- "intergrowth"

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
######################    Mortalidade (Lista para usar depois  --  15 min Mortalidade )
#########

resul_mortalidade_cidade<- list() ; Sys.time() 

###  Calcula cada ano e salva em uma lista

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
Sys.time() ; save.image(file = paste0("~/Downloads/resul_peso_",referencia_pig,"_mort_cidade.RData"))






###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################






######### 
################    Como Demora Para Rodar Todos os Anos e Ocupa Muita Memoria, Salvo num .RData
################                   e Em Seguida Realizo as Outras Analises
#########



###  Lendo os Dados Salvos
rm(list = ls()) ; setwd(dir = "~/Dropbox/Unicamp - Cristiano/SINASC/") ; library(ggplot2) ; library(ggspatial) ; library(spdep) ; library(dplyr) ; library(tmap) ; library(biscale) ; library(cowplot) ; library(geobr) ; library(RColorBrewer) ; library(stringr) ; library(ggpubr) 


### Carregando Dados do Baixo Peso: Escolher entre WHO e Intergrowth

# Dados do Baixo Peso da WHO
load(file = "~/Documentos/opendatasus - PIG WHO Intergrowth/resul_peso_who_mort_cidade.RData")             

# Dados do Baixo Peso da Intergrowth
#load(file = "~/Documentos/opendatasus - PIG WHO Intergrowth/resul_peso_intergrowth_mort_cidade.RData")             

# Salvando dados numa lista, para poder agregar estado, micro e cidade
resul_baixo_peso<- list() ; resul_baixo_peso$cidade<-resul_baixo_peso_cidade 


### Carregando Dados da Mortalidade
load(file = "~/Documentos/opendatasus/resul_mortalidade.RData") ; 
resul_mortalidade$micro$ano_2019$Obitos_1ano[is.na(resul_mortalidade$micro$ano_2019$Obitos_1ano)]<- 0 # NA em Mortalidade, deveria ser zero. 2019 (41008) ; 2020 (41035 e 51015). Corrigindo
resul_mortalidade$micro$ano_2020$Obitos_1ano[is.na(resul_mortalidade$micro$ano_2020$Obitos_1ano)]<- 0


###  Subnotificacoes - Coloquei em um If para rodar junto a parte do codigo relativo as correcoes por subnotificacoes 
corrigir<- TRUE ; if (corrigir == TRUE){ 
  cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = T) ; cidade_mapa$code_muni<- as.numeric( substr(x = as.character(cidade_mapa$code_muni), start = 1, stop = 6) )
  
  
  ### Carregando os dados; 2019 e 2020 estao com 7 digitos =(  ; 
  for (aaa in 2015:2020) {  
    eval(parse(text = paste0('  sub_nasc_', aaa, '<- read.csv(file = "Subnotitificacao/Sub_nascidos_',aaa,'.csv", header = T, sep = ";", dec = ",")   ')  ))  
    if (aaa>= 2019){ eval(parse(text = paste0('  sub_nasc_', aaa, '$CODMUNRES<- as.numeric( substr(x = as.character(sub_nasc_', aaa, '$CODMUNRES), start = 1, stop = 6) )  ') )) }
    eval(parse(text = paste0('  sub_nasc_', aaa, '<- left_join(cidade_mapa, sub_nasc_', aaa, '[,c("CODMUNRES","Sub_MS")], by=c("code_muni"="CODMUNRES"))  ')  ))  
    
    eval(parse(text = paste0('  sub_obit_', aaa, '<- read.csv(file = "Subnotitificacao/Sub_obitos_', aaa, '.csv", header = T, sep = ";", dec = ",")  ')  ))  
    if (aaa>= 2019){ eval(parse(text = paste0('  sub_obit_', aaa, '$CODMUNRES<- as.numeric( substr(x = as.character(sub_obit_', aaa, '$CODMUNRES), start = 1, stop = 6) )  ') ))   } 
    eval(parse(text = paste0('  sub_obit_', aaa, '<- left_join(cidade_mapa, sub_obit_', aaa, '[,c("CODMUNRES","Sub_MS")], by=c("code_muni"="CODMUNRES"))   ')  ))  
  } #; sub_obit_2019$Sub_MS<- as.numeric( sub_obit_2019$Sub_MS ) ; rm(aaa) # Obitos 2019 lendo como caracter =( ; Figueirao-MS 500390 com traco
  
  # Verificando se ha valores faltando. Match completo
  tt_na<-NULL ; for (bbb in 1:length(2015:2020) ) { tt_na[bbb]<-  eval(parse(text = paste0(' sum(is.na(sub_nasc_', 2014+bbb, '$Sub_MS)) ') ))
  tt_na[bbb+6]<-  eval(parse(text = paste0(' sum(is.na(sub_obit_', 2014+bbb, '$Sub_MS)) ') )) }  ; tt_na ; rm(bbb)
  
  # Assumindo, como sugerido, os anos que nao tem nos dados
  sub_nasc_2012<- sub_nasc_2013<- sub_nasc_2014<- sub_nasc_2015  ;  sub_obit_2012<-sub_obit_2013<- sub_obit_2014<- sub_obit_2015 ; sub_nasc_2021<- sub_nasc_2022<- sub_nasc_2020 ; sub_obit_2021<- sub_obit_2022<- sub_obit_2020
  
  
  ### Corrigindo e Uniformizando as Informcacoes das Cidades para cada ano
  for (ccc in 2012:2022) {
    eval(parse(text = paste0(' resul_baixo_peso$cidade$ano_', ccc, '$Nascidos<- round( (resul_baixo_peso$cidade$ano_', ccc, '$Nascidos) * (1 + ((sub_nasc_', ccc, '$Sub_MS)/100) ) ) ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$cidade$ano_', ccc, '$Baixo_Peso<- round( (resul_baixo_peso$cidade$ano_', ccc, '$Baixo_Peso) * (1 + ((sub_nasc_', ccc, '$Sub_MS)/100) )   ) ')  ))
    eval(parse(text = paste0(' resul_mortalidade$cidade$ano_', ccc, '$Obitos_1ano<- round( (resul_mortalidade$cidade$ano_', ccc, '$Obitos_1ano) * (1 + ((sub_obit_', ccc, '$Sub_MS)/100) ) )   ')  )) }
  
  # Organizando as microrregioes (numeracao e nomes) 
  microrregiao_mapa <- read_micro_region(code_micro="all", year=2020, showProgress = T) 
  microrregiao_dtb<- read.csv(file = "Dados/DTB_2022.csv", header = T, sep = ";", colClasses = c("character") )
  aux_dtb<- cbind( microrregiao_dtb[, c("UF", "Nome_UF", "Microrregião.Geográfica", "Nome_Microrregião", "Código.Município.Completo", "Nome_Município")], 
                   data.frame(codmun = as.numeric( substr(microrregiao_dtb$Código.Município.Completo,1,6) ) , code_micro = as.numeric( paste0(microrregiao_dtb$UF,microrregiao_dtb$Microrregião.Geográfica) ) )  )

  # Adicionando as capitais no aux_dtb  como cidades "0000". No SINASC aparece mais de 480000 nesse formato
  capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", 
               Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
  aux_dtb_0000<- aux_dtb[aux_dtb$codmun %in% capitais,] ; aux_dtb_0000$codmun<- as.numeric( paste0(aux_dtb_0000$UF,"0000") ) ; aux_dtb<-rbind(aux_dtb_0000, aux_dtb)
  
  aux_correcao<-  left_join(resul_baixo_peso$cidade$ano_2022, aux_dtb[, c("Nome_UF","Nome_Microrregião", "Nome_Município", "code_micro", "codmun")], by=c("code_muni"="codmun"))
  aux_micro1<- microrregiao_mapa$code_micro   ; 
  
  # Organizando os Estados
  estado_mapa <- read_state(code_state="all", year=2020, showProgress = T) ; estado_mapa$code_state<- as.numeric(estado_mapa$code_state) 
  aux_estado1<- estado_mapa$code_state
  
  # Adicionando as informacoes das microrregioes e estados, para cada ano, nos dados
  for (ccc in 2012:2022) {
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_', ccc, '<- microrregiao_mapa ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_', ccc, '<- estado_mapa ')  ))  }  
  
  
  ### Somando as cidades para corrigir as microrregioes
  sum(resul_baixo_peso$cidade$ano_2022$code_muni != aux_correcao$code_muni) # Conferindo a ordem das cidades e micro (sempre seguindo a ordem do "read_micro")
  for (eee in 2012:2022) { for(ddd in 1:length(aux_micro1) ){
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Nascidos[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Nascidos[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Baixo_Peso[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Baixo_Peso[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_mortalidade$micro$ano_' ,eee, '$Obitos_1ano[ddd]<-  sum(resul_mortalidade$cidade$ano_' ,eee, '$Obitos_1ano[aux_correcao$code_micro == aux_micro1[ddd]] , na.rm = T )  ')  ))
  } ; eval(parse(text = paste0(' resul_baixo_peso$micro$ano_' ,eee, '$Porcentagem<- (resul_baixo_peso$micro$ano_' ,eee, '$Baixo_Peso)/(resul_baixo_peso$micro$ano_' ,eee, '$Nascidos)*100  ')  ))  }
  
  # Conferindo as somas por microrregiao 
  #set.seed(1234); conferindo_micro<- sample(aux_correcao$code_micro, 1)
  #st_drop_geometry( resul_baixo_peso$micro$ano_2022[ resul_baixo_peso$micro$ano_2022$code_micro == conferindo_micro, c("Nascidos","Baixo_Peso")])
  #colSums( st_drop_geometry( resul_baixo_peso$cidade$ano_2022[aux_correcao$code_micro == conferindo_micro,c("Nascidos","Baixo_Peso")] ) )
  
  
  ### Somando as cidades para os estados
  sum(resul_baixo_peso$cidade$ano_2022$code_muni != aux_correcao$code_muni) # Conferindo a ordem das cidades e micro (sempre seguindo a ordem do "read_micro")
  for (eee in 2012:2022) { for(ddd in 1:length(aux_estado1) ){
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Nascidos[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Nascidos[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Baixo_Peso[ddd]<-  sum(resul_baixo_peso$cidade$ano_' ,eee, '$Baixo_Peso[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
    eval(parse(text = paste0(' resul_mortalidade$estado$ano_' ,eee, '$Obitos_1ano[ddd]<-  sum(resul_mortalidade$cidade$ano_' ,eee, '$Obitos_1ano[aux_correcao$code_state == aux_estado1[ddd]] , na.rm = T )  ')  ))
  } ; eval(parse(text = paste0(' resul_baixo_peso$estado$ano_' ,eee, '$Porcentagem<- (resul_baixo_peso$estado$ano_' ,eee, '$Baixo_Peso)/(resul_baixo_peso$estado$ano_' ,eee, '$Nascidos)*100  ')  ))  }
  
  # Conferindo as somas por estado 
  #dados_agregado_estado<- aggregate(cbind(Nascidos, Baixo_Peso ) ~ code_state , data = resul_baixo_peso$cidade$ano_2021, FUN = sum, na.rm = TRUE)
  #sum(dados_agregado_estado$Nascidos != resul_baixo_peso$estado$ano_2021$Nascidos)
  #sum(dados_agregado_estado$Baixo_Peso != resul_baixo_peso$estado$ano_2021$Baixo_Peso)
  
}




### Elementos na mesma ordem. Usei read_micro_region (geobr) como base e adicionei via leftjoin as informacoes
### Conferindo mais uma vez que todos estao da mesma forma, seguindo a ordem do read_micro_region  =D

aux_ordem<- NULL ; for (bb in 1:11) { 
  aux_ordem[bb]<-       sum( resul_baixo_peso$micro$ano_2022$code_micro == resul_baixo_peso$micro[[bb]]$code_micro ) 
  aux_ordem[bb +11 ]<-  sum( resul_baixo_peso$micro$ano_2022$code_micro == resul_mortalidade$micro[[bb]]$code_micro ) } ; aux_ordem

aux_ordem<- NULL ; for (bb in 1:11) { 
  aux_ordem[bb]<-       sum( resul_baixo_peso$cidade$ano_2022$code_muni == resul_baixo_peso$cidade[[bb]]$code_muni ) 
  aux_ordem[bb +11 ]<-  sum( resul_baixo_peso$cidade$ano_2022$code_muni == resul_mortalidade$cidade[[bb]]$code_muni ) } ; aux_ordem


####   Taxa de Mortalidade Infantil - Sugestao do Prof. Everton
# Numerador = (  Obitos 2020 < 1ano + Obitos 2021 < 1ano + Obito 2022 < 1ano) / 3
# Denominador = ( Nascidos 2020 * Nascidos 2022)^(1/2)

# Janela de 3 anos - Ordem indo de 2022 - 2014 (decrescente)
aux_mortalidade<- data.frame(NULL) ; for (mm in 1:9) {  
  aux_mortalidade[1:558 ,mm]<- ((resul_mortalidade$micro[[mm]]$Obitos_1ano + resul_mortalidade$micro[[mm +1]]$Obitos_1ano +resul_mortalidade$micro[[mm +2]]$Obitos_1ano 
  )/3)/(  ((resul_baixo_peso$micro[[mm]]$Nascidos)^(1/2)) * ((resul_baixo_peso$micro[[mm+2]]$Nascidos)^(1/2))   )*1000  } ; names(aux_mortalidade)<- paste0("Ano_",2022:2014) ; aux_mortalidade<- aux_mortalidade[,9:1]

# Ordem do cbind influencia. Trocando a ordem deixa de ser "sf"
#View(resul_baixo_peso$micro$ano_2022[,c(7,8,9)])
tx_mortalidade<- cbind(resul_baixo_peso$micro$ano_2022[,c(-7,-8,-9)], aux_mortalidade )


####   Taxa de Baixo Peso
# Numerador = (  Baixo_Peso 2020 + Baixo_Peso 2021 + Baixo_Peso 2022 ) / 3
# Denominador = ( Nascidos 2020 + Nascidos 2021 + Nascidos 2022) / 3

aux_baixo_peso_br<- data.frame(NULL) ;  for( mm in 1:9){  # Janela de 3 anos - Ordem indo de 2022 - 2014 (decrescente)
  aux_baixo_peso_br[1:558, mm]<-  ( resul_baixo_peso$micro[[mm]]$Baixo_Peso + resul_baixo_peso$micro[[mm +1]]$Baixo_Peso + resul_baixo_peso$micro[[mm +2]]$Baixo_Peso
  )/(   resul_baixo_peso$micro[[mm]]$Nascidos + resul_baixo_peso$micro[[mm +1]]$Nascidos + resul_baixo_peso$micro[[mm +2]]$Nascidos   )*100   } ; names(aux_baixo_peso_br)<- paste0("Ano_",2022:2014) ;  aux_baixo_peso_br<- aux_baixo_peso_br[, 9:1]

# Ordem do cbind influencia. Trocando a ordem deixa de ser "sf"
#View(resul_baixo_peso$micro$ano_2022[,c(7,8,9)])
baixo_peso_br<- cbind(resul_baixo_peso$micro$ano_2022[,c(-7,-8,-9)], aux_baixo_peso_br)


#save(list = c("referencia_pig", "resul_baixo_peso", "resul_mortalidade" , "baixo_peso_br", "tx_mortalidade", "estado_mapa"), file = paste0("~/Downloads/Baixo_Peso_Mortalidade_",referencia_pig,".RData"))
#save(list = c("referencia_pig", "baixo_peso_br", "tx_mortalidade", "estado_mapa"), file = paste0("~/Downloads/Taxa_Baixo_Peso_Mortalidade_",referencia_pig,".RData"))


