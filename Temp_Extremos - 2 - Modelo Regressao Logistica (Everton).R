###############################################################################################
######### 
################       PARTE I :  ORGANIZANDO DADOS
#########
###############################################################################################



### Pacotes
rm(list = ls()) ; options(scipen = 999) ; library(arrow); library(dplyr) ; library(geobr) ; library(stringr) ; library(roll) ; library(Kendall) ; library(trend) ; library(ggplot2) ; library(RColorBrewer) ; library(mltools) ; library(data.table) ; library(sf) ; library(ResourceSelection) ; 
setwd("~/Dropbox/Unicamp - Cristiano/") 
estado_mapa<- read_state(code_state = "all", year = 2020, showProgress = F)
cidade_mapa <- read_municipality(code_muni = "all", year = 2020, showProgress = F) 
my_data<- 2023; load(file = paste0("~/Documentos/opendatasus/dados_col condicao-Tw Logistico/dados_col_SINASC_Condicao_Logistico (",my_data,").RData")) # Dados Organizados e categorizados

###  Incorporando as Zonas Climaticas - Koppen 
# Dados Retirados do IPEF: https://www.ipef.br/publicacoes/acervohistorico/geodatabase/
municipios_koppen<- read.csv("Indice de Koppen/Koppen_municipios.csv", header = T, sep = ";", dec = "," ) ; dim(municipios_koppen)
#st_crs(shapename) ; cidade_mapa_wgs84 <- st_transform(cidade_mapa, 4326) ; st_crs(cidade_mapa_wgs84) ; inter_zonas <- st_intersects(shapename, cidade_mapa_wgs84, sparse = F)

# Shapefile do artigo: https://centrodametropole.fflch.usp.br/pt-br/noticia/base-de-dados-do-cem-traz-divisoes-climaticas-do-brasil-em-escala-municipal
shapename <- read_sf("Indice de Koppen/CLK_21_BR_CEM Zonas Climaticas/CLK_21_BR_CEM.shp")

# Faltam 3 Municipio
table(municipios_koppen$Koppen) ; shapename$MUNIC[order( shapename$CLIMA_K)] ; c(sum(table(municipios_koppen$Koppen)) , sum(shapename$MUNIC[order( shapename$CLIMA_K)]))
#setdiff(cidade_mapa$code_muni, municipios_koppen$IBGE.Code )


# Adicionando as capitais como "0000" (no SINASC aparece assim as vezes)
capitais<- c(Porto_Velho = "1100205", Rio_Branco = "1200401", Manaus = "1302603", Boa_Vista = "1400100", Belém = "1501402", Macapá = "1600303", Palmas = "1721000", São_Luís = "2111300",Teresina = "2211001", Fortaleza  ="2304400", Natal = "2408102", João_Pessoa = "2507507", Recife = "2611606", Maceió = "2704302", Aracaju = "2800308", Salvador = "2927408", Belo_Horizonte = "3106200", Vitória = "3205309", Rio_de_Janeiro = "3304557", São_Paulo = "3550308", Curitiba = "4106902", Florianópolis = "4205407", Porto_Alegre = "4314902", Campo_Grande = "5002704", Cuiabá = "5103403", Goiânia = "5208707", Brasília = "5300108") ;  capitais<- as.numeric( substr(capitais,1,6) )
municipios_koppen$IBGE.Code<- as.numeric( substr(x = as.character(municipios_koppen$IBGE.Code), start = 1, stop = 6) )
municipios_koppen$IBGE.Code[municipios_koppen$IBGE.Code %in% capitais]<- as.numeric( paste0( substr(municipios_koppen$IBGE.Code[municipios_koppen$IBGE.Code %in% capitais], 1, 2)  , "0000") ) 

# Left_join para adicionar as Zonas de Koppen aos dados do SINASC_CLIMA
dados_col<- left_join(dados_col, municipios_koppen[, c("IBGE.Code", "Koppen")], by=c("CODMUNRES"="IBGE.Code"))
setdiff(levels(as.factor(dados_col$CODMUNRES )), as.character(municipios_koppen$IBGE.Code) ) 

# Removendo os NA dos dados - Se necesario
dim(dados_col) ; dados_col<- dados_col[rowSums(is.na(dados_col) ) == 0, ] ; dim(dados_col)  # Removendo os NA

# Verificando se fez certo: Testando colunas aleatorias
#aux_codmun<- as.numeric( levels(as.factor(dados_col$CODMUNRES ) )) ; verificando<- data.frame( CODMUNRES = NA, Quantidade_Zona = NA, Zona_certa = NA, Qual_Zona_CODMUNRES = NA)
#for (kkk in 1:length(aux_codmun) ) { cat("CODMUNRES = ", aux_codmun[kkk], " - ",kkk,"\n")
#  dados_abc<- dados_col[dados_col$CODMUNRES == aux_codmun[kkk],]
#  zona_koppen_cidade<- levels(as.factor(dados_abc$Koppen )) ; 
#  zc<- municipios_koppen$Koppen[municipios_koppen$IBGE.Code == aux_codmun[kkk] ] == zona_koppen_cidade
#  verificando[kkk,]<- data.frame(CODMUNRES = aux_codmun[kkk], Quantidade_Zona = length(zona_koppen_cidade), Zona_certa = zc, Qual_Zona_CODMUNRES = zona_koppen_cidade  )   #verificando[kkk,]<- c(aux_codmun[kkk], length(zona_koppen_cidade), zc,zona_koppen_cidade  )
#} ; View(verificando)



###############################################################################################
######### 
################       PARTE II :  REGRESSAO LOGISTICA
#########
###############################################################################################




###  Funcao para calcular a Regressao Logistica (Sugestao do Prof. Everton)
regressao_logit<- function(estado, desf_vari, meses_quentes, expo_vari, quantos_dias_expo){

# Selecionando o Estado e os Meses Quentes
escolhendo_meses<- dados_col$MES_R %in% meses_quentes

# Escolheno Estado de Sao Paulo, Meses Quentes e Outras Situacoes
#dados_col_estado<- dados_col 
#dados_col_estado<- dados_col[( escolhendo_estado ), ]  ; 
if ( (tolower(estado) == "brasil") ){ cat( paste0("Analise do Brasil \n" ))
  dados_col_estado<- dados_col[( escolhendo_meses  ), ] }

if ( (is.numeric(estado) == TRUE)  ) { cat( paste0("Analise do estado: ", estado,"\n" ))
  escolhendo_estado<- as.numeric( substr(x = as.character(dados_col$CODMUNRES), start = 1, stop = 2) ) %in% estado
  dados_col_estado<- dados_col[(escolhendo_estado & escolhendo_meses), ]  }

if (  (is.numeric(estado) == FALSE) & (tolower(estado) != "brasil")  ) { cat( paste0("Analise da Zona Climatica: ", estado,"\n" ))
  escolhendo_estado<- tolower(dados_col$Koppen) %in% tolower(estado)
  dados_col_estado<- dados_col[(escolhendo_estado & escolhendo_meses), ]  }


###  Regressao Logistica
# Desfecho = Prematuridade e suas faixas 
eval(parse(text = paste0(' Y_desfecho<- data.frame(Desfecho = dados_col_estado$',desf_vari,' ) ' )   ))

# Exposicao = Dias de Condicao Umida ou Seca
# Se quantos_dias_expo == 0, entao FALSE == 0 e TRUE > 0
# Se quantos_dias_expo == c(10), entao FALSE < 10 e TRUE >= 10
# Se quantos_dias_expo == c(a,b,c), entao 0 se <= a; 1 se >a e <= b ; 2 se > b
if (length(quantos_dias_expo) == 1 ) {
    if (quantos_dias_expo == 0){eval(parse(text = paste0('  Y_exposicao<- data.frame(Exposicao = as.numeric(dados_col_estado$',expo_vari,'
                               > ',quantos_dias_expo,'  ) )  ')    )) }
    if (quantos_dias_expo > 0 ){eval(parse(text = paste0('  Y_exposicao<- data.frame(Exposicao = as.numeric(dados_col_estado$',expo_vari,'
                               >= ',quantos_dias_expo,'  ) )  ')    ))   }  }

if (length(quantos_dias_expo) > 1 ){Y_exposicao<- data.frame(Exposicao =rep(NA, nrow(dados_col_estado)) )
eval(parse(text = paste0('  Y_exposicao[dados_col_estado$',expo_vari,' <= ',quantos_dias_expo[1],'    , 1]<- 0 ')    ))

for (qqq in 2:(length(quantos_dias_expo)) ) {
  eval(parse(text = paste0('  Y_exposicao[ (dados_col_estado$',expo_vari,' > ',quantos_dias_expo[qqq-1],'    ) & 
                           (dados_col_estado$',expo_vari,' <= ',quantos_dias_expo[qqq],'   ) , 1]<- ',qqq -1,'  ')    ))     }

 eval(parse(text = paste0('  Y_exposicao[dados_col_estado$',expo_vari,' > ',quantos_dias_expo[length(quantos_dias_expo)],'  , 1]<- length(quantos_dias_expo)  ')    ))
} #View(cbind(Y_exposicao, dados_col_estado$condicao_umido_30dias))

# Covariaveis do Modelo 
#X<-dados_col_estado[,c("PESO,"PARIDADE", "SEXO_0", "SEXO_1", "RACACOR_2", "RACACOR_3", "RACACOR_4", "RACACOR_5", "ESCMAE_1", "ESCMAE_4", "GRAVIDEZ_2", "GRAVIDEZ_3", "CONSULTAS_2", "CONSULTAS_3", "CONSULTAS_4", "ESTCIVMAE_1", "ESTCIVMAE_3", "IDADEMAE_18", "IDADEMAE_35mais")]
#X<-dados_col_estado[,c("PESO", "GESTACAO", "PARTO", "PARIDADE", "SEXO", "RACACOR", "ESCMAE", "CONSULTAS", "ESTCIVMAE", "IDADEMAE", "GRAVIDEZ")]
X<-dados_col_estado[,c("PESO", "PARIDADE", "SEXO", "RACACOR", "ESCMAE", "CONSULTAS", "ESTCIVMAE", "IDADEMAE", "GRAVIDEZ")]
dados_ajuste<- data.frame(Y_desfecho, Y_exposicao, X) ; dados_ajuste$PESO<- dados_ajuste$PESO/1000

# Regressao Logistica: Ajustando pelo Desfecho com o pesos
# mods= ~ relevel(factor(group), ref="b")
#formula_2<- as.formula(paste("Desfecho ~ Exposicao + PESO + IDADEMAE +",paste( paste0( "as.factor(", names(X)[(names(X) != "PESO") & (names(X) != "IDADEMAE")], ") " ) , collapse='+')))
#mod_match <- glm(formula_2, data = dados_ajuste, family=binomial(link='logit') )

#mod_match <- glm(formula = Desfecho ~ Exposicao + PESO + IDADEMAE + as.factor(PARIDADE) + 
#as.factor(SEXO) + as.factor(RACACOR) + as.factor(ESCMAE) +  as.factor(CONSULTAS) + 
#  as.factor(ESTCIVMAE) + as.factor(GRAVIDEZ) , data = dados_ajuste, family=binomial(link='logit') )

# Transforma as colunas em factors (menos o Peso e a Idade da Mae)
col_names <- names(X)[(names(X) != "PESO") & (names(X) != "IDADEMAE")]
dados_ajuste[col_names] <- lapply(dados_ajuste[col_names] , factor)
# Exposicao Categorica
dados_ajuste[c("Exposicao")] <- lapply(dados_ajuste[c("Exposicao")] , factor)


# Modelo Logitico para os Fatores
formula_2<- as.formula(paste("Desfecho ~  Exposicao + ",paste(names(X), collapse='+')))
mod_match <- glm(formula_2, data = dados_ajuste, family=binomial(link='logit') )

return(mod_match)}




###############################################################################################
######### 
################       PARTE III :  ANALISANDO OS RESULTADOS
#########
###############################################################################################



### Escolhendo: Estado, Meses Quentes e Faixas de Prematuridade  (Estado pela Numeracao IBGE)
table(municipios_koppen$Koppen) ; my_data
estado<- "Aw"; desf_vari<- "TARDIO_PREMATURO"; meses_quentes<- c(10,11,12)   ; 
expo_vari<-"condicao_umido_30dias" ; quantos_dias_expo<- 10 ; #quantos_dias_expo<-  c(0, 4, 6, 8, 10, 12, 16)
#expo_vari<-"condicao_umido_7dias"  ; quantos_dias_expo<- 7
#expo_vari<-"condicao_umido_3dias"  ; quantos_dias_expo<- 3
#expo_vari<-"condicao_seco_30dias"  ; quantos_dias_expo<- 5
#expo_vari<-"categoria_condicao_umido_30dias" ; quantos_dias_expo<- 0
#expo_vari<-"categoria_condicao_umido_7dias" ; quantos_dias_expo<- 0
#expo_vari<-"categoria_condicao_umido_3dias" ; quantos_dias_expo<- 0

# Calculando o Score de Propensao
mod_match<- regressao_logit(estado = estado, desf_vari = desf_vari, meses_quentes = meses_quentes,expo_vari = expo_vari, quantos_dias_expo = quantos_dias_expo) ; summary(mod_match)
summary(mod_match)  # Significancia Estatistica

# Resultados do Propensity Score
cat(paste0("Dados: ",estado," nos meses quentes ",paste(meses_quentes,collapse = ","),"/",my_data," \n\n Y_Desfecho = ",desf_vari," \n Y_Exposicao = ",expo_vari," >= ",quantos_dias_expo," antes do parto \n X = Mesmas Covariaveis do arquivo enviado\n\n" )) ; exp(mod_match$coefficients) 

# Analise de Normalidade dos Residuos - Hosmer-Lemeshow Goodness of Fit (GOF) Test. 
hoslem.test(x = mod_match$data$Desfecho, y = fitted(mod_match))

# Intervalo de Confianca
#intervalo_conf<- exp( cbind(OR = coef(mod_match), confint(mod_match)) )  ; 
#cat(paste0("Dados: ",estado_mapa$name_state[ estado_mapa$code_state == estado]," nos meses quentes ",paste(meses_quentes,collapse = ","),"/2023 \n\n Y_Desfecho = ",desf_vari," \n Y_Exposicao = ",expo_vari," >= ",quantos_dias_expo," antes do parto \n X = Mesmas Covariaveis do arquivo enviado\n\n" ))  ; intervalo_conf

# Anova
#anova(mod_match)

#

