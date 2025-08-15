#########################################################################
##############               Mapas e Graficos              ##############
#########################################################################


###  Pacotes Utilizados
rm(list = ls()) ; setwd(dir = "~/Dropbox/Unicamp - Cristiano/SINASC/") ; library(ggplot2) ; library(ggspatial) ; library(spdep) ; library(dplyr) ; library(tmap) ; library(biscale) ; library(cowplot) ; library(geobr) ; library(RColorBrewer) ; library(stringr) ; library(ggpubr) #library(egg) ; 

### Carregando Dados Antigos do Baixo Peso ( acima de 14 semanas gestacionais)
#load(file = "~/Documentos/opendatasus/resul_baixo_peso.RData") ; referencia_pig<- "WHO" 
# Media da Diferenca entre os dados de Baixo Peso antes e Atual (cortando a semana gestacional)
#colMeans( st_drop_geometry(baixo_peso_br_antigo[,6:14]) - st_drop_geometry(baixo_peso_br[,6:14]))
#Ano_2014   Ano_2015   Ano_2016   Ano_2017   Ano_2018   Ano_2019   Ano_2020   Ano_2021   Ano_2022 
#0.5688478  0.8826119  0.7033683  0.3202328  0.1000008 -0.1199882 -0.4956158 -0.6642914 -0.3308530 

# Media do Valor Absoluto da Diferenca entre os dados de Baixo Peso antes e Atual (cortando a semana gestacional)
#colMeans(abs( st_drop_geometry(baixo_peso_br_antigo[,6:14]) - st_drop_geometry(baixo_peso_br[,6:14])))
#Ano_2014  Ano_2015  Ano_2016  Ano_2017  Ano_2018   Ano_2019  Ano_2020  Ano_2021  Ano_2022 
#1.3485674 1.4118318 1.2249197 0.7893024 0.1068989  0.7482762 1.1386915 1.3210624 1.3031343


### Carregando Dados do Baixo Peso e Mortalidade
# Dados do Baixo Peso da WHO
load(file = "~/Documentos/opendatasus - PIG WHO Intergrowth/Taxa_Baixo_Peso_Mortalidade_who.RData")             

# Dados do Baixo Peso do Intergrowth
#load(file = "~/Documentos/opendatasus - PIG WHO Intergrowth/Taxa_Baixo_Peso_Mortalidade_intergrowth.RData")




##############################################################################################
#######     Mapa do Baixo Peso por Microrrerigao no Brasil - 2014 a 2022 (janela de 3 anos)  # Low Birth Weights
# Escolhendo a cor
#display.brewer.pal(n = 9, name = 'YlOrRd') ; brewer.pal(n = 9, name = "YlOrRd")[c(1,2,3,6,8)] # Maximo eh 9
summary( st_drop_geometry(baixo_peso_br[,6:14])) #summary( unlist(as.vector(st_drop_geometry(baixo_peso_br[,6:14]))) )

###    Gerando o Mapa de Baixo Peso
for (ano_dado in 2014:2022) {  

if (referencia_pig == "who") {  # Legenda para os dados da WHO
  eval(parse(text = paste0(' cortes_bp_', ano_dado, ' <- cut(baixo_peso_br$Ano_', ano_dado, ', breaks = c(4, 8, 12, 16, 20, 24, 32.4),  labels = c("[4:8)", "[8:12)", "[12:16)", "[16:20)", "[20:24)", "24+"),  rigth = FALSE)  ')  )) ; no_axis <- theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) }
if (referencia_pig == "intergrowth") {  # Legenda para os dados do Intergrowth
  eval(parse(text = paste0(' cortes_bp_', ano_dado, ' <- cut(baixo_peso_br$Ano_', ano_dado, ', breaks = c(0, 7, 8, 9, 10, 11, 20),  labels = c("[0:7)", "[7:8)", "[8:9)", "[9:10)", "[10:11)", "11+"),  rigth = FALSE)  ')  )) ; no_axis <- theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) }

eval(parse(text = paste0(' fig_bp_',ano_dado -2013 ,'<- ggplot() + 
  geom_sf(data = baixo_peso_br, aes(fill = cortes_bp_', ano_dado, ' ), color= NA,  size= .15) + # "grey46" ; "black"           # Escala discreta de cores
  
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) +         # Estados destacados
  geom_sf_text(data = estado_mapa,aes(label = abbrev_state),size = 2.5, fontface="bold") + 
  
  labs(subtitle=paste0( ano_dado -2 , " - ", ano_dado) , size=8) +  
  scale_fill_manual(values = brewer.pal(n = 9, name = "YlOrRd")[c(1,2,3,5,8,9)], drop = FALSE, name="Small for Gestational\nAge (%)") +   # Em Ingles: Small for Gestational Age
  #scale_fill_manual(values = brewer.pal(n = 9, name = "YlOrRd")[c(1,2,3,5,8,9)], drop = FALSE, name="Preterm Birth (%)")+   # Em Ingles: Preterm Birth
    
  theme_minimal() + theme(legend.title = element_text(size = 10), plot.subtitle = element_text(hjust = 0.5) )+
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), 
                          style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + no_axis ')  )) 
} 

#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Baixo Peso - Microrregiao 2014-2022.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   # width = 8.3, height = 11.7, # width = 14, height = 12,   #legendas_bp<- get_legend(fig_bp_1)
gg_baixo_peso<- ggarrange(fig_bp_1, fig_bp_2, fig_bp_3,   fig_bp_4, fig_bp_5, fig_bp_6,   fig_bp_7, fig_bp_8, fig_bp_9,   nrow=3, ncol = 3, common.legend = TRUE, legend = "right")   # ,legend = 'right', legend.grob = legendas_bp
annotate_figure(gg_baixo_peso, top = text_grob("Smal for Gestational Age across Brazilian microregions", color = "black", face = "bold", size = 14))
#dev.off()
#
 




##############################################################################################
#######     Mapa da Mortalidade por Microrrerigao no Brasil - 2014 a 2022 (janela de 3 anos)
# Escolhendo a cor
#display.brewer.pal(n = 9, name = "Blues") ; brewer.pal(n = 9, name = "Blues")[c(1,2,3,5,8,9)] # Maximo eh 9

###    Gerando o Mapa de Mortalidade
for (ano_dado in 2014:2022) {  #ano_dado<- 2022 ;  

eval(parse(text = paste0(' cortes_mort_', ano_dado,' <- cut(tx_mortalidade$Ano_', ano_dado, ', breaks = c(-0.1, 8, 12, 16, 20, 24, 120),  labels = c("[0:8)", "[8:12)", "[12:16)", "[16:20)", "[20:24)", "24+"),  rigth = FALSE)  ')  )) ; no_axis <- theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank());
eval(parse(text = paste0(' fig_mort_',ano_dado -2013 ,'<- ggplot() + 
  geom_sf(data = tx_mortalidade, aes(fill = cortes_mort_', ano_dado,' ), color= NA,  size= .15) + # "grey46" ; "black"           # Escala discreta de cores
  
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) + 
  geom_sf_text(data = estado_mapa,aes(label = abbrev_state),size = 2.5, fontface="bold") +  

  labs(subtitle=paste0( ano_dado -2 , " - ", ano_dado) , size=8) +                                                 # Em Ingles  
  scale_fill_manual(values = brewer.pal(n = 9, name = "Blues")[c(1,2,3,5,8,9)], drop = FALSE, name = "Infant Mortality\nRate" ) +
  
  theme_minimal() + theme(legend.title = element_text(size = 10), plot.subtitle = element_text(hjust = 0.5) )+
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), 
                          style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + no_axis ')  )) ; 
}

#jpeg( paste0("~/Downloads/Baixo Peso/Mortalidade - Microrregiao 2014-2022.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   
gg_mort<- ggarrange(fig_mort_1, fig_mort_2, fig_mort_3,   fig_mort_4, fig_mort_5, fig_mort_6,   fig_mort_7, fig_mort_8, fig_mort_9,   nrow=3, ncol = 3, common.legend = TRUE, legend = "right")   # ,legend = 'right', legend.grob = legendas_bp
annotate_figure(gg_mort, top = text_grob("Infant Mortality Rates across Brazilian microregions", color = "black", face = "bold", size = 14))
#dev.off() 
#





##############################################################################################
#######     Analise Bivariada

###    Gerando o Mapa Bivariado
for (ano_dado in 2014:2022) { #ano_dado<- 2022 ;

# Fixando o Baixo peso e Adicionando a Tx de Mort. do respectivo ano
aux_bivariado<- baixo_peso_br ; eval(parse(text = paste0( " aux_bivariado$tx_mort_", ano_dado, " <- tx_mortalidade$Ano_", ano_dado )  ))

# Adiciona uma nova coluna aos dados (bi_class)
eval(parse(text = paste0( ' bivaridado <- bi_class(aux_bivariado, x = Ano_',ano_dado, ' , y = tx_mort_', ano_dado,' , style = "quantile", dim = 3)  ')  ))

# Gerando o Mapa Bivariado: Child ou Infant
eval(parse(text = paste0( ' fig_biv_',ano_dado -2013 ,'<-  ggplot() + geom_sf(data = bivaridado, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) + bi_theme() +
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) +
  geom_sf_text(data = estado_mapa, aes(label = abbrev_state), size = 2.5, color = "white", fontface = "bold") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(1.3, "in"), pad_y = unit(0.1, "in"), 
     style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + 
  labs( subtitle = paste0( ano_dado-2, " - ",ano_dado )  ) + theme( plot.subtitle = element_text(face = "plain", size = 10) ) ')  ))
}

# Baixo Peso e Mortalidade
legendas_bv <- bi_legend(pal = "GrPink", dim = 3, xlab = "Small for Gestational Age", ylab = "Infant Mortality Rate", size = 10) #eval(parse(text = paste0( ' fig_biv_',ano_dado -2013 ,'<- ggdraw() + draw_plot(mapa_bivariado, 0, 0, 1, 1)  ')  ))

#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Bivariado BaixoPeso_Mortalidade - Microrregiao 2014-2022.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   
gg_biv<- ggarrange(fig_biv_1, fig_biv_2, fig_biv_3,   fig_biv_4, fig_biv_5, fig_biv_6,   fig_biv_7, fig_biv_8, fig_biv_9,   nrow=3, ncol = 3)   # ,legend = 'right', legend.grob = legendas_bp  # , common.legend = TRUE, legend = "right"
gg_biv_aux<- ggarrange(gg_biv,legendas_bv, nrow = 1, ncol = 2, widths = c(6,1)  )
annotate_figure(gg_biv_aux, top = text_grob("Bivariate map, Small for Gestational Age and Infant Mortality Rates across Brazilian microregions", color = "black", face = "bold", size = 14))
#dev.off() 
#





##############################################################################################
#######     Moran Local Bivariado: Baixo Peso e Tx. Mortalidade

###    Gerando o Mapa do Moran Local Bivariado
for (ano_dado in 2014:2022) { #ano_dado<- 2022 ; 

#    Gerando o Mapa com os Vizinhos retirando Fernando de Noronha - PE
moran_bv_baixo_peso<-   baixo_peso_br[!(baixo_peso_br$name_micro == "Fernando De Noronha"), ] ; moran_bv_mort<- tx_mortalidade[!(tx_mortalidade$name_micro == "Fernando De Noronha"), ] ; nome_titulo<- "Moran Bivariado"

# Gerando os vizinhos pela proximidade da rainha (queen true, do xadrez). Vizinhanca a mesma para Baixo Peso e Mortalidade
vizinhos <- poly2nb(moran_bv_baixo_peso, queen=TRUE) #; vizinhos[1]

# Peso dos vizinhos ; style = W e normalizacao pela linha
viz_pesos <- nb2listw(vizinhos, style="W", zero.policy=TRUE) #; viz_pesos$weights[1] ; print.listw(viz_pesos, zero.policy = TRUE)

###   Moran Global e Local Bivariado (moran_bv e localmoran_bv = ja normalizam os dados na entrada, refiz por preciosismo)
eval(parse(text = paste0("  xx<- ( moran_bv_baixo_peso$Ano_", ano_dado, "  - mean( moran_bv_baixo_peso$Ano_", ano_dado, " , na.rm = T)  )/( sd( moran_bv_baixo_peso$Ano_", ano_dado, "  , na.rm = T)   ) ")  ))
eval(parse(text = paste0("  yy<- ( moran_bv_mort$Ano_", ano_dado, "  - mean( moran_bv_mort$Ano_", ano_dado, " , na.rm = T) )/( sd( moran_bv_mort$Ano_", ano_dado, " , na.rm = T) ) ") ))
mapa_moran_local<- localmoran_bv(x = xx, y = yy, listw = viz_pesos, nsim = 500)   
lmoran_bv<- cbind(moran_bv_baixo_peso, mapa_moran_local )  


# Criando os quadrantes para valores 
W  <- matrix(data = 0, nrow = length(viz_pesos$neighbours) , ncol = length(viz_pesos$neighbours) ) ; for (ii in 1:(length(viz_pesos$neighbours)) ) {  
  W[ii, viz_pesos$neighbours[[ii]] ]<- 1 }

# Avaliando as Significancias
eval(parse(text = paste0(' signif <- 0.05 ; lmoran_bv <- lmoran_bv%>%  # Com  W multiplicando  W%*%
  mutate(quadrant_',ano_dado,'= ifelse(xx > 0 & W%*% yy > 0, 1, 0)) %>%        ## alto-alto
  mutate(quadrant_',ano_dado,'= ifelse(xx < 0 & W%*% yy < 0, 2, quadrant_',ano_dado,')) %>% ## baixo-baixo
  mutate(quadrant_',ano_dado,'= ifelse(xx < 0 & W%*% yy > 0, 3, quadrant_',ano_dado,')) %>% ## baixo-alto
  mutate(quadrant_',ano_dado,'= ifelse(xx > 0 & W%*% yy < 0, 4, quadrant_',ano_dado,')) %>% ## alto-baixo   
  mutate(quadrant_',ano_dado,'= ifelse(lmoran_bv$Pr.z....E.Ibvi.. > signif, 0, quadrant_',ano_dado,'))  ')  ))

# Construindo os Mapas
eval(parse(text = paste0(' cortes_moran_bv_',ano_dado,'<- cut(lmoran_bv$quadrant_',ano_dado,', breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5),  labels = c("Not significant", "High-High","Low-Low","Low-High","High-Low"),  rigth = FALSE)  ')  )) ; no_axis <- theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank());
eval(parse(text = paste0(' fig_moran_bv_',ano_dado -2013 ,'<-  ggplot() + 
  geom_sf(data = lmoran_bv, aes(fill = cortes_moran_bv_',ano_dado,' ), color= NA,  size= .15) + 
  
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) + 
  geom_sf_text(data = estado_mapa,aes(label = abbrev_state),size = 2.5, fontface="bold") +  

  labs(subtitle = paste0( ano_dado -2 , " - ", ano_dado), size=8) +                                                 
  scale_fill_manual(values = c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), drop = FALSE, name = "" ) +
  
  theme_minimal() + theme(legend.title = element_text(size = 10), plot.subtitle = element_text(hjust = 0.5) )+
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), 
                          style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + no_axis  ')  )) 
}

# Moran Bivariado: Baixo Peso - Mortalidade 
#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Moran Local Bivariado BaixoPeso_Mortalidade por Microrregiao 2014-2022.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   
gg_moran_bv<- ggarrange(fig_moran_bv_1, fig_moran_bv_2, fig_moran_bv_3,   fig_moran_bv_4, fig_moran_bv_5, fig_moran_bv_6,   fig_moran_bv_7, fig_moran_bv_8, fig_moran_bv_9,   nrow=3, ncol = 3, common.legend = TRUE, legend = "right")   
annotate_figure(gg_moran_bv, top = text_grob("Bivariate local Moran I, Small for Gestational Age and\nInfant Mortality Rates across Brazilian microregions", color = "black", face = "bold", size = 14))
#dev.off() 
#





##########################################################################################################################
##############################################################################################
##############
#######     Analisando os Saldos Migratorios e a Taxa de Migracao
taxas.migracao  <- read.csv(file = "~/Downloads/Baixo Peso/Dados/TaxasMigracao.csv", header = T, sep = ";" , dec = ",")  
#saldos.migracao <- read.csv(file = "~/Downloads/Baixo Peso/Dados/SaldosMigratario.csv", header = T, sep = ";" ) 



#######     Analise Bivariada  --  Baixo Peso e Taxa de Migracao
# Fixando o Baixo peso e Adicionando o Saldo Migratorio ou a Taxa de Migracao
for (ano_dado in 2014:2018) { # ano_dado<- 2018 ; 

# Adicionando os dados de Taxa de Migracao ao dados do Baixo Peso
aux_bivariado_migracao<- left_join(baixo_peso_br, taxas.migracao[,-c(2,3)], by=c("code_micro"="CD_MICRO")) ; nome_st<-"Taxa" ; nome_st_titulo<- "Net Migration Rate" ;    
##aux_bivariado_migracao<- left_join(baixo_peso_br, saldos.migracao[,-c(2,3)], by=c("code_micro"="CD_MICRO")) ; nome_st<-"Saldo" ; nome_st_titulo<- "Net Migration" ; 

# Adiciona uma nova coluna aos dados (bi_class)
eval(parse(text = paste0( ' bivaridado_migracao <- bi_class(aux_bivariado_migracao, x = Ano_',ano_dado, ' , y = X', ano_dado,' , style = "quantile", dim = 3)  ')  ))

# Gerando o Mapa Bivariado: Child ou Infant
eval(parse(text = paste0( ' fig_biv_migracao_',ano_dado -2013 ,'<-  ggplot() + geom_sf(data = bivaridado_migracao, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) + bi_theme() +
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) +
  geom_sf_text(data = estado_mapa, aes(label = abbrev_state), size = 2.5, color = "white", fontface = "bold") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(1.3, "in"), pad_y = unit(0.1, "in"), 
     style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + 
  labs( subtitle = paste0( ano_dado)   ) + theme( plot.subtitle = element_text(face = "plain", size = 10) ) ')  ))
}

# Baixo Peso e Taxa de Migracao
legendas_bv_migracao <- bi_legend(pal = "GrPink", dim = 3, xlab = "Small for Gestational Age", ylab = nome_st_titulo, size = 10) 

#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Bivariado BaixoPeso_",nome_st," - Microrregiao 2014-2018.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   
gg_biv_migracao<-  ggarrange( ggarrange(fig_biv_migracao_1, fig_biv_migracao_2, fig_biv_migracao_3,   fig_biv_migracao_4, nrow=2, ncol = 2),
                     fig_biv_migracao_5,   nrow = 2, ncol = 1, heights = c(2,1)) #gg_biv<- ggarrange(fig_biv_1, fig_biv_2, fig_biv_3,   fig_biv_4, fig_biv_5,   nrow=3, ncol = 2, align = "hv")   # ,legend = 'right', legend.grob = legendas_bp  # , common.legend = TRUE, legend = "right"
gg_biv_aux_migracao<- ggarrange(gg_biv_migracao, legendas_bv_migracao, nrow = 1, ncol = 2, widths = c(6,1)  )
annotate_figure(gg_biv_aux_migracao, top = text_grob(paste0("Bivariate map, Small for Gestational Age and ",nome_st_titulo," across Brazilian microregions"), color = "black", face = "bold", size = 14))
#dev.off() 
#


#######     Analise Bivariada  --  Mortalidade Infantil e Taxa de Migracao
# Fixando o Baixo peso e Adicionando a Tx de Mort. do respectivo ano
for (ano_dado in 2014:2018) { # ano_dado<- 2018 ; 

# Adicionando os dados de Taxa de Migracao ao dados de Mortalidade
aux_bivariado_migracao_mort<- left_join(tx_mortalidade, taxas.migracao[,-c(2,3)], by=c("code_micro"="CD_MICRO")) ; nome_st<-"Taxa" ;   nome_st_titulo<- "Net Migration Rate"   
## aux_bivariado<- left_join(tx_mortalidade, saldos.migracao[,-c(2,3)], by=c("code_micro"="CD_MICRO")) ; nome_st<-"Saldo" ;  nome_st_titulo<- "Net Migration" ; 

# Adiciona uma nova coluna aos dados (bi_class)
eval(parse(text = paste0( ' bivaridado_migracao_mort <- bi_class(aux_bivariado_migracao_mort, x = Ano_',ano_dado, ' , y = X', ano_dado,' , style = "quantile", dim = 3)  ')  ))

# Gerando o Mapa Bivariado: Child ou Infant
eval(parse(text = paste0( ' fig_biv_migracao_mort_',ano_dado -2013 ,'<-  ggplot() + geom_sf(data = bivaridado_migracao_mort, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) + bi_theme() +
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) +
  geom_sf_text(data = estado_mapa, aes(label = abbrev_state), size = 2.5, color = "white", fontface = "bold") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(1.3, "in"), pad_y = unit(0.1, "in"), 
     style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + 
  labs( subtitle = paste0( ano_dado )  ) + theme( plot.subtitle = element_text(face = "plain", size = 10) ) ')  ))
}

# Baixo Peso e Mortalidade
legendas_bv_migracao_mort <- bi_legend(pal = "GrPink", dim = 3, xlab = "Infant Mortality Rate", ylab = nome_st_titulo, size = 10) 

#jpeg( paste0("~/Downloads/Baixo Peso/Bivariado Mortalidade_",nome_st," - Microrregiao 2014-2018.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   
gg_biv_migracao_mort<-  ggarrange( ggarrange(fig_biv_migracao_mort_1, fig_biv_migracao_mort_2, fig_biv_migracao_mort_3,   fig_biv_migracao_mort_4, nrow=2, ncol = 2),
                     fig_biv_migracao_mort_5,   nrow = 2, ncol = 1, heights = c(2,1)) #gg_biv<- ggarrange(fig_biv_1, fig_biv_2, fig_biv_3,   fig_biv_4, fig_biv_5,   nrow=3, ncol = 2)  
gg_biv_aux_migracao_mort<- ggarrange(gg_biv_migracao_mort, legendas_bv_migracao_mort, nrow = 1, ncol = 2, widths = c(6,1)  )
annotate_figure(gg_biv_aux_migracao_mort, top = text_grob(paste0("Bivariate map, Infant Mortality Rate and ",nome_st_titulo," across Brazilian microregions"), color = "black", face = "bold", size = 14))
#dev.off() 
#





##############################################################################################
#######     Indice de Lee
# https://stackoverflow.com/questions/45177590/map-of-bivariate-spatial-correlation-in-r-bivariate-lisa

for (ano_dado in 2014:2022) { # ano_dado<- 2022 ; 

# Gerando o Mapa com os Vizinhos retirando Fernando de Noronha - PE
lee_bv_baixo_peso<- baixo_peso_br[!(baixo_peso_br$name_micro == "Fernando De Noronha"), ] ; 
lee_bv<- lee_bv_baixo_peso[,-(6:14)] # Salvando as Informacoes Iniciais das Microrregioes
lee_bv_mort<- tx_mortalidade[!(tx_mortalidade$name_micro == "Fernando De Noronha"), ] ; nome_titulo<- "Lee's Statistic"

# Gerando os vizinhos pela proximidade da rainha (queen true, do xadrez). Vizinhanca a mesma para Baixo Peso e Mortalidade
vizinhos <- poly2nb(lee_bv_baixo_peso, queen=TRUE) #; vizinhos[1]
viz_pesos <- nb2listw(vizinhos, style="W", zero.policy=TRUE) # Peso dos vizinhos ; style = W e normalizacao pela linha

#   Normalizando os Dados  (moran_bv e localmoran_bv = ja normalizam os dados na entrada, refiz por preciosismo)
eval(parse(text = paste0("  xx<- ( lee_bv_baixo_peso$Ano_", ano_dado, "  - mean( lee_bv_baixo_peso$Ano_", ano_dado, " , na.rm = T)  )/( sd( lee_bv_baixo_peso$Ano_", ano_dado, "  , na.rm = T)   ) ")  ))
eval(parse(text = paste0("  yy<- ( lee_bv_mort$Ano_", ano_dado, "  - mean( lee_bv_mort$Ano_", ano_dado, " , na.rm = T) )/( sd( lee_bv_mort$Ano_", ano_dado, " , na.rm = T) ) ") ))

# Lee index and # Lee’s L statistic (Global)
lee_xy <- lee(x = xx, y = yy, listw = viz_pesos, n = length(xx), zero.policy = TRUE) ; 
#lee_xy[1] ; summary(lee_xy$localL)

# Organizando os Dados Numa Tabela
eval(parse(text = paste0(" lee_bv<- cbind(lee_bv, data.frame( indice_lee_",ano_dado ," = lee_xy$localL ))" ) ))



### Mapa do Indice de Lee Bivariado do Baixo Peso e Mortalidade (escalas de valores)
eval(parse(text = paste0(' cortes_lee_', ano_dado, ' <- cut(lee_bv$indice_lee_', ano_dado, ', breaks = c(-2, -0.5, 0.5, 1, 2, 20),  labels = c("[-2:-0.5)", "[-0.5:0.5)", "[0.5:1.0)", "[1.0:2.0)", "[2.0+"),  rigth = FALSE)  ')  )) ; no_axis <- theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) 

eval(parse(text = paste0(' fig_lee_',ano_dado -2013 ,'<- ggplot() + 
  geom_sf(data = lee_bv, aes(fill = cortes_lee_', ano_dado, ' ), color= NA,  size= .15) + # "grey46" ; "black"           # Escala discreta de cores
  
  geom_sf(data = estado_mapa, fill = NA, color= "black", size = .15, lwd = 0.45) +         # Estados destacados
  geom_sf_text(data = estado_mapa,aes(label = abbrev_state),size = 2.5, fontface="bold") + 
  
  labs(subtitle=paste0( ano_dado -2 , " - ", ano_dado) , size=8) +  
  scale_fill_manual(values =c("#0000FF", "#FFFFFF", "#FFBFBF", "#FF0000", "darkred"), drop = FALSE, name="Lee\'s Index") +   # Em Ingles: Small for Gestational Age
  #scale_fill_manual(values = c("#0000FF", "#FFFFFF", "#FFBFBF", "#FF0000", "darkred"), drop = FALSE, name="Preterm Birth (%)")+   # Em Ingles: Preterm Birth
    
  theme_minimal() + theme(legend.title = element_text(size = 10), plot.subtitle = element_text(hjust = 0.5) )+
  annotation_north_arrow( location = "br", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), 
                          style = north_arrow_fancy_orienteering ) + ggspatial::annotation_scale() + no_axis ')  )) ; 
}


#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Indice de Lee - Brazil Standard - Microrregiao 2014-2022.jpg"), width = 14, height = 12, units = 'in', res = 400, quality = 80)   # width = 8.3, height = 11.7, # width = 14, height = 12,   #legendas_bp<- get_legend(fig_bp_1)
gg_baixo_peso_lee<- ggarrange(fig_lee_1, fig_lee_2, fig_lee_3,   fig_lee_4, fig_lee_5, fig_lee_6,   fig_lee_7, fig_lee_8, fig_lee_9,   nrow=3, ncol = 3, common.legend = TRUE, legend = "right")   # ,legend = 'right', legend.grob = legendas_bp
annotate_figure(gg_baixo_peso_lee, top = text_grob("Lee's Index, Small for Gestational Age and\nInfant Mortality Rates across Brazilian microregions", color = "black", face = "bold", size = 14))
#dev.off()





##############################################################################################
#######     Mapa do Indice de Moran Global e Local - Univariado

#    Gerando o Mapa com os Vizinhos retirando Fernando de Noronha - PE
ano_dado<- 2022 ; moran_dados_ano<-   baixo_peso_br[!(baixo_peso_br$name_micro == "Fernando De Noronha"), ] ; nome_titulo<- "Baixo Peso"
#ano_dado<- 2022 ; moran_dados_ano<- tx_mortalidade[!(tx_mortalidade$name_micro == "Fernando De Noronha"), ] ; nome_titulo<- "Mortalidade"


# Gerando os vizinhos pela proximidade da rainha (queen true, do xadrez). Vizinhanca a mesma para Baixo Peso e Mortalidade
vizinhos <- poly2nb(moran_dados_ano, queen=TRUE) #; vizinhos[1]

# Peso dos vizinhos ; style = W e normalizacao pela linha
viz_pesos <- nb2listw(vizinhos, style="W", zero.policy=TRUE) #; viz_pesos$weights[1] ; print.listw(viz_pesos, zero.policy = TRUE)

#   Moran Global I
eval(parse(text = paste0(" mapa_moran_I <- moran(x = moran_dados_ano$Ano_", ano_dado , ", listw = viz_pesos, n = length(vizinhos), S0 = Szero(viz_pesos), zero.policy = TRUE) ") ))


###    Gerando o Indice de Moran Local
eval(parse(text = paste0(" mapa_moran_local<- localmoran(x = moran_dados_ano$Ano_", ano_dado, ", listw = viz_pesos, adjust.x=TRUE, zero.policy = TRUE)" ) )) 

# Salvando em um objeto mapa
lmoran<- cbind(moran_dados_ano, mapa_moran_local )  

# Centraliza o Moran local em torno da média
lmoran$Ii <- lmoran$Ii - mean(lmoran$Ii, na.rm = TRUE) 

# Cria-se o valor lag espacial (a média dos valores vizinhos de cada meso)
eval(parse(text = paste0(" lmoran$lag.prob8091<- lag.listw(x = viz_pesos, var = lmoran$Ano_", ano_dado, ", zero.policy = TRUE)" )  ))
eval(parse(text = paste0(" lmoran$prob8091.n <- lmoran$Ano_", ano_dado, " - mean(lmoran$Ano_", ano_dado, ", na.rm = TRUE)")  ))  # Centraliza a variável de interesse em torno de sua média 
lmoran$lag.prob8091 <- lmoran$lag.prob8091 - mean(lmoran$lag.prob8091, na.rm = TRUE) 


# Criando os quadrantes para valores 
signif <- 0.05 ; lmoran <- lmoran%>% 
  mutate(quadrant= ifelse(prob8091.n > 0 & lag.prob8091 > 0, 1, 0)) %>%        ## alto-alto
  mutate(quadrant= ifelse(prob8091.n < 0 & lag.prob8091 < 0, 2, quadrant)) %>% ## baixo-baixo
  mutate(quadrant= ifelse(prob8091.n < 0 & lag.prob8091 > 0, 3, quadrant)) %>% ## baixo-alto
  mutate(quadrant= ifelse(prob8091.n > 0 & lag.prob8091 < 0, 4, quadrant)) %>% ## alto-baixo   
  mutate(quadrant= ifelse(lmoran$Pr.z....E.Ii.. > signif, 0, quadrant))  

# Fazendo um mapa com áreas 
#jpeg( paste0("~/Downloads/Baixo Peso/",referencia_pig,"_Moran Local - ", nome_titulo," Microrregiao ", ano_dado,".jpg"), width = 7, height =5, units = 'in',  res = 400,quality = 80)   
tm_shape(lmoran) + tm_fill(col = "quadrant",  breaks = c(0, 1, 2, 3, 4, 5) , palette= c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)),
                           labels = c("Not significant", "High-High","Low-Low","Low-High","High-Low"), title="") +  tm_legend(text.size = 0.85)  + 
  tm_borders(alpha= 0.0) + 
  tm_layout(main.title = paste0("Moran Local: ", nome_titulo," ", ano_dado-2, " - ", ano_dado ), main.title.position = "center", frame = F) +   #tm_layout( frame = F) + 
  tm_shape(estado_mapa) + tm_borders( alpha= 1) + tm_text(text = "abbrev_state", size = 0.6)
#dev.off()
#

