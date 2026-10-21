# Figuras para relatorio academico ENEM 2020-2025
# Executar na pasta raiz Projeto_ENEM
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

entrada <- 'resultados/estatisticas_enem_2020_2025.csv'
if (!file.exists(entrada)) stop('Arquivo nao encontrado: ', entrada)
dir.create('figuras_relatorio', showWarnings = FALSE)

estat <- fread(entrada)
requeridas <- c('NU_ANO','REGIAO','AREA','N','MEDIA','MEDIANA','DESVIO_PADRAO')
if (!all(requeridas %in% names(estat))) stop('Faltam colunas na tabela de estatisticas.')
if (nrow(estat) != 150L) stop('Esperadas 150 linhas de estatisticas; encontrado: ',nrow(estat))

nomes_areas <- c(
  NU_NOTA_CN = 'Ciências da Natureza',
  NU_NOTA_CH = 'Ciências Humanas',
  NU_NOTA_LC = 'Linguagens',
  NU_NOTA_MT = 'Matemática',
  NU_NOTA_REDACAO = 'Redação'
)
estat[, AREA_LABEL := factor(nomes_areas[AREA], levels = unname(nomes_areas))]
estat[, REGIAO := factor(REGIAO, levels = c('Norte','Nordeste','Centro-Oeste','Sudeste','Sul'))]
if (anyNA(estat$AREA_LABEL) || anyNA(estat$REGIAO)) stop('Areas ou regioes inesperadas no CSV')

salvar <- function(g, nome, largura=11, altura=6.5) {
  destino <- file.path('figuras_relatorio', paste0(nome,'.png'))
  ggsave(destino, plot=g, width=largura, height=altura, dpi=240, bg='white')
  cat('Salvo: ',destino,'\n',sep='')
}
base_tema <- theme_minimal(base_size=12) +
  theme(plot.title=element_text(face='bold',size=15),
        plot.subtitle=element_text(size=10),
        legend.position='bottom',
        panel.grid.minor=element_blank(),
        strip.text=element_text(face='bold'))

# FIGURA 1 - volumetria do tratamento
volume <- data.table(
  ANO = 2020:2025,
  ORIGINAIS = c(5783109,3389832,3476105,3933955,4332944,4810772),
  ANALITICOS = c(2762899,2386116,2504014,2836806,3182843,3473543)
)
volume[, REMOVIDOS_SEM_NOTA := ORIGINAIS - ANALITICOS]
vol_long <- melt(volume, id.vars='ANO', measure.vars=c('ANALITICOS','REMOVIDOS_SEM_NOTA'),
                 variable.name='SITUACAO',value.name='REGISTROS')
vol_long[, SITUACAO := factor(SITUACAO, levels=c('ANALITICOS','REMOVIDOS_SEM_NOTA'),
                             labels=c('Ao menos uma nota','Sem nenhuma nota'))]
g <- ggplot(vol_long,aes(x=factor(ANO), y=REGISTROS/1e6,fill=SITUACAO)) +
  geom_col(width=.72) +
  scale_fill_manual(values=c('Ao menos uma nota'='#2563a6','Sem nenhuma nota'='#b8c8d6')) +
  labs(title='Preparação dos dados: registros por edição do ENEM',
       subtitle='25.726.717 registros originais; 17.146.221 mantidos na base analítica',
       x='Ano',y='Registros (milhões)',fill='Situação') + base_tema
salvar(g,'01_volume_tratamento')

# FIGURAS 2 a 6 - evolucao das medias nas cinco areas
for (area in names(nomes_areas)) {
  d <- estat[AREA==area]
  g <- ggplot(d,aes(x=NU_ANO,y=MEDIA,color=REGIAO,group=REGIAO)) +
    geom_line(linewidth=1) + geom_point(size=2.2) +
    scale_x_continuous(breaks=2020:2025) +
    labs(title=paste('Evolução das médias —',nomes_areas[[area]]),
         subtitle='Médias entre participantes com nota disponível nesta área',
         x='Ano',y='Nota média',color='Região') + base_tema
  salvar(g,paste0('02_evolucao_',area))
}

# FIGURA 7 - matriz de medias de 2025
ultimo <- estat[NU_ANO==2025]
g <- ggplot(ultimo,aes(x=AREA_LABEL,y=REGIAO,fill=MEDIA)) +
  geom_tile(color='white',linewidth=.9) +
  geom_text(aes(label=sprintf('%.1f',MEDIA)),size=3.8) +
  scale_fill_gradient(low='#dcebf5',high='#4c80a8') +
  labs(title='Médias regionais por área — ENEM 2025',
       subtitle='Os valores são médias, não notas de participantes individuais',
       x='Área',y='Região',fill='Média') +
  base_tema + theme(axis.text.x=element_text(angle=20,hjust=1))
salvar(g,'07_matriz_medias_2025',11,6)

# FIGURA 8 - variacoes entre extremos do periodo
comparacao <- dcast(estat[NU_ANO %in% c(2020,2025)], REGIAO+AREA+AREA_LABEL~NU_ANO,value.var='MEDIA')
comparacao[, VARIACAO := `2025`-`2020`]
g <- ggplot(comparacao,aes(x=AREA_LABEL,y=REGIAO,fill=VARIACAO)) +
  geom_tile(color='white',linewidth=.8) +
  geom_text(aes(label=sprintf('%+.1f',VARIACAO)),size=3.8) +
  scale_fill_gradient2(low='#b24c4c',mid='white',high='#287e70',midpoint=0) +
  labs(title='Variação das médias regionais de 2020 a 2025',
       subtitle='Diferença em pontos: média de 2025 menos média de 2020',
       x='Área',y='Região',fill='Pontos') +
  base_tema + theme(axis.text.x=element_text(angle=20,hjust=1))
salvar(g,'08_variacao_2020_2025',11,6)

# FIGURA 9 - amplitude entre maior e menor media por ano e area
amplitudes <- estat[,.(MAIOR=max(MEDIA), MENOR=min(MEDIA)),by=.(NU_ANO,AREA,AREA_LABEL)]
amplitudes[, AMPLITUDE := MAIOR-MENOR]
g <- ggplot(amplitudes,aes(x=NU_ANO,y=AMPLITUDE,color=AREA_LABEL,group=AREA_LABEL)) +
  geom_line(linewidth=1) + geom_point(size=2.4) +
  scale_x_continuous(breaks=2020:2025) +
  labs(title='Distância entre a maior e a menor média regional',
       subtitle='Amplitude das cinco médias regionais em cada área e ano',
       x='Ano',y='Diferença em pontos',color='Área') +base_tema
salvar(g,'09_amplitude_regional')

# FIGURA 10 - variacao de amplitude entre 2020 e 2025
ampcomp <- dcast(amplitudes[NU_ANO %in% c(2020,2025)], AREA_LABEL~NU_ANO,value.var='AMPLITUDE')
ampcomp[, VARIACAO := `2025`-`2020`]
g <- ggplot(ampcomp,aes(x=AREA_LABEL,y=VARIACAO,fill=VARIACAO>0)) +
  geom_col(width=.65,show.legend=FALSE) +
  geom_hline(yintercept=0,color='gray35')+
  geom_text(aes(label=sprintf('%+.2f',VARIACAO)),vjust=ifelse(ampcomp$VARIACAO>0,-.4,1.4),size=4)+
  scale_fill_manual(values=c('TRUE'='#bb675b','FALSE'='#318779'))+
  labs(title='Mudança na distância entre médias regionais',
       subtitle='Amplitude em 2025 menos amplitude em 2020; valores em pontos',
       x='Área',y='Variação da amplitude (pontos)')+base_tema+
  theme(axis.text.x=element_text(angle=15,hjust=1))+
  expand_limits(y=c(-6,20))
salvar(g,'10_mudanca_amplitude',11,6)

# FIGURA 11 - variacoes ano a ano mais extremas de cada area
setorder(estat,AREA,REGIAO,NU_ANO)
estat[, VARIACAO_ANUAL := MEDIA-shift(MEDIA),by=.(AREA,REGIAO)]
variacoes <- estat[!is.na(VARIACAO_ANUAL)]
extremos <- rbindlist(list(
  variacoes[,.SD[which.max(VARIACAO_ANUAL)],by=AREA],
  variacoes[,.SD[which.min(VARIACAO_ANUAL)],by=AREA]
))
extremos[, TIPO := ifelse(VARIACAO_ANUAL>0,'Maior alta','Maior queda')]
extremos[, ROTULO := paste0(as.character(REGIAO),' (',NU_ANO-1,'–',NU_ANO,')')]
g <- ggplot(extremos,aes(x=AREA_LABEL,y=VARIACAO_ANUAL,fill=TIPO))+
  geom_col(width=.65,position=position_dodge(width=.7))+
  geom_text(aes(label=paste0(sprintf('%+.1f',VARIACAO_ANUAL),'\n',ROTULO)),
            size=3, vjust=ifelse(extremos$VARIACAO_ANUAL>0,-.15,1.1))+
  scale_fill_manual(values=c('Maior alta'='#28816f','Maior queda'='#b75e58'))+
  labs(title='Maiores altas e quedas anuais por área',
       subtitle='Maior variação positiva e negativa observada entre regiões e anos consecutivos',
       x='Área',y='Variação da média (pontos)',fill='Tipo') + base_tema+
  theme(axis.text.x=element_text(angle=15,hjust=1))+
  expand_limits(y=c(-58,54))
salvar(g,'11_extremos_anuais',12,7)

# FIGURA 12 - ranking regional 2025 sem implicar causalidade
ultimo[, POSICAO := frank(-MEDIA,ties.method='min'),by=AREA]
g <- ggplot(ultimo,aes(x=AREA_LABEL,y=REGIAO,fill=POSICAO))+
  geom_tile(color='white',linewidth=.9)+geom_text(aes(label=POSICAO),size=4.2)+
  scale_fill_gradient(low='#8fb6d1',high='#e3e9ed',breaks=1:5)+
  labs(title='Posição das regiões por média — 2025',
       subtitle='1 = maior média da área; 5 = menor média da área',
       x='Área',y='Região',fill='Posição') +base_tema+
  theme(axis.text.x=element_text(angle=20,hjust=1))
salvar(g,'12_ranking_2025',11,6)

# Exportar tabelas pequenas que ajudam a revisar os numeros do Word
fwrite(volume,'figuras_relatorio/tabela_volume_tratamento.csv')
fwrite(comparacao[,.(REGIAO,AREA,MEDIA_2020=`2020`,MEDIA_2025=`2025`,VARIACAO)],
       'figuras_relatorio/tabela_variacao_2020_2025.csv')
fwrite(amplitudes,'figuras_relatorio/tabela_amplitudes_anuais.csv')
fwrite(extremos[,.(AREA,REGIAO,NU_ANO,TIPO,VARIACAO_ANUAL)],
       'figuras_relatorio/tabela_extremos_anuais.csv')
cat('\nCONCLUIDO: 12 imagens PNG e 4 CSVs auxiliares na pasta figuras_relatorio.\n')
