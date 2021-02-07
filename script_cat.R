library(pdftools)
library(tm)
library(SnowballC)
library(stringr)
library(fastmatch)
library(wordcloud)
library(tidytext)
library(tidyverse)
library(patchwork)
library(ggwordcloud)
library(factoextra)
library(igraph)
library(ggraph)

cs <- as.character(read.table("cs.txt", sep = "\t", encoding = "UTF-8")[,1])

pp <- as.character(read.table("pp_sincom.txt", sep = "\t", encoding = "UTF-8")[,1])

cup <- tabulizer::extract_text("cup.pdf", encoding = "UTF-8")
cup <- paste0(str_sub(cup, 1, str_locate(cup, "76 Continguts\r\nCONTINGUTS")[1]),
              str_sub(cup, str_locate(cup, "CON\r\nTIN\r\nGUTS")[2]+1, -1))
cup <- str_replace_all(cup, "-\r\n", "")
cup <- str_replace_all(cup, "–\r\n", "")
cup <- str_replace_all(cup, "\r\n", " ")
cup <- str_remove_all(cup, "UN PLA DE RESCAT SOCIAL PER GARANTIR LA VIDA")
cup <- str_remove_all(cup, "CANVIEM EL MODEL, PER GUANYAR")
cup <- str_remove_all(cup, "CONFRONTACIÓ DEMOCRÀTICA PER LA INDEPENDÈNCIA")

pod <- tabulizer::extract_text("ecp.pdf", encoding = "UTF-8")
pod <- str_sub(pod, str_locate(pod, "3\r\nIntroducció")[2]+1, -1)
pod <- str_remove_all(pod, "\r\nEn Comú Podem    ")
pod <- str_remove_all(pod, "Programa Eleccions al Parlament de Catalunya 2021")
pod <- str_replace_all(pod, "-\r\n", "")
pod <- str_replace_all(pod, "–\r\n", "")
pod <- str_replace_all(pod, "\r\n", " ")

psoe <- pdf_text("psc.pdf")
psoe <- psoe[-c(1:2)]
psoe <-  str_replace_all(psoe, "\r\n", " ")
psoe <- str_remove_all(psoe, "DOCUMENT PROGRAMÀTIC A DEBAT")
psoe <- str_remove_all(psoe, "FEDERALISME I BON GOVERN")

erc <- pdf_text("erc.pdf")
erc <- erc[-c(1:3)]
erc <- str_remove_all(erc, "Eleccions al Parlament de Catalunya 2021 / Programa electoral")
erc <-  str_replace_all(erc, "\r\n", " ")

junts <- pdf_text("junts.pdf")
junts <- junts[-c(1:7)]
junts <-  str_replace_all(junts, "\r\n", " ")
junts <-  str_replace_all(junts, "\t", " ")

pdecat <- tabulizer::extract_text("pdecat.pdf", encoding = "UTF-8")
pdecat <- str_sub(pdecat, start = 325, end = -1)
pdecat <- str_replace_all(pdecat, "-\r\n", "")
pdecat <- str_replace_all(pdecat, "–\r\n", "")
pdecat <- str_replace_all(pdecat, "\r\n", " ")

preprocesamiento <- function(x) removePunctuation(removeNumbers(tolower(x)))

stopwords_cat <- read.table(
  "https://raw.githubusercontent.com/stopwords-iso/stopwords-ca/master/stopwords-ca.txt",
  encoding = "UTF-8", stringsAsFactors = F)
stopwords_cat <- stopwords_cat[,1]
stopwords_cat <- preprocesamiento(stopwords_cat)

preproc <- function(texto){
  #quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))
  
  texto2 <- preprocesamiento(texto)
  for(i in 1:length(stopwords_cat)) texto2 <- str_replace_all(texto2, paste0(" ",stopwords_cat[i]," "), " ")
  texto2 <- removeWords(texto2, stopwords("catalan"))
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")
  texto2 <- str_remove_all(texto2, "¦")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "—")
  texto2 <- str_remove_all(texto2, "-")
  texto2 <- str_replace_all(texto2, "·l", "l")
  texto2 <- str_remove_all(texto2, "<U+200B>")
  texto2 <- str_remove_all(texto2, "€")
  texto2 <- str_remove_all(texto2, "´")
  texto2 <- str_replace_all(texto2, "’", " ")
  texto2 <- str_remove_all(texto2, "●")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "–")

  DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(texto2))))
  DTM1<-as.matrix(DTM1)
  DTM1<-as.data.frame(DTM1)
  return(DTM1)
}

d_pp <- preproc(pp)
tail(sort(apply(d_pp,2,sum)),20)

d_psoe <- preproc(psoe)
tail(sort(apply(d_psoe,2,sum)),20)

d_cup <- preproc(cup)
tail(sort(apply(d_cup,2,sum)),20)

d_pod <- preproc(pod)
tail(sort(apply(d_pod,2,sum)),20)

d_cs <- preproc(cs)
tail(sort(apply(d_cs,2,sum)),20)

d_junts <- preproc(junts)
tail(sort(apply(d_junts,2,sum)),20)

d_pdecat <- preproc(pdecat)
tail(sort(apply(d_pdecat,2,sum)),20)

d_erc <- preproc(erc)
tail(sort(apply(d_erc,2,sum)),20)

palabras <- c(colnames(d_pp), colnames(d_psoe), colnames(d_cup), colnames(d_pod),
              colnames(d_cs), colnames(d_junts), colnames(d_pdecat), colnames(d_erc))
palabras <- names(table(palabras))

palabrizador <- function(d, palabras = palabras){
  ret <- merge(data.frame(palabras = palabras), data.frame(palabras = colnames(d), freq = apply(d,2,sum)), all.x = T)
  return(ret)
}

d2pp <- palabrizador(d_pp, palabras)
d2psoe <- palabrizador(d_psoe, palabras)
d2cup <- palabrizador(d_cup, palabras)
d2pod <- palabrizador(d_pod, palabras)
d2cs <- palabrizador(d_cs, palabras)
d2junts <- palabrizador(d_junts, palabras)
d2pdecat <- palabrizador(d_pdecat, palabras)
d2erc <- palabrizador(d_erc, palabras)

d <- data.frame(palabras = d2pp[,1], pp = d2pp[,2], psoe = d2psoe[,2], cup = d2cup[,2], 
                pod = d2pod[,2], cs = d2cs[,2], junts = d2junts[,2], 
                pdecat = d2pdecat[,2], erc = d2erc[,2])

apply(d[,2:9], 2, sum, na.rm = T)
#pp   psoe    cup    pod     cs  junts pdecat    erc 
#1530  30015  29785  50186  15290  44378  30012  41422 

data.frame(part = c("PP", "PSC", "CUP", "ECP", "Cs", "Junts", "PDeCAT", "ERC"),
           term = c(1530, 30015, 29785, 50186, 15290, 44378, 30012, 41422)) %>%
  mutate(part = factor(part, levels = part[order(term)])) %>%
  ggplot(aes(x = part, y = term, label = term)) + geom_col(fill = "dodgerblue") +
  geom_text(nudge_y = 1500, family = "Liberation Sans", size = 5) +
  theme_minimal(base_size = 15) +
  theme(text = element_text(family = "Liberation Sans")) +
  labs(x = "Número de palabras no vacías", y = "Partido",
       title = "Número de palabras no vacías en cada programa electoral de las elecciones catalanas del 14F",
       caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros")
ggsave("numpalabras.png", dpi = 300)

for(i in 2:9){
  d[which(is.na(d[,i])),i] <- 0
  d[,i] <- d[,i]/sum(d[,i])
}

#Aquí guardo el data.frame con las frecuencias relativas (sería como un check-point)
write.csv(d, "terminos.csv", row.names = F)

d_rank <- d
for(i in 2:9){
  d_rank[,i] <- nrow(d) + 1 - rank(d_rank[,i], ties.method = "last")
}
row.names(d_rank) <- d_rank[,1]
d_rank["covid",]
d_rank["científiques",]

g1 <- d %>% filter(pp >= quantile(d$pp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PP") + theme(title = element_text(colour = "dodgerblue"),
                             plot.title = element_text(hjust = 0.5))
g2 <- d %>% filter(psoe >= quantile(d$psoe,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PSC") + theme(title = element_text(colour = "red2"),
                               plot.title = element_text(hjust = 0.5),
                               plot.subtitle = element_text(hjust = 0.5))
g3 <- d %>% filter(cs >= quantile(d$cs,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = cs)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Ciudadanos") + theme(title = element_text(colour = "orange"),
                                     plot.title = element_text(hjust = 0.5))
g4 <- d %>% filter(pod >= quantile(d$pod,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "En Comú Podem") + theme(title = element_text(colour = "purple"),
                              plot.title = element_text(hjust = 0.5))
g5 <- d %>% filter(junts >= quantile(d$junts,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = junts)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Junts") + theme(title = element_text(colour = "cyan3"),
                                        plot.title = element_text(hjust = 0.5))
g6 <- d %>% filter(pdecat >= quantile(d$pdecat,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pdecat)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PDeCAT") + theme(title = element_text(colour = "blue"),
                                        plot.title = element_text(hjust = 0.5))
g7 <- d %>% filter(erc >= quantile(d$erc,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = erc)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "ERC") + theme(title = element_text(colour = "goldenrod1"),
                                        plot.title = element_text(hjust = 0.5))
g8 <- d %>% filter(cup >= quantile(d$cup,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = cup)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "CUP") + theme(title = element_text(colour = "yellow"),
                              plot.title = element_text(hjust = 0.5))

(g1 + g2 + g3 + g4) / (g5 + g6 + g7 + g8) + plot_annotation(title = "Términos más repetidos en los programas electorales de las elecciones catalanas del 14F de los principales partidos",
                                                    caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros")
ggsave("wordcloud.png", dpi = 300, width = 12, height = 13)

d$media <- apply(d[,2:9],1,mean)
d_comp <- d[which(d$media > quantile(d$media, 0.95)),]

par(mfrow=c(2,4))
barplot(tail(sort(d_comp$pp/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pp/d_comp$media)],20),
        horiz = TRUE, las=1, xlim = c(0,8), 
        main = "Palabras más repetidas en el programa del PP\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$psoe/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$psoe/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa del PSC\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$cs/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$cs/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa de Ciudadanos\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$pod/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pod/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa de En Comú Podem\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$junts/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$junts/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa de Junts\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$pdecat/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pdecat/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa del PDeCAT\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$erc/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$erc/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa de ERC\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")
barplot(tail(sort(d_comp$cup/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$cup/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,8),
        main = "Palabras más repetidas en el programa de las CUP\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 8 programas")

#### Análisis de componentes principales ####

rownames(d_comp) <- d_comp[,1]
colnames(d_comp)[2:9] <- c("PP","PSOE","CUP","ECP", "Cs", "Junts", "PDeCAT", "ERC")
res.pca <- FactoMineR::PCA(d_comp[,2:9])
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros",
  x = "Componente 1 (70.34% de varianza explicada)", y = "Componente 2 (12.25% de varianza explicada)"
)
ggsave("comp1.png", dpi = 300, width = 8.4*1.3, height = 6.4*1.25)
ind <- get_pca_ind(res.pca)

fviz_pca_biplot(res.pca, repel = F) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros",
  x = "Componente 1 (70.34% de varianza explicada)", y = "Componente 2 (12.25% de varianza explicada)"
)
ggsave("comp2.png", dpi = 300, height = 12, width = 14)

#### Escalamiento multidimensional ####

mds <- dist(t(d_comp[,2:9]))

fit <- cmdscale(mds,eig=TRUE, k=2)
puntos <- as.data.frame(fit$points)
ggplot(puntos, aes(x = V1, y = V2, label = row.names(fit$points))) + 
  geom_point() + ggrepel::geom_label_repel(size = 6.5) +
  theme_classic(base_size = 15) + labs(x = "Dimensión 1 (50.1% suma autovalores)",
                                       y = "Dimensión 2 (16.1% suma autovalores)",
                                       title = "Representación bidimensional de los programas de cada partido mediante\nescalamiento multidimensional (MDS) según las distancias entre ellos",
                                       subtitle = "Distancias obtenidas a partir de la distancia euclídea entre frecuencias relativas de palabras",
                                       caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros")

ggsave("mds.png", dpi = 300)

#### N-GRAMS ####

preproc_tidy <- function(texto){
  #quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))
  
  texto2 <- preprocesamiento(texto)
  for(i in 1:length(stopwords_cat)) texto2 <- str_replace_all(texto2, paste0(" ",stopwords_cat[i]," "), " ")
  texto2 <- removeWords(texto2, stopwords("catalan"))
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")
  texto2 <- str_remove_all(texto2, "¦")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "—")
  texto2 <- str_remove_all(texto2, "-")
  texto2 <- str_replace_all(texto2, "·l", "l")
  texto2 <- str_remove_all(texto2, "<U+200B>")
  texto2 <- str_remove_all(texto2, "€")
  texto2 <- str_remove_all(texto2, "´")
  texto2 <- str_replace_all(texto2, "’", " ")
  texto2 <- str_remove_all(texto2, "●")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "–")

  return(texto2)
}

pp_2gram <- preproc_tidy(pp) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

cs_2gram <- preproc_tidy(cs) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

psoe_2gram <- preproc_tidy(psoe) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

pod_2gram <- preproc_tidy(pod) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

junts_2gram <- preproc_tidy(junts) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

pdecat_2gram <- preproc_tidy(pdecat) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

erc_2gram <- preproc_tidy(erc) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

cup_2gram <- preproc_tidy(cup) %>% tidy() %>% unnest_tokens(bigram, x, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% mutate(programa = "PSC")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "En Comú Podem")) %>%
  bind_rows(junts_2gram %>% mutate(programa = "Junts")) %>%
  bind_rows(pdecat_2gram %>% mutate(programa = "PDeCAT")) %>%
  bind_rows(erc_2gram %>% filter(str_detect(bigram, "b ")==F & str_detect(bigram, " b")==F) %>% 
              filter(str_detect(bigram, "c ")==F & str_detect(bigram, " c")==F) %>% mutate(programa = "ERC")) %>%
  bind_rows(cup_2gram %>% mutate(programa = "CUP")) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                           substr(palabras, 1, 7) == "cientif" |
                                           substr(palabras, 1, 4) == "cièn"), "palabras"]) |
           word2 %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                             substr(palabras, 1, 7) == "cientif" |
                                             substr(palabras, 1, 4) == "cièn"), "palabras"])) %>%
  select(word1, word2, programa, n) %>% pivot_longer(-c(programa, n), names_to = "orden", values_to = "word") %>% group_by(word, programa) %>% summarise(n = sum(n)) %>%
  filter(word %in% as.character(d[which(substr(palabras, 1, 7) == "científ" |
                                          substr(palabras, 1, 7) == "cientif" |
                                          substr(palabras, 1, 4) == "cièn"), "palabras"])==F) %>%
  ggplot(., aes(x = 1, y = 1, size = n, label = word)) +
  ggrepel::geom_text_repel(segment.size = 0, segment.alpha = 0) +
  scale_size(range = c(3, 9), guide = FALSE) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_line(colour = "white", size=1),
        axis.text = element_text(colour = "white", size = 0),
        axis.ticks = element_line(colour = "white", size = 0)) +
  facet_wrap(~programa) +
  labs(title = "Wordcloud de los términos que acompañan a aquellos relacionados con la ciencia en cada programa de las elecciones catalanas del 14F",
       x = "", y = "",
       subtitle = "A más grande el término, más aparece acompañando a términos relacionados con la ciencia.\nNo se han filtrado por número de apariciones, por lo que los wordclouds son totalmente exhaustivos.",
       caption = "Programas electorales accedidos a partir del listado de Betevé (https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros")
ggsave("words_ciencia.png", dpi = 300, width = 18, height = 10)

pp_2gram %>% mutate(programa = "PP") %>%
  bind_rows(psoe_2gram %>% filter(str_detect(bigram, "p ")==F & str_detect(bigram, " p")==F) %>% mutate(programa = "PSC")) %>%
  bind_rows(cs_2gram %>% mutate(programa = "Ciudadanos")) %>%
  bind_rows(pod_2gram %>% mutate(programa = "En Comú Podem")) %>%
  bind_rows(junts_2gram %>% mutate(programa = "Junts")) %>%
  bind_rows(pdecat_2gram %>% mutate(programa = "PDeCAT")) %>%
  bind_rows(erc_2gram %>% filter(str_detect(bigram, "b ")==F & str_detect(bigram, " b")==F) %>% 
              filter(str_detect(bigram, "c ")==F & str_detect(bigram, " c")==F) %>% mutate(programa = "ERC")) %>%
  bind_rows(cup_2gram %>% mutate(programa = "CUP")) %>%
  group_by(programa) %>% mutate(n = n/sum(n)) %>% ungroup() %>%
  group_by(bigram) %>% mutate(media = mean(n)) %>% ungroup() %>%
  mutate(lift = n/media) %>% arrange(desc(lift)) %>%
  group_by(programa) %>% slice_max(lift, n = 10) %>%
  ungroup() %>%
  ggplot(aes(lift, fct_reorder(bigram, lift), fill = programa)) +
  geom_col(show.legend = FALSE) +
  theme_minimal(base_size = 16) +
  facet_wrap(~programa, ncol = 2, scales = "free") +
  labs(x = "Nº de veces que aparece dividido entre la media de los 8 programas", y = NULL,
       title = "Bigramas más repetidos en el programa de cada partido\nen comparación con el resto de partidos",
       caption = "Programas electorales accedidos a partir del listado de Betevé\n(https://beteve.cat/politica/programes-electorals-2021-catalunya-eleccions-14f/) | @Picanumeros")
  # bind_tf_idf(bigram, programa, n) %>%
  # arrange(desc(tf_idf))
ggsave("bigramas.png", dpi = 300, width = 10, height = 12)

gg1 <- pp_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(pp_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral del PP para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_pp.png", dpi = 300, width = 15, height = 11)
gg2 <- psoe_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "p" & word2 != "p") %>%
  filter(n >= quantile(psoe_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral del PSC para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_psc.png", dpi = 300, width = 15, height = 11)
gg3 <- cs_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(cs_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral de Ciudadanos para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_cs.png", dpi = 300, width = 15, height = 11)

gg4 <- pod_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(pod_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral de En Comú Podem para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_pod.png", dpi = 300, width = 15, height = 11)

gg5 <- junts_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(junts_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
    show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(.15, "inches")), 
    end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral de Junts para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_junts.png", dpi = 300, width = 15, height = 11)

gg6 <- pdecat_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(pdecat_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral del PDeCAT para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_pdecat.png", dpi = 300, width = 15, height = 11)

gg7 <- erc_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "b" & word2 != "b" & word1 != "c" & word2 != "c") %>%
  filter(n >= quantile(erc_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral de ERC para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_erc.png", dpi = 300, width = 15, height = 11)

gg8 <- cup_2gram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n >= quantile(cup_2gram$n, 0.995)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = log(n)), 
                 show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.15, "inches")), 
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "dodgerblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3, col = "red") + theme_void(base_size = 16) +
  labs(title = "Grafos de co-ocurrencia de términos en el programa electoral de la CUP para las elecciones catalanas del 14F",
       subtitle = "Obtenido con el 0.5% de bigramas que más aparecen",
       caption = "@Picanumeros")
ggsave("g_cup.png", dpi = 300, width = 15, height = 11)
