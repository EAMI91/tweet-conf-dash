# librerías
library(tidytext)

# Frecuencias
# tb con una columna llamada Texto que contiene el texto a analizar.
# filtros
# absoluta, relativa
# sw: Tibble de stopwords con nombre de columna igual al token (Palabra, Enunciado, etc.)
# grupos
# Falta filtros, top_n, arrange, sw.
discursera_frec <- function(tb, relativo=F, sw=NULL, grupos="", unnested=T, token=NULL,
                            filtrar_base="", filtrar_resultado="", ordenar="", top=0){
  if(filtrar_base != "") tb %<>% filter(!! rlang::parse_expr(filtrar_base)) 
  if(! unnested){
    token_ingles<-switch (token,
      "Palabra" = "words", "Enunciado" = "sentences"
    )
    tb %<>% unnest_tokens_(output = "Palabra", input = "Texto", token = token_ingles)
  }
  if(! sw %>% is.null()) tb %<>% anti_join(sw)
  tb %<>% group_by(!!! rlang::parse_exprs(grupos))
  tb %<>% summarise(n=n())
  if(relativo) tb %<>% mutate(n=round(100*n/sum(n),2))
  if(filtrar_resultado != "") tb %<>% filter(!! rlang::parse_expr(filtrar_resultado)) 
  if(ordenar != "") tb %<>% arrange(!!! rlang::parse_exprs(ordenar)) 
  if(top > 0) tb %<>% top_n(n=top, wt=n) 
  return(tb)
}


# Palabras clave
discursera_clave <-function(tb, sw=NULL, grupos="", unnested=T, token=NULL, filtrar_base="",
                            filtrar_resultado="", top=0, ordenar=""){
  if(filtrar_base != "") tb %<>% filter(!! rlang::parse_expr(filtrar_base)) 
  if(unnested==F){
    tb %<>% unnest_tokens_(output = "Palabra", input = "Texto", token = "words")
  }
  if(! sw %>% is.null()) tb %<>% anti_join(sw) 
  aux1 <- tb %>% group_by(!!! rlang::parse_exprs(grupos)) %>% group_by(Palabra, add = T) %>% summarise(o=n()) 
  aux2 <- tb %>%  group_by(!!! rlang::parse_exprs(grupos)) %>% summarise(n=n()) 
  aux3 <-tb %>% group_by(Palabra) %>% summarise(e=n()) %>% mutate(e=e/sum(e))
  tb <- left_join(aux1, aux2) %>% left_join(aux3) %>%
    mutate(e=e*n) %>% select(-n) %>% mutate(G2=-2*o*log(e/o)) %>% 
    mutate(Rango=rank(-G2,ties.method = "min"))
  if( top > 0) tb %<>% top_n(n=top, wt = G2)
  if(ordenar != "") tb %<>% arrange(!!! rlang::parse_exprs(ordenar))   
  if(filtrar_resultado != "") tb %<>% filter(!! rlang::parse_expr((filtrar_resultado))) 
  return(tb)
}

# Similitudes
discursera_similitudes <-function(tb, vector,elemento ,sw=NULL, filtrar_base="", 
                                  filtrar_resultado="", unnested=T, token="", frecuencia_min=0, cobertura_min=0,
                                  especial="", ordenar=""){
  if(filtrar_base != "") tb %<>% filter(!! rlang::parse_expr(filtrar_base)) 
  if(vector == "Palabra") documento <- elemento
  else documento <- vector
  if(unnested==F){
    token_ingles<-switch (token,
                          "Palabra" = "words", "Enunciado" = "sentences"
    )
    tb %<>% unnest_tokens_(output = token, input = "Texto", token = token_ingles)
    if(token == "Enunciado") tb %<>% rename(auxiliar= Enunciado) %>% mutate(Enunciado=seq(nrow(.))) %>% 
      unnest_tokens_(output = "Palabra", input = "auxiliar", token = "words")
    }
  if(! sw %>% is.null()) tb %<>% anti_join(sw) 
  tb %<>% group_by(!! rlang::parse_expr(vector), !! rlang::parse_expr(elemento)) %>%
    summarise(n=n()) %>% ungroup() %>% 
    bind_tf_idf(document =!! rlang::parse_expr(documento),term = Palabra, n) %>% na.omit()
  if(frecuencia_min >0){
    palabras_frecuentes <- tb %>% group_by(!! rlang::parse_expr(vector)) %>% summarise(n=sum(n)) %>% filter(n>frecuencia_min) %>% 
                              pull(!! rlang::parse_expr(vector)) %>% unique()
    tb %<>% filter(!! rlang::parse_expr(vector) %in% palabras_frecuentes)
  }
  if(cobertura_min >0) tb %<>% filter(1/exp(idf) > cobertura_min)
  if(especial != ""){
    e_especial<-tb %>% filter(!! rlang::parse_expr(vector) %in% especial) %>%
      pull(!! elemento) %>% unique()
    tb %<>% filter(!! rlang::parse_expr(elemento)%in% e_especial)
  }
  tb %<>% rename("vector" := !!quo_name(vector), "elemento" := !!quo_name(elemento))
  tb %<>% pairwise_similarity(item=vector, feature= elemento, value=tf_idf) %>%
    rename(!! paste0(quo_name(vector),1)  := "item1",
           !! paste0(quo_name(vector),2)  := "item2",
           similitud = similarity)
  if(filtrar_resultado != "") tb %<>% filter(!! rlang::parse_expr((filtrar_resultado)))
  if(especial != "")  tb %<>% filter(!! paste0(rlang::parse_expr(vector),1) == especial |
                                       !! paste0(rlang::parse_expr(vector),2)== especial)
  if(ordenar != "") tb %<>% arrange(!! rlang::parse_expr(ordenar))
  return(tb)
}

# Extrae el cuerpo del texto de media solutions
discursera_media <- function(tb, hipervinculo="Link"){
  hipervinculo <- tb %>% pull(!! rlang::parse_expr(hipervinculo))
  lectura <- function(hipervinculo){
    pagina <- readLines(hipervinculo, encoding = "CP1252")
    inicio <- pagina %>% grep(pattern = 'id=\"notaTexto\"')
    fin <- pagina %>% grep(pattern = "<hr>")
    fin <- fin[(fin > inicio)] %>% min()
    pagina[inicio:fin] %>% paste(collapse = "") %>%  
      gsub(pattern='<span id=\"notaTexto\">|<p>|</p>|</span>|<span>|<hr>', replacement="")
    
  }
  lectura %<>% possibly(otherwise=NA)
  tb %<>% mutate(Texto=map_chr(.x=hipervinculo, .f = ~lectura(.x)))
}
# Collocations
discursera_collocations <- function(tb, texto, sw,n=200, solo_col=F){
  # Unigramas
  (uni <- tb %>% select_(texto) %>% 
     unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=1) %>% 
     na.omit()  %>% 
     count(Palabras) %>% 
     arrange(desc(n))) 
  # Bigramas
  (bi <- tb %>% select_(texto) %>% 
      unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=2) %>% 
      na.omit() %>% separate(col=Palabras, into=c("p1","p2"),remove = F) %>% 
      mutate(swords=(p1%in% sw$Palabra)+(p2%in% sw$Palabra)) %>% filter(swords<=0) %>%
      group_by(Palabras, p1, p2) %>% summarise(n=n()) %>%  
      arrange(desc(n)))
  # Trigrama
  (tri <- tb %>% select_(texto) %>% 
      unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=3) %>% 
      na.omit() %>% separate(col=Palabras, into=c("p1","p2","p3"),remove = F) %>% 
      mutate(swords=(p1%in% sw$Palabra)+(p2%in% sw$Palabra)+(p3%in% sw$Palabra)) %>%
      filter(swords<=1) %>% 
      group_by(Palabras, p1, p2, p3) %>% summarise(n=n()) %>%  
      arrange(desc(n)))
  # Tretragrama
  (tetra <- tb %>% select_(texto) %>% 
      unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=4) %>% 
      na.omit() %>% separate(col=Palabras, into=c("p1","p2","p3", "p4"),remove = F) %>% 
      mutate(swords=(p1%in% sw$Palabra)+(p2%in% sw$Palabra)+(p3%in% sw$Palabra)+
               (p4%in% sw$Palabra)) %>%
      filter(swords<=2) %>% 
      group_by(Palabras, p1, p2, p3, p4) %>% summarise(n=n()) %>%  
      arrange(desc(n)))
  # Pentagrama
  (penta <- tb %>% select_(texto) %>% 
      unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=5) %>% 
      na.omit() %>% separate(col=Palabras, into=c("p1","p2","p3", "p4","p5"),remove = F) %>% 
      mutate(swords=(p1%in% sw$Palabra)+(p2%in% sw$Palabra)+(p3%in% sw$Palabra)+
               (p4%in% sw$Palabra)+(p5%in% sw$Palabra)) %>%
      filter(swords<=3) %>% 
      group_by(Palabras, p1, p2, p3, p4,p5) %>% summarise(n=n()) %>%  
      arrange(desc(n)))
  # Hexagrama
  (hexa <- tb %>% select_(texto) %>% 
      unnest_tokens_(input=texto, output="Palabras",token="ngrams", n=6) %>% 
      na.omit() %>% separate(col=Palabras, into=c("p1","p2","p3", "p4","p5","p6"),remove = F) %>% 
      mutate(swords=(p1%in% sw$Palabra)+(p2%in% sw$Palabra)+(p3%in% sw$Palabra)+
               (p4%in% sw$Palabra)+(p5%in% sw$Palabra)+(p6%in% sw$Palabra)) %>%
      filter(swords<=4) %>% 
      group_by(Palabras, p1, p2, p3, p4,p5,p6) %>% summarise(n=n()) %>%  
      arrange(desc(n)))
  # Calculo
  N <- sum(uni$n)
  c2 <-bi %>% mutate(n1=uni$n[match(p1, uni$Palabras)],
                     n2=uni$n[match(p2, uni$Palabras)],
                     Dice=(2*n)/(n1+n2), PMI=log(N)+log(n)-log(n1)-log(n2)) %>%
    filter(n>1, PMI>0)%>% arrange(desc(PMI)) %>% pull(Palabras)
  
  c3 <-tri %>%  ungroup() %>% filter(n>1)%>% mutate(n1=uni$n[match(p1, uni$Palabras)],
                                                    n2=bi$n[match(paste(p1,p2), bi$Palabras)],
                                                    n3=uni$n[match(p3, uni$Palabras)],
                                                    Dice=(2*n)/(n1+n2), PMI=log(N)+log(n)-log(n2)-log(n3)) %>%
    filter(PMI>0) %>% arrange(desc(PMI)) %>% pull(Palabras)
  
  c4 <-tetra %>%  ungroup() %>% filter(n>1)%>% mutate(n1=uni$n[match(p1, uni$Palabras)],
                                                      n2=bi$n[match(paste(p1,p2,p3), tri$Palabras)],
                                                      n3=uni$n[match(p4, uni$Palabras)],
                                                      Dice=(2*n)/(n1+n2),
                                                      PMI=log(N)+log(n)-log(n2)-log(n3)) %>%
    filter(PMI>0) %>% arrange(desc(PMI)) %>% pull(Palabras)
  c5 <-penta %>%  ungroup() %>% filter(n>1)%>% mutate(n1=uni$n[match(p1, uni$Palabras)],
                                                      n2=tetra$n[match(paste(p1,p2,p3,p4), tetra$Palabras)],
                                                      n3=uni$n[match(p5, uni$Palabras)],
                                                      Dice=(2*n)/(n1+n2),
                                                      PMI=log(N)+log(n)-log(n2)-log(n3)) %>%
    filter(PMI>0) %>% arrange(desc(PMI)) %>% pull(Palabras)
  c6 <- hexa %>%  ungroup() %>% filter(n>1)%>% mutate(n1=uni$n[match(p1, uni$Palabras)],
                                                      n2=tetra$n[match(paste(p1,p2,p3,p4,p5), penta$Palabras)],
                                                      n3=uni$n[match(p6, uni$Palabras)],
                                                      Dice=(2*n)/(n1+n2),
                                                      PMI=log(N)+log(n)-log(n2)-log(n3)) %>%
    filter(PMI>0) %>% arrange(desc(PMI)) %>% pull(Palabras)
  auxiliar <- tb %>% pull(texto)
  if(length(c6)>0){
    for(i in 1:length(c6)) auxiliar <- gsub(pattern = c6[i], x=auxiliar,
                                            replacement = gsub(x = c6[i], pattern = " ",
                                                               replacement = "zxw"))
  }
  if(length(c5)>0){
    for(i in 1:length(c5)) auxiliar<- gsub(pattern = c5[i], x=auxiliar,
                                           replacement = gsub(x = c5[i], pattern = " ",
                                                              replacement = "zxw"))
  }
  if(length(c4)>0){
    for(i in 1:length(c4)) auxiliar <- gsub(pattern = c4[i], x=auxiliar,
                                            replacement = gsub(x = c4[i], pattern = " ",
                                                               replacement = "zxw"))
  }
  
  if(length(c3)>0){
    for(i in 1:length(c3)) auxiliar <- gsub(pattern = c3[i], x=auxiliar,
                                            replacement = gsub(x = c3[i], pattern = " ",
                                                               replacement = "zxw"))
  }
  if(length(c2)){
    for(i in 1:length(c2)) auxiliar <- gsub(pattern = c2[i], x=auxiliar,
                                            replacement = gsub(x = c2[i], pattern = " ",
                                                               replacement = "zxw"))
  }
  tb <- tibble(texto=auxiliar)
  tb %<>% select(texto) %>% unnest_tokens(input=texto, output=Palabras,token="ngrams", n=1) %>%
    na.omit()  %>%
    filter(!Palabras %in% sw$Palabra) %>%
    count(Palabras)
  if(solo_col) tb %<>% filter(grepl("zxw",Palabras))
  tb %>% mutate(Palabras=gsub(x=Palabras, replacement = " ", pattern="zxw")) %>%
    arrange(desc(n)) %>%  top_n(wt = n, n)
  
}
# Clasificación



