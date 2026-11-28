# =====================================================================
# Chave interativa de Lumbrineridae da Bacia de Pelotas
# R-base + pacote shiny.
# Para rodar: deixe este arquivo na mesma pasta que os cinco arquivos de
# dados e use shiny::runApp("caminho/da/pasta").
#   matriz_lumbrineridae.csv  caracteres x táxons
#   taxons.csv                um táxon por linha: número no WoRMS, figura
#                             (em www/taxons), legenda, descrição e aviso
#   glossario.csv             um verbete por caráter
#   pranchas.csv              figuras do glossário, com créditos
#   chave_dicotomica.csv      chave dicotômica (aba Chave linear), um lead
#                             por linha
# =====================================================================

library(shiny)

# ---------------------------------------------------------------------
# 1. DADOS
# A matriz tem o formato do monographaR::interactiveKey():
# linhas = caracteres; coluna 1 = grupo; coluna 2 = caráter;
# demais colunas = táxons. Estados separados por "/" = polimorfismo.
# Célula vazia = dado desconhecido (não elimina o táxon).
# "não se aplica" = caráter dependente (ex.: detalhes das brânquias em
# táxon sem brânquias); não aparece como opção, mas elimina o táxon
# quando o usuário escolhe um estado nesse caráter.
# ---------------------------------------------------------------------
dat <- read.csv("matriz_lumbrineridae.csv", check.names = FALSE,
                na.strings = c("", "NA"), fileEncoding = "UTF-8",
                stringsAsFactors = FALSE)

grupo   <- dat[[1]]
carater <- dat[[2]]
mat     <- as.matrix(dat[, -(1:2)])              # caracteres x táxons

taxons <- gsub(".", " ", colnames(mat), fixed = TRUE)   # "Ninoe.gayheadia" -> "Ninoe gayheadia"
taxons <- sub(" sp$", " sp.", taxons)
colnames(mat) <- taxons

rotulo <- paste(grupo, carater, sep = ": ")      # nome completo do caráter
n_car  <- nrow(mat)
n_tax  <- ncol(mat)
ids    <- paste0("car", seq_len(n_car))          # nome de cada controle na tela

# Dados de cada táxon: número do registro no WoRMS (AphiaID), nome como
# está lá e autoridade. Um táxon que não estiver no arquivo fica sem link.
tax <- read.csv("taxons.csv", fileEncoding = "UTF-8", colClasses = "character")

# Glossário. Nos textos dos dois arquivos valem três convenções:
#   *Nome*       vira itálico
#   Prancha 3    vira um link que abre a prancha 3
#   " | "        separa os itens de uma lista (estados, painéis da legenda)
glo <- read.csv("glossario.csv", fileEncoding = "UTF-8", colClasses = "character")
pr  <- read.csv("pranchas.csv",  fileEncoding = "UTF-8", colClasses = "character")

# Linha do glossário que corresponde a cada caráter da matriz (NA = sem verbete)
lin_glo <- match(rotulo, paste(glo$Group, glo$Character, sep = ": "))

# Chave dicotômica (aba Chave linear). Um lead (metade de um dístico) por linha:
#   distico  número do dístico e letra do lead (1a, 1b, 2a...)
#   texto    caracteres do lead (aceita *Nome* em itálico)
#   destino  número do dístico seguinte ou nome do táxon
#   genero   gênero ao qual o dístico seguinte leva (pode ficar vazio)
chv <- read.csv("chave_dicotomica.csv", fileEncoding = "UTF-8", colClasses = "character")

# ---------------------------------------------------------------------
# 2. FUNÇÕES
# ---------------------------------------------------------------------

# Estados de uma célula: "cônico/arredondado" -> c("cônico", "arredondado")
estados <- function(x) {
  if (is.na(x)) character(0) else strsplit(x, "/", fixed = TRUE)[[1]]
}

# Estados possíveis de cada caráter (todos os que aparecem na linha)
est_car <- lapply(seq_len(n_car), function(i) {
  setdiff(sort(unique(unlist(lapply(mat[i, ], estados)))), "não se aplica")
})

# Quais táxons combinam com os estados escolhidos (sel) no caráter i?
# Combina se o dado é desconhecido ou se tem pelo menos um estado escolhido.
compativel <- function(i, sel) {
  vapply(seq_len(n_tax), function(j) {
    is.na(mat[i, j]) || any(estados(mat[i, j]) %in% sel)
  }, logical(1))
}

# Nome científico em itálico, exceto o "sp." final, que fica em redondo
# (como manda a convenção): "Abyssoninoe sp." -> <em>Abyssoninoe</em> sp.
nome_italico <- function(x) {
  sub("^(.*?)( sp\\.)?$", "<em>\\1</em>\\2", x)
}

# Nome do táxon em itálico e como link. O clique não abre outra página:
# só avisa o servidor (input$ver_taxon) de qual táxon foi clicado.
link_taxon <- function(x) {
  paste0("<a href='#' onclick=\"Shiny.setInputValue('ver_taxon', '", x,
         "', {priority: 'event'}); return false;\">", nome_italico(x), "</a>")
}

# Troca os caracteres especiais do HTML (<, >, &) para que um texto vindo
# dos arquivos apareça na tela exatamente como foi escrito.
esc <- htmltools::htmlEscape

# Link para um ponto do glossário. "id" é o nome do ponto: "glo5" é o
# verbete do 5º caráter e "prancha2" é a prancha 2.
link_glossario <- function(id, texto, classe = "") {
  paste0("<a href='#' class='", classe, "' title='Ver no glossário' ",
         "onclick=\"irGlossario('", id, "'); return false;\">", texto, "</a>")
}

# Comando (JavaScript) que avisa o servidor para abrir a prancha n
abre_prancha <- function(n) {
  paste0("Shiny.setInputValue('ver_prancha', '", n, "', {priority: 'event'}); return false;")
}

# Texto do glossário -> HTML, aplicando as convenções: itálico e links
formatar <- function(x) {
  x <- esc(x)
  x <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", x)
  gsub("Prancha ([0-9]+)",
       paste0("<a href='#' onclick=\"", abre_prancha("\\1"), "\">Prancha \\1</a>"), x)
}

# Texto com itens separados por " | " -> lista; o que vem antes dos
# primeiros dois-pontos de cada item (o estado, ou o painel) fica em negrito
lista <- function(x) {
  itens <- strsplit(x, " | ", fixed = TRUE)[[1]]
  tags$ul(lapply(itens, function(item) {
    tags$li(HTML(sub("^([^:]+):", "<strong>\\1:</strong>", formatar(item))))
  }))
}

# Endereço da imagem de uma prancha. Se o arquivo estiver na pasta
# www/pranchas, usa o arquivo; senão, usa o endereço na internet.
# "coluna" é "miniatura" (imagem pequena) ou "imagem" (tamanho real).
img_prancha <- function(n, coluna) {
  k <- match(n, pr$numero)
  arq_local <- file.path("pranchas", pr$arquivo[k])
  if (file.exists(file.path("www", arq_local))) arq_local else pr[[coluna]][k]
}

# ---------------------------------------------------------------------
# 3. TELA (ui)
# ---------------------------------------------------------------------

# Árvore de caracteres: grupo > caráter > estados (caixas de marcar).
# <details> e <summary> são elementos do próprio HTML que abrem e fecham
# com um clique, então a árvore não precisa de nenhum pacote extra.
ramo_carater <- function(i) {
  tags$details(
    # o "?" ao lado do nome leva ao verbete do caráter no glossário
    tags$summary(carater[i], HTML(link_glossario(paste0("glo", i), "?", "glo-ajuda"))),
    checkboxGroupInput(ids[i], label = NULL, choices = est_car[[i]])
  )
}
ramo_grupo <- function(g) {
  tags$details(
    tags$summary(g),
    lapply(which(grupo == g), ramo_carater)
  )
}

# Aparência da árvore. A última regra põe em negrito, com uma marca, os
# ramos que têm algum estado marcado, mesmo quando estão fechados.
estilo_arvore <- "
  .arvore summary { display: list-item; cursor: pointer; padding: 3px 0; }
  .arvore > details > summary { font-size: 16px; }
  .arvore details details { margin-left: 18px; }
  .arvore .shiny-input-container { margin: 0 0 4px 34px; }
  .arvore details:has(input:checked) > summary { font-weight: bold; }
  .arvore details:has(input:checked) > summary::after { content: ' \\2713'; color: #2a9d5c; }
"

# Glossário: um bloco (verbete) por caráter, na mesma ordem da árvore.
# Cada parte do verbete só aparece se a coluna correspondente estiver
# preenchida no glossario.csv.
verbete <- function(i) {
  k <- lin_glo[i]
  parte <- function(titulo, coluna, classe = NULL) {
    if (nzchar(glo[[coluna]][k])) p(class = classe, strong(titulo), HTML(formatar(glo[[coluna]][k])))
  }
  conteudo <- if (is.na(k)) {
    p(em("Verbete ainda não escrito."))
  } else {
    # números das pranchas citadas no verbete, para mostrar as miniaturas
    citadas <- unlist(regmatches(glo$pranchas[k], gregexpr("Prancha [0-9]+", glo$pranchas[k])))
    citadas <- unique(sub("Prancha ", "", citadas))
    tagList(
      p(HTML(formatar(glo$definicao[k]))),
      if (nzchar(glo$estados[k])) tagList(p(strong("Estados")), lista(glo$estados[k])),
      parte("Como observar: ", "como_observar"),
      parte("Atenção: ", "atencao", "glo-atencao"),
      parte("Figuras: ", "pranchas"),
      div(class = "glo-mini", lapply(citadas, miniatura)),
      parte("Fontes: ", "fontes", "glo-fontes")
    )
  }
  div(id = paste0("glo", i), class = "glo-verbete",
      h4(tags$small(grupo[i]), carater[i]), conteudo)
}

# Imagem pequena de uma prancha; o clique abre a prancha em tamanho real
miniatura <- function(n) {
  tags$a(href = "#", onclick = abre_prancha(n),
         tags$img(src = img_prancha(n, "miniatura"), alt = paste("Prancha", n),
                  title = paste("Prancha", n, "(clique para ampliar)"),
                  referrerpolicy = "no-referrer"))
}

# Legenda e crédito de uma prancha (usados no glossário e na janela popup)
legenda_prancha <- function(n) {
  k <- match(n, pr$numero)
  tagList(
    lista(pr$legenda[k]),
    p(class = "glo-fontes",
      # a coluna "licenca" traz a licença e as observações sobre a figura
      paste("Fonte:", pr$credito[k], pr$licenca[k]),
      tags$a(href = pr$fonte[k], target = "_blank", pr$fonte[k]))
  )
}

# Bloco de uma prancha na seção "Pranchas" do glossário
bloco_prancha <- function(n) {
  k <- match(n, pr$numero)
  div(id = paste0("prancha", n), class = "glo-verbete",
      h4(HTML(paste0("Prancha ", n, ". ", formatar(pr$titulo[k])))),
      div(class = "glo-mini", miniatura(n)),
      legenda_prancha(n))
}

estilo_glossario <- "
  .glo-linha { display: flex; flex-wrap: wrap; }
  .glo-indice { position: sticky; top: 10px; max-height: 95vh; overflow-y: auto; font-size: 13px; }
  .glo-indice ul { padding-left: 18px; }
  .glo-verbete { border-top: 1px solid #ddd; padding: 6px 10px 10px 10px; scroll-margin-top: 10px; }
  .glo-alvo { background: #fff6cc; }
  .glo-atencao { background: #fdf3e1; border-left: 4px solid #e0a030; padding: 6px 10px; }
  .glo-fontes { color: #777; font-size: 12px; }
  .glo-mini a { display: inline-block; height: 162px; margin: 4px 8px 4px 0; overflow: hidden; }
  .glo-mini img { height: 160px; border: 1px solid #ccc; cursor: zoom-in; }
  .ficha-fig img { max-width: 100%; max-height: 420px; border: 1px solid #ccc; }
  .ficha-semfig { background: #f4f4f4; border: 1px dashed #bbb; padding: 20px; color: #666; }
  .ficha-texto, .ficha-aviso { text-align: justify; max-width: 800px; }
  .glo-ajuda { margin-left: 6px; padding: 0 5px; border: 1px solid; border-radius: 50%;
               font-size: 11px; font-weight: normal; text-decoration: none; }
"

# Função (JavaScript) chamada pelos links do glossário: abre a aba
# Glossário, rola a página até o ponto pedido e o destaca por 2,5 segundos.
script_glossario <- "
  function irGlossario(id) {
    // fecha a janela popup, se houver uma aberta (ex.: descrição do táxon)
    if (window.jQuery) jQuery('.modal').modal('hide');
    document.querySelector('#abas a[data-value=\"Glossário\"]').click();
    setTimeout(function() {
      var alvo = document.getElementById(id);
      if (!alvo) return;
      alvo.scrollIntoView();
      alvo.classList.add('glo-alvo');
      setTimeout(function() { alvo.classList.remove('glo-alvo'); }, 2500);
    }, 200);
  }
  // o mesmo para a aba Referências
  // abre uma aba pelo nome (ex.: irAba('Caminhos'))
  function irAba(nome) {
    document.querySelector('#abas a[data-value=\"' + nome + '\"]').click();
    window.scrollTo(0, 0);
  }
  function irReferencia(chave) {
    if (window.jQuery) jQuery('.modal').modal('hide');
    document.querySelector('#abas a[data-value=\"Referências\"]').click();
    setTimeout(function() {
      var alvo = document.getElementById('ref-' + chave);
      if (!alvo) return;
      alvo.scrollIntoView();
      alvo.classList.add('glo-alvo');
      setTimeout(function() { alvo.classList.remove('glo-alvo'); }, 2500);
    }, 200);
  }
"

# Referências: texto e endereço do PDF ou da página da obra
# (coluna "pdf"; vazio = sem link).
referencias <- data.frame(texto = c(
  "Borisova, P. & Budaeva, N. (2022). First molecular phylogeny of Lumbrineridae (Annelida). Diversity 14(2): 83.",
  "Camargo, M.G. & Lana, P.C. (1995a). Lumbrineridae (Polychaeta: Eunicemorpha) da costa sul e sudeste do Brasil. I. Lysarete, Arabelloneris, Lumbrineriopsis, Lumbrinerides, Paraninoe e Ninoe. Iheringia, Série Zoologia 79: 77-91.",
  "Camargo, M.G. & Lana, P.C. (1995b). Lumbrineridae (Polychaeta: Eunicemorpha) da costa sul e sudeste do Brasil. II. Lumbrineris. Iheringia, Série Zoologia 79: 93-120.",
  "Carrera-Parra, L.F. (2001). Recognition of Cenogenus Chamberlin, 1919 (Polychaeta: Lumbrineridae) based on type material. Proceedings of the Biological Society of Washington 114(3): 720-724.",
  "Carrera-Parra, L.F. (2006). Phylogenetic analysis of Lumbrineridae Schmarda, 1861 (Annelida: Polychaeta). Zootaxa 1332: 1-36.",
  "Hartman, O. (1965). Deep-water benthic polychaetous annelids off New England to Bermuda and other North Atlantic areas. Allan Hancock Foundation Occasional Papers 28: 1-378.",
  "Hartman, O. & Fauchald, K. (1971). Deep-water benthic polychaetous annelids off New England to Bermuda and other North Atlantic areas. Part II. Allan Hancock Monographs in Marine Biology 6: 1-327.",
  "Neal, L., Abrahams, E., Wiklund, H., Rabone, M., Bribiesca-Contreras, G., Stewart, E.C.D., Dahlgren, T.G. & Glover, A.G. (2023). Taxonomy, phylogeny, and biodiversity of Lumbrineridae (Annelida, Polychaeta) from the Central Pacific Clarion-Clipperton Zone. ZooKeys 1172: 61-100.",
  "Orensanz, J.M. (1990). The eunicemorph polychaete annelids from Antarctic and Subantarctic seas. Antarctic Research Series 52: 1-183.",
  "Oug, E. (2003). Lumbrineridae from north east Atlantic waters. NMBAQC.",
  "Oug, E. (2012). Guide to identification of Lumbrineridae (Polychaeta) in north east Atlantic waters, versão 3.2. NMBAQC 2010 taxonomic workshop, Dove Marine Laboratory, 31 p.",
  "Scamparle Teodoro, N.M., Steiner, T.M. & Amaral, A.C.Z. (2023). A new species of Lumbrineriopsis (Annelida, Eunicida, Lumbrineridae) from southeastern Brazil. ZooKeys 1174: 175-189.",
  "Zanol, J., Carrera-Parra, L.F., Steiner, T.M., Amaral, A.C.Z., Wiklund, H., Ravara, A. & Budaeva, N. (2021). The current state of Eunicida (Annelida) systematics and biodiversity. Diversity 13(2): 74."
), pdf = c(
  "https://www.mdpi.com/1424-2818/14/2/83/pdf",
  "https://archive.org/download/biostor-80166/biostor-80166.pdf",
  "https://archive.org/download/biostor-80163/biostor-80163.pdf",
  "https://archive.org/download/biostor-74228/biostor-74228.pdf",
  "https://doi.org/10.5281/zenodo.174245",
  "https://doi.org/10.25549/hancock-ouc111637644",
  "https://repository.si.edu/handle/10088/3458",
  "https://zookeys.pensoft.net/article/100483/download/pdf/",
  "https://doi.org/10.1029/AR052p0001",
  "https://nmbaqcs.org/media/wl5igasw/oug-2003-lumbrineridae-from-north-east-atlantic-waters.pdf",
  "https://nmbaqcs.org/media/ucjbgxs0/oug2012_lumbrineridae_p.pdf",
  "https://zookeys.pensoft.net/article/101059/download/pdf/",
  "https://www.mdpi.com/1424-2818/13/2/74/pdf"
), stringsAsFactors = FALSE)
# Texto do link de cada referência: "[PDF]" quando abre o PDF direto;
# nos outros casos, o que o link mostra.
referencias$rotulo <- "[PDF]"
referencias$rotulo[6] <- "[USC Digital Library]"
referencias$rotulo[7] <- "[PDF em 4 partes, Smithsonian]"
referencias$rotulo[9] <- "[DOI, acesso restrito]"
# Chave de cada referência (usada nos textos das fichas: {CL1995a}) e a
# citação curta que aparece no link.
referencias$chave <- c("BB2022", "CL1995a", "CL1995b", "CP2001", "CP2006", "H1965",
                       "HF1971", "N2023", "O1990", "O2003", "O2012", "ST2023", "Z2021")
referencias$cita  <- c("Borisova & Budaeva 2022", "Camargo & Lana 1995a",
                       "Camargo & Lana 1995b", "Carrera-Parra 2001", "Carrera-Parra 2006",
                       "Hartman 1965", "Hartman & Fauchald 1971", "Neal et al. 2023",
                       "Orensanz 1990", "Oug 2003", "Oug 2012",
                       "Scamparle Teodoro et al. 2023", "Zanol et al. 2021")

# Texto das fichas: além de *Nome* em itálico, as citações entre chaves
# viram links que levam ao item da aba Referências:
#   {CL1995a}          -> (Camargo & Lana 1995a)
#   {CL1995b; O1990}   -> (Camargo & Lana 1995b; Orensanz 1990)
#   {@CL1995a}         -> Camargo & Lana (1995a)   (citação no meio da frase)
link_ref <- function(chave, narrativa = FALSE) {
  k <- match(chave, referencias$chave)
  if (is.na(k)) return(chave)
  texto <- referencias$cita[k]
  if (narrativa) texto <- sub(" ([0-9]{4}[a-z]?)$", " (\\1)", texto)
  paste0("<a href='#' title='Ver nas referências' onclick=\"irReferencia('",
         chave, "'); return false;\">", esc(texto), "</a>")
}
formatar_ficha <- function(x) {
  x <- formatar(x)
  m <- gregexpr("\\{@?[A-Za-z0-9; ]+\\}", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(cits) {
    vapply(cits, function(cit) {
      narrativa <- startsWith(cit, "{@")
      chaves <- strsplit(gsub("[{}@ ]", "", cit), ";")[[1]]
      links <- vapply(chaves, link_ref, "", narrativa = narrativa)
      if (narrativa) paste(links, collapse = "; ")
      else paste0("(", paste(links, collapse = "; "), ")")
    }, "")
  })
  x
}

# Aba Caminhos: as etapas de construção da chave, em ordem. A coluna
# "texto" aceita as mesmas convenções das fichas (*Nome* e {chave}).
caminhos <- data.frame(etapa = c(
  "Abertura do projeto.",
  "Planilha de partida.",
  "Primeira ação.",
  "Levantamento bibliográfico.",
  "Busca das referências.",
  "Matriz de táxons × caracteres.",
  "Primeiro app.",
  "Customização em abas.",
  "Fichas dos táxons.",
  "Glossário.",
  "Acesso às referências.",
  "Matriz bruta."
), texto = c(
  "Criar o projeto PolyTIIKS (Polychaeta Taxonomic Interactive Identification Keys in Shiny), começando por Lumbrineridae e expansível para outras famílias.",
  "Inserir no projeto a planilha do checklist de poliquetas da Bacia de Pelotas, anexada em xlsx.",
  "Listar as espécies de Lumbrineridae da planilha: 14 táxons, sendo 13 espécies e um táxon identificado só até gênero (*Abyssoninoe* sp.).",
  "Listar, por meio de deep research, a bibliografia existente sobre os 14 táxons: descrições originais, redescrições, revisões da família e guias de identificação, incluindo também a literatura cinza (teses, dissertações e relatórios).",
  "Localizar as 13 referências citadas, em versão digital.",
  paste("Montar a planilha de táxons × caracteres no formato do pacote *monographaR*",
        "(Reginato 2016): 24 caracteres organizados em grupos, com estados separados",
        "por \"/\" nos casos de polimorfismo. As células vazias foram preenchidas, sempre",
        "que possível, com as diagnoses dos gêneros e as descrições das espécies."),
  paste("Criar um web app em Shiny semelhante ao gerado pela função interactiveKey()",
        "do *monographaR*: o usuário marca os estados dos caracteres e a lista de táxons",
        "restantes se atualiza."),
  paste("Reorganizar o app nas abas atuais: Chave interativa (árvore de grupos,",
        "caracteres e estados, usados em qualquer ordem), Táxons, Glossário,",
        "Referências, Matriz, Caminhos e Sobre."),
  paste("Transformar o nome de cada táxon em link para uma ficha com figura, link para",
        "o WoRMS, um apanhado das descrições publicadas e a tabela com o estado de cada",
        "caráter."),
  paste("Escrever um verbete para cada um dos 24 caracteres (definição, estados, como",
        "observar, cuidados e fontes), ilustrado por 6 pranchas retiradas da literatura,",
        "com os créditos. Esta etapa foi feita em grande parte com o auxílio do Fable 5.2, modelo da",
        "categoria Mythos, a mais avançada da Anthropic. O modelo leu as",
        "descrições, redescrições e guias, cruzou as informações das 13 diferentes fontes e",
        "redigiu os verbetes citando cada uma delas."),
  paste("Ligar cada referência ao PDF ou à página da obra (revista, BioStor,",
        "Smithsonian, Zenodo ou DOI). As citações nas fichas dos táxons levam direto ao",
        "item correspondente na aba Referências."),
  paste("Mostrar a matriz completa, exatamente como é usada pela chave, com a opção de",
        "baixá-la em .csv, para efeito de reprodutibilidade científica.")
), stringsAsFactors = FALSE)

# ---------------------------------------------------------------------
# Aba Chave linear: a chave dicotômica, com aparência de página impressa.
# Cada lead ocupa uma linha da grade: número do dístico | texto, com uma
# linha pontilhada até a margem | destino.
# ---------------------------------------------------------------------
chv$num <- sub("[ab]$", "", chv$distico)         # "13b" -> "13"
num_disticos <- unique(chv$num)

# De qual dístico se chega a cada um. Aparece entre parênteses ao lado do
# número, para quem quiser voltar atrás na chave; o dístico 1 fica sem.
origem <- chv$num[match(num_disticos, chv$destino)]
names(origem) <- num_disticos

# Um lead. Destino táxon = link para a ficha do táxon;
# destino número = link que rola a página até aquele dístico.
lead_chave <- function(k) {
  d <- chv$destino[k]
  destino <- if (d %in% taxons) link_taxon(d) else
    paste0("<a href='#' onclick=\"irDistico('", d, "'); return false;\">", d, "</a>",
           if (nzchar(chv$genero[k])) paste0(" (<em>", chv$genero[k], "</em>)"))
  num <- chv$distico[k]
  de  <- origem[chv$num[k]]
  if (endsWith(num, "a") && !is.na(de)) num <- paste0(num, "<small>(", de, ")</small>")
  # ch-reserva é uma cópia invisível do destino no fim do texto: guarda o
  # espaço dele só na última linha, e o destino de verdade (ch-dest) fica
  # por cima, no canto direito. Assim as outras linhas usam a largura toda.
  div(class = "ch-lead",
      span(class = "ch-num", HTML(num)),
      div(class = "ch-texto",
          span(class = "ch-palavras", HTML(formatar(chv$texto[k]))),
          span(class = "ch-reserva", `aria-hidden` = "true", HTML(destino)),
          span(class = "ch-dest", HTML(destino))))
}

# Um dístico: os dois leads juntos, com um id para os links de destino
distico_chave <- function(n) {
  div(class = "ch-distico", id = paste0("distico-", n),
      lapply(which(chv$num == n), lead_chave))
}

intro_chave <- paste(
  "Chave para os 14 táxons de Lumbrineridae da lista do projeto PolyTIIKS. Foi montada",
  "a partir da matriz de caracteres usada na chave interativa",
  "Cada dístico usa",
  "um caráter principal, em primeiro lugar, e caracteres de reforço em seguida. Os",
  "caracteres de mais fácil observação (ganchos, brânquias, prostômio) vêm antes; as",
  "maxilas ficam para o fim ou como reforço.")

notas_chave <- data.frame(titulo = c(
  "Fragmentos anteriores.",
  "Papilas de *Augeneria*.",
  "*Abyssoninoe* sp.",
  "Acículas negras.",
  "Conferência."
), texto = c(
  paste("Em *Ninoe*, *Cenogenus antarctica*, *Abyssoninoe* sp. e *Scoletoma atlantica*,",
        "os ganchos nítidos só aparecem depois do setígero 10 (até o 32 em *S. atlantica*).",
        "Um fragmento anterior curto dessas espécies pode ser levado por engano ao",
        "dístico 2. Antes de seguir para o 2, confirme a ausência de ganchos num fragmento",
        "com mais de 35 setígeros, ou confira as maxilas e as brânquias."),
  paste("Variam de 0 a 3 (média de 2,4) e muitas vezes ficam escondidas sob o peristômio.",
        "Por isso não decidem sozinhas o dístico 9: o número de pares de maxilas, a MIV com",
        "centro claro e as acículas castanhas pesam mais."),
  paste("O material local só foi identificado até gênero. Os caracteres do dístico 12 vêm",
        "da diagnose do gênero e de *A. abyssorum*; devem ser revistos quando a espécie for",
        "determinada."),
  paste("Só *Cenogenus antarctica* e as duas *Ninoe* têm acículas negras; é um bom reforço",
        "para os dísticos 5 e 6. A cor das acículas de *Lysarete brasiliensis* não é",
        "conhecida e não entra na chave."),
  paste("A chave foi conferida contra a matriz: cada táxon foi levado pelos dísticos com",
        "os seus estados e chegou ao próprio nome, e nenhum caráter citado em um dístico",
        "contradiz a matriz para os táxons que passam por ele. Nenhum dístico decide por um",
        "caráter em que o táxon que passa por ele seja variável na matriz.")
), stringsAsFactors = FALSE)

# Obras citadas na chave (as mesmas da aba Referências)
refs_chave <- match(c("CL1995a", "CL1995b", "H1965", "HF1971", "O1990", "O2012"),
                    referencias$chave)

# Aparência de folha impressa. --fundo é a cor do papel: o texto de cada
# lead fica sobre ela e esconde a linha pontilhada, que só aparece depois
# da última palavra. As regras de print-color-adjust fazem a linha
# pontilhada sair também na impressão e no PDF.
estilo_chave <- "
  .ch-barra { max-width: 860px; margin: 12px auto 0; text-align: right; }
  .ch-folha { --fundo: #fff; max-width: 860px; margin: 8px auto 30px; padding: 32px 44px;
              background: var(--fundo); color: #222; border: 1px solid #ddd;
              box-shadow: 0 2px 8px rgba(0,0,0,.08);
              font-family: Georgia, 'Times New Roman', serif; font-size: 15px; line-height: 1.5; }
  .ch-folha h3 { font-family: inherit; font-size: 21px; font-weight: bold;
                 text-align: center; margin: 0 0 18px; }
  .ch-folha h4 { font-family: inherit; font-size: 16px; font-weight: bold; margin: 26px 0 8px;
                 padding-bottom: 3px; border-bottom: 1px solid #bbb; }
  .ch-folha code { font-size: 13px; color: #444; background: #f3f3f3; padding: 0 3px; }
  .ch-intro { text-align: justify; }
  .ch-distico { --fundo: #fff; margin: 0 -6px; padding: 4px 6px; border-radius: 3px;
                scroll-margin-top: 80px; }
  .ch-distico + .ch-distico { margin-top: 6px; }
  .ch-distico.glo-alvo { --fundo: #fff6cc; background: var(--fundo); }
  .ch-lead { display: grid; grid-template-columns: 4.6em 1fr; column-gap: 4px; }
  .ch-num { font-weight: bold; }
  .ch-num small { font-size: 80%; font-weight: normal; color: #888; margin-left: 1px; }
  .ch-texto { position: relative;
              background-image: radial-gradient(circle, #999 0.9px, transparent 1.2px);
              background-size: 6px 4px; background-repeat: repeat-x;
              background-position: left bottom 0.3em; }
  .ch-palavras { background: var(--fundo); padding-right: 5px; }
  .ch-reserva, .ch-dest { white-space: nowrap; font-weight: bold; }
  .ch-reserva { display: inline-block; visibility: hidden; padding-left: 1.2em; }
  .ch-dest { position: absolute; right: 0; bottom: 0; padding-left: 5px;
             background: var(--fundo); }
  .ch-texto, .ch-palavras, .ch-dest { -webkit-print-color-adjust: exact;
                                      print-color-adjust: exact; }
  .ch-notas, .ch-refs { font-size: 13.5px; text-align: justify; padding-left: 22px; }
  .ch-notas li, .ch-refs li { margin-bottom: 4px; }
  @media print {
    .ch-folha { border: none; box-shadow: none; margin: 0; padding: 0; max-width: none; }
    .ch-folha a { color: #000; text-decoration: none; }
    .ch-distico { break-inside: avoid; }
  }
  @page { margin: 2cm; }
"

# irDistico: rola até o dístico e o destaca por 2,5 segundos.
# imprimirChave: abre só a folha da chave numa janela nova e chama a
# impressão do navegador (onde dá para escolher \"Salvar como PDF\").
script_chave <- "
  function irDistico(n) {
    var alvo = document.getElementById('distico-' + n);
    if (!alvo) return;
    alvo.scrollIntoView({block: 'center', behavior: 'smooth'});
    alvo.classList.add('glo-alvo');
    setTimeout(function() { alvo.classList.remove('glo-alvo'); }, 2500);
  }
  function imprimirChave() {
    var w = window.open('', '_blank');
    if (!w) { window.print(); return; }
    // A janela nova é montada pelo DOM, sem escrever as marcas da página
    // (head, body, html) em texto: o Posit Connect Cloud procura essas
    // marcas no HTML do app para inserir o próprio código, e uma cópia delas
    // aqui quebrava este script e a página inteira.
    var d = w.document;
    d.title = 'Chave dicotômica de Lumbrineridae da Bacia de Pelotas';
    var estilo = d.createElement('style');
    estilo.textContent = document.getElementById('estilo-chave').textContent;
    d.head.appendChild(estilo);
    d.body.innerHTML = document.querySelector('.ch-folha').outerHTML;
    w.focus();
    setTimeout(function() { w.print(); }, 300);
  }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(estilo_arvore)),
            tags$style(HTML(estilo_glossario)),
            tags$style(id = "estilo-chave", HTML(estilo_chave)),
            tags$script(HTML(script_glossario)),
            tags$script(HTML(script_chave))),
  titlePanel("Chave interativa de Lumbrineridae (Polychaeta) da Bacia de Pelotas"),
  tabsetPanel(id = "abas",        # o id permite trocar de aba pelo servidor
    tabPanel("Chave interativa",
      sidebarLayout(
        sidebarPanel(
          actionButton("recomecar", "Recomeçar"),
          helpText("Marque o grupo, o caráter e os estados, em qualquer ordem."),
          div(class = "arvore", lapply(unique(grupo), ramo_grupo))
        ),
        mainPanel(
          # "sticky" mantém o resultado à vista enquanto se rola a lista de caracteres
          div(style = "position: sticky; top: 10px; max-height: 95vh; overflow-y: auto;",
            h2("Táxons"),
            textOutput("n_taxons"),
            textOutput("n_caracteres"),
            uiOutput("lista_caracteres"),
            br(),
            tableOutput("restantes")
          )
        )
      )
    ),
    tabPanel("Chave linear",
      div(class = "ch-barra",
        tags$button(type = "button", class = "btn btn-default", onclick = "imprimirChave()",
                    "Imprimir ou salvar em PDF")),
      div(class = "ch-folha",
        h3("Chave dicotômica para Lumbrineridae da Bacia de Pelotas"),
        p(class = "ch-intro", HTML(intro_chave)),
        h4("Chave"),
        lapply(num_disticos, distico_chave),
        h4("Notas"),
        tags$ol(class = "ch-notas", lapply(seq_len(nrow(notas_chave)), function(k) {
          tags$li(strong(HTML(formatar(notas_chave$titulo[k]))),
                  HTML(formatar(notas_chave$texto[k])))
        })),
        h4("Referências"),
        tags$ul(class = "ch-refs", lapply(refs_chave, function(k) tags$li(referencias$texto[k])))
      )
    ),
    tabPanel("Táxons",
      fluidRow(
        column(4,
          selectInput("taxon", "Táxon", choices = taxons),
          uiOutput("link_worms"),
          p(actionLink("abrir_descricao", "Descrição do táxon (estados dos caracteres)"))
        ),
        # à direita: figura e descrição do táxon escolhido
        column(8, uiOutput("ficha"))
      )
    ),
    tabPanel("Glossário",
      fluidRow(class = "glo-linha",
        column(3,
          div(class = "glo-indice",
            h4("Índice"),
            lapply(unique(grupo), function(g) {
              tagList(strong(g), tags$ul(lapply(which(grupo == g), function(i) {
                tags$li(HTML(link_glossario(paste0("glo", i), esc(carater[i]))))
              })))
            }),
            strong("Pranchas"),
            tags$ul(lapply(pr$numero, function(n) {
              tags$li(HTML(link_glossario(paste0("prancha", n), paste("Prancha", n))))
            }))
          )
        ),
        column(9,
          helpText("Texto preliminar, compilado da literatura; precisa de revisão.",
                   "Clique em uma figura, ou em 'Prancha', para ver a prancha ampliada."),
          lapply(unique(grupo), function(g) {
            tagList(h3(g), lapply(which(grupo == g), verbete))
          }),
          h3("Pranchas"),
          lapply(pr$numero, bloco_prancha)
        )
      )
    ),
    tabPanel("Referências",
      h4("Referências"),
      p("Obras usadas na matriz, no glossário e nas pranchas."),
      tags$ul(lapply(seq_len(nrow(referencias)), function(k) {
        tags$li(id = paste0("ref-", referencias$chave[k]), referencias$texto[k],
                if (nzchar(referencias$pdf[k]))
                  tags$a(href = referencias$pdf[k], target = "_blank", referencias$rotulo[k]))
      }))
    ),
    tabPanel("Matriz",
      p(br(), downloadButton("baixar_matriz", "Baixar a matriz (.csv)")),
      # font-size: letra um pouco menor que a do resto do app (14px)
      div(style = "overflow-x: auto; font-size: 12px;", tableOutput("matriz"))
    ),
    tabPanel("Prompts",
      div(class = "ficha-texto",
        h4("Como a chave foi construída"),
        p("A seguir, o resumo da sequência de prompts usados para construir a chave interativa com auxílio da IA",
          "Fable 5.2, da Anthropic:"),
        tags$ol(lapply(seq_len(nrow(caminhos)), function(k) {
          tags$li(strong(caminhos$etapa[k]), HTML(formatar_ficha(caminhos$texto[k])))
        }))
      )
    ),
    tabPanel("Sobre",
      h4("Sobre esta chave"),
      p("Autores: Maurício Camargo, Raphael Pinotti e Mariana Lisboa."),
      p("Esta é uma chave politômica interativa de múltiplo acesso para as",
        "espécies da família Lumbrineridae registradas na Bacia de Pelotas,",
        "criada em R-shiny com a assistência de IA pelo modelo Fable 5.2 da Anthropic,",
        "como parte do projeto PolyTIIKS (Polychaeta Taxonomic Interactive Identification Keys",
        "in Shiny), do Laboratório de Ecologia de Invertebrados Bentônicos",
        HTML("(<a href='https://www.instagram.com/bentosfurg/' target='_blank'>@bentosfurg</a>).")),
      p("A chave foi inspirada no pacote", tags$i("monographaR"), "(Reginato 2016)."),
      p("A matriz original contém", n_tax, "táxons e", n_car, "caracteres."),
      p("O glossário foi compilado da literatura listada na aba",
        HTML(paste0(as.character(actionLink("ir_referencias", "Referências")), ".")),  # ponto colado ao link
        "As figuras podem vir de artigos de acesso aberto (licença CC BY 4.0),",
        "de revistas que permitem a reprodução com citação da fonte ou ser",
        "fornecidas pelo autor. O crédito de cada uma está junto da prancha.")
    )
  )
)

# ---------------------------------------------------------------------
# 4. LÓGICA (server)
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Estados escolhidos em cada caráter (NULL = caráter não usado)
  escolhas <- reactive(lapply(ids, function(id) input[[id]]))
  usados   <- reactive(which(lengths(escolhas()) > 0))

  # Vetor lógico: TRUE para os táxons que ainda combinam com tudo
  resta <- reactive({
    ok <- rep(TRUE, n_tax)
    for (i in usados()) ok <- ok & compativel(i, escolhas()[[i]])
    ok
  })

  output$n_taxons     <- renderText(paste(sum(resta()), "táxon(s) restante(s)"))
  output$n_caracteres <- renderText(paste(length(usados()), "caractere(s) selecionado(s)"))

  # Lista dos caracteres selecionados, com os estados marcados em cada um
  # (na ordem da matriz). Sem nada marcado, a lista não aparece.
  # O nome de cada caráter é um link para o verbete dele no glossário.
  output$lista_caracteres <- renderUI({
    i <- usados()
    if (length(i) == 0) return(NULL)
    marcados <- vapply(escolhas()[i], paste, character(1), collapse = " ou ")
    nomes <- paste0("<strong>", esc(paste0(grupo[i], " – ", carater[i], ":")), "</strong>")
    tags$ul(lapply(seq_along(i), function(k) {
      tags$li(HTML(link_glossario(paste0("glo", i[k]), nomes[k])), marcados[k])
    }))
  })

  # Táxons restantes: tabela sem cabeçalho nem bordas, em até 5 colunas,
  # nomes em itálico e em ordem alfabética (como no app do monographaR).
  # Cada nome é um link para a descrição do táxon.
  output$restantes <- renderTable({
    nomes <- sort(taxons[resta()])
    if (length(nomes) == 0) return(NULL)
    n_col <- 5
    tab <- matrix("", nrow = ceiling(length(nomes) / n_col), ncol = n_col)
    tab[seq_along(nomes)] <- link_taxon(nomes)
    tab
  }, colnames = FALSE, bordered = FALSE, sanitize.text.function = function(x) x)

  # Clique no nome de um táxon: escolhe esse táxon na aba "Táxons" e muda
  # para ela. As caixas marcadas na chave interativa continuam.
  observeEvent(input$ver_taxon, {
    updateSelectInput(session, "taxon", selected = input$ver_taxon)
    updateTabsetPanel(session, "abas", selected = "Táxons")
  })

  # Link "Referências" do texto da aba Sobre: muda para a aba Referências.
  observeEvent(input$ir_referencias, {
    updateTabsetPanel(session, "abas", selected = "Referências")
  })

  # Link para a página do táxon no WoRMS (World Register of Marine
  # Species); abre em outra janela do navegador.
  output$link_worms <- renderUI({
    k <- match(input$taxon, tax$taxon)
    if (is.na(k) || !nzchar(tax$aphia_id[k])) return(NULL)
    endereco <- paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=",
                       tax$aphia_id[k])
    p(tags$a(href = endereco, target = "_blank",
             "WoRMS:", em(tax$nome_worms[k]), tax$autoridade[k]))
  })

  # Ficha do táxon (lado direito da aba): figura, legenda e descrição em
  # português, com links para a aba Referências. Figuras em www/taxons.
  output$ficha <- renderUI({
    k <- match(input$taxon, tax$taxon)
    if (is.na(k)) return(NULL)
    figura <- if (nzchar(tax$figura[k])) {
      tagList(
        div(class = "ficha-fig",
            tags$a(href = file.path("taxons", tax$figura[k]), target = "_blank",
                   title = "Abrir a figura em outra janela",
                   tags$img(src = file.path("taxons", tax$figura[k]),
                            alt = input$taxon))),
        p(class = "glo-fontes", HTML(formatar_ficha(tax$legenda[k]))))
    } else {
      div(class = "ficha-semfig", HTML(formatar_ficha(tax$legenda[k])))
    }
    tagList(
      h3(HTML(nome_italico(input$taxon)), tags$small(tax$autoridade[k])),
      figura,
      h4("Descrição (apanhado)"),
      div(class = "ficha-texto", p(HTML(formatar_ficha(tax$descricao[k])))),
      # aviso (coluna "aviso"), ex.: figura de outra espécie do gênero
      if (nzchar(tax$aviso[k])) p(class = "glo-atencao ficha-aviso", strong("Atenção: "),
                                  HTML(formatar_ficha(tax$aviso[k])))
    )
  })

  # Descrição do táxon: abre em uma janela popup a tabela com o estado de
  # cada caráter. O nome de cada caráter é um link para o glossário; ao
  # clicar nele, a janela fecha e o glossário abre no verbete.
  observeEvent(input$abrir_descricao, {
    showModal(modalDialog(
      title = HTML(paste("Descrição do táxon:", nome_italico(input$taxon))),
      div(style = "font-size: 13px;", tableOutput("descricao")),
      size = "l", footer = modalButton("Fechar"), easyClose = TRUE
    ))
  })

  output$descricao <- renderTable({
    est <- mat[, input$taxon]
    est[is.na(est)] <- "não informado"
    # o nome do grupo aparece só na primeira linha de cada grupo
    nome_grupo <- ifelse(duplicated(grupo), "", paste0("<strong>", esc(grupo), "</strong>"))
    tab <- data.frame(nome_grupo,
                      link_glossario(paste0("glo", seq_len(n_car)), esc(carater)),
                      esc(gsub("/", " ou ", est)), stringsAsFactors = FALSE)
    names(tab) <- c("Grupo", "Caráter", "Estado")
    tab
  }, sanitize.text.function = function(x) x)

  # Clique em "Prancha N" ou em uma miniatura: abre a prancha em uma
  # janela popup, com a imagem em tamanho real, a legenda e o crédito.
  observeEvent(input$ver_prancha, {
    n <- input$ver_prancha
    k <- match(n, pr$numero)
    if (is.na(k)) return()
    showModal(modalDialog(
      title = HTML(paste0("Prancha ", n, ". ", formatar(pr$titulo[k]))),
      tags$img(src = img_prancha(n, "imagem"), alt = paste("Prancha", n),
               style = "width: 100%;", referrerpolicy = "no-referrer"),
      p(tags$a(href = img_prancha(n, "imagem"), target = "_blank",
               "Abrir a imagem em outra janela")),
      legenda_prancha(n),
      size = "l", footer = modalButton("Fechar"), easyClose = TRUE
    ))
  })

  output$matriz <- renderTable({
    tab <- data.frame(grupo, carater, mat, check.names = FALSE, stringsAsFactors = FALSE)
    names(tab)[1:2] <- c("Grupo", "Caráter")
    tab
  }, na = "")

  # Botão da aba Matriz: entrega o próprio arquivo lido pelo app
  output$baixar_matriz <- downloadHandler(
    filename = "matriz_lumbrineridae.csv",
    content = function(file) file.copy("matriz_lumbrineridae.csv", file)
  )

  # Botão Recomeçar: desmarca todas as caixas
  observeEvent(input$recomecar, {
    for (id in ids) updateCheckboxGroupInput(session, id, selected = character(0))
  })
}

shinyApp(ui, server)
