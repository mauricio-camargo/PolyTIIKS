# =====================================================================
# Chave interativa de Lumbrineridae da Bacia de Pelotas
# R-base + pacote shiny.
# Para rodar: deixe este arquivo na mesma pasta que os quatro arquivos de
# dados e use shiny::runApp("caminho/da/pasta").
#   matriz_lumbrineridae.csv  caracteres x táxons
#   taxons.csv                um táxon por linha: número no WoRMS, figura
#                             (em www/taxons), legenda, descrição e aviso
#   glossario.csv             um verbete por caráter
#   pranchas.csv              figuras do glossário, com créditos
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
  .ficha-texto { text-align: justify; max-width: 800px; }
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

# Referências: texto e, quando houver, endereço do PDF de acesso livre
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
  "",
  "",
  "https://archive.org/download/biostor-74228/biostor-74228.pdf",
  "https://doi.org/10.5281/zenodo.174245",
  "",
  "",
  "https://zookeys.pensoft.net/article/100483/download/pdf/",
  "",
  "https://nmbaqcs.org/media/wl5igasw/oug-2003-lumbrineridae-from-north-east-atlantic-waters.pdf",
  "https://nmbaqcs.org/media/ucjbgxs0/oug2012_lumbrineridae_p.pdf",
  "https://zookeys.pensoft.net/article/101059/download/pdf/",
  "https://www.mdpi.com/1424-2818/13/2/74/pdf"
), stringsAsFactors = FALSE)
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

ui <- fluidPage(
  tags$head(tags$style(HTML(estilo_arvore)),
            tags$style(HTML(estilo_glossario)),
            tags$script(HTML(script_glossario))),
  titlePanel("Lumbrineridae da Bacia de Pelotas: chave interativa"),
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
    tabPanel("Táxons",
      fluidRow(
        column(4,
          selectInput("taxon", "Táxon", choices = taxons),
          uiOutput("link_worms"),
          p(actionLink("abrir_descricao", "Descrição do táxon (estados dos caracteres)")),
          p(actionLink("abrir_original", "Descrição original do táxon e referências"))
        ),
        # à direita: figura e descrição do táxon escolhido
        column(8, uiOutput("ficha"))
      )
    ),
    tabPanel("Matriz",
      # font-size: letra um pouco menor que a do resto do app (14px)
      div(style = "overflow-x: auto; font-size: 12px;", tableOutput("matriz"))
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
                  tags$a(href = referencias$pdf[k], target = "_blank", "[PDF]"))
      }))
    ),
    tabPanel("Sobre",
      h4("Sobre esta chave"),
      p("Chave interativa de múltiplo acesso para os Lumbrineridae registrados",
        "na Bacia de Pelotas, feita em R-base com o pacote shiny."),
      p(n_tax, "táxons e", n_car, "caracteres. A matriz segue o formato do",
        "pacote monographaR (Reginato 2016)."),
      p(strong("Atenção:"), "matriz preliminar, compilada da literatura",
        "(Camargo & Lana 1995a, b; Carrera-Parra 2001, 2006; Hartman 1965;",
        "Kurt-Sahin et al. 2016). Os estados precisam ser conferidos contra",
        "os originais e contra o material da coleção."),
      p("Dado desconhecido não elimina o táxon: uma célula vazia na matriz",
        "combina com qualquer estado escolhido."),
      p("Mais de um estado marcado no mesmo caráter significa 'um ou outro':",
        "o táxon permanece se tiver pelo menos um dos estados marcados."),
      p("O glossário foi compilado da literatura listada na aba Referências.",
        "As figuras vêm de artigos de acesso aberto (licença CC BY 4.0) ou de",
        "revista que permite a reprodução com citação da fonte;",
        "o crédito de cada uma está junto da prancha.")
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
      # aviso (coluna "aviso"), ex.: figura de outra espécie do gênero
      if (nzchar(tax$aviso[k])) p(class = "glo-atencao", strong("Atenção: "),
                                  HTML(formatar_ficha(tax$aviso[k]))),
      figura,
      h4("Descrição"),
      div(class = "ficha-texto", p(HTML(formatar_ficha(tax$descricao[k]))))
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

  # Link "Descrição original do táxon e referências": abre uma janela
  # (popup) sobre a página, para o táxon escolhido no seletor.
  # O conteúdo ainda não foi preenchido.
  observeEvent(input$abrir_original, {
    showModal(modalDialog(
      title = HTML(nome_italico(input$taxon)),
      h4("Descrição original"),
      p("(a preencher)"),
      h4("Referências"),
      p("(a preencher)"),
      footer = modalButton("Fechar"),
      easyClose = TRUE            # também fecha com um clique fora da janela
    ))
  })

  output$matriz <- renderTable({
    tab <- data.frame(grupo, carater, mat, check.names = FALSE, stringsAsFactors = FALSE)
    names(tab)[1:2] <- c("Grupo", "Caráter")
    tab
  }, na = "")

  # Botão Recomeçar: desmarca todas as caixas
  observeEvent(input$recomecar, {
    for (id in ids) updateCheckboxGroupInput(session, id, selected = character(0))
  })
}

shinyApp(ui, server)
