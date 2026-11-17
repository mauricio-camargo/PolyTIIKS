# =====================================================================
# OPCIONAL: baixa as pranchas do glossário para a pasta www/pranchas.
# Sem isso, o app busca as figuras na internet cada vez que é aberto.
# Com as figuras na pasta, ele funciona também sem internet.
# Para rodar: abra o R na pasta do app e use source("baixar_pranchas.R").
# =====================================================================

pr <- read.csv("pranchas.csv", fileEncoding = "UTF-8", colClasses = "character")
dir.create(file.path("www", "pranchas"), recursive = TRUE, showWarnings = FALSE)

for (k in seq_len(nrow(pr))) {
  destino <- file.path("www", "pranchas", pr$arquivo[k])
  if (file.exists(destino)) next                  # já foi baixada
  # mode = "wb" grava o arquivo como imagem (binário); o cabeçalho
  # "User-Agent" faz o pedido parecer o de um navegador comum
  resultado <- suppressWarnings(try(
    download.file(pr$imagem[k], destino, mode = "wb", quiet = TRUE,
                  headers = c("User-Agent" = "Mozilla/5.0")),
    silent = TRUE))
  if (inherits(resultado, "try-error") || resultado != 0) {
    unlink(destino)                               # apaga o arquivo incompleto
    message("Não consegui baixar a prancha ", pr$numero[k],
            ". Abra o endereço abaixo no navegador e salve a imagem como ",
            destino, ":\n  ", pr$imagem[k])
  } else {
    message("Prancha ", pr$numero[k], " salva em ", destino)
  }
}
