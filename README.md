# PolyTIIKS: chave interativa de Lumbrineridae da Bacia de Pelotas

Chave de identificação interativa, de múltiplo acesso, para os Lumbrineridae
(Annelida, Polychaeta) registrados na Bacia de Pelotas, inspirada no Intkey (sistema
DELTA). Feita em R-base, com o pacote `shiny` como única dependência.

Parte do Trabalho de Conclusão de Curso de Mariana Lisboa, orientado por Maurício
Garcia de Camargo (Instituto de Oceanografia, FURG).

## Como rodar

Direto do GitHub, no R:

```r
shiny::runGitHub("PolyTIIKS", "mauricio-camargo")
```

Ou baixe a pasta e rode localmente:

```r
shiny::runApp("caminho/da/pasta")
```

As figuras do glossário são carregadas dos sites das revistas, então o app precisa de
internet para mostrá-las. Para usá-lo sem internet, rode uma vez, na pasta do app:

```r
source("baixar_pranchas.R")
```

## O que o app faz

- **Identificação:** os caracteres ficam numa árvore (grupo, caráter, estados) e podem
  ser usados em qualquer ordem. A lista de táxons restantes é atualizada a cada
  escolha, e cada nome leva à descrição do táxon.
- **Descrição do táxon:** os estados de cada caráter, com link para o WoRMS.
- **Glossário:** um verbete por caráter, com definição, estados, como observar,
  cuidados, figuras e fontes.
- **Matriz:** a matriz completa de táxons por caracteres.
- **Referências:** as obras usadas na matriz, no glossário e nas pranchas.

Regras de comparação, como no ajuste padrão do Intkey: uma célula vazia (dado
desconhecido) não elimina o táxon, e dois estados marcados no mesmo caráter valem como
"um ou outro".

## Arquivos

| Arquivo | Conteúdo |
|---|---|
| `app.R` | o aplicativo |
| `matriz_lumbrineridae.csv` | matriz caracteres × táxons, no formato do pacote monographaR |
| `taxons.csv` | um táxon por linha, com o número no WoRMS (AphiaID) |
| `glossario.csv` | um verbete por caráter |
| `pranchas.csv` | figuras do glossário, com créditos e licenças |
| `baixar_pranchas.R` | opcional: baixa as figuras para a pasta `www/pranchas` |
| `docs/matriz-preenchimento-por-diagnose.md` | origem de cada célula preenchida na matriz |

Nos textos do glossário, `*Nome*` vira itálico e "Prancha 3" vira um link para a
prancha.

## Situação

Matriz preliminar: 14 táxons, 24 caracteres e uma célula sem dado. Os estados foram
compilados da literatura (Camargo & Lana 1995a, b; Orensanz 1990; Hartman 1965;
Carrera-Parra 2006, entre outros) e precisam ser conferidos contra o material da
coleção.

## Créditos das figuras

As pranchas do glossário vêm de artigos de acesso aberto com licença CC BY 4.0
(Zanol et al. 2021; Borisova & Budaeva 2022; Teodoro et al. 2023; Neal et al. 2023).
O crédito completo de cada uma está em `pranchas.csv` e no próprio app.
