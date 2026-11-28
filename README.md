# PolyTIIKS: chave interativa de Lumbrineridae da Bacia de Pelotas

Chave de identificação interativa, de múltiplo acesso, para os Lumbrineridae
(Annelida, Polychaeta) registrados na Bacia de Pelotas, inspirada no Intkey (sistema
DELTA). Feita em R-base, com o pacote `shiny` como única dependência.

Parte do Trabalho de Conclusão de Curso de Mariana Lisboa, orientado por Maurício
Camargo (Instituto de Oceanografia, FURG).

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

- **Chave interativa:** os caracteres ficam numa árvore (grupo, caráter, estados) e podem
  ser usados em qualquer ordem. A lista de táxons restantes é atualizada a cada
  escolha, e cada nome leva à aba Táxons.
- **Chave linear:** chave dicotômica tradicional, com 13 dísticos, tirada da matriz e
  conferida táxon a táxon contra ela. Os destinos são links (para o dístico seguinte ou
  para a ficha do táxon), e um botão imprime a chave ou a salva em PDF.
- **Táxons:** link para o WoRMS, descrição do táxon (estados dos caracteres, numa janela
  com links para o glossário) e descrição original.
- **Matriz:** a matriz completa de táxons por caracteres.
- **Glossário:** um verbete por caráter, com definição, estados, como observar,
  cuidados, figuras e fontes.
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
| `chave_dicotomica.csv` | chave dicotômica da aba Chave linear, um lead por linha |
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

As pranchas 1 a 5 do glossário vêm de artigos de acesso aberto com licença CC BY 4.0
(Zanol et al. 2021; Borisova & Budaeva 2022; Teodoro et al. 2023; Neal et al. 2023).
A prancha 6 reúne desenhos de Camargo & Lana (1995b), publicados na Iheringia, Série
Zoologia, que permite a reprodução com citação da fonte; o arquivo está em
`www/pranchas/prancha6.png`. O crédito completo de cada prancha está em `pranchas.csv`
e no próprio app.
