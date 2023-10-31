################################################################################
#       =====       Déficit habitacional no Distrito Federal      ====
################################################################################


# Esses códigos foram elaborados pela equipe da Diretoria de Estudos e
# Políticas Ambientais e Territoriais (DEPAT),
# sob trabalhos diretos da Coordenação de Estudos Territoriais (COET).

# O relatório da pesquisa está disponível em
# https://www.ipe.df.gov.br/deficit-e-demanda-habitacional-do-distrito-federal-2021/

# A DEPAT é parte do IPEDF Instituto de Pesquisa e Estatística do Distrito Federal (IPEDF)

# Os códigos a seguir dizem respeito ao ajuste da base de dados da Pesquisa
# de modo a permitir a expansão e realização de inferências. Há também algumas
# das tabelas utilizadas. Todos os demais agrupamentos
# e cálculos presentes no relatório foram feitos com base na metodologia.

# Pedimos a gentileza de reportarem bugs ou erros nos códigos abaixo.
# E-mail: gab-depat@ipe.df.gov.br

# Todos os dados foram obtidos a partir da Pesquisa Distrital por
# Amostra de Domicílios (PDAD) 
# O dicionário de variáveis e outras informações sobre a PDAD 2021
# podem ser encontrados no site do IPEDF:
# https://www.ipe.df.gov.br/microdados-pdad-2021/


################################################################################
#                         ===== Configurações R ====
################################################################################


# Criando um ambiente virtual

# Para manter a memória dos pacotes utilizados, de modo que possam ser totalmente reproduzidos,
# sugere-se utilizar o pacote [`renv`](https://rstudio.github.io/renv/articles/renv.html). 
# 
# Para isso, o pacote deve ser instalado inicialmente. Este procedimento foi feito 
# com o renv 1.0.2.
# 
# ## Adotando a versão correta dos pacotes.
# 
# Junto com esse script, estão disponíveis os arquivos `renv.lock`, `.Rprofile`,
# `renv/settings.json` e `renv/activate.R`.
#
# Para que os pacotes sejam os mesmos utilizados neste projeto,
# as informações das versões dos pacotes ficarão registradas no arquivo `renv.lock`.
# Com esse arquivo na mesma pasta do projeto, pode-se recuperar as versões adequadas
# em qualquer outra máquina com conexão ao repositório
# ou à internet com a função `renv::restore()`.



# Configurações opcionais
rm(list = ls(all = TRUE))

options(scipen = 999)


################################################################################
#         =====  Carregar pacotes e leitura dos dados ====
################################################################################


# Pacotes necessários

  # O pacote `pacman` é utilizado para instalar e carregar os pacotes necessários
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,     # Pacote para manipulação de dados
  readxl,        # Pacote para leitura de arquivos .xls e .xlsx
  survey,        # Pacote para manipulação de dados survey
  srvyr          # Pacote que ajusta a família tidyverse para dados survey     
)   

# Leitura dos dados da PDAD 2021


  # URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Domilícios
url <- "https://www.ipe.df.gov.br/wp-content/uploads/2022/07/PDAD_2021-Domicilios.csv"

  # Definir o nome do arquivo temporário
temp_file <- tempfile()

  # Baixar o arquivo .csv e salve-o no arquivo temporário
download.file(url, temp_file)

  # Ler o arquivo .csv no R
pdad_dom_2021 <- read_csv2(temp_file)

  # Excluir o arquivo temporário
unlink(temp_file)



  # URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Moradores
url <- "https://www.ipe.df.gov.br/wp-content/uploads/2022/07/PDAD_2021-Moradores.csv"

  # Definir o nome do arquivo temporário
temp_file <- tempfile()

  # Baixar o arquivo .csv e salve-o no arquivo temporário
download.file(url, temp_file)

  # Ler o arquivo .csv no R
pdad_mor_2021 <- read_csv2(temp_file)

  # Excluir o arquivo temporário
unlink(temp_file)
rm(temp_file, url)
  



################################################################################
# =====  Dados para atualização monetária das informações de renda ====
################################################################################


  # Calcular inflator do aluguel, para referência de jul/2021
inflator_aluguel <-
  dplyr::bind_rows(
    # Baixar informações do Sidra IBGE
    sidrar::get_sidra(api = '/t/7060/n6/5300108/v/63/p/202103,202104,202105,202106,202107/c315/7448/d/v63%202')  %>%
      # Selecionar colunas de interesse
      dplyr::select("Mês (Código)", "Mês", "Valor") %>%
      # Renomear colunas
      dplyr::rename_all(list( ~ c(
        "ref", "ref1", "valor"
      ))) %>%
      # Criar variável para o mês
      dplyr::mutate(MES_N = as.numeric(substr(ref, 6, 6)),
                    MES = str_trim(gsub("[0-9]", "", ref1))) %>%
      # Organizar variáveis
      dplyr::arrange(-MES_N) %>%
      # Criar o inflator de março a julho
      dplyr::mutate(inflator_aluguel = cumprod((valor / 100) + 1)),
    # Fazer o mesmo processo para julho a outubro
    sidrar::get_sidra(api = '/t/7060/n6/5300108/v/63/p/202107,202108,202109,202110/c315/7448/d/v63%202') %>%
      dplyr::select("Mês (Código)", "Mês", "Valor") %>%
      dplyr::rename_all(list( ~ c(
        "ref", "ref1", "valor"
      ))) %>%
      dplyr::mutate(MES_N = as.numeric(substr(ref, 6, 6)),
                    MES = str_trim(gsub("[0-9]", "", ref1))) %>%
      dplyr::mutate(inflator_aluguel = 1 / cumprod((valor / 100) + 1))
  ) %>%
  # Selecionar variáveis de interesse
  dplyr::select(MES, inflator_aluguel) %>%
  # Ajustar mês de julho
  dplyr::mutate(inflator_aluguel = ifelse(MES == "julho", 1, inflator_aluguel)) %>%
  # Eliminar duplicadas
  dplyr::distinct()

  # Calcular inflator geral, para referência de jul/2021
  # Mesmo processo que o anterior, alterando somente o índice
inflator_geral <-
  dplyr::bind_rows(
    sidrar::get_sidra(api = '/t/7060/n6/5300108/v/63/p/202103,202104,202105,202106,202107/c315/7169/d/v63%202') %>%
      dplyr::select("Mês (Código)", "Mês", "Valor") %>%
      dplyr::rename_all(list( ~ c(
        "ref", "ref1", "valor"
      ))) %>%
      dplyr::mutate(MES_N = as.numeric(substr(ref, 6, 6)),
                    MES = str_trim(gsub("[0-9]", "", ref1))) %>%
      dplyr::arrange(-MES_N) %>%
      dplyr::mutate(inflator_geral = cumprod((valor / 100) + 1)),
    sidrar::get_sidra(api = '/t/7060/n6/5300108/v/63/p/202107,202108,202109,202110/c315/7169/d/v63%202') %>%
      dplyr::select("Mês (Código)", "Mês", "Valor") %>%
      dplyr::rename_all(list( ~ c(
        "ref", "ref1", "valor"
      ))) %>%
      dplyr::mutate(MES_N = as.numeric(substr(ref, 6, 6)),
                    MES = str_trim(gsub("[0-9]", "", ref1))) %>%
      dplyr::mutate(inflator_geral = 1 / cumprod((valor / 100) + 1))
  ) %>%
  dplyr::select(MES, inflator_geral) %>%
  dplyr::mutate(inflator_geral = ifelse(MES == "julho", 1, inflator_geral)) %>%
  dplyr::distinct()




################################################################################
#                 ===== Ajustes iniciais da base da PDAD ====
################################################################################


  # Calcular a renda domiciliar
renda_domiciliar <- pdad_mor_2021 %>%
  # Mudar para ausente os valores das variáveis G16,G19
  # com códigos 77777 ou 88888.
  # Vamos também mudar para 0 quando os valores que não se aplicarem
  # ou não forem observados rendimentos (66666,99999)
  dplyr::mutate_at(vars(I20, I21), # Variáveis a serem alteradas
                   # Função a ser aplicada
                   list( ~ case_when(
                     . %in% c(77777, 88888) ~ NA_real_,
                     . %in% c(66666, 99999) ~ 0,
                     TRUE ~ as.numeric(.)
                   ))) %>%
  # Mudar para ausente os valores das variáveis G201 até G204
  # com código 77777 e zerar os valores sem rendimento,
  # casos que não se aplica e não sabia responder (66666,88888,99999)
  dplyr::mutate_at(vars(I04_1_1, I04_2_1, I22_1, I22_2),
                   # Função a ser aplicada
                   list( ~ case_when(
                     . %in% c(77777) ~ NA_real_,
                     . %in% c(66666, 88888, 99999) ~ 0,
                     TRUE ~ as.numeric(.)
                   ))) %>%
  # Selecionar apenas as variáveis de interesse
  dplyr::select(A01nficha, E05, I20, I21, I04_1_1, I04_2_1, I22_1, I22_2) %>%
  # Somar as variáveis modificadas para construir a renda individual
  dplyr::mutate(renda_individual = rowSums(.[, c("I20", "I21",
                                                 "I04_1_1", "I04_2_1",
                                                 "I22_1", "I22_2")], na.rm = T)) %>%
  # Desconsiderar os empregados domesticos moradores e seus parentes
  dplyr::filter(!E05 %in% c(18, 19)) %>%
  # Agrupar por domicílio
  dplyr::group_by(A01nficha) %>%
  # Somar os valores por domicílios
  dplyr::summarise(
    renda_dom = sum(renda_individual, na.rm = T),
    # Construir o número de pessoas no domicílio, por esse critério de rendiment0
    pessoas = n(),
    # Calcular a renda domiciliar per capita
    renda_pc = renda_dom / pessoas
  )


# Fazer a junção das bases
pdad_2021 <- pdad_dom_2021 %>%
  # Trazer as informações de pessoas para domicílios
  dplyr::left_join(
    pdad_mor_2021,
    by = c(
      "A01ra"     = "A01ra",
      "A01nficha" = "A01nficha",
      "A01setor"  = "A01setor"
    )
  ) %>%
  # Ajustar o nome das RAs
  dplyr::mutate(
    RA_nome = factor(
      case_when(
        A01ra == 1 ~ "Plano Piloto",
        A01ra == 2 ~ "Gama",
        A01ra == 3 ~ "Taguatinga",
        A01ra == 4 ~ "Brazlândia",
        A01ra == 5 ~ "Sobradinho",
        A01ra == 6 ~ "Planaltina",
        A01ra == 7 ~ "Paranoá",
        A01ra == 8 ~ "Núcleo Bandeirante",
        A01ra == 9 ~ "Ceilândia",
        A01ra == 10 ~ "Guará",
        A01ra == 11 ~ "Cruzeiro",
        A01ra == 12 ~ "Samambaia",
        A01ra == 13 ~ "Santa Maria",
        A01ra == 14 ~ "São Sebastião",
        A01ra == 15 ~ "Recanto das Emas",
        A01ra == 16 ~ "Lago Sul",
        A01ra == 17 ~ "Riacho Fundo",
        A01ra == 18 ~ "Lago Norte",
        A01ra == 19 ~ "Candangolândia",
        A01ra == 20 ~ "Águas Claras",
        A01ra == 21 ~ "Riacho Fundo II",
        A01ra == 22 ~ "Sudoeste/Octogonal",
        A01ra == 23 ~ "Varjão",
        A01ra == 24 ~ "Park Way",
        A01ra == 25 ~ "SCIA/Estrutural",
        A01ra == 26 ~ "Sobradinho II",
        A01ra == 27 ~ "Jardim Botânico",
        A01ra == 28 ~ "Itapoã",
        A01ra == 29 ~ "SIA",
        A01ra == 30 ~ "Vicente Pires",
        A01ra == 31 ~ "Fercal",
        A01ra == 32 ~ "Sol Nascente/Pôr do Sol",
        A01ra == 33 ~ "Arniqueira"
      )
    ),
    grupos_ped = ifelse(
      RA_nome == 'Plano Piloto' |
        RA_nome == 'Jardim Botânico' |
        RA_nome == 'Lago Norte' |
        RA_nome == 'Lago Sul' |
        RA_nome == 'Park Way' |
        RA_nome == 'Sudoeste/Octogonal',
      "Grupo 1",
      ifelse(
        RA_nome == 'Águas Claras' |
          RA_nome == 'Arniqueira' |
          RA_nome == 'Candangolândia' |
          RA_nome == 'Cruzeiro' |
          RA_nome == 'Gama' |
          RA_nome == 'Guará' |
          RA_nome == 'Núcleo Bandeirante' |
          RA_nome == 'Sobradinho' |
          RA_nome == 'Sobradinho II' |
          RA_nome == 'Taguatinga' |
          RA_nome == 'Vicente Pires',
        "Grupo 2",
        ifelse(
          RA_nome == 'Brazlândia' |
            RA_nome == 'Ceilândia' |
            RA_nome == 'Planaltina' |
            RA_nome == 'Riacho Fundo' |
            RA_nome == 'Riacho Fundo II' |
            RA_nome == 'Samambaia' |
            RA_nome == 'Santa Maria' |
            RA_nome == 'São Sebastião' |
            RA_nome == 'SIA',
          "Grupo 3",
          ifelse(
            RA_nome == 'Fercal' |
              RA_nome == 'Itapoã' |
              RA_nome == 'Paranoá' |
              RA_nome == 'Recanto das Emas' |
              RA_nome == 'SCIA/Estrutural' |
              RA_nome == 'Varjão' |
              RA_nome == 'Sol Nascente/Pôr do Sol',
            "Grupo 4",
            NA
          )
        )
      )
    ),
    # Criar uma variável com o mês da pesquisa para o join com os inflatores
    MES = months(lubridate::ymd(endtime)),
    count = 1
  ) %>%
  # Pegar informações do inflator do aluguel
  dplyr::left_join(inflator_aluguel) %>%
  # Pegar informações do inflator geral
  dplyr::left_join(inflator_geral) %>%
  # Pegar informação da renda domiciliar
  dplyr::left_join(renda_domiciliar)



# Criação das classificações de famílias
matriz_familiar <- pdad_2021 %>%
  #filtrando mais de 4 moradorese razao maior ou igual a 2
  filter(A01npessoas > 4 & A01npessoas / B11 > 2) %>%
  mutate(
    condicao = 1,
    E05_edit = case_when(
      E05 == 1 ~ "responsavel",
      E05 == 2 ~ 'conjuge',
      E05 == 3 ~ 'conjuge',
      E05 == 4 &
        idade < 18 ~ 'filho_menor18',
      E05 == 5 &
        idade < 18 ~ 'filho_menor18',
      E05 == 6 &
        idade < 18 ~ 'filho_menor18' ,
      E05 == 4 &
        idade >= 18 ~ 'filho_maior18',
      E05 == 5 &
        idade >= 18 ~ 'filho_maior18',
      E05 == 6 &
        idade >= 18 ~ 'filho_maior18',
      E05 == 7 ~ 'genro_nora',
      E05 == 8 ~ 'pais',
      E05 == 9 ~ 'sogros',
      E05 == 10 ~ 'neto',
      E05 == 11 ~ 'bisneto',
      E05 == 12  ~ 'irmao',
      E05 == 13 ~ 'avos',
      E05 == 14 ~ 'outro_parente',
      E05 == 15 ~ 'agregado',
      E05 == 16 ~ 'convivente',
      E05 == 17 ~ 'pensionista',
      E05 == 18 ~ 'empregado_domestico',
      E05 == 19 ~ 'parente_empregado'
    )
  ) %>%
  group_by(A01nficha, A01npessoas, B11, E05_edit) %>%
  summarise(n = sum(condicao)) %>%
  spread(E05_edit, n) %>%
  mutate(
    primaria = case_when(((responsavel == 1 & !is.na(conjuge)) |
                            (responsavel == 1 &
                               !is.na(conjuge) & (
                                 !is.na(filho_menor18) | !is.na(filho_maior18)
                               )) |
                            (responsavel == 1 &
                               (
                                 !is.na(filho_menor18) | !is.na(filho_maior18)
                               )) |
                            (responsavel == 1 & !is.na(pais)) |
                            (responsavel == 1 &
                               !is.na(pais) & !is.na(irmao))
    ) ~ 1),
    secundaria = ifelse(
      A01npessoas >= 4 &
        ((!is.na(filho_maior18) &
            !is.na(genro_nora)) |
           (!is.na(filho_maior18) &
              !is.na(genro_nora) & !is.na(neto)) |
           (
             !is.na(filho_maior18) &
               !is.na(genro_nora) & !is.na(neto) & !is.na(bisneto)
           ) |
           (!is.na(filho_maior18) &
              !is.na(genro_nora) & !is.na(bisneto)) |
           (!is.na(filho_maior18) &
              !is.na(neto)) |
           (!is.na(filho_maior18) &
              !is.na(neto) & !is.na(bisneto)) |
           (!is.na(filho_maior18) &
              !is.na(bisneto)) |
           (!is.na(neto) &
              !is.na(bisneto)) |
           (!is.na(genro_nora) &
              !is.na(neto)) |
           (!is.na(genro_nora) &
              !is.na(bisneto)) |
           (!is.na(genro_nora) &
              !is.na(neto) & !is.na(bisneto)) |
           outro_parente >= 2
        ),
      1,
      NA
    ),
    
    
    
    arr.depat.primaria = ifelse(!is.na(conjuge), 1,
                                ifelse(
                                  !is.na(conjuge) & (!is.na(filho_menor18) | !is.na(filho_maior18)), 2,
                                  ifelse((!is.na(filho_maior18) |
                                            !is.na(filho_menor18)),
                                         3,
                                         ifelse(
                                           !is.na(pais) & A01npessoas > 2,
                                           4,
                                           ifelse(
                                             !is.na(pais) & A01npessoas > 2 & !is.na(irmao),
                                             5,
                                             ifelse(!is.na(pais), 6, NA)
                                           )
                                         )
                                  )
                                )),
    #NUCLEO SECUNDARIO
    arr.depat.secund = ifelse((!is.na(filho_maior18)) &
                                !is.na(genro_nora),
                              7,
                              ifelse((!is.na(filho_maior18)) &
                                       !is.na(genro_nora) &
                                       !is.na(neto),
                                     8,
                                     ifelse((!is.na(filho_maior18)) &
                                              !is.na(genro_nora) &
                                              !is.na(neto) & !is.na(bisneto),
                                            9,
                                            ifelse((!is.na(filho_maior18)) &
                                                     !is.na(genro_nora),
                                                   10,
                                                   ifelse((!is.na(filho_maior18)) &
                                                            !is.na(neto),
                                                          11,
                                                          ifelse((!is.na(filho_maior18)) &
                                                                   !is.na(neto) &
                                                                   !is.na(bisneto),
                                                                 12,
                                                                 ifelse((!is.na(filho_maior18)) &
                                                                          !is.na(bisneto),
                                                                        13,
                                                                        ifelse(
                                                                          !is.na(neto) & !is.na(bisneto),
                                                                          14,
                                                                          ifelse(
                                                                            !is.na(genro_nora) & !is.na(neto),
                                                                            15,
                                                                            ifelse(
                                                                              !is.na(genro_nora) & !is.na(bisneto),
                                                                              16,
                                                                              ifelse(
                                                                                !is.na(genro_nora) & !is.na(neto) & !is.na(bisneto),
                                                                                17,
                                                                                ifelse(
                                                                                  !is.na(outro_parente),
                                                                                  18,
                                                                                  ifelse(
                                                                                    !is.na(agregado) |
                                                                                      !is.na(convivente) |
                                                                                      !is.na(pensionista),
                                                                                    19,
                                                                                    NA
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
    ),
    arr.depat.dom4 = ifelse(A01npessoas > 4 &
                              A01npessoas / B11 > 2, 1, NA)
  )

  # Calculando a variável primaria_final e secundaria_final
matriz_familiar_f <- matriz_familiar %>%
  select(
    A01nficha,
    A01npessoas,
    B11,
    primaria,
    secundaria,
    arr.depat.primaria,
    arr.depat.secund,
    arr.depat.dom4
  ) %>%
  mutate(
    primaria_final = ifelse(!is.na(arr.depat.primaria), 1, NA),
    secundaria_final = ifelse(!is.na(arr.depat.secund), 1, NA)
  )


  # Junção da matriz familiar com a base de domicílios
pdad_2021 <- pdad_2021 %>% left_join(matriz_familiar_f) 


  # Removendo tabelas não mais usadas
rm(matriz_familiar, matriz_familiar_f, pdad_mor_2021, pdad_dom_2021)

  

## Classificando a renda comiciliar em faixas de renda (nova coluna chamada faixa_renda)
## Classificando se o responsável pelo comicílio trabalha na mesma RA que mora
## (nova coluna chamada trabalho_onde_mora)
## Agrupando as regiões para a análise do déficit, de acordo com estudos passados
## (nova coluna chamada agrup_regiao)
## Criando uma coluna chamada DF, com atributo DF apenas para designo
## de estudos feitos para o DF como um todo.

pdad_2021 <- pdad_2021 %>%
  mutate(faixa_renda = ifelse(
    renda_domiciliar <= 3 * 1100,
    "até 3 s.m",
    ifelse(
      renda_domiciliar <= 5 * 1100,
      "3 a 5 s.m",
      ifelse(renda_domiciliar <= 12 * 1100, "5 a 12 s.m", "mais de 12 s.m")
    )
  )) %>%
  mutate(trabalho_onde_mora = ifelse(A01ra == I08, "sim", "nao"))  %>%
  mutate(agrup_regiao = ifelse(
    A01ra %in% c("1", "16", "18", "22", "24", "27"),
    "Grupo 1",
    ifelse(
      A01ra %in% c("2", "3", "5", "8", "10", "11", "19", "20", "26", "30", "33"),
      "Grupo 2",
      ifelse(
        A01ra %in% c("4", "6", "9", "12", "13", "14", "17", "21", "29"),
        "Grupo 3",
        ifelse(
          A01ra %in% c("7", "15", "23", "25", "28", "31", "32"),
          "Grupo 4",
          "ERRO"
        )
      )
    )
  )) %>%
  mutate(df = "DF")



###############################################################################
#                       Ajuste dos dados de déficit                           #
###############################################################################



## Para caracterização do subcomponente "Domicílios Improvisados",
## foi realizada verificação detalhada dos domicílios da amostra da PDAD 2021,
## apoiada por análises geoespaciais e visuais por meio do *Google Street View*,
## resultando na variável `B01_déficit`, disponível no site do instituto


## Para a caracterização dos domicílios rústicos, baseada no cruzamento das
## respostas "alvenaria sem revestimento" (variável B07=2) aliada à presença
## apenas de "contrapiso" (variável B08=1) ou "cimento alisado" (variável B08=2)
## e "cobertura em telha de fibrocimento sem laje" (variável B09=4).
## Também foi realizada verificação detalhada dos domicílios da amostra da PDAD 2021,
## apoiada por análises geoespaciais e visuais por meio do *Google Street View*,
## resultando na variável `B07_atualizada` disponível também no site do instituto.


## A nova coluna B01 é agregada à base de dados com o nome B01_edit_2 e 
## os domicílios após análise aparecem como "Improvisado" ou "Permanente"


pdad_2021 <- pdad_2021 %>% 
  rename("B07_pdad" = B07)                                                                                                         


  # URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Moradores
url <- "https://www.ipe.df.gov.br/wp-content/uploads/2023/10/B01_deficit.xlsx"

  # Definir o nome do arquivo temporário
temp_file <- tempfile()

  # Baixar o arquivo .csv e salve-o no arquivo temporário
download.file(url, temp_file, mode = "wb")

  # Ler o arquivo .csv no R
novo_B01 <- read_excel(temp_file)

  # Excluir o arquivo temporário
unlink(temp_file)


  # URL do site do IPEDF que contém o arquivo .csv de microdados da PDAD de Moradores
url <- "https://www.ipe.df.gov.br/wp-content/uploads/2023/10/B07_atualizado.xlsx"

  # Definir o nome do arquivo temporário
temp_file <- tempfile()

  # Baixar o arquivo .csv e salve-o no arquivo temporário
download.file(url, temp_file, mode = "wb")

  # Ler o arquivo .csv no R
novo_B07 <- read_excel(temp_file)

  # Excluir o arquivo temporário
unlink(temp_file)
rm(temp_file, url)


  # Inclusão das novas variáveis na base de dados
pdad_2021 <- pdad_2021 %>%
  left_join(novo_B07) %>%
  left_join(novo_B01) %>% 
  rename("B01_edit_2" = "B01_deficit")

  # Remover as variáveis que não serão utilizadas
rm(novo_B01, novo_B07)


###############################################################################
#                       Expansão da amostra                                   #
###############################################################################


  # Declarar o desenho inicial
amostra_dom <-
  survey::svydesign(
    id      = ~ A01nficha,
    strata  = ~ A01setor,
    weights = ~ PESO_PRE,
    nest    = TRUE,
    data    = dplyr::filter(pdad_2021, E05 == 1)
  )

  #Criar um objeto para pós estrato
post.pop <- pdad_2021 %>%
  dplyr::group_by(A01setor) %>%
  dplyr::summarise(Freq=first(TOT_DOM)) %>%
  dplyr::ungroup()

  # Declarar o objeto de pós-estrato
amostra_dom <- survey::postStratify(amostra_dom,  ~ A01setor, post.pop)

  # Criar o objeto amostra de pessoas para 2021
amostra_dom <- srvyr::as_survey(amostra_dom)

  # Ajustar estratos com apenas uma UPA (adjust=centered)
options( survey.lonely.psu = "adjust")

  # Removendo tabelas não mais necessárias
rm(post.pop)

  

################################################################################
#                         Casos Déficit por Componentes DF                     #
################################################################################

  # Componente Habitação precária e subcomponentes
habitacao_precaria_componentes_df <- amostra_dom %>%
  # Construir variáveis
  srvyr::mutate(
    domicilio =
      # Domicílios rústicos
      case_when(
        B01_edit_2 == "Permanente" &
          B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 ~ "Rústico",
        B01_edit_2 == "Permanente" &
          B07 == 33 ~ "Rústico",
        B01_edit_2 == "Permanente" &
          B07 == 44 ~ "Rústico",
        # Domicílios improvisados
        B01_edit_2 == "Improvisado" ~ "Improvisado",
        B01_edit_2 == "Permanente" &
          B11 == 0 ~ "Improvisado"
      )
  )  %>%
  # Agrupar por subcomponente
  srvyr::group_by(df, domicilio) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n = survey_total(vartype = "cv")) %>%
  filter(!is.na(domicilio))


  # Componente Habitação precária (união dos filtros dos subcompomentes)
habitacao_precaria_df <- amostra_dom %>%
  # Construir variáveis
  srvyr::mutate(
    habitacao =
      # Habitação precária
      case_when(
        B01_edit_2 == "Permanente" &
          B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
          B01_edit_2 == "Permanente" &
          B07 == 33 |
          B01_edit_2 == "Permanente" &
          B07 == 44 |
          B01_edit_2 == "Improvisado" |
          B01_edit_2 == "Permanente" &
          B11 == 0 ~ "Habitação precária"
      )
  )  %>%
  srvyr::group_by(df, habitacao) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n = survey_total(vartype = "cv")) %>%
  filter(!is.na(habitacao))


##################################################################################

  # Componente coabitação
coabitacao_df <- amostra_dom %>%
  srvyr::mutate(
    # Familias conviventes
    fam_conv = case_when(B01_edit_2 == "Permanente" &
                           ((primaria_final == 1 &
                               secundaria_final == 1) &
                              (A01npessoas > 4 &
                                 (A01npessoas / B11) > 2)
                           ) ~ 1,
                         TRUE ~ 0),
    # Cômodos
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    # Uma ou outra situação
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0)
  ) %>%
  srvyr::group_by(df) %>%
  # Calcular os totais para cada situação
  srvyr::summarise(
    coabit_familiar = survey_total(coabit_familiar, vartype = "cv"),
    fam_conv        = survey_total(fam_conv, vartype = "cv"),
    comodo          = survey_total(comodo, vartype = "cv")
  )


###########################################################################


  # Componente Ônus excessivo com aluguel 
onus_aluguel_df <- amostra_dom %>%
  # Aplicar filtros
  srvyr::filter(
    # Para o responsável (estatística por domicílio)
    E05==1,
    # Renda domiciliar positiva
    renda_dom*inflator_geral>0,
    # Renda até três salários mínimos
    (renda_dom*inflator_geral)<=3*1100,
    # Retirar valores inválidos para aluguel
    !B05 %in% c(77777,88888),
    # Manter somente domicílios com renda informada
    !is.na(renda_dom)) %>% 
  # Calcular o ônus do aluguel
  srvyr::mutate(onus_aluguel = case_when(
    B03 == 3 &
      B02 != 4 &
      B01_edit_2 == "Permanente" &
      (B05 * inflator_aluguel) >= (0.3 * renda_dom * inflator_geral) ~ 1,
    TRUE ~ 0
  )) %>% 
  srvyr::group_by(df) %>% 
  # Calcular os totais 
  srvyr::summarise(onus_aluguel=survey_total(onus_aluguel, vartype = "cv"))


###########################################################################
#                Déficit habitacional total (2021)                        #
###########################################################################


    # Fazer todos os cálculos de uma vez para verificar quem está em, pelo menos, alguma situação
deficit_df <- amostra_dom %>%
  srvyr::filter(E05 == 1) %>%
  srvyr::mutate(
    dom_rust = case_when(
      B01_edit_2 == "Permanente" &
        B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
        B01_edit_2 == "Permanente" & B07 == 33 |
        B01_edit_2 == "Permanente" &
        B07 == 44 ~ 1,
      TRUE ~ 0
    ),
    dom_imp = case_when(
      B01_edit_2 == "Improvisado" |
        B01_edit_2 == "Permanente" & B11 == 0 ~ 1,
      TRUE ~ 0
    ),
    habit_prec = case_when(dom_rust == 1 | dom_imp == 1 ~ 1,
                           TRUE ~ 0),
    fam_conv = case_when((
      B01_edit_2 == "Permanente" &
        (primaria_final == 1 &
           secundaria_final == 1) &
        (A01npessoas > 4 & (A01npessoas / B11) > 2)
    ) ~ 1,
    TRUE ~ 0),
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0),
    onus_aluguel = case_when(
      B03 == 3 & B02 != 4 & B01_edit_2 == "Permanente" &
        (B05 * inflator_aluguel >= 0.3 * renda_dom *
           inflator_geral) &
        renda_dom * inflator_geral > 0 &
        (renda_dom * inflator_geral) <= 3 * 1100 &
        !B05 %in% c(77777, 88888) ~ 1,
      TRUE ~ 0
    ),
    deficit_habit = case_when(habit_prec == 1 |
                                onus_aluguel == 1 |
                                coabit_familiar == 1 ~ 1,
                              TRUE ~ 0)
  ) %>%
  srvyr::group_by(df) %>%
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit = survey_total(deficit_habit, vartype = "cv"))


###########################################################################


  # Déficit habitacional por raça
deficit_df_raca <- amostra_dom %>%
  srvyr::filter(E05 == 1) %>%
  srvyr::mutate(
    dom_rust = case_when(
      B01_edit_2 == "Permanente" &
        B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
        B01_edit_2 == "Permanente" & B07 == 33 |
        B01_edit_2 == "Permanente" &
        B07 == 44 ~ 1,
      TRUE ~ 0
    ),
    dom_imp = case_when(
      B01_edit_2 == "Improvisado" |
        B01_edit_2 == "Permanente" & B11 == 0 ~ 1,
      TRUE ~ 0
    ),
    habit_prec = case_when(dom_rust == 1 | dom_imp == 1 ~ 1,
                           TRUE ~ 0),
    fam_conv = case_when((
      B01_edit_2 == "Permanente" &
        (primaria_final == 1 &
           secundaria_final == 1) &
        (A01npessoas > 4 & (A01npessoas / B11) > 2)
    ) ~ 1,
    TRUE ~ 0),
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0),
    onus_aluguel = case_when(
      B03 == 3 & B02 != 4 & B01_edit_2 == "Permanente" &
        (B05 * inflator_aluguel >= 0.3 * renda_dom *
           inflator_geral) &
        renda_dom * inflator_geral > 0 &
        (renda_dom * inflator_geral) <= 3 * 1100 &
        !B05 %in% c(77777, 88888) ~ 1,
      TRUE ~ 0
    ),
    deficit_habit = case_when(habit_prec == 1 |
                                onus_aluguel == 1 | coabit_familiar == 1 ~ 1,
                              TRUE ~ 0),
    negro_naonegro = ifelse(E06 %in% c(2, 4), "negro", "nao negro")
  ) %>%
  srvyr::group_by(df, negro_naonegro) %>%
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit1 = survey_total(deficit_habit, vartype = "cv")) 


###########################################################################


  # Déficit habitacional por sexo
deficit_df_sexo <- amostra_dom %>%
  srvyr::filter(E05 == 1) %>%
  srvyr::mutate(
    dom_rust = case_when(
      B01_edit_2 == "Permanente" &
        B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
        B01_edit_2 == "Permanente" & B07 == 33 |
        B01_edit_2 == "Permanente" &
        B07 == 44 ~ 1,
      TRUE ~ 0
    ),
    dom_imp = case_when(
      B01_edit_2 == "Improvisado" |
        B01_edit_2 == "Permanente" & B11 == 0 ~ 1,
      TRUE ~ 0
    ),
    habit_prec = case_when(dom_rust == 1 | dom_imp == 1 ~ 1,
                           TRUE ~ 0),
    fam_conv = case_when((
      B01_edit_2 == "Permanente" &
        (primaria_final == 1 &
           secundaria_final == 1) &
        (A01npessoas > 4 & (A01npessoas / B11) > 2)
    ) ~ 1,
    TRUE ~ 0),
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0),
    onus_aluguel = case_when(
      B03 == 3 & B02 != 4 & B01_edit_2 == "Permanente" &
        (B05 * inflator_aluguel >= 0.3 * renda_dom *
           inflator_geral) &
        renda_dom * inflator_geral > 0 &
        (renda_dom * inflator_geral) <= 3 * 1100 &
        !B05 %in% c(77777, 88888) ~ 1,
      TRUE ~ 0
    ),
    deficit_habit = case_when(habit_prec == 1 |
                                onus_aluguel == 1 | coabit_familiar == 1 ~ 1,
                              TRUE ~ 0),
    negro_naonegro = ifelse(E06 %in% c(2, 4), "negro", "nao negro")
  ) %>%
  srvyr::group_by(df, E04) %>%
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit1 = survey_total(deficit_habit, vartype = "cv")) 




################################################################################
#                     Casos Déficit por RA                                     #
################################################################################


  # Fazer todos os cálculos de uma vez para verificar quem está em, pelo menos, alguma situação
deficit_por_ra <- amostra_dom %>%
  srvyr::filter(E05 == 1) %>%
  srvyr::mutate(
    dom_rust = case_when(
      B01_edit_2 == "Permanente" &
        B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
        B01_edit_2 == "Permanente" & B07 == 33 |
        B01_edit_2 == "Permanente" &
        B07 == 44 ~ 1,
      TRUE ~ 0
    ),
    dom_imp = case_when(
      B01_edit_2 == "Improvisado" |
        B01_edit_2 == "Permanente" & B11 == 0 ~ 1,
      TRUE ~ 0
    ),
    habit_prec = case_when(dom_rust == 1 | dom_imp == 1 ~ 1,
                           TRUE ~ 0),
    fam_conv = case_when((
      B01_edit_2 == "Permanente" &
        (primaria_final == 1 &
           secundaria_final == 1) &
        (A01npessoas > 4 & (A01npessoas / B11) > 2)
    ) ~ 1,
    TRUE ~ 0),
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0),
    onus_aluguel = case_when(
      B03 == 3 & B02 != 4 & B01_edit_2 == "Permanente" &
        (B05 * inflator_aluguel >= 0.3 * renda_dom *
           inflator_geral) &
        renda_dom * inflator_geral > 0 &
        (renda_dom * inflator_geral) <= 3 * 1100 &
        !B05 %in% c(77777, 88888) ~ 1,
      TRUE ~ 0
    ),
    deficit_habit = case_when(habit_prec == 1 |
                                onus_aluguel == 1 |
                                coabit_familiar == 1 ~ 1,
                              TRUE ~ 0)
  ) %>%
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>%
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit1 = survey_total(deficit_habit, vartype = "cv")) 

###############################################################################
#                       Casos Déficit por Grupos RA                               #
###############################################################################


  # Fazer todos os cálculos de uma vez para verificar quem está em, pelo menos, alguma situação
deficit_grupo_ra <- amostra_dom %>%
  srvyr::filter(E05 == 1) %>%
  srvyr::mutate(
    dom_rust = case_when(
      B01_edit_2 == "Permanente" &
        B07 %in% c(2) & B08 %in% c(1, 2) & B09 == 4 & B02 != 4 |
        B01_edit_2 == "Permanente" & B07 == 33 |
        B01_edit_2 == "Permanente" &
        B07 == 44 ~ 1,
      TRUE ~ 0
    ),
    dom_imp = case_when(
      B01_edit_2 == "Improvisado" |
        B01_edit_2 == "Permanente" & B11 == 0 ~ 1,
      TRUE ~ 0
    ),
    habit_prec = case_when(dom_rust == 1 | dom_imp == 1 ~ 1,
                           TRUE ~ 0),
    fam_conv = case_when((
      B01_edit_2 == "Permanente" &
        (primaria_final == 1 &
           secundaria_final == 1) &
        (A01npessoas > 4 & (A01npessoas / B11) > 2)
    ) ~ 1,
    TRUE ~ 0),
    comodo = case_when(B02 == 4 ~ 1,
                       TRUE ~ 0),
    coabit_familiar = case_when(fam_conv == 1 | comodo == 1 ~ 1,
                                TRUE ~ 0),
    onus_aluguel = case_when(
      B03 == 3 & B02 != 4 & B01_edit_2 == "Permanente" &
        (B05 * inflator_aluguel >= 0.3 * renda_dom *
           inflator_geral) &
        renda_dom * inflator_geral > 0 &
        (renda_dom * inflator_geral) <= 3 * 1100 &
        !B05 %in% c(77777, 88888) ~ 1,
      TRUE ~ 0
    ),
    deficit_habit = case_when(habit_prec == 1 |
                                onus_aluguel == 1 |
                                coabit_familiar == 1 ~ 1,
                              TRUE ~ 0)
  ) %>%
  # Agrupar por grupo de RA
  srvyr::group_by(df, agrup_regiao) %>%
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit = survey_total(deficit_habit, vartype = "cv"))


  # Removendo tabelas não mais necessárias
rm(inflator_aluguel, inflator_geral)

################################################################################
#                               FIM 
################################################################################

