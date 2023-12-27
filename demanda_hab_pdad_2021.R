################################################################################
#     =====       Demanda habitacional no Distrito Federal      ====
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
#                     ===== Configurações R ====
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
# ===== 0. Carregar pacotes e leitura dos dados ====
################################################################################



# Pacotes necessários

  # O pacote `pacman`, por meio de sua função p_load()
  # é utilizado para instalar e carregar os pacotes necessários
  # Primeiro, testa-se se o pacote está instalado. Se não, esta será feita.
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
               tidyverse,     # Pacote para manipulação de dados
               readxl,        # Pacote para leitura de arquivos .xls e .xlsx
               lubridate,     # Pacote para manipulação de datas
               data.table,    # Pacote para manipulação de dados
               survey,        # Pacote para manipulação de dados survey
               srvyr          # Pacote que ajusta a família tidyverse para dados survey        
               )   



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
#                 ===== Ajustes iniciais ====
################################################################################


# O objetivo é calcular a demanda habitacional com a taxa de chefia tradicional
# - que leva em conta todos os chefes daquela faixa etária.


  # Fazer a junção das bases de moradores e domicílios
pdad_2021 <- pdad_dom_2021 %>%
  # Trazer as informações de pessoas para domicílios
  dplyr::left_join(
    pdad_mor_2021 #%>%
    #dplyr::select(-FATOR_PROJ)
    ,
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
    # Ajustar os setores de interesse da PDAD 2018
    ra_setor = factor(
      case_when(
        A01setor == 53011 ~ "Asa Norte",
        A01setor == 53012 ~ "Asa Sul",
        A01setor == 53013 ~ "Noroeste",
        A01setor == 53014 ~ "Demais",
        A01setor == 53020 ~ "Gama",
        A01setor == 53030 ~ "Taguatinga",
        A01setor == 53040 ~ "Brazlândia",
        A01setor == 53050 ~ "Sobradinho",
        A01setor == 53060 ~ "Planaltina",
        A01setor == 53070 ~ "Paranoá",
        A01setor == 53080 ~ "Núcleo Bandeirante",
        A01setor == 53090 ~ "Ceilândia",
        A01setor == 53100 ~ "Guará",
        A01setor == 53110 ~ "Cruzeiro",
        A01setor == 53120 ~ "Samambaia",
        A01setor == 53130 ~ "Santa Maria",
        A01setor == 53140 ~ "São Sebastião",
        A01setor == 53150 ~ "Recanto Das Emas",
        A01setor == 53160 ~ "Lago Sul",
        A01setor == 53170 ~ "Riacho Fundo",
        A01setor == 53180 ~ "Lago Norte",
        A01setor == 53190 ~ "Candangolândia",
        A01setor == 53200 ~ "Águas Claras",
        A01setor == 53210 ~ "Riacho Fundo II",
        A01setor == 53220 ~ "Sudoeste/Octogonal",
        A01setor == 53230 ~ "Varjão",
        A01setor == 53240 ~ "Park Way",
        A01setor == 53250 ~ "SCIA-Estrutural",
        A01setor == 53260 ~ "Sobradinho II",
        A01setor == 53271 ~ "Jardim Botânico - Tradicional",
        A01setor == 53272 ~ "Jardim Mangueiral",
        A01setor == 53280 ~ "Itapoã",
        A01setor == 53290 ~ "SIA",
        A01setor == 53300 ~ "Vicente Pires",
        A01setor == 53310 ~ "Fercal",
        A01setor == 53320 ~ "Sol Nascente/Pôr do Sol",
        A01setor == 53330 ~ "Arniqueira"
      )
    )
  )


  # Excluir as bases parciais
rm(pdad_dom_2021, pdad_mor_2021)

  # Deixar apenas os domicílios onde o responsável tem entre 24 e 64 anos.
pdad_2021_original <- pdad_2021

pdad_2021 <-
  subset(pdad_2021,
         A01nficha %in% subset(pdad_2021, E05 == 1 &
                                 idade %in% c(24:64))$A01nficha)

# Agora, deve-se identificar os domicílios que têm demanda habitacional, e trabalhar apenas com estes.

  # Criação das classificações de famílias
matriz_familiar <- pdad_2021 %>% 
  # select(A01nficha, A01npessoas, E05, idade) %>% 
  mutate(condicao = 1, 
         E05_edit = case_when(E05 == 1 ~ 'responsavel', 
                              E05 == 2 ~ 'conjuge', 
                              E05 == 3 ~ 'conjuge', 
                              E05 == 4 & idade < 24 ~ 'filho_menor24', 
                              E05 == 5 & idade < 24 ~ 'filho_menor24',
                              E05 == 6 & idade < 24 ~ 'filho_menor24' , 
                              E05 == 4 & idade >=24 ~ 'filho_maior24', 
                              E05 == 5 & idade >=24 ~ 'filho_maior24',
                              E05 == 6 & idade >=24 ~ 'filho_maior24', 
                              E05 == 7 ~ 'genro_nora', 
                              E05 == 8 ~ 'pais', 
                              E05 == 9 ~ 'sogros', 
                              E05 == 10 ~ 'neto', 
                              E05 == 11 ~ 'bisneto', 
                              E05 == 12 ~ 'irmao', 
                              E05 == 13 ~ 'avos',
                              E05 == 14 ~ 'outro_parente', 
                              E05 == 15 ~ 'agregado',
                              E05 == 16 ~ 'convivente', 
                              E05 == 17 ~ 'pensionista',
                              E05 == 18 ~ 'empregado_domestico', 
                              E05 == 19 ~ 'parente_empregado')) %>% 
  group_by(A01nficha, A01npessoas, E05_edit) %>% 
  summarise(n = sum(condicao)) %>%  
  spread(E05_edit, n) 

  # Criar variável indicadora de idoso no domicílio
pdad_2021$idoso <- ifelse(pdad_2021$idade >= 65, 1, 0)

matriz_idosos <-
  as.data.frame(table(pdad_2021$A01nficha, pdad_2021$idoso))

pdad_2021$idoso <- NULL

matriz_idosos$idosos <-
  as.numeric(levels(matriz_idosos$Var2))[matriz_idosos$Var2] * matriz_idosos$Freq

names(matriz_idosos)[1] <- "A01nficha"

matriz_idosos <- subset(matriz_idosos, idosos > 0)

matriz_familiar <-
  merge(matriz_familiar,
        matriz_idosos,
        by = "A01nficha",
        all.x = TRUE)

rm(matriz_idosos)

  # Indicadora se o idoso é responsável ou cônjuge.
matriz_familiar$idoso_conj <-
  ifelse(
    matriz_familiar$A01nficha %in% subset(pdad_2021, E05 %in% c(2, 3) &
                                            idade >= 65)$A01nficha,
    1,
    0
  )

  # Criar variável indicadora se alguém entre 24 e 64 anos mora no domicílio e não é responsável ou cônjuge.
pdad_2021$adulto <-
  ifelse(pdad_2021$idade >= 24 & pdad_2021$idade < 65, 1, 0)

matriz_adulto <-
  as.data.frame(table(pdad_2021$A01nficha, pdad_2021$adulto))

pdad_2021$adulto <- NULL

matriz_adulto$adulto <-
  as.numeric(levels(matriz_adulto$Var2))[matriz_adulto$Var2] * matriz_adulto$Freq

names(matriz_adulto)[1] <- "A01nficha"

matriz_adulto <- subset(matriz_adulto, adulto > 0)

matriz_familiar <-
  merge(matriz_familiar,
        matriz_adulto,
        by = "A01nficha",
        all.x = TRUE)

rm(matriz_adulto)

  # Indicadora se o adulto é responsável ou cônjuge.
matriz_familiar$adulto_resp <-
  ifelse(
    matriz_familiar$A01nficha %in% subset(pdad_2021, E05 == 1 &
                                            idade >= 24 & idade < 65)$A01nficha,
    1,
    0
  )

matriz_familiar$adulto_conj <-
  ifelse(
    matriz_familiar$A01nficha %in% subset(pdad_2021, E05 %in% c(2, 3) &
                                            idade >= 24 & idade < 65)$A01nficha,
    1,
    0
  )


  # Indicação de que não há demanda nos domicílios onde mora só o responsável com o cônjuge,
  # filhos menores de 24 anos e idosos.
matriz_familiar$demanda <- NA

matriz_familiar$demanda <-
  ifelse(((
    rowSums(matriz_familiar[, c("responsavel", "conjuge", "filho_menor24", "idosos")], na.rm =TRUE) 
    - (matriz_familiar[, c("idoso_conj")])
  ) == matriz_familiar$A01npessoas) == TRUE,
  0,
  matriz_familiar$demanda)

  # Número de pessoas no domicílio que demandam habitação - i.e tem pessoas entre 24 e 64 anos
  # que não são responsáveis ou cônjuges.
matriz_familiar$demanda <-
  ifelse((matriz_familiar$A01npessoas - (
    rowSums(matriz_familiar[, c("responsavel", "conjuge", "filho_menor24", "idosos")], na.rm =
              TRUE) - (matriz_familiar[, c("idoso_conj")])
  )) > 0,
  (
    matriz_familiar$adulto - rowSums(matriz_familiar[, c("adulto_resp", "adulto_conj")], na.rm =
                                       TRUE)
  ),
  matriz_familiar$demanda
  )

  # Transformando a demanda dos casos em que os pais, avós filhos/genro/nora moram no domicílio.
  # Considerar a idade da pessoa mais velha do casal. 
  # Se a pessoa mais velha do casal for idosa, o casal não tem demanda por habitação.
  # Primeiro, cria-se uma variável indicadora para o tipo de casal.
matriz_familiar$casal_avos <- ifelse(matriz_familiar$avos == 2, 1, NA)

matriz_familiar$casal_pais <- ifelse(matriz_familiar$pais == 2, 1, NA)

matriz_familiar$casal_sogros <-
  ifelse(matriz_familiar$sogros == 2, 1, NA)

  # A indicação da existência de casal de filhos é a própria variável de de genro/nora. 
  # Faz-se para cada tipo de casal separado porque pode ter mais de um tipo de casal por domicílio.

  # Separa-se os indivíduos de cada casal para calcular a idade máxima dele.
idade_casal_avos <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, casal_avos == 1 &
                            demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 13)

idade_casal_sogros <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, casal_sogros == 1 &
                            demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 9)

idade_casal_pais <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, casal_pais == 1 &
                            demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 8)

  # Calculo a idade do cônjuge mais velho para determinar o chefe do casal.
idade_casal_avos2 <- idade_casal_avos %>%
  group_by(A01nficha) %>%
  summarise(chefe_idade = max(idade))

idade_casal_avos <-
  merge(idade_casal_avos,
        idade_casal_avos2,
        by = "A01nficha",
        all.x = TRUE)

idade_casal_avos <- idade_casal_avos %>%
  group_by(A01nficha) %>%
  mutate(chefe_id = ifelse(
    sum(idade) / 2 == chefe_idade,
    min(morador_id),
    ifelse(idade == chefe_idade, morador_id, 0)
  )) %>%
  mutate(chefe_casal = ifelse(morador_id == chefe_id, 1, 0)) %>%
  mutate(conj_casal = ifelse(morador_id != chefe_id, 1, 0))

rm(idade_casal_avos2)

idade_casal_sogros2 <- idade_casal_sogros %>% 
  group_by(A01nficha) %>% 
  summarise(chefe_idade = max(idade))

idade_casal_sogros <-
  merge(idade_casal_sogros,
        idade_casal_sogros2,
        by = "A01nficha",
        all.x = TRUE)

idade_casal_sogros <- idade_casal_sogros %>%
  group_by(A01nficha) %>%
  mutate(chefe_id = ifelse(
    sum(idade) / 2 == chefe_idade,
    min(morador_id),
    ifelse(idade == chefe_idade, morador_id, 0)
  )) %>%
  mutate(chefe_casal = ifelse(morador_id == chefe_id, 1, 0)) %>%
  mutate(conj_casal = ifelse(morador_id != chefe_id, 1, 0))

rm(idade_casal_sogros2)

idade_casal_pais2 <- idade_casal_pais %>%
  group_by(A01nficha) %>%
  summarise(chefe_idade = max(idade))

idade_casal_pais <-
  merge(idade_casal_pais,
        idade_casal_pais2,
        by = "A01nficha",
        all.x = TRUE)

idade_casal_pais <- idade_casal_pais %>%
  group_by(A01nficha) %>%
  mutate(chefe_id = ifelse(
    sum(idade) / 2 == chefe_idade,
    min(morador_id),
    ifelse(idade == chefe_idade, morador_id, 0)
  )) %>%
  mutate(chefe_casal = ifelse(morador_id == chefe_id, 1, 0)) %>%
  mutate(conj_casal = ifelse(morador_id != chefe_id, 1, 0))

rm(idade_casal_pais2)


  # Identificação dos casos de genro/nora e filhos.
idade_casal_filhos01 <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, genro_nora == 1 &
                            filho_maior24 == 1, demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 7 | (E05 %in% c(4:6) & idade >= 24))

idade_casal_filhos01b <- idade_casal_filhos01 %>% 
  group_by(A01nficha) %>% 
  summarise(chefe_idade = max(idade))

idade_casal_filhos01 <-
  merge(idade_casal_filhos01,
        idade_casal_filhos01b,
        by = "A01nficha",
        all.x = TRUE)

idade_casal_filhos01 <- idade_casal_filhos01 %>%
  group_by(A01nficha) %>%
  mutate(chefe_id = ifelse(
    sum(idade) / 2 == chefe_idade,
    min(morador_id),
    ifelse(idade == chefe_idade, morador_id, 0)
  )) %>%
  mutate(chefe_casal = ifelse(morador_id == chefe_id, 1, 0)) %>%
  mutate(conj_casal = ifelse(morador_id != chefe_id, 1, 0))

rm(idade_casal_filhos01b)

idade_casal_filhos02 <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, genro_nora == 1 &
                            filho_maior24 >= 2, demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 7 | (E05 %in% c(4:6) & idade >= 24))

idade_casal_filhos02ad <- idade_casal_filhos02 %>%
  group_by(A01nficha) %>%
  filter(E05 %in% c(4:6)) %>%
  summarise(idade_filho = max(idade))

idade_casal_filhos02 <-
  merge(idade_casal_filhos02,
        idade_casal_filhos02ad,
        by = "A01nficha",
        all.x = TRUE) %>%   subset(E05 == 7 | idade == idade_filho) %>%
  subset(!(A01nficha == 57658 & morador_id == 4)) # Retira-se este caso específico em que os dois filhos mais velhos são gêmeos.

rm(idade_casal_filhos02ad)

idade_casal_filhos02b <- idade_casal_filhos02 %>%
  group_by(A01nficha) %>%
  summarise(chefe_idade = max(idade))

idade_casal_filhos02 <-
  merge(idade_casal_filhos02,
        idade_casal_filhos02b,
        by = "A01nficha",
        all.x = TRUE)

idade_casal_filhos02 <- idade_casal_filhos02 %>%
  group_by(A01nficha) %>%
  mutate(chefe_id = ifelse(
    sum(idade) / 2 == chefe_idade,
    min(morador_id),
    ifelse(idade == chefe_idade, morador_id, 0)
  )) %>%
  mutate(chefe_casal = ifelse(morador_id == chefe_id, 1, 0)) %>%
  mutate(conj_casal = ifelse(morador_id != chefe_id, 1, 0))

rm(idade_casal_filhos02b)

  # O genro/nora vai ser o chefe neste caso. Imputam-se os filhos mais velhos como cônjuge.
idade_casal_filhos03genro <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, genro_nora == 2 &
                            demanda > 0)$A01nficha
  ) %>%
  subset(E05 == 7)

  # Tem alguns casos que tem dois genros/noras, e não tem filhos com mais de 24 anos, apenas filhos menores.
idade_casal_filhos03filho <-
  subset(
    pdad_2021,
    A01nficha %in% subset(matriz_familiar, genro_nora == 2 &
                            demanda > 0)$A01nficha
  ) %>%
  subset(E05 %in% c(4:6)) %>%
  arrange(desc(idade)) %>%
  group_by(A01nficha) %>%
  slice(1:2)

  # Junto os dados dos genros e filhos.
idade_casal_filhos03 <-
  rbind(idade_casal_filhos03genro, idade_casal_filhos03filho)

idade_casal_filhos03 <- idade_casal_filhos03 %>%
  group_by(A01nficha) %>%
  mutate(chefe_casal = ifelse(E05 == 7, 1, 0)) %>%
  mutate(conj_casal = ifelse(E05 != 7, 1, 0)) %>%
  mutate(chefe_id = ifelse(chefe_casal == 1, morador_id, 0))

rm(idade_casal_filhos03genro, idade_casal_filhos03filho)

  # Junção de todos os casos de casais
casais <-
  rbindlist(
    list(
      idade_casal_avos,
      idade_casal_pais,
      idade_casal_sogros,
      idade_casal_filhos01,
      idade_casal_filhos02,
      idade_casal_filhos03
    ),
    fill = TRUE
  )

  # Apesar de o objeto ser "casais", o número de observações é ímpar porque têm casais que só tem o genro.
casais <-
  casais[, c("A01nficha",
             "morador_id",
             "chefe_casal",
             "conj_casal",
             "chefe_id")]

casais <- as.data.frame(casais)

  # Adição da informação sobre casais na PDAD.
pdad_2021_original <-
  merge(
    pdad_2021_original,
    casais,
    all.x = TRUE,
    by = c("A01nficha", "morador_id")
  )

pdad_2021_original$conj_casal <-
  ifelse(is.na(pdad_2021_original$conj_casal) == TRUE,
         0,
         pdad_2021_original$conj_casal)


  # Juntando as informações de demanda habitacional com a PDAD.
pdad_2021_original <-
  merge(pdad_2021_original,
        matriz_familiar[, c("A01nficha", "demanda")],
        by = "A01nficha",
        all.x = TRUE)

  # Contabilizando a renda do responsável pelo domicílio.
pdad_renda <- pdad_2021_original %>%
  filter(E05 == 1) %>%
  mutate(
    renda_resp = case_when(
      renda_ind_r <= 1100 ~ "ate01SM",
      renda_ind_r > 1100 &
        renda_ind_r <= 3300 ~ "1a3SM",
      renda_ind_r > 3300 &
        renda_ind_r <= 5500 ~ "3a5SM",
      renda_ind_r > 5500 &
        renda_ind_r <= 13200 ~ "5a12SM",
      renda_ind_r > 13200 ~ "maisde12SM"
    )
  ) %>%
  select(A01nficha, renda_resp)

  # Junção dos dados de renda dos responsável com as demais informações da PDAD.
pdad_2021_original <-
  merge(pdad_2021_original,
        pdad_renda,
        by = "A01nficha",
        all.x = TRUE)

rm(pdad_renda, idade_casal_avos, idade_casal_filhos01,
   idade_casal_filhos02, idade_casal_filhos03, idade_casal_pais,
   idade_casal_sogros, matriz_familiar, casais)


################################################################################
#                         Expansão da amostra 
################################################################################


  # Declarar o desenho incial
sample_pdad <- 
  survey::svydesign(id      = ~A01nficha,        # Identificador único da unidade amostrada
                    strata  = ~A01setor,         # Identificação do estrato
                    weights = ~PESO_MOR,         # Inverso da fração amostral
                    nest    = TRUE,              # Parâmetro de tratamento para os IDs dos estratos
                    data    = pdad_2021_original # Declarar a base a ser utilizada
  )

  # Criar um objeto para pós estrato
post_pop <- pdad_2021_original %>%
  dplyr::group_by(POS_ESTRATO) %>%              # Agrupar por pós-estrato
  dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população

  # Declarar o objeto de pós-estrato
  # Estamos dizendo nesse passo qual é a população alvo para cada
  # pós-estrato considerado
amostra <-
  survey::postStratify(sample_pdad,  ~ POS_ESTRATO, post_pop)

  # Ajustar para tratamento de estratos com apenas uma UPA (adjust=centered)
options(survey.lonely.psu = "adjust")

  # Ajustar objeto de amostra, para uso com o pacote srvyr (como tibble)
amostra_mor <- srvyr::as_survey(amostra)

  # Exclui arquivos não mais usados
rm(amostra, sample_pdad, post_pop, pdad_2021_original)


################################################################################
#                         Resultados e inferências
################################################################################


  # Calculando a taxa de chefia por grupo etário.
tx_chefia <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci")))

tx_chefia <- subset(tx_chefia, !is.na(grupo_etario))

  # Calculando o total de pessoas na população.
pop_total <- amostra_mor %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    )
  ) %>%
  srvyr::group_by(grupo_etario) %>%
  srvyr::summarise(pop_total = survey_total(vartype = c("cv", "ci")))

pop_total <- subset(pop_total,!is.na(grupo_etario))

  # Juntando as tabelas para calcular a taxa de chefia por idade.
tx_chefia_idade <- cbind(tx_chefia, pop_total[, -1])

tx_chefia_idade$tx_chefia <-
  tx_chefia_idade$total_chefes / tx_chefia_idade$pop_total

  # Teste para ver se é possível fazer inferência sobre os dados.
  # Necessário coeficiente de variação (cv) menor do que 25%
tx_chefia_idade <- as.data.frame(tx_chefia_idade)

tx_chefia_idade$tx_chefia <-
  ifelse(tx_chefia_idade$total_chefes_cv > 0.25,
         NA,
         tx_chefia_idade$tx_chefia)

rm(tx_chefia, pop_total)


  # Cálculo a taxa de chefia por idade e RA.
tx_chefia_idade_ra <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario, RA_nome) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci")))

tx_chefia_idade_ra <- subset(tx_chefia_idade_ra, !is.na(grupo_etario))

pop_idade_ra <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::group_by(grupo_etario, RA_nome) %>% 
  srvyr::summarise(pop_total=survey_total(vartype = c("cv", "ci")))

pop_idade_ra <- subset(pop_idade_ra, !is.na(grupo_etario))

tx_chefia_idade_raf <-
  cbind(tx_chefia_idade_ra, pop_idade_ra[, -c(1, 2)])

tx_chefia_idade_raf$tx_chefia <-
  tx_chefia_idade_raf$total_chefes / tx_chefia_idade_raf$pop_total

tx_chefia_idade_raf <-
  subset(tx_chefia_idade_raf,!is.na(grupo_etario))

  # Teste para ver se inferências podem ser feitas 
tx_chefia_idade_raf <- as.data.frame(tx_chefia_idade_raf)

tx_chefia_idade_raf$tx_chefia <-
  ifelse(tx_chefia_idade_raf$total_chefes_cv > 0.25,
         NA,
         tx_chefia_idade_raf$tx_chefia)


   # Calcular o número de adultos que têm demanda habitacional por grupo etário.
demanda_idade <- amostra_mor %>%
   srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
   srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                        conj_casal == 0, 1, 0)) %>%
   srvyr::mutate(
     grupo_etario = case_when(
       idade %in% c(24:29) ~ "idade_24_29",
       idade %in% c(30:39) ~ "idade_30_39",
       idade %in% c(40:64) ~ "idade_40_64"
     )
   ) %>%
   srvyr::group_by(grupo_etario) %>%
   srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                              TRUE))
 
 
  # Teste para ver se é possível fazer inferência sobre os dados.
 demanda_idade <- as.data.frame(demanda_idade)
 
 demanda_idade$demanda_ <-
   ifelse(demanda_idade$demanda__cv > 0.25, NA, demanda_idade$demanda_)
 
 demanda_idade <- subset(demanda_idade,!is.na(grupo_etario))
 
 demanda_idade_final  <-
   merge(demanda_idade, tx_chefia_idade, by = c("grupo_etario"))
 
 demanda_idade_final$demanda_final <-
   demanda_idade_final$demanda_ * demanda_idade_final$tx_chefia

  # Tabela final 
demanda_idade_final 
 


  # Calcular o número de adultos que têm demanda habitacional por grupo etário e RA.
demanda_idade_RA <- amostra_mor %>%
  srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
  srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                       conj_casal == 0, 1, 0)) %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    )
  ) %>%
  srvyr::group_by(grupo_etario, RA_nome) %>%
  srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                             TRUE))

  # Teste para ver se é possível fazer inferência sobre os dados.
demanda_idade_RA <- as.data.frame(demanda_idade_RA)

demanda_idade_RA$demanda_ <-
  ifelse(demanda_idade_RA$demanda__cv > 0.25,
         NA,
         demanda_idade_RA$demanda_)

demanda_idade_RA <- subset(demanda_idade_RA,!is.na(grupo_etario))

demanda_idade_RA_final  <-
  merge(demanda_idade_RA,
        tx_chefia_idade_raf,
        by = c("grupo_etario", "RA_nome"))

demanda_idade_RA_final$demanda_final <-
  demanda_idade_RA_final$demanda_ * demanda_idade_RA_final$tx_chefia

 # Tabela final
demanda_idade_RA_final

 # Remover tabelas não mais usadas
rm(tx_chefia_idade, tx_chefia_idade_ra, tx_chefia_idade_raf,
   demanda_idade_RA, pop_idade_ra)



  # Comparação grupo etário e rendimento
demanda_ss <- amostra_mor %>% 
  srvyr::filter(demanda>0) %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64"),
                arranjo2 = ifelse(arranjos %in% c(1,3:6),"Uni/Casais",
                                  ifelse(arranjos == 2,"Monoparental Fem",
                                         ifelse(arranjos == 7,"Outro",NA)))) %>%
  srvyr::group_by(grupo_etario, arranjo2) %>% 
  srvyr::summarise(demanda_=survey_total(vartype = c("cv", "ci"), na.rm=TRUE)) %>% 
  dplyr::filter(!is.na(grupo_etario))

tx_chefia <- amostra_mor %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64"),
                arranjo2 = ifelse(arranjos %in% c(1,3:6),"Uni/Casais",
                                  ifelse(arranjos == 2,"Monoparental Fem",
                                         ifelse(arranjos == 7,"Outro",NA)))) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci"))) %>% 
  dplyr::filter(!is.na(grupo_etario))

pop_total <- amostra_mor %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(pop_total=survey_total(vartype = c("cv", "ci"))) %>% 
  dplyr::filter(!is.na(grupo_etario))

tb_tx_chefia <- 
demanda_ss %>% 
  inner_join(tx_chefia) %>% 
  inner_join(pop_total) %>% 
  select(-contains("cv"), -contains("low"), -contains("upp")) %>% 
  mutate(demanda_final = demanda_*total_chefes/pop_total) %>% 
  janitor::adorn_totals()

tb_demanda_geral <- 
demanda_idade %>% 
  inner_join(tx_chefia) %>% 
  inner_join(pop_total) %>% 
  select(-contains("cv"), -contains("low"), -contains("upp")) %>% 
  mutate(demanda_final = demanda_*total_chefes/pop_total) %>% 
  janitor::adorn_totals() 

tb_tx_chefia <- 
tb_demanda_geral %>% 
  mutate(tx.chefia = total_chefes/pop_total) %>% 
  select(grupo_etario,tx.chefia) 

  # Tabela de renda domiciliar dos demandantes
tb_renda_demandante <-
  amostra_mor %>%
  srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
  srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                       conj_casal == 0, 1, 0)) %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    ),
    renda_dom = case_when(
      renda_ind_r <= 1100 ~ "ate01SM",
      renda_ind_r > 1100 &
        renda_ind_r <= 3300 ~ "1a3SM",
      renda_ind_r > 3300 &
        renda_ind_r <= 5500 ~ "3a5SM",
      renda_ind_r > 5500 &
        renda_ind_r <= 13200 ~ "5a12SM",
      renda_ind_r > 13200 ~ "maisde12SM"
    )
  ) %>%
  srvyr::group_by(grupo_etario, renda_dom) %>%
  srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                             TRUE)) %>%
  dplyr::filter(!is.na(grupo_etario)) %>%
  inner_join(tb_tx_chefia) %>%
  mutate(demanda_final = demanda_ * tx.chefia) %>%
  select(grupo_etario, renda_dom, demanda_final) %>%
  tidyr::spread(grupo_etario, demanda_final) 

  # Tabela final
tb_renda_demandante

  # Remove as tabelas não mais utilizadas
rm(demanda_idade, demanda_ss, tx_chefia, pop_total, tb_tx_chefia, tb_demanda_geral)

################################################################################
#                               FIM 
################################################################################
