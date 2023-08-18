

# Instalacao e carregamento de pacotes ------------------------------------

if (!require("pacman")) install.packages("pacman");pacman:: p_load(lubridate,data.table, magrittr, stringr, dplyr, tidyr, googledrive)

#a primeira etapa foi realizada pelo python vamos agora dar continuidade com o tratamento dos dados pelo divo do tidyverse. Dito isso, R pode entrar.

#CNS_PAC = Número do CNS (Cartão Nacional de Saúde) do paciente (criptografia)


DATASUS <- rio::import("DATASUS.csv")


# Verificando informacoes em colunas --------------------------------------

unique(DATASUS$DT_ATEND) #202112 202111 202110, foram retornadas 3 datas de 2021 verificar com a prof


# Transformando as datas --------------------------------------------------


DATASUS_TRATADO <- DATASUS %>%
  mutate(data_inicio = format(ymd(DT_INICIO), "%d-%m-%Y"),
         data_fim = format(ymd(DT_FIM), "%d-%m-%Y"),
         inicio2 = format(ymd(INICIO), "%d-%m-%Y"),
         fim2 = format(ymd(DT_INICIO), "%d-%m-%Y"),
         nascimento = format(ymd(DTNASC), "%d-%m-%Y")
         )


# Ordenando por pessoa e selecionando ultimos atendimentos ----------------


DATASUS_final <- DATASUS_TRATADO %>% 
  dplyr::group_by(CNS_PAC) %>%
  dplyr::summarise(DT_INICIO = min(inicio2),
                   DT_FIM = max(fim2),
                   PERMANENCIA = sum(PERMANEN),
                   SEXO = unique(SEXOPAC),
                   RACA = unique(RACACOR),
                   SIT_RUA = unique(SIT_RUA),
                   TP_DROGA = toString(unique(TP_DROGA)),
                   LOC_REALIZ = unique(LOC_REALIZ),
                   CIDPRI = toString(unique(CIDPRI)),
                   CIDASSOC = toString(unique(CIDASSOC)),
                   UFMUN = toString(unique(UFMUN)),
                   MUNPAC = toString(unique(MUNPAC)))




write.csv2(x = DATASUS_final, file = "DATASUS_final.csv")



# Censuras ----------------------------------------------------------------

#a variavel tempo de permanencia indica o periodo que o paciente ficou em atendimento.
#existem das 58.321, 40.752 não possuem registro de tempo inicial, se caracterizando como censura a esquerda.

table(is.na(DATASUS_final$PERMANENCIA))


