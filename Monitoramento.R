library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(gdata)
library(RColorBrewer)
library(lubridate)

extrator = function (x) {
  setwd(x)
  faces_pendentes_assoc_27 = read.csv("faces_pendentes_assoc_27.csv")
  faces_pendentes_assoc_27$COD_SETOR = as.character(faces_pendentes_assoc_27$COD_SETOR)
  
  municipios_al = faces_pendentes_assoc_27 %>%
  mutate(cod_mun = str_sub(faces_pendentes_assoc_27$COD_SETOR, end = 7))
  
  cod_municipios = read.csv("cod_municipios.csv" , header = FALSE)
  cod_municipios$V1 = as.character(cod_municipios$V1)
  
  join_munic = municipios_al %>%
    left_join(cod_municipios, by = c("cod_mun" = "V1"))
  
  munic_fechado = cod_municipios %>%
    anti_join(municipios_al, by = c("V1" = "cod_mun"))
  
  situacao = join_munic %>%
    rename.vars(c("V2","V3"), c("MUNICIPIO","AGENCIA")) %>%
    select(NUM_FACE, COD_SETOR, MUNICIPIO, AGENCIA) %>%
    group_by(AGENCIA) %>%
    summarise(desassociadas = n(), setores = n_distinct(COD_SETOR), municipios = n_distinct(MUNICIPIO)) 
  return(situacao)
  }

lapply(1:31,function(i){
  tabela <- paste0("tabela", i)
  x <- paste0("E:/Users/vitor.vieira/Downloads/Monitoramento_Atividades/pendencias_associacao_e_trackings", i)
     assign(tabela, extrator(x), envir = .GlobalEnv)
  })

tabelas = sprintf("tabela%s", 1:31) # or paste0("tabela", 1:3)
df.list = mget(tabelas) # or mget(ls(pattern = '^tabela\\d+$'))
tabela18 %>% add_row(AGENCIA = "SANTANA DO IPANEMA", .before = 7)

df_row = tabelas %>% 
  reduce(.init = get(tabelas[1]), ~ bind_rows(..1, get(..2))) %>%
  mutate(Quinzenal = rep(seq(as.Date("2020-06-12"), by = 15, length.out = 32), each = 10))

ggplot(df_row, aes(x = Quinzenal, y = desassociadas,
                      group = AGENCIA,
                      colour = AGENCIA)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_hue(name="Agências", l=60)  +   
  xlab("Tempo Quinzenal") + ylab("Total Desassociadas") +
  ggtitle("Faces Desassociadas por Agência") +
  theme_minimal()




