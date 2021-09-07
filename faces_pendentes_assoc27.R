setwd("/Users/vitorcouto599/Downloads/Monitoramento_Atividades/pendencias_associacao_e_trackings23")
library(ggplot2)
library(dplyr)
library(stringr)
library(gdata)
library(RColorBrewer)

pendencia_al = read.csv("faces_pendentes_assoc_27.csv")
pendencia_al$COD_SETOR = as.character(pendencia_al$COD_SETOR)

alagoas_munic = pendencia_al %>%
  mutate(cod_mun = str_sub(pendencia_al$COD_SETOR, end = 7))

munic = read.csv("cod_municipios.csv" , header = FALSE)
munic$V1 = as.character(munic$V1)

join_munic = alagoas_munic %>%
  left_join(munic, by = c("cod_mun" = "V1"))

munic_fechado = munic %>%
  anti_join(alagoas_munic, by = c("V1" = "cod_mun"))

agencias = join_munic %>%
  rename.vars(c("V2","V3"), c("MUNICIPIO","AGENCIA")) %>%
  select(NUM_FACE, COD_SETOR, MUNICIPIO, AGENCIA) %>%
  group_by(AGENCIA) %>%
  summarise(desassociadas = n(), setores = n_distinct(COD_SETOR), municipios = n_distinct(MUNICIPIO))

med_global = mean(agencias$desassociadas)
x_start <- med_global + 60
y_start <- 4.7
x_end <- med_global
y_end <- 7.5

ggplot(agencias, aes(setores, desassociadas, fill = AGENCIA)) +
  geom_col() + theme_minimal()

# escala de cor
palette <- brewer.pal(5, "Reds")[-(2:4)]

# Add tema
plot_cnefe = ggplot(agencias, aes(x = desassociadas, y = AGENCIA, color = desassociadas)) +
  geom_point(size = 9) + 
  geom_segment(aes(xend = -50, yend = AGENCIA), size = 3) +
  geom_text(aes(label = round(desassociadas,1)), color = "black", size = 3) +
  scale_x_discrete("", limits = c(500, 1000), position = "top") +
  expand_limits(x = 1200, y = -0)+
  scale_color_gradientn(colors = palette) +
  labs(title = "Monitoramento das atividades de associação de faces, 27-ago", caption = "Fonte: Relatório PR/COC/CNEFE")

plot_cnefe + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept = med_global, color = "grey40", linetype = 3) +
  annotate(
    "text",
    x = x_start, y = y_start,
    label = "A\nmédia\nAlagoas",
    vjust = 1, size = 3, color = "grey40"
  ) +
  annotate(
    "curve",
    x = x_start, y = y_start,
    xend = x_end, yend = y_end,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )

library(xlsx)
write.xlsx(join_munic,file="join_munic.xlsx")
