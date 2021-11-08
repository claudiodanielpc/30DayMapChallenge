#30DayMapChallenge
#


##Borrar datos del entorno
rm(list=ls())





# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, sf, showtext)

#Carga de fuentes necesarias para los gráficos
font_add_google("Montserrat")
showtext_auto()


#Directorio de trabajo para almacenar datos de ENIGH
setwd("D:/datos/enigh")

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_csv.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)

# unzip

unzip(tf, files="viviendas.csv", exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,"viviendas.csv")
unlink(td)

#Leer el archivo

enigh<-read.csv(fpath)%>%
  rename(folioviv=1)%>%
#Calcular rezago habitacional====
  mutate(
    across(starts_with("mat") & where(is.character),
           ~ replace_na(parse_number(.x, na = c('', 'NA', '&')), 0)),
    rezago=if_else(
      ((tot_resid / num_cuarto) > 2.5) |
        (mat_pared %in% 1:6) |
        (mat_techos %in% c(1:4, 6, 7, 9)) |
        (mat_pisos == 1) |
        (excusado == 2),
      "En rezago",
      "Fuera de rezago"
    ))%>%
                        #Obtener la clave de entidad de la variable folioviv
                        mutate(cve_ent=
                                 case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                                           TRUE ~ substr(folioviv,1,2)),
                               cve_ent=
                                 case_when(nchar(cve_ent)==1 ~ paste0("0",cve_ent),
                                                 TRUE ~ cve_ent
                               
                               ))%>%
  #Agrupar para obtener porcentaje de vivienda en rezago
  group_by(cve_ent,rezago)%>%
  summarise(viviendas=(sum(factor)))%>%
  ungroup()%>%
  group_by(cve_ent)%>%
  mutate(pct=viviendas/sum(viviendas)*100)%>%
  filter(rezago=="En rezago")%>%
  select(cve_ent,pct)




#Directorio de trabajo
#Esto se debe de cambiar en cada computadora
setwd("D:/Documentos/GitHub/30DayMapChallenge")

hexagono<-st_read("https://raw.githubusercontent.com/JuveCampos/30DayMapChallenge2021/main/04%20-%20Hexa%CC%81gonos/2021-10-13%20Mapa%20miel/hexagonos_estatales_dv.geojson")%>%

#Pegamos los datos a los hexágonos
left_join(., enigh, by = c("region" = "cve_ent"))%>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])%>%
  mutate(label = str_wrap(str_c(toupper(state_abbr_official), "\n", round(pct, 2), "%"), 10))

#Mapa

  mapa<-hexagono %>%
  ggplot(aes(fill = pct)) +
  geom_sf(size = 1.5,
          color = "#636363") +
  geom_text(aes(x = X,
                y = Y,
                label = label),
            size = 7,
            fontface="bold",
            family="Montserrat"
            ) +
    
    scale_fill_distiller("%",
                         palette = "YlOrBr", direction = 1, limits=c(0,80),
                         labels = scales::comma_format(suffix = "%"))+
  labs(title = "Viviendas en condición de rezago habitacional por entidad federativa, 2020",
       subtitle = "(Porcentaje respecto del total de viviendas particulares habitadas de cada entidad)",
       fill = "Porcentaje de viviendas particulares habitadas en rezago (%)",
       caption = "Fuente: @claudiodanielpc con datos de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2020.") +
  theme_bw() +
  theme(text=element_text(family="Montserrat"),
    panel.background = element_blank(),
        panel.grid =  element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, size=35,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=30, face="italic"),
        plot.caption = element_text(hjust = 0,size=22),
        axis.title = element_blank(),
        legend.position = "bottom",
    legend.text = element_text(size=30),
    legend.title = element_text(size=30)
    
    ) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5
  ))


  
  #Salvar
  ggsave("day_4_hexagon.png", height = 7,
         width = 6.5, units="in", dpi=300)  
