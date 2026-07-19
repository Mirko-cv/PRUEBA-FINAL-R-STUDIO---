### PARTE 1 DEL TRABAJO

library(dplyr) # manipulación
library(ggplot2) # graficos
library(readr) # lee el csv


df <- read_csv("Import02_2026.csv")

# %>%  el pipe nos sirve para concatenar funciones
df %>% summary()

df %>% is.na() %>% colSums() # ver valoresfaltantes

df %>% glimpse()

####################################
# EDA : exploratory data analysis  #
####################################

# variables categoricas

df %>% 
  select(Sede) %>% # selecciono sede
  table() %>% # realizo la tabla
  as_tibble() %>% # formato tabla amigable
  arrange(desc(n) ) # ordeno la tabla descendente por n


df %>% 
  select(Tipo.de.Envase) %>% # selecciono sede
  table() %>% # realizo la tabla
  as_tibble() %>% # formato tabla amigable
  arrange(desc(n) ) # ordeno la tabla descendente por n



df %>% 
  select(Pais.Origen) %>% # selecciono sede
  table() %>% # realizo la tabla
  as_tibble() %>% # formato tabla amigable
  arrange(desc(n) ) # ordeno la tabla descendente por n

# variables numericas

df %>% 
  select(Peso.Kg.) %>% 
  summary()

df %>% 
  select(Peso.Kg.) %>% # selecciona pesos
  summarise( # saca la ssiguientes estadisticas
    promedio = mean(Peso.Kg., na.rm= T),
    mediana = median(Peso.Kg., na.rm= T),
    sd = sd(Peso.Kg., na.rm= T),
    asimetria = moments::skewness(Peso.Kg., na.rm= T),
    min = min(Peso.Kg., na.rm= T),
    max = max(Peso.Kg., na.rm= T)
    
  )


# 1 variable categoria y 1 numerica


df %>% 
  group_by(Tipo.de.Envase) %>% # agrupa por tipo de envase 
  select(Peso.Kg.) %>% # selecciona pesos
  summarise( # saca la ssiguientes estadisticas
    promedio = mean(Peso.Kg., na.rm= T),
    mediana = median(Peso.Kg., na.rm= T),
    sd = sd(Peso.Kg., na.rm= T),
    asimetria = moments::skewness(Peso.Kg., na.rm= T),
    min = min(Peso.Kg., na.rm= T),
    max = max(Peso.Kg., na.rm= T)
    
  ) %>% 
  arrange(desc(promedio) ) # ordena de mayor a menor por promedio

# boxplot pro grupos
plot1 <- df %>% 
  ggplot(aes(x=Peso.Kg., y =Tipo.de.Envase, fill=factor(Tipo.de.Envase) ))+ # grafica el peso y que su relleno(fill) se diferencie por tipo de envase
  geom_boxplot(outliers = F)+
  geom_jitter(size=2,shape=21,alpha=0.1 )+
  scale_x_continuous(limits = c(0,1e+06))+
  stat_summary(fun=mean, col = "black", shape=1 )+
  ggthemes::theme_fivethirtyeight()+
  theme(legend.position = "none") # elimina leyenda

#densidad por grupos

df <- df %>% 
  mutate(
    
  Pais = case_when( Pais.Origen %in% c("BOLIVIA","CHILE","ECUADOR","BRASIL","COLOMBIA") ~ "Pais vecino",
               T ~ "Otro")
    
  ) # crea columnas


promedio <- df %>% 
  group_by(Pais) %>% 
  summarise(promedio = mean(Peso.Kg.))

P_dist_pais <- df %>% 
  ggplot(aes(x=Peso.Kg., fill=factor(Pais) ))+ # grafica el peso y que su relleno(fill) se diferencie por pais
  geom_density()+
  geom_vline(data = promedio, aes(xintercept = promedio, col="red"), size = 1, linetype="dashed")+
  scale_x_continuous(limits = c(-1,5e+05))+
  facet_grid(Pais~., scale="free")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Accent")+
  labs(title="Distibución del peso de importaciónes de productos vegetales según país origen",
       y="",
       caption="Fuente:MIDAGRI") 


### POR SUBPRODUCTO

df <- df %>% 
  tidyr::separate( col = Producto, sep= ", ", into = c("Producto2","Subproducto") ) # creamos nueva columnas a partir de  prodcuto


# la soya es el producto mas consumido
df %>% 
  select(Producto2) %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(desc(n))



## nos concentramos en la soya
df %>% 
  filter(Producto2=="SOYA") %>% 
  select(Subproducto)%>% 
  table() %>% 
  as_tibble() %>% 
  arrange(desc(n))

#################################################################################
#         Formato tidy data
#################################################################################

df_soya <- df %>% filter(Producto2 == "SOYA")


df_largo <- df_soya %>% select(Producto2,Subproducto,Peso.Kg.)

#df_ancho <- 

  df_largo %>% tidyr::pivot_wider(names_from = Subproducto, values_from = Peso.Kg.) %>% View()
  

  
  
  
######################### primera parte
  library(patchwork)
  
  pjoin <- P_dist_pais + plot1
  
  ggsave("./collage/grafico.png") 
  
  
  