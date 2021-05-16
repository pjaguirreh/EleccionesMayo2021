###################
# CARGAR PAQUETES #
###################

library(RSelenium)
library(rvest)
library(openxlsx)
library(tidyverse)

#######################
# CONECTAR A SELENIUM #
#######################

remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browser = "firefox"
)

rd <- remDr$client

# Ir a la web del SERVEL
url <- 'http://www.servelelecciones.cl/'
#rd$open()
rd$navigate(url)

##################################################################
# Crear listas de regiones, distritos, comunas, locales, y mesas #
##################################################################
# Función de ayuda (se usará para lista de regiones, comunas, circunscripciones, locales, y mesas)
limpiar_lista <- function(x){
  x %>% 
    str_extract_all(">(.*?)<") %>% 
    data.frame() %>% as_tibble() %>% 
    rename(nombre = 1) %>% 
    filter(nombre != "><",
           !str_detect(nombre, "\\.\\.\\.")) %>% 
    mutate(nombre = str_remove(nombre, ">"),
           nombre = str_remove(nombre, "<")) %>% 
    mutate(inicial = str_sub(nombre, 1, 1)) %>% 
    arrange(inicial)
}

## -----------------------------------------
## Extraer lista de comunas para cada región
## -----------------------------------------

# Objeto que identifica la lista de regiones
webElemRegion <- rd$findElement(using = "css", 
                                value = "#selRegion")

# Extraer lista y limpiar
lista_regiones <- webElemRegion$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

# Sacar lista de comunas para cada región
reg_com <- tibble()
for (r in seq_along(lista_regiones$inicial)){
  
  # inicial de la region
  ini_reg <- lista_regiones$inicial[r]
  
Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemRegion <- rd$findElement(using = "css", 
                                  value = "#selRegion")
  webElemRegion$sendKeysToElement(list(ini_reg))
  
Sys.sleep(0.5)
  
  # Generar lista de comunas
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  lista_comunas <- webElemComuna$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
Sys.sleep(0.5)
  
  com_r <- lista_comunas %>% 
    mutate(comuna = lista_regiones$nombre[r])
  
Sys.sleep(0.5)
  
  reg_com <- bind_rows(reg_com, com_r)
}

reg_com <- reg_com %>% 
  arrange(nombre)

rd$navigate(url)
Sys.sleep(0.5)

## -------------------------------------------------
## Extraer lista de circ. electoral para cada comuna
## -------------------------------------------------
com_circelectoral <- tibble()
for (c in seq_along(reg_com$inicial)){
  
  # inicial de la comuna
  ini_com <- reg_com$inicial[c]
  
  Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  webElemComuna$sendKeysToElement(list(ini_com))
  
  Sys.sleep(0.5)
  
  # Generar lista de circunscripciones
  webElemCircunscripcion <- rd$findElement(using = "css", 
                                  value = "#selCircunscripcionElectorales")
  lista_circunscripciones <- webElemCircunscripcion$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  Sys.sleep(0.5)
  
  circ_com <- lista_circunscripciones %>% 
    mutate(reg = reg_com$nombre[c])
  
  Sys.sleep(0.5)
  
  com_circelectoral <- bind_rows(com_circelectoral, circ_com)
}

com_circelectoral <- com_circelectoral %>% 
  arrange(nombre)

rd$navigate(url)
Sys.sleep(0.5)

## -------------------------------------
## Extraer lista de comunas por distrito
## -------------------------------------

# Objeto que identifica la lista de distritos
webElemDistrito <- rd$findElement(using = "css",
                                  value = "#selDistrito")

# Extraer lista y limpiar
lista_distritos <- webElemDistrito$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

distrito_comuna <- tibble()
for (d in seq_along(lista_distritos$inicial)){
  
  # inicial de la comuna
  ini_dis <- lista_distritos$inicial[d]
  
  Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemDistrito <- rd$findElement(using = "css",
                                    value = "#selDistrito")
  webElemDistrito$sendKeysToElement(list(ini_dis))
  
  Sys.sleep(0.5)
  
  # Generar lista de comunas
  webElemComuna <- rd$findElement(using = "css", 
                                           value = "#selComunas")
  lista_comunas <- webElemComuna$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  # Extraer N de candidatos por distrito
  webElemNCandidatos <- rd$findElement(using = "css", value = "b.ng-binding")
  n_constituyentes <- webElemNCandidatos$getElementText()[[1]] %>% 
    str_remove("Candidatos a elegir: ") %>% 
    as.numeric()
  
  Sys.sleep(0.5)
  
  com_d <- lista_comunas %>% 
    mutate(distrito = lista_distritos$nombre[d],
           n_constituyentes = n_constituyentes)
  
  Sys.sleep(0.5)
  
  distrito_comuna <- bind_rows(distrito_comuna, com_d)
}

distrito_comuna <- distrito_comuna %>% 
  arrange(nombre)

rd$navigate(url)
Sys.sleep(0.5)

## -----------------------------------------------
## Extraer locales de votación por circunscripcion
## -----------------------------------------------

# Objeto que identifica la lista de distritos
webElemCircunscripcion <- rd$findElement(using = "css",
                                  value = "#selCircunscripcionElectorales")

# Extraer lista y limpiar
lista_circunscripciones <- webElemCircunscripcion$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

locales_circunscripcion <- tibble()
for (c in seq_along(lista_circunscripciones$inicial)){
  
  # inicial de la comuna
  ini_cir <- lista_circunscripciones$inicial[c]
  
  Sys.sleep(0.5)
  
  # insertar inicial en menú desplegable de web
  webElemCircunscripcion <- rd$findElement(using = "css",
                                           value = "#selCircunscripcionElectorales")
  webElemCircunscripcion$sendKeysToElement(list(ini_cir))
  
  Sys.sleep(0.5)
  
  # Generar lista de comunas
  webElemLocales <- rd$findElement(using = "css",
                                   value = "#selLocalesVotacion")
  lista_locales <- webElemLocales$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  Sys.sleep(0.5)
  
  circ_l <- lista_locales %>% 
    mutate(circunscripcion = lista_circunscripciones$nombre[c])
  
  Sys.sleep(0.5)
  
  locales_circunscripcion <- bind_rows(locales_circunscripcion, circ_l)
}

locales_circunscripcion <- locales_circunscripcion %>% 
  arrange(nombre)

rd$navigate(url)
Sys.sleep(0.5)

###############################
## TABLA AGREGADA PRELIMINAR ##
###############################

com_circelectoral <- com_circelectoral %>% 
  rename("circunscripcion" = nombre) %>% 
  select(-inicial)

distrito_comuna <- distrito_comuna %>% 
  rename("comuna" = nombre) %>% 
  select(-inicial)

locales_circunscripcion <- locales_circunscripcion %>% 
  rename("local" = nombre) %>% 
  select(-inicial)

pre_tabla_agregada <- reg_com %>% 
  rename("comuna" = nombre) %>% 
  select(-inicial) %>% 
  left_join(distrito_comuna, by = "comuna") %>% 
  left_join(com_circelectoral, by = "comuna") %>% 
  left_join(locales_circunscripcion, by = "circunscripcion") %>% 
  select(region = reg, distrito, n_constituyentes, comuna, circunscripcion, local) %>% 
  mutate(ini_circunscripcion = str_sub(circunscripcion, 1, 1),
         ini_local = str_sub(local, 1, 1)) %>% 
  arrange(circunscripcion, local)

## -------------------------------------
## Extraer mesas por locales de votación
## -------------------------------------
#mesas_locales <- tibble()
#for (c in seq_along(pre_tabla_agregada$ini_circunscripcion)){
#  
#  # inicial de la circunscripcion
#  ini_cir <- pre_tabla_agregada$ini_circunscripcion[c]
#  
#  Sys.sleep(0.5)
#  
#  # insertar inicial en menú desplegable de web
#  webElemCircunscripcion <- rd$findElement(using = "css",
#                                           value = "#selCircunscripcionElectorales")
#  webElemCircunscripcion$sendKeysToElement(list(ini_cir))
#  
#  Sys.sleep(0.5)
#  
#  circ_zoom <- pre_tabla_agregada %>% distinct(circunscripcion) %>% slice(c) %>% pull()
#  
#  mesa_buscar <- pre_tabla_agregada %>% 
#    filter(circunscripcion == circ_zoom)
#  
#  for (m in seq_along(mesa_buscar$ini_local)){
#    
#    #ini_mesa <- mesa_buscar$ini_local[m]
#
#    #Sys.sleep(0.5)
#    
#    # insertar inicial en menú desplegable de web
#    #webElemLocal <- rd$findElement(using = "css",
#    #                              value = "#selLocalesVotacion")
#    #webElemLocal$sendKeysToElement(list(ini_mesa))
#    
#    rd$findElement(using = "css",
#                   value = paste0("#selLocalesVotacion > option:nth-child(", m+1,")"))$clickElement()
#    
#    Sys.sleep(0.5)
#    
#    # Generar lista de mesas
#    webElemMesas <- rd$findElement(using = "css",
#                                     value = "#selMesasReceptoras")
#    lista_mesas <- webElemMesas$getElementAttribute("outerHTML") %>% 
#      limpiar_lista()
#    
#    Sys.sleep(0.5)
#    
#    webElemtextocheck <- rd$findElement(using = "css", value = "#divCabecera > h5:nth-child(1) > span:nth-child(1)")
#    texto_check <- webElemtextocheck$getElementText()[[1]] %>% 
#      str_remove("Votación Candidatos por Circ. Electoral ")
#    
#    
#    mesas_l <- lista_mesas %>% 
#      mutate(circunscripcion = mesa_buscar$circunscripcion[m],
#             local = mesa_buscar$local[m],
#             texto_check = texto_check
#             )
#    
#    Sys.sleep(0.5)
#    
#    mesas_locales <- bind_rows(mesas_locales, mesas_l)
#    
#    print(mesas_l)
#  }
#}
#
#mesas_locales <- mesas_locales %>% 
#  arrange(nombre)