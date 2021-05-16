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
# Funciones de ayuda 
source("Funciones.R", encoding = "UTF-8")


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
  select(region = reg, distrito, n_constituyentes, comuna, circunscripcion) %>% 
  arrange(comuna)

write_excel_csv(pre_tabla_agregada, "datos/Lista_RegionDistritoComunaCircunscripcion.csv")
