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

# Ir a la eleccion de gobernador
rd$findElement(using = "css",
               value = ".nav > li:nth-child(2)")$clickElement()

# Funciones de ayuda 
source("Funciones.R", encoding = "UTF-8")

# Cargar lista de regiones, comunas, distritos, y circunscripciones
lista_RegComDisCir <- read_csv("datos/Lista_RegionDistritoComunaCircunscripcion.csv")

circunscripcionesaextraer <- pre_tabla_agregada %>% 
  filter(region == "METROPOLITANA DE SANTIAGO") %>% # "SILENCIAR" SI NO SE QUIERE FILTRAR
  distinct(circunscripcion) %>% pull()

# N° de circunscripcion que se usará para scraping
n_circunscripciones <- rd$findElement(using = "css",
                                      value = "#selCircunscripcionElectorales")$getElementAttribute("outerHTML") %>% 
  limpiar_lista() %>% 
  mutate(orden = row_number()) %>% 
  filter(nombre %in% circunscripcionesaextraer) %>%
  pull(orden)

#n_circunscripciones <- rd$findElement(using = "css",
#               value = "#selCircunscripcionElectorales")$getElementAttribute("outerHTML") %>% 
#  limpiar_lista() %>% 
#  nrow()

mesas_local_gob <- tibble()
resultados_mesas_gob <- list()
conteo <- 1
for (c in n_circunscripciones){
  
  rd$findElement(using = "css",
                 value = paste0("#selCircunscripcionElectorales > option:nth-child(", c+1,")"))$clickElement()
  
  Sys.sleep(0.5)
  
  x <- rd$findElement(using = "css",
                      value = "#selLocalesVotacion")
  
  n_locales <- x$getElementAttribute("outerHTML") %>% 
    limpiar_lista() %>% 
    nrow()
  
  for (l in 1:n_locales){
    
    rd$findElement(using = "css",
                   value = paste0("#selLocalesVotacion > option:nth-child(", l+1,")"))$clickElement()
    
    Sys.sleep(0.5)
    
    y <- rd$findElement(using = "css",
                        value = "#selMesasReceptoras")
    
    n_mesas <- y$getElementAttribute("outerHTML") %>% 
      limpiar_lista() %>% 
      nrow()
    
    for (m in 1:n_mesas){
      
      rd$findElement(using = "css",
                     value = paste0("#selMesasReceptoras > option:nth-child(", m+1,")"))$clickElement()
      
      Sys.sleep(0.5)
      
      texto <- rd$findElement(using = "css",
                              value = "#divCabecera > h5:nth-child(1) > span:nth-child(1)")$getElementText()[[1]] %>% 
        str_remove("Votación Candidatos por Circ. Electoral ") %>% 
        str_split(" > ") %>% 
        `[[`(1)
      
      
      mesas_local_gob <- bind_rows(mesas_local_gob,
                                     tibble(circunscripcion = texto[1], local = texto[2], mesa = texto[3]))
      
      Sys.sleep(0.5)
      
      resultados_mesas_gob[[conteo]] <- rd$findElement(using = "css",
                                                         value = paste0(".table > tbody:nth-child(2)"))$getElementText()[[1]] %>% 
        funcion_limpiar_resultados_gob()
      
      
      #print(resultados_mesas[[conteo]])
      print(paste(circunscripcion = texto[1], "///",local = texto[2], "///", mesa = texto[3]))
      
      conteo <- conteo+1
      
    }
    
  }
  
}

resultados_mesas_gob_final <- bind_rows(resultados_mesas_gob)
