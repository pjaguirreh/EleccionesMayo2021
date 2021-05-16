funcion_limpiar_resultados <- function(x){
  
  x %>% 
    str_split("\n") %>% 
    `[[`(1) %>% 
    enframe() %>% 
    select(-name) %>% 
    mutate(check_candidato = as.numeric(str_sub(value, 1, 1)),
           check2 = ifelse(is.na(check_candidato), value, NA)) %>% 
    fill(check2, .direction = "down") %>% 
    filter(!is.na(check_candidato)) %>% 
    select(lista = check2, value) %>% 
    separate(value, into = c("nombre", "otro"), sep = " \\(") %>% 
    separate(otro, into = c("sexo", "partido", "votos", "per"), sep = " ") %>% 
    mutate(sexo = str_remove(sexo, "\\)"),
           nombre = str_remove(nombre, "\\."),
           nombre = str_remove(nombre, "[[:digit:]]+"),
           nombre = str_squish(nombre),
           lista = str_remove(lista, " \\(.*"),
           lista = str_remove(lista, " 0.*")) %>% 
    mutate(circunscripcion = texto[1], 
           local = texto[2], 
           mesa = texto[3]) %>% 
    select(circunscripcion, local, mesa, everything())
  
}


mesas_local <- tibble()
resultados_mesas <- list()
conteo <- 1
for (c in 1:n_circunscripciones){
  
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
      
      
      mesas_local <- bind_rows(resultados_mesas,
                                    tibble(circunscripcion = texto[1], local = texto[2], mesa = texto[3]))
      
      Sys.sleep(0.5)
      
      #resultados_mesas[[conteo]] <- rd$findElement(using = "css",
      #                                             value = paste0(".table > tbody:nth-child(2)"))$getElementText()[[1]] %>% 
      #  funcion_limpiar_resultados()
      
      
      #print(resultados_mesas[[conteo]])
      print(paste(circunscripcion = texto[1], "///",local = texto[2], "///", mesa = texto[3]))
      
      conteo <- conteo+1
      
    }
    
  }
  
}










