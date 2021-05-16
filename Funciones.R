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


funcion_limpiar_resultados_gob <- function(x){
  
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
    mutate(nombre = str_remove_all(value, "[[:digit:]]+"),
           nombre = str_remove_all(nombre, "\\."),
           nombre = str_remove_all(nombre, "\\,%"),
           nombre = str_squish(nombre),
           votos = str_extract(value, " [[:digit:]]+ "),
           votos = as.numeric(str_squish(votos)),
           lista = str_remove_all(lista, "[[:digit:]]+"),
           lista = str_remove_all(lista, "\\,%"),
           lista = str_squish(lista)) %>% 
    mutate(circunscripcion = texto[1], 
           local = texto[2], 
           mesa = texto[3]) %>% 
    select(circunscripcion, local, mesa, everything(), -value)
  
}
