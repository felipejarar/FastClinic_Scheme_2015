#lang racket

(provide ctdadRegistros)
(provide caddddr)
(provide cadddddr)
(provide caddddddr)
(provide cadddddddr)
(provide caddddddddr)
(provide listar_Y_Contar_Elementos_En_Listas_V1)
(provide verificarElementoEnLista_V1)

(provide concatenar_Listas_De_Texto)
(provide obtener_Maximo_Lista)
(provide obtener_Maximos_Elementos_Lista_Contada)
(provide convertir_Fecha_String_en_Lista_Number)
(provide encontrar_Fecha_Mas_Reciente_Entre_Dos)
(provide encontrar_Fecha_Mas_Reciente)

(provide escribirOutput)

; Función ctdadRegistros para contar la cantidad de registros especificamente en una de las bases de datos (Descuenta la primera linea de referencia).
(define ctdadRegistros 
  (lambda (BD) 
          (- (length BD) 1)
  )
)

; Funciones para extender las funciones tipo (car ) para obtener el elemento de una lista.
(define (caddddr lista)
  (cadddr(cdr lista))
)

(define (cadddddr lista)
  (cadddr(cddr lista))
)

(define (caddddddr lista)
  (cadddr(cdddr lista))
)

(define (cadddddddr lista)
  (cadddr(cdddr (cdr lista)))
)

(define (caddddddddr lista)
  (cadddr(cdddr (cddr lista)))
)

(define (cadddddddddr lista)
  (cadddr(cdddr (cdddr lista)))
)

#|
  Función verificarElementoEnLista_V1

  Descripción: Función utilizada para verificar la existencia de un elemento en una lista.

  INPUT:
    Elemento cuya existencia en la lista se desea verificar.
    Lista sobre al cual se pretende trabajar.

  OUTPUT:
    #f  Si el elemento no se encuentra en la lista.
    #t  Si el elemento se encuentra en la lista.
|#
(define verificarElementoEnLista_V1
  (lambda (elemento lista)
    
    (if (equal? lista '() ) 
        
        ;En el caso de que "lista" no corresponda a una lista o ésta se encuentra vacía
        #f
        
        ;Para todo lo demas
        (if (= (length lista) 1 )
            
            ;Si se esta trabajando sobre el ultimo elemento de la lista
            (if (equal? elemento (car (car lista)))
                
                ;Si el elemento ingresado coincide con el ultimo elemento de la lista
                #t
                
                ;Si no lo hace
                #f
                )
            
            ;Si se esta trabajando sobre cualquier otro elemento de la lista
            (if (equal? elemento (car (car lista)))
                
                ;Si el elemento ingresado coincide con el elemento de la lista considerado
                #t
                
                ;Si no lo hace
                (verificarElementoEnLista_V1 elemento (cdr lista))
                )
            )
        )
    )
  )

        
#|
  Función contarElementoEnLista
  
  Descripción: Función utilizada para contar la cantidad de veces que se repite un elemento en una lista, utilizando para ello una recursión del tipo
  lineal no por cola.

  INPUT:
     Elemento a contar.
     Lista.

  OUTPUT:
     Cantidad de veces que se repite el elemento en la lsita.
    
|#
(define contarElementoEnLista
  (lambda (elemento lista)
   
    (if (equal? lista null)
        
        ;Si la lista esta vacia
        0
        
        ;Si la lista no se encuentra vacia
        (if (= (length lista) 1)
            
            ;Si se esta leyendo el ultimo elemento de la lista
            (if (equal? elemento (car lista))
                
                ;Si se ha encontrado una ocurrencia
                1
                
                ;Si no se ha encontrado una ocurrencia
                0
                )
      
            ;Si se esta leyendo cualquier otro elemento de la lista
            (if (equal? elemento (car lista))    
                             
                ;Si se ha encontrado una ocurrencia del elemento en la lista
                (+ 1 (contarElementoEnLista elemento (cdr lista)))
                
                ;Si no se ha encontrado una ocurrencia
                (contarElementoEnLista elemento (cdr lista))
                )
            )
        )
    )
  )

#| 
 Función listar_Y_Contar_Elementos_En_Listas_V1

 Descripción: Función utilizada para contar los diferentes elementos de una lista y devolver otra construida de la forma que se indica a continuación:
 
                      '( '(elemento-1 ctdad-1) '(elemento-2 ctdad-2) ...  ... '(elemento-n ctdad-n))

 INPUT
   Lista cuyos elementos se desea contar.
   Lista de retorno (Inicialmente vacia).

 OUTPUT
   Lista de elementos contados.
|#
(define listar_Y_Contar_Elementos_En_Listas_V1
  (lambda (lista listaListaRetorno)
    (if (= (length lista) 1)
        
        ;En el caso de que se este trabajando en el ultimo elemento de la lista
        (if (equal? (verificarElementoEnLista_V1 (car lista) listaListaRetorno) #f)
            
            ;En el caso de que el identificador no se encuentre en la lista de retorno, se agrega:
            (cons (list (car lista) 1) listaListaRetorno)
            
            ;En el caso de que el identificador se encuentra en la lista de retorno, no se hace nada:
            listaListaRetorno
            )
        
        ;En el caso de que se este trabajando en cualquier otro elemento
        (if (equal? (verificarElementoEnLista_V1 (car lista) listaListaRetorno) #f)  
            
            ;En el caso de que el identificador no se encuentre en la lista de retorno, se agrega:
            (listar_Y_Contar_Elementos_En_Listas_V1 (cdr lista) (cons (list (car lista) (contarElementoEnLista (car lista) lista)) listaListaRetorno))
            
            ;En el caso de que el identificador ya se encuentra en la lista de retorno, se pasa al siguiente identificador en la lista
            (listar_Y_Contar_Elementos_En_Listas_V1 (cdr lista) listaListaRetorno)
            )
        )
    )
  )

#|
  Función concatenar_Listas_De_Texto

  Descripción: Función que recurre a una recursión del tipo lineal no por cola para concatenar todos los elementos del tipo string contenidos en una
  lista.

  INPUT:
    Lista cuyos elementos son todos string.

  OUTPUT:
    String con todos los elementos de la lista concatenados.
|#
(define concatenar_Listas_De_Texto
  (lambda (lista)
    (if (not (list? lista))
        
        ;Lista ingresada no es lista
        null
        
        ;Lista ingresada si es lista
        (if (= (length lista) 1)
            
            ;Se esta trabajando sobre el ultimo elemento de la lista
            (car lista)
            
            ;Se esta trabajando sobre cualquier otro elemento de la lista
            (string-append (car lista) (concatenar_Listas_De_Texto (cdr lista)))
            )
        )
    )
  )

#|
  Función obtener_Maximo_Lista

  Descripción: Función que utiliza una recursión del tipo lineal por cola para poder obtener el elemento (numerico) de mayor valor en una lista.

  INPUT:
    Lista de elementos numericos.
    Elemento de mayor valor (inicialmente el primer elemento de la lista).

  OUTPUT:
    Elemento de mayor valor

|#
(define obtener_Maximo_Lista
  (lambda (lista maximo)
    (if (and (not (list? lista)) (< maximo 0))
        
        ;Lista ingresada no es lista o el maximo inicial es menor a cero
        null
        
        ;En el caso de que los parametros de entrada hayan sido ingresados correctamente
        (if (= (length lista) 1)
            
            ;Se esta trabajando sobre el ultimo elemento de la lista
            (if (> (car lista) maximo)
                
                ;El elemento de la lista resulta ser mas grande que el "maximo" actual, por lo tanto, ese elemento pasa a ser el nuevo "maximo"
                (car lista)
                
                ;El elemento de la lista es menor que el "maximo" actual
                maximo
                )
            
            ;Se esta trabajando sobre cualquier otro elemento de la lista
            (if (> (car lista) maximo)
                
                ;El elemento de la lista resulta ser mas grande que el "maximo" actual, por lo tanto, ese elemento pasa a ser el nuevo "maximo"
                (obtener_Maximo_Lista (cdr lista) (car lista))
                
                ;El elemento de la lista es menor que el "maximo" actual
                (obtener_Maximo_Lista (cdr lista) maximo)
                ) 
            )
        )
    )
  )

#|
  Función obtener_Maximos_Elementos_Lista_Contada

  Descripción: Función que trabaja sobre una lista, la cual debe haber pasado antes por listar_Y_Contar_Elementos_En_Listas_V1, para poder 
  obtener los elementos que se repiten una mayor cantidad de veces. Para ello se reurre a una recursión del tipo lineal.

  INPUT:
    lista de elementos contados
    Maximo de la lista (Inicialmente el primer elemento).

  OUTPUT:
    Lista de elementos que se repiten una mayor cantdad de veces.
|#
(define obtener_Maximos_Elementos_Lista_Contada
  (lambda (lista maximo)
    (if (and (not (list? lista)) (< maximo 0))
        
        ;Lista ingresada no es lista o el maximo ingresado es menor a cero
        null
        
        ;En el caso de que los parametros de entrada hayan sido ingresados correctamente
        (if (= (length lista) 1)
            
            ;Se esta trabajando sobre el ultimo elemento de la lista
            (if (= (cadr(car lista)) maximo)
                
                ;El contador del elemento en la lista resulta ser igual al maximo
                (list (car lista))
                
                ;El contador del elemento en la lista resulta ser diferente al maximo
                null
                )
            
            ;Se esta trabajando sobre cualquier otro elemento de la lista
            (if (= (cadr(car lista)) maximo)
                
                ;El contador del elemento en la lista resulta ser igual al maximo
                (cons (car lista) (obtener_Maximos_Elementos_Lista_Contada (cdr lista) maximo))
                
                ;El contador del elemento en la lista resulta ser diferente al maximo
                (obtener_Maximos_Elementos_Lista_Contada (cdr lista) maximo)
                )        
            )
        )
    )
  )
;

#|
   Función convertir_Fecha_String_en_Lista_Number
   
   Descripción: Función utilizada para convertir una fecha escrita como string (en formato dd/mm/aa) en una lista de number. Recurre a una recursión de tipo lineal no por la cola
   para enlistar los numeros que representan dias, meses y años. 

   INPUT:
   Fecha (string)

   OUTPUT:
   Fecha (list)
|#
(define convertir_Fecha_String_en_Lista_Number
  (lambda (fechaString)
      (if (not(string? fechaString))
        ;Si la fecha entregada no es un string
        null
        
        ;Si la fecha entregada es un string
        (map (lambda x (string->number (car x))) (string-split fechaString "/"))
        )
    )
  )

#|
   Función encontrar_Fecha_Mas_Reciente_Entre_Dos
   
   Descripción: Función utilizada para comparar dos fechas escritas como string (en formato dd/mm/aa) y devolver aquella que es más
   reciente. No aplica ningun tipo de recursión.

   INPUT:
   Fecha1 (string)
   Fecha2 (string)

   OUTPUT:
   Fecha mas reciente (string)
|#
(define encontrar_Fecha_Mas_Reciente_Entre_Dos
  (lambda (Fecha1 Fecha2)
    (let
        (
         #|Se declaran las constantes Fecha1List y Fecha2List, los cuales son una conversión de las 
           fechas entregadas como entrada en una lista de numeros|#
         (Fecha1List (convertir_Fecha_String_en_Lista_Number Fecha1))
         (Fecha2List (convertir_Fecha_String_en_Lista_Number Fecha2))
         )
      
      (cond
        
        #|Se realiza una comparación de los años de ambas fechas|#
        ;1) Si Fecha1List es más actual, con respecto a los años, que Fecha2List. Se retorna la más actual
        [(> (caddr Fecha1List) (caddr Fecha2List))    Fecha1]
        
        ;2) Si Fecha2List es más actual, con respecto a los años, que Fecha1List. Se retorna la más actual
        [(< (caddr Fecha1List) (caddr Fecha2List))    Fecha2]
        
        ;3) Cualquier otro caso, es decir, si ambas fechas tienen el mismo año, se procede a comparar los meses
        [(= (caddr Fecha1List) (caddr Fecha2List))
         
         (cond
           
           #|Se realiza una comparación de los meses de ambas fechas |#
           ;3.1) Si Fecha1List es más actual, con respecto a los meses, que Fecha2List. Se retorna la más actual
           [(> (cadr Fecha1List) (cadr Fecha2List))    Fecha1]
           
           ;3.2) Si Fecha2List es más actual, con respecto a los meses, que Fecha1List. Se retorna la más actual
           [(< (cadr Fecha1List) (cadr Fecha2List))    Fecha2]
           
           ;3.3) Cualquier otro caso, es decir, si ambas fechas tienen el mismo mes, se procede a comparar los días
           [(= (cadr Fecha1List) (cadr Fecha2List))
            
            (cond 
              
              #|Se realiza una comparación de los días de ambas fechas |#
              ;3.3.1) Si Fecha1List es más actual, con respecto a los días, que Fecha2List. Se retorna la más actual
              [(> (car Fecha1List) (car Fecha2List))    Fecha1]
              
              ;3.3.2) Si Fecha2List es más actual, con respecto a los días, que Fecha1List. Se retorna la más actual
              [(< (car Fecha1List) (car Fecha2List))    Fecha2]
              
              ;3.3.3) Cualquier otro caso, es decir, si ambas fechas tienen el mismo día. Se retorna cualquiera de las dos fechas
              [(= (car Fecha1List) (car Fecha2List))    Fecha1]
              )
            ]
           )
         ]
        )
      )
    )
  )
       
#|
   Función encontrar_Fecha_Mas_Reciente
   
   Descripción: Función utilizada para comparar una lista de fechas escritas como string (en formato dd/mm/aa) y devolver aquella que es más
   reciente. Para funcionar, recurre a una recursión del tipo lineal sin cola. Comparando una de las fechas de la lista con aquella que
   en el momento es la más reciente, para devolver la fecha que realmente es más reciente y compararla con el siguiente elemento de la lista 

   INPUT:
   Lista de fechas escritas como string

   OUTPUT:
   Fecha mas reciente (string)
|#
(define encontrar_Fecha_Mas_Reciente
  (lambda (fechas fechaReciente)
    
    [if (not(and (list? fechas) (> (length fechas) 0)))
        
        ;Si "fechas" no corresponde realmente a una lista o Si su dimensión es cero (Lista vacia)
        null
        
        ;Cualquier otro caso:
        [if (= (length fechas) 1)
            
            ;Si se esta trabajando con el ultimo elemento de la lista
            (encontrar_Fecha_Mas_Reciente_Entre_Dos fechaReciente (car fechas))
            
            ;Si no se esta trabajando con el ultimo elemento
            (encontrar_Fecha_Mas_Reciente (cdr fechas) (encontrar_Fecha_Mas_Reciente_Entre_Dos fechaReciente (car fechas)))
            ]
        ]
    )
  )

#|
  Función escribirOutput

  Descripción:  Función utilizada para escribir en un archivo cuyo directorio es entregado como parametro un texto que tambien
  es ingresado como parametro.

  INPUT:
    texto a escribir.
    directorio del archivo sobre el cual se desea escribir.

  OUTPUT:
    -1 Solamente si ocurre un error durante el proceso.
|#
(define escribirOutput
  (lambda (texto directorio)
    
    [if (and (string? texto) (not (= (string-length texto) 0)) (string? directorio))
        
        ;Si texto y directorio son (open-output-file directorio)
        (let
            (
             (archivo (open-output-file directorio #:mode 'binary #:exists 'replace))
             )
     
          [if (not (= (write-string texto archivo) 0))
              
              ;Se cierra el archivo
              (close-output-port archivo)
              
              ;Error
              -1
              ]
          )
        
        ;Error
        -1
        ]
    )
  )





