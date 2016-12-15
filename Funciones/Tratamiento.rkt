#lang racket

(require "FuncionesGenerales.rkt")
(require (planet neil/csv:2:0))

(provide BDTratamientos)
(provide createTratamiento)
(provide isTratamiento?)
(provide getIDTratamiento)
(provide getNombreTratamiento)
(provide getDescripcionTratamiento)
(provide getNivelDeRiesgo)
(provide extraer_Registros_Tratamientos)
(provide obtener_Registro_Tratamiento_Segun_Identificador)
(provide obtener_Registros_Tratamientos_Segun_Nivel)
(provide obtener_Nombres_Tratamientos_Segun_Lista)
(provide obtener_Nombres_Tratamientos_Segun_Lista_2EL)
(provide obtener_Nombre_Tratamiento_Segun_Identificador)
(provide obtener_Nombres_Tratamientos_Segun_Identificadores)


(define BDTratamientos (csv->list (open-input-file "Base_de_Datos/Tratamiento.txt")))

;CONSTRUCTOR:
(define (createTratamiento IDTratamiento nombreTratamiento descripcionTratamiento nivelDeRiesgo)
        (if (and 
            (number? IDTratamiento) 
            (> IDTratamiento -1)       
            (string? nombreTratamiento)
            (string? descripcionTratamiento)
            (string? nivelDeRiesgo)
            )
            
            (list IDTratamiento nombreTratamiento descripcionTratamiento nivelDeRiesgo)
            
            null
            
        )
     
  );FIN CONSTRUCTOR.

;FUNCIÓN DE PERTENENCIA:
(define (isTratamiento? tratamiento)
  (if (list? tratamiento)
      (if (= (length tratamiento) 4)
          ;Si cumple
          (if (and
               (number? (car tratamiento))
	       (> (car tratamiento) -1)
               (string? (cadr tratamiento))
               (string? (caddr tratamiento))
               (string? (cadddr tratamiento))
              )
              
              #t
              
              #f
              
              ); fin de la sentecia if n°3.
          
          #f
          
          ); fin de la sentencia if n°2.
      
      #f
      
      ); fin de la sentencia if n°1.
 )

;SELECTORES:
(define (getIDTratamiento tratamiento)
  (if (isTratamiento? tratamiento)
      (car tratamiento)
      -1
      )
  )

(define (getNombreTratamiento tratamiento)
  (if (isTratamiento? tratamiento)
      (cadr tratamiento)
      -1
      )
  )

(define (getDescripcionTratamiento tratamiento)
  (if (isTratamiento? tratamiento)
      (caddr tratamiento)
      -1
      )
  )

(define (getNivelDeRiesgo tratamiento)
  (if (isTratamiento? tratamiento)
      (cadddr tratamiento)
      -1
      )
  )


;FUNCIONES:

#|
    Función extraer_Registros_Tratamientos

    Descripción: 
      Función utilizada para retornar una lista de tratamientos construidos a partir de los registros contenidos en la base de datos Tratamiento.txt. Se basa en el uso de una
      aplicación de recursión lineal, no por cola, para poder extraer cada uno de los registros y construir con ellos los diferentes tratamientos contenidos en la base de datos.
      Estos son agregados a una lista, a la cual se retorna tras finalizar la ejecución de la función en el caso de que todo salga bien. Por otro lado, si algún error ocurre
      durante el proceso, se devuelve una lista vacia (null). 

    INPUT: 
      Lista de registros contenidos en la base de datos Tratamiento.txt

    OUTPUT:
      Lista de tratamientos
|#
(define extraer_Registros_Tratamientos
  (lambda (registros)
    (if (= (length registros) (+ (ctdadRegistros BDTratamientos) 1))
        
        #|
        Sentencia IF N°1:
        En el caso de que se este leyendo la primera linea de los registros obtenidos a partir de la base de datos
        -> Si cumple: Se ignora el primer registro, el cual solamente contiene una referencia
        -> Si no cumple: Se construye un tratamiento a partir del registro considerado, revisando primero si el registro corresponde a un tratamiento
        |#
       
        (extraer_Registros_Tratamientos (cdr registros))
        
        (if (= (length registros) 1)
            
            #|
                Sentencia IF N°2:
                Si se esta considerando el ultimo registro de la base de datos
                -> Si cumple: Se construye el tratamiento del ultimo registro
                -> Si no cumple: Se construye el tratamiento del registro correspondiente y se intenta pasar al siguiente
                |#
            
            (list (createTratamiento    
             (string->number (car (car registros)))
             (cadr (car registros))
             (caddr (car registros))
             (cadddr (car registros))
             ))
            
            (cons (createTratamiento    
                   (string->number (car (car registros)))
                   (cadr (car registros))
                   (caddr (car registros))
                   (cadddr (car registros))
                   )
                  
                  (extraer_Registros_Tratamientos (cdr registros))
                  )
            
            );Fin de la sentencia condicional IF N°2
        );Fin de la sentencia condificonal IF N°1
    );Fin de lambda
  );Fin de define


#|
    Función obtener_Registro_Tratamiento_Segun_Identificador

    Descripción:
      Función que utiliza una recursión por cola para encontrar y retornar un registro de tipo tratamiento que cumpla con la siguiente condición: 
      El registro debe contener el mismo identificador que el que es ingresado como argumento de la función. En el caso de que no exista registro que cumpla con
      ello, se retorna null

    INPUT: 
      Identificador del tratamiento
      Lista de registros de tratamientos
      

    OUTPUT:
      Registro Tratamiento relacionado con el Identificador de entrada
|#
(define obtener_Registro_Tratamiento_Segun_Identificador
  (lambda  (identificador registros)
    
    (if (= (length registros) 1)
        
        ;En el caso de que se este considerando el ultimo registro de la lista
        (if (= (getIDTratamiento (car registros)) identificador)
                                            
            ;En el caso de que el ultimo registro contenga el identificador ingresado
            (car registros)
            
            ;En el caso de que el ultimo registro NO contenga el identificador ingresado
            null
            )
        
        ;En el caso de que se este considerando cualquier otro registro
        (if (= (getIDTratamiento (car registros)) identificador)
            
            ;En el caso de que el registro contenga el identificador ingresado
            (car registros)
                  
            ;En el caso de que no lo tenga, se continua con el siguiente registro
            (obtener_Registro_Tratamiento_Segun_Identificador identificador (cdr registros))
            
            )
        )
    )
  )

#|
    Función obtener_Nombre_Tratamiento_Segun_Identificador

    Descripción: 
      Función que utiliza una recursión por cola para encontrar y retornar el dato "nombre" de un registro de tipo tratamiento que cumpla con la siguiente condición: 
      El registro debe contener el mismo identificador que el que es ingresado como argumento de la función. En el caso de que no cumpla con ello, se retorna null.
    
    INPUT: 
      Identificador del tratamiento
      Lista de registros tratamiento

    OUTPUT:
      Nombre del tratamiento
|#
(define obtener_Nombre_Tratamiento_Segun_Identificador
  (lambda (identificador registros)
    
    (if (= (length registros) 1)
        
        ;En el caso de que se trabaje con el ultimo registro
        (if (= (getIDTratamiento (car registros)) identificador)
            
            ;Si hay una coincidencia
            (getNombreTratamiento (car registros))
            
            ;Si no la hay
            null
            )
            
        ;En el caso de que se trabaje con cualquier otro registro
        (if (= (getIDTratamiento (car registros)) identificador)
            
            ;Si hay una coincidencia
            (getNombreTratamiento (car registros))
            
            ;Si no la hay
            (obtener_Nombre_Tratamiento_Segun_Identificador identificador (cdr registros))
            )
        )
    )
  )

#|
    Función obtener_Nombres_Tratamientos_Segun_Identificadores

    Descripción: 
      Función utilizada para retornar una lista de nombres de los registros de tipo tratamiento que estan relacionados con una lista de identificadores
      entregados como parametro de la función. Para cumplir con dicho proposito, la función requiere hacer uso de una recursión del tipo lineal, no por 
      cola, para construir la lista requerida.
 
    INPUT: 
      Lista de identificadores
      Lista de registros tratamiento

    OUTPUT:
      Lista de nombres de tratamiento
|#
(define obtener_Nombres_Tratamientos_Segun_Identificadores
  (lambda (identificadores registros)
    
    (if (= (length identificadores) 1)
        
        ;En el caso de que se este considerando el ultimo de los identificadores
        (list (obtener_Nombre_Tratamiento_Segun_Identificador (car identificadores) registros))
        
        ;En el caso de que se este considerando cualquier otro identificador
        (cons (obtener_Nombre_Tratamiento_Segun_Identificador (car identificadores) registros) (obtener_Nombres_Tratamientos_Segun_Identificadores (cdr identificadores) registros))
        )
    )
  )
        
        
        
        

#|
    Función obtener_Registros_Tratamientos_Segun_Identificadores

    Descripción: 
      Función utilizada para retornar una lista de tratamientos relacionados con una lista de identificadores de tratamiento. Se recurre a una recursión del tipo lineal por cola 
      para unir cada uno de los registros que cumplen con la condición señalada en una lista.
    
    INPUT: 
      Lista de identificadores
      Lista de registros de tratamiento

    OUTPUT:
      Lista de tratamientos que contienen un nivel de riesgo en especifico
|#
(define obtener_Registros_Tratamientos_Segun_Identificadores
  (lambda (identificadores tratamientos)
    (if (= (length identificadores) 1)
        
        ;En el caso de que se considere el ultimo identificador en la lista
        (list (obtener_Registro_Tratamiento_Segun_Identificador (car identificadores) tratamientos))
        
        ;En el caso de que se considere cualquier otro identificador en la lista
        (cons (obtener_Registro_Tratamiento_Segun_Identificador (car identificadores) tratamientos) (obtener_Registros_Tratamientos_Segun_Identificadores (cdr identificadores) tratamientos))
        
        )
    )
  )
         
#|
    Función obtener_Registros_Tratamientos_Segun_Nivel

    Descripción: 
      Función utilizada para retornar una lista de tratamientos que contienen un nivel de riesgo especifico. Se utiliza una recursión del tipo lineal por cola 
      para unir cada uno de los registros que cumplen con la condición señalada en una sola lista.
    
    INPUT: 
      Lista de registros de tratamientos
      Nivel de riesgo

    OUTPUT:
      Lista de tratamientos que contienen un nivel de riesgo en especifico
|#
(define obtener_Registros_Tratamientos_Segun_Nivel
  (lambda  (nivel registros)
    
    (if (= (length registros) 1)
        
        ;En el caso de que se este considerando el ultimo registro de la lista
        (if (equal? nivel (getNivelDeRiesgo (car registros)))
            ;En el caso de que el ultimo registro contenga el nivel ingresado
            (list (car registros))
            ;En el caso de que el ultimo registro NO contenga el nivel ingresado
            null
            )
        
        ;En el caso de que se este considerando cualquier otro registro
        (if (equal? nivel (getNivelDeRiesgo (car registros)))
            
            ;En el caso de que el registro contenga el nivel ingresado
            (cons (car registros) (obtener_Registros_Tratamientos_Segun_Nivel nivel (cdr registros)))
                  
            ;En el caso de que no lo tenga, se continua con el siguiente registro
            (obtener_Registros_Tratamientos_Segun_Nivel nivel (cdr registros))
            
            )
        )
    )
  )




#|
    Función obtener_Nombres_Tratamientos_Segun_Lista

    Descripción: 
      Función utilizada para retornar una cadena de texto de nombres de tratamientos contenidos en una lista de tratamientos. Se recurre a una recursión del tipo lineal por cola
      para poder unir en un string los nombres contenidos en la lista ingresada.
    
    INPUT: 
      Lista de tratamientos

    OUTPUT:
      Lista de nombres de tratamientos
|#
(define obtener_Nombres_Tratamientos_Segun_Lista
  (lambda (lista)
    
    (if (= (length lista) 1)
        
        ;En el caso de que se considere el ultimo elemento de la lista
        (getNombreTratamiento (car lista))
        
        ;En cualquier otro caso
        (string-append (string-append (getNombreTratamiento (car lista)) "\n") (obtener_Nombres_Tratamientos_Segun_Lista (cdr lista) ) )
        
        )
    )
  )

;Variante
(define obtener_Nombres_Tratamientos_Segun_Lista_2EL
  (lambda (lista)
    (if (= (length lista) 1)
        
        ;En el caso de que se considere el ultimo elemento de la lista
        (getNombreTratamiento (car lista))
        
        ;En cualquier otro caso
        (string-append (string-append (getNombreTratamiento (car lista)) "\n") (obtener_Nombres_Tratamientos_Segun_Lista (cdr lista) ) )
        
        )
    )
  )
