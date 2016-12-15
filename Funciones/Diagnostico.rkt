#lang racket

(require "FuncionesGenerales.rkt")
(require (planet neil/csv:2:0))


(provide BDDiagnosticos)
(provide isDiagnostico?)
(provide getIDDiagnostico)
(provide getDescripcionDiagnostico)
(provide getNivelGravedad)
(provide extraer_Registros_Diagnosticos)
(provide obtener_Registros_Diagnosticos_Segun_Nivel)
(provide obtener_Registros_Diagnosticos_Segun_Nombre)


(define BDDiagnosticos (csv->list (open-input-file "Base_de_Datos/Diagnostico.txt")))

;CONSTRUCTOR: Función para construir un registro Diagnostico, verificando si los datos con los que se pretende construir el registro son correctos
(define (createDiagnostico IDDiagnostico descripcionDiagnostico nivelGravedad)
        (if (and 
            (number? IDDiagnostico) 
            (> IDDiagnostico -1)       
            (string? descripcionDiagnostico)
            (string? nivelGravedad)
            )
            
            (list IDDiagnostico descripcionDiagnostico nivelGravedad)
            
            null
            
        )
     
  );FIN CONSTRUCTOR.

;FUNCIÓN DE PERTENENCIA: Función para verificar si un registro corresponde efectivamente a un Diagnostico.
(define (isDiagnostico? diagnostico)
  (if (list? diagnostico)
      (if (= (length diagnostico) 3)
          ;Si cumple
          (if (and
               (number? (car diagnostico))
	       (> (car diagnostico) -1)
               (string? (cadr diagnostico))
               (string? (caddr diagnostico))
              )
              
              #t
              
              #f
              
              ); fin de la sentecia if n°3.
          
          #f
          
          ); fin de la sentencia if n°2.
      
      #f
      
      ); fin de la sentencia if n°1.
 )

;SELECTORES: Funciones para obtener cierto dato de un registro, preguntando primero si el registro corresponde a un Diagnostico.
(define (getIDDiagnostico diagnostico)
  (if (isDiagnostico? diagnostico)
      (car diagnostico)
      -1
      )
  )

(define (getDescripcionDiagnostico diagnostico)
  (if (isDiagnostico? diagnostico)
      (cadr diagnostico)
      -1
      )
  )

(define (getNivelGravedad diagnostico)
  (if (isDiagnostico? diagnostico)
      (caddr diagnostico)
      -1
      )
  )

;FUNCIONES:

#|
    Función extraer_Registros_Diagnosticos

    Descripción: 
      Función utilizada para retornar una lista de diagnosticos construidos a partir de los registros contenidos en la base de datos Diagnostico.txt. Se basa en el uso de una
      aplicación de recursión lineal, no por cola, para poder extraer cada uno de los registros y construir con ellos los diferentes diagnosticos contenidos en la base de datos.
      Estos son agregados a una lista, a la cual se retorna tras finalizar la ejecución de la función en el caso de que todo salga bien. Por otro lado, si algún error ocurre
      durante el proceso, se devuelve una lista vacia (null). 
    
    INPUT: 
      Lista de registros contenidos en la base de datos Diagnostico.txt

    OUTPUT:
      Lista de diagnosticos
|#
(define extraer_Registros_Diagnosticos
  (lambda (registros)
    (if (= (length registros) (+ (ctdadRegistros BDDiagnosticos) 1))
        
        #|
        Sentencia IF N°1:
        En el caso de que se este leyendo la primera linea de los registros obtenidos a partir de la base de datos
        -> Si cumple: Se ignora el primer registro, el cual solamente contiene una referencia
        -> Si no cumple: Se construye un diagnostico a partir del registro considerado, revisando primero si el registro corresponde a un diagnostico
        |#
       
        (extraer_Registros_Diagnosticos (cdr registros))
        
        (if (= (length registros) 1)
            
            #|
                Sentencia IF N°2:
                Si se esta considerando el ultimo registro de la base de datos
                -> Si cumple: Se construye el diagnostico del ultimo registro
                -> Si no cumple: Se construye el diagnostico del registro correspondiente y se intenta pasar al siguiente
                |#
            
            (list (createDiagnostico    
             (string->number (car (car registros)))
             (cadr (car registros))
             (caddr (car registros))
             ))
            
            (cons (createDiagnostico    
                   (string->number (car (car registros)))
                   (cadr (car registros))
                   (caddr (car registros))
                   )
                  
                  (extraer_Registros_Diagnosticos (cdr registros))
                  )
            
            );Fin de la sentencia condicional IF N°2
        );Fin de la sentencia condificonal IF N°1
    );Fin de lambda
  );Fin de define

#|
    Función obtener_Registros_Diagnosticos_Segun_Nivel

    Descripción: 
      Función utilizada para retornar una lista de diagnosticos que contiene un nivel de gravedad especifico. Recurre a una recursión del tipo lineal no por cola para poder unir cada 
      uno de los registros que cumplen con las condiciones mencionadas.
    
    INPUT: 
      Lista de registros de diagnosticos
      Nivel de riesgo

    OUTPUT:
      Lista de diagnosticos que contienen un nivel de gravedad en especifico
|#
(define obtener_Registros_Diagnosticos_Segun_Nivel
  (lambda  (nivel registros)
    
    (if (= (length registros) 1)
        
        ;En el caso de que se este considerando el ultimo registro de la lista
        (if (equal? nivel (getNivelGravedad (car registros)))
            ;En el caso de que el ultimo registro contenga el nivel ingresado
            (list (car registros))
            ;En el caso de que el ultimo registro NO contenga el nivel ingresado
            null
            )
        
        ;En el caso de que se este considerando cualquier otro registro
        (if (equal? nivel (getNivelGravedad (car registros)))
            
            ;En el caso de que el registro contenga el nivel ingresado
            (cons (car registros) (obtener_Registros_Diagnosticos_Segun_Nivel nivel (cdr registros)))
                  
            ;En el caso de que no lo tenga, se continua con el siguiente registro
            (obtener_Registros_Diagnosticos_Segun_Nivel nivel (cdr registros))
            
            )
        )
    )
  )

#|
    Función obtener_Registros_Diagnosticos_Segun_Nombre

    Descripción: 
      Función utilizada para retornar el registro de un Diagnostico cuyo nombre se conoce, asumiendo que el nombre de un diagnostico es unico, por lo que se 
      recurre a una recursión del tipo lineal no por cola para analizar registro por registro hasta encontrar el que corresponde.
  
    INPUT: 
      Nombre del diagnostico
      Lista de registros de diagnosticos
      

    OUTPUT:
      Registro del diagnostico que contiene el nombre ingresado
|#
(define obtener_Registros_Diagnosticos_Segun_Nombre
  (lambda  (nombre registros)
    
    (if (= (length registros) 1)
        
        ;En el caso de que se este considerando el ultimo registro de la lista
        (if (equal? nombre (getDescripcionDiagnostico (car registros)))
            ;En el caso de que el ultimo registro contenga el nombre ingresado
            (car registros)
            ;En el caso de que el ultimo registro NO contenga el nombre ingresado
            null
            )
        
        ;En el caso de que se este considerando cualquier otro registro
        (if (equal? nombre (getDescripcionDiagnostico (car registros)))
            
            ;En el caso de que el registro contenga el nombre ingresado
            (car registros)
                  
            ;En el caso de que no lo tenga, se continua con el siguiente registro
            (obtener_Registros_Diagnosticos_Segun_Nombre nombre (cdr registros))
            
            )
        )
    )
  )