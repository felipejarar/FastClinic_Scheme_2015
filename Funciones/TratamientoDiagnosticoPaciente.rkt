#lang racket

(require "FuncionesGenerales.rkt")
(require (planet neil/csv:2:0))

(provide BDTratamientoDiagnosticoPacientes)
(provide createTratamientoDiagnosticoPaciente)
(provide isTratamientoDiagnosticoPaciente?)
(provide getIDDiagnosticoPaciente_vv)
(provide getIDTratamiento_vv)
(provide getIDMedico_vv)
(provide getFechaInicio)
(provide getDuracionDias)
(provide getResultado)
(provide setResultado)
(provide extraer_Registros_TratamientoDiagnosticoPacientes)
(provide obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente)
(provide obtener_Registros_TratamientoDiagnosticoPaciente_segun_IDTratamiento)
(provide obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha)
(provide modificar_Resultado_Registro_TratamientoDiagnosticoPaciente)

(define BDTratamientoDiagnosticoPacientes (csv->list (open-input-file "Base_de_Datos/TratamientoDiagnosticoPaciente.txt")))

;CONSTRUCTOR:
(define (createTratamientoDiagnosticoPaciente IDDiagnosticoPaciente IDTratamiento IDMedico fechaInicio duracionDias resultado)
        (if (and 
            (number? IDDiagnosticoPaciente) 
            (> IDDiagnosticoPaciente -1)
            (number? IDTratamiento) 
            (> IDTratamiento -1)       
            (number? IDMedico) 
            (> IDMedico -1)       
            (string? fechaInicio)
            (number? duracionDias) 
            (> duracionDias -1)   
            (string? resultado)
            )
            
            (list IDDiagnosticoPaciente IDTratamiento IDMedico fechaInicio duracionDias resultado)
            
            null
            
        )
     
  );FIN CONSTRUCTOR.

;FUNCIÓN DE PERTENENCIA:
(define (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
  (if (list? tratamientoDiagnosticoPaciente)
      (if (= (length tratamientoDiagnosticoPaciente) 6)
          ;Si cumple
          (if (and
               ;IDDiagnosticoPaciente
               (number? (car tratamientoDiagnosticoPaciente))
	       (> (car tratamientoDiagnosticoPaciente) -1)
               ;IDTratamiento
               (number? (cadr tratamientoDiagnosticoPaciente))
	       (> (cadr tratamientoDiagnosticoPaciente) -1)
               ;IDMedico
               (number? (caddr tratamientoDiagnosticoPaciente))
	       (> (caddr tratamientoDiagnosticoPaciente) -1)
               ;fechaInicio
               (string? (cadddr tratamientoDiagnosticoPaciente))
               ;duracionDias
               (number? (caddddr tratamientoDiagnosticoPaciente))
	       (> (caddddr tratamientoDiagnosticoPaciente) -1)
               ;resultado
               (string? (cadddddr tratamientoDiagnosticoPaciente))
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
(define (getIDDiagnosticoPaciente_vv tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (car tratamientoDiagnosticoPaciente)
      -1
      )
  )

(define (getIDTratamiento_vv tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (cadr tratamientoDiagnosticoPaciente)
      -1
      )
  )

(define (getIDMedico_vv tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (caddr tratamientoDiagnosticoPaciente)
      -1
      )
  )

(define (getFechaInicio tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (cadddr tratamientoDiagnosticoPaciente)
      -1
      )
  )

(define (getDuracionDias tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (caddddr tratamientoDiagnosticoPaciente)
      -1
      )
  )


(define (getResultado tratamientoDiagnosticoPaciente)
  (if (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente)
      (cadddddr tratamientoDiagnosticoPaciente)
      -1
      )
  )

;MODIFICADORES
(define (setResultado tratamientoDiagnosticoPaciente nuevoResultado)
    (if (and (isTratamientoDiagnosticoPaciente? tratamientoDiagnosticoPaciente) (string? nuevoResultado))
      
      (createTratamientoDiagnosticoPaciente
                            (getIDDiagnosticoPaciente_vv tratamientoDiagnosticoPaciente) 
                            (getIDTratamiento_vv tratamientoDiagnosticoPaciente) 
                            (getIDMedico_vv  tratamientoDiagnosticoPaciente)
                            (getFechaInicio tratamientoDiagnosticoPaciente) 
                            (getDuracionDias tratamientoDiagnosticoPaciente) 
                            nuevoResultado
                            )
      
      null
     
     )
  )

;FUNCIONES:

#|
    Función extraer_Registros_TratamientoDiagnosticoPacientes

    Descripción:
      Función utilizada para retornar una lista de tratamientoDiagnosticoPacientes construidos a partir de los registros contenidos en la base de datos TratamientoDiagnosticoPaciente.txt. Se basa en el uso de una
      aplicación de recursión lineal, no por cola, para poder extraer cada uno de los registros y construir con ellos los diferentes tratamientoDiagnosticoPacientes contenidos en la base de datos.
      Estos son agregados a una lista, a la cual se retorna tras finalizar la ejecución de la función en el caso de que todo salga bien. Por otro lado, si algún error ocurre
      durante el proceso, se devuelve una lista vacia (null). 

    INPUT: 
      Lista de registros contenidos en la base de datos TratamientoDiagnosticoPaciente.txt

    OUTPUT:
      Lista de tratamientoDiagnosticoPacientes
|#
(define extraer_Registros_TratamientoDiagnosticoPacientes
  (lambda (registros)
    (if (= (length registros) (+ (ctdadRegistros BDTratamientoDiagnosticoPacientes) 1))
        
        #|
        Sentencia IF N°1:
        En el caso de que se este leyendo la primera linea de los registros obtenidos a partir de la base de datos
        -> Si cumple: Se ignora el primer registro, el cual solamente contiene una referencia
        -> Si no cumple: Se construye un tratamientoDiagnosticoPaciente a partir del registro considerado, revisando primero si el registro corresponde a un tratamientoDiagnosticoPaciente
        |#
       
        (extraer_Registros_TratamientoDiagnosticoPacientes (cdr registros))
        
        (if (= (length registros) 1)
            
            #|
                Sentencia IF N°2:
                Si se esta considerando el ultimo registro de la base de datos
                -> Si cumple: Se construye el tratamientoDiagnosticoPaciente del ultimo registro
                -> Si no cumple: Se construye el tratamientoDiagnosticoPaciente del registro correspondiente y se intenta pasar al siguiente
                |#
            
            (list (createTratamientoDiagnosticoPaciente    
                   (string->number (car (car registros)))
                   (string->number(cadr (car registros)))
                   (string->number(caddr (car registros)))
                   (cadddr (car registros))
                   (string->number(caddddr (car registros)))
                   (cadddddr (car registros))
                   )
                  )
            
            (cons (createTratamientoDiagnosticoPaciente    
                   (string->number (car (car registros)))
                   (string->number(cadr (car registros)))
                   (string->number(caddr (car registros)))
                   (cadddr (car registros))
                   (string->number(caddddr (car registros)))
                   (cadddddr (car registros))
                   )
                  
                  (extraer_Registros_TratamientoDiagnosticoPacientes (cdr registros))
                  )
            
            );Fin de la sentencia condicional IF N°2
        );Fin de la sentencia condificonal IF N°1
    );Fin de lambda
  );Fin de define

#|
   Función obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente

   Descripción: Función utilizada para obtener el registro TratamientoDiagnosticoPaciente relacionado
   con el identificador DiagnosticoPaciente entregado como parametro, recurriendo a una recursión para
   comparar registro por registro hasta encontrar una similitud

   INPUT:
    IDDiagnosticoPaciente
    Registros TratamientoDiagnosticoPaciente

   OUTPUT:
    Registro TratamientoDiagnosticoPaciente
|#
(define obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente
  (lambda (IDDiagnosticoPaciente registros)
    
    [if (= (length registros) 1) 
        
        ;Si se esta trabajando sobre el ultimo de los registros
        [if (= IDDiagnosticoPaciente (getIDDiagnosticoPaciente_vv (car registros)))
            
            ;Si se ha encontrado una similitud
            (car registros)
            
            ;Si no se ha encontrado similitud
            null
            ]
        
        ;Si se esta trabajando sobre cualquier otro registro
        [if (= IDDiagnosticoPaciente (getIDDiagnosticoPaciente_vv (car registros)))
            
            ;Si se ha encontrado una similitud
            (car registros)
            
            ;Si no se ha encontrado una similitud
            (obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente IDDiagnosticoPaciente (cdr registros))
            ]
        ]
    )
  )


(define obtener_Registros_TratamientoDiagnosticoPaciente_segun_IDTratamiento
  (lambda (IDTratamiento registros)
    
    [if (= (length registros) 1) 
        
        ;Si se esta trabajando sobre el ultimo de los registros
        [if (= IDTratamiento (getIDTratamiento_vv (car registros)))
            
            ;Si se ha encontrado una similitud
            (list (car registros))
            
            ;Si no se ha encontrado similitud
            null
            ]
        
        ;Si se esta trabajando sobre cualquier otro registro
        [if (= IDTratamiento (getIDTratamiento_vv (car registros)))
            
            ;Si se ha encontrado una similitud
            (cons (car registros) (obtener_Registros_TratamientoDiagnosticoPaciente_segun_IDTratamiento IDTratamiento (cdr registros)))
            
            ;Si no se ha encontrado una similitud
            (obtener_Registros_TratamientoDiagnosticoPaciente_segun_IDTratamiento IDTratamiento (cdr registros))
            ]
        ]
    )
  )


#|
   Función obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha

   Descripción: Función utilizada para obtener el registro TratamientoDiagnosticoPaciente relacionado
   con la fecha de inicio entregado como parametro, recurriendo a una recursión para
   comparar registro por registro hasta encontrar una similitud

   INPUT:
    fecha
    Registros TratamientoDiagnosticoPaciente

   OUTPUT:
    Registro TratamientoDiagnosticoPaciente
|#
(define obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha
  (lambda (fecha registros)
    
    [if (= (length registros) 1) 
        
        ;Si se esta trabajando sobre el ultimo de los registros
        [if (string=? fecha (getFechaInicio (car registros)))
            
            ;Si se ha encontrado una similitud
            (car registros)
            
            ;Si no se ha encontrado similitud
            null
            ]
        
        ;Si se esta trabajando sobre cualquier otro registro
        [if (string=? fecha (getFechaInicio (car registros)))
            
            ;Si se ha encontrado una similitud
            (car registros)
            
            ;Si no se ha encontrado una similitud
            (obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha fecha (cdr registros))
            ]
        ]
    )
  )

#|
  Función modificar_Resultado_Registro_TratamientoDiagnosticoPaciente
  
  Descripción: Función utilizada para devolver los registros ingresados inicialmente, con la diferencia de que uno de los resultados de sus registros ha sido
  modificado. Utiliza una recursión del tipo lineal no por cola para unir cada uno de los registros en una lista, incluyendo aquel que fue modificado.

  INPUT:
    Nuevo Resultado
    Registro a modificar
    Registros TratamientoDiagnosticoPaciente

  OUTPUT:
    Registros TratamientoDiagnosticoPaciente con la modificación señalada
|#
(define modificar_Resultado_Registro_TratamientoDiagnosticoPaciente
  (lambda (nuevoResultado registro registros)
    [if (= (length registros) 1 )
        
        ;Se esta trabajando sobre el ultimo de los registros
        [if (equal? (car registros) registro)
            
            ;Se ha encontrado el registro a modificar
            (list (setResultado (car registros) nuevoResultado))
            
            ;Si no, entonces se retorna el registro sin modificar
            (list (car registros))
            ]
        
        ;Se esta trabajando sobre cualquier otro registro
        [if (equal? (car registros) registro)
            
            ;Se ha encontrado el registro a modificar
            (cons (setResultado (car registros) nuevoResultado) (modificar_Resultado_Registro_TratamientoDiagnosticoPaciente nuevoResultado registro (cdr registros)))
            
            ;Se no, entonces no se realiza modificación sobre el registro
            (cons (car registros) (modificar_Resultado_Registro_TratamientoDiagnosticoPaciente nuevoResultado registro (cdr registros)))
            
            ]
        ]
    )
  )


