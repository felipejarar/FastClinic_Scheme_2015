#lang racket

(require "Funciones/FuncionesGenerales.rkt")
(require "Funciones/Diagnostico.rkt")
(require "Funciones/Doctor.rkt")
(require "Funciones/DiagnosticoPaciente.rkt")
(require "Funciones/Paciente.rkt")
(require "Funciones/Tratamiento.rkt")
(require "Funciones/TratamientoDiagnostico.rkt")
(require "Funciones/TratamientoDiagnosticoPaciente.rkt")

(provide obtenerNombrePaciente)
(provide especialidad)
(provide tratamientoRiesgoso)
(provide tratamientoMasUsadoPorDiagnostico)
(provide medicoMasAltas)
(provide tratamientoMasUsado)
(provide medicosTratantes)
(provide riesgoUltimoTratamiento)
(provide diagnosticoPacienteMedico)
(provide listarMedicosTratantesPaciente)
(provide modificarCorreoPaciente)
(provide modificarResultadoTratamiento)
(provide modificarMedicoAlta)

;FUNCIONES DE SELECCIÓN

;A. (NIVEL BAJO) Obtener el nombre de un paciente dado su rut. 
(define obtenerNombrePaciente
  (lambda (rut)
    
    #| CONDICIONES:
    1) El rut ingresado debe ser un string
    2) Debe poderse obtener una lista de registros a partir de la base de datos
    3) El rut ingresado debe existir en la base de datos 
    |#
    
    (if (not (string? rut))
        
        "El rut debe ser ingresado como una cadena de caracteres delimitada por comillas\n"
     
        (if (equal? (extraer_Registros_Pacientes BDPacientes) null)
            
            "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
            
           (if (equal? (obtener_Registro_Paciente_segun_RUT rut (extraer_Registros_Pacientes BDPacientes)) null)
               
               "El rut ingresado no pudo ser encontrado en los registros obtenidos a partir de la base de datos\n"
               
               ;Todas las condiciones se cumplieron
               (string-append (getNombrePaciente (obtener_Registro_Paciente_segun_RUT rut (extraer_Registros_Pacientes BDPacientes))) "\n")
               )
           )
        )
    )
  )

;B. (NIVEL BAJO) Obtener la especialidad de un médico a partir de su rut. 
(define especialidad
  (lambda (rut)
    
    #| CONDICIONES:
    1) El rut ingresado debe ser un string
    2) Debe poderse obtener una lista de registros a partir de la base de datos
    3) El rut ingresado debe existir en la base de datos 
    |#
    
    (if (not(string? rut))
        
        "El rut debe ser ingresado como una cadena de caracteres delimitada por comillas\n"
        
        (if (equal? (extraer_Registros_Doctores BDDoctores) null)
            
            "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
            
            (if (equal? (obtener_Registro_Doctor_segun_RUT rut (extraer_Registros_Doctores BDDoctores)) null)
                
                 "El rut ingresado no pudo ser encontrado en los registros obtenidos a partir de la base de datos\n"
                 
                 (string-append (getEspecialidadDoctor (obtener_Registro_Doctor_segun_RUT rut (extraer_Registros_Doctores BDDoctores))) "\n")
                 
                 )
            )
        )
    )
  )

;C. (NIVEL BAJO) Listar el (o los) tratamiento(s) de acuerdo a un nivel de riesgo dado, deben mostrarse en el mismo orden en que aparecen en la tabla “Tratamiento”.
(define tratamientoRiesgoso
  (lambda (nivel)

    #| CONDICIONES:
    1) El nivel ingresado debe ser un string
    2) Debe poderse obtener una lista de registros a partir de la base de datos
    3) El nivel ingresado debe existir en la base de datos 
    |#
    (if (not (string? nivel))
        
        "El nivel de riesgo debe ser ingresado como una cadena de caracteres delimitada por comillas\n"
        
        (if (equal? (extraer_Registros_Tratamientos BDTratamientos) null)
            
            "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
            
            (if (equal? (obtener_Registros_Tratamientos_Segun_Nivel nivel (extraer_Registros_Tratamientos BDTratamientos)) null)
                
                "No se ha podido encontrar el nivel de riesgo ingresado en la base de datos\n"
                
                (obtener_Nombres_Tratamientos_Segun_Lista (obtener_Registros_Tratamientos_Segun_Nivel nivel (extraer_Registros_Tratamientos BDTratamientos)))
    
                )
            )
        )
    )
  )

#|
H. (NIVEL MEDIO) Determinar el (o los) tratamiento(s) más usado(s) para un diagnóstico particular indicando la cantidad de veces que se ha empleado, deben mostrarse en el mismo  
   orden que la tabla “Tratamiento”. 
|#
(define tratamientoMasUsadoPorDiagnostico
  (lambda (IDDiagnostico)
    
    #| CONDICIONES:
    1) El identificador ingresado debe ser un number
    2) Debe poderse obtener una lista de registros a partir de la base de datos
    3) El identificador ingresado debe existir en la base de datos 
    |#
    
    (if (not(and (number? IDDiagnostico) (> IDDiagnostico -1)))
        
        "El identificador debe ser ingresado como un numero mayor o igual que cero\n"
        
        (if (equal? (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos) null)
            
            "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
            
            (if (equal? (obtener_IDTratamientos_Segun_IDDiagnostico IDDiagnostico (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null)
                
                "No ha sido posible encontrar el identificador del diagnostico ingresado en la base de datos\n"
                
                (let 
                    ( 
                     
                     ;Declaracion de una constante
                     (contadores
                      
                      ;Procedimiento relacionado con la definicion de la constante "contadores"
                      (map 
                       (lambda (L) (cadr L))
                       (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_IDTratamientos_Segun_IDDiagnostico IDDiagnostico (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null))
                       )
                      )
                     
                     );Fin de la declaracion de constantes
                  
                  ;Se busca a los identificadores más utilizados, es decir, aquellos con un contador más alto
                  (let
                      (
                       
                       ;Declaración de una constante
                       (maximosIdentificadores
                        
                        ;Procedimiento relacionado con la definición de la constante "maximosIdentificadores"
                        (map
                         (lambda (L) (car L))
                         (obtener_Maximos_Elementos_Lista_Contada (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_IDTratamientos_Segun_IDDiagnostico IDDiagnostico (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null)) (obtener_Maximo_Lista contadores 0))
                         )
                        )
                       )
                              
                    (concatenar_Listas_De_Texto
                     (map (lambda (L) (string-append (car L) " " (number->string (cadr L)) "\n" )) 
                          (map (lambda (L2) (list L2 (obtener_Maximo_Lista contadores 0)))
                          (obtener_Nombres_Tratamientos_Segun_Identificadores (sort maximosIdentificadores <) (extraer_Registros_Tratamientos BDTratamientos))
                          )
                          )
                     )
                   ) ;Fin de let 2
                  ); Fin de let 1
                )
            )
        )
    )
  )
   
#|
I. (NIVEL MEDIO) Identificar el (o los) médico(s) que más altas otorga indicando su rut, nombre y apellido, y la cantidad de altas, el rut debe separarse mediante una
   coma del nombre y el apellido, lo mismo debe ocurrir para la cantidad de altas. En caso de haber dos o más médicos con igual cantidad de altas se deben mostrar todos
   estos ordenados según como aparecen en la tabla “Doctor”. 
|#
(define medicoMasAltas 
  (if (equal? (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes) null)
      
      "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
      
      (let
          (           
           ;Declaracion de una constante
           (contadores
            (map (lambda (L) (cadr L))
                 (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_Identificadores_Doctor_Alta (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes)) null))
                 )
            )
           )
            
        ;Procedimiento relacionado con la declaracion de la constante "contadores"
        (let
            (
             ;Declaracion de una constante
             (maximosIdentificadores
              (map 
               (lambda (L) (car L))
                (obtener_Maximos_Elementos_Lista_Contada  (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_Identificadores_Doctor_Alta (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes)) null)) (obtener_Maximo_Lista contadores 0))
                )
              )
             )
          
          ;Procedimiento relacionado con la declaracion de la constante "maximosIdentificadores"
          
           (concatenar_Listas_De_Texto 
            (map
             (lambda (L) (string-append L "," (number->string (obtener_Maximo_Lista contadores 0)) "\n"))
             (obtener_RUT_Nombre_Apellido_Doctores_segun_Identificadores (sort maximosIdentificadores <) (extraer_Registros_Doctores BDDoctores))
             )
            )
           
          )
        )
      )
  )

#|
J. (NIVEL MEDIO)  Identificar el (o los) tratamiento(s) más usado(s) en todo el sistema indicando la cantidad de veces que se ha(n) usado, En caso de haber
   dos o más tratamientos con igual cantidad de veces que han sido usados, se deben mostrar todos estos ordenados según como aparecen en la tabla “Tratamiento”.
|#
(define tratamientoMasUsado
  
  (if (equal? (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos) null)
      
      "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
      
      (if (equal? (obtener_IDTratamientos (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null)
          
          "Se ha producido un error al momento de extraer los identificadores de los diferentes tratamientos \n"
          
          (let
              (
               ;Declaracion de una constante
               (contadores
                (map (lambda (L) (cadr L))
                     (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_IDTratamientos (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null))
                     )
                )
               )
               
            ;Procedimiento relacionado con la declaración de la constante "contadores"
            (let
                (
                 ;Declaracion de una constante
                 (identificadoresTratamientosMasUsados
                  (map 
                   (lambda (L) (car L))
                   (obtener_Maximos_Elementos_Lista_Contada  (reverse (listar_Y_Contar_Elementos_En_Listas_V1 (obtener_IDTratamientos (extraer_Registros_TratamientoDiagnosticos BDTratamientoDiagnosticos)) null)) (obtener_Maximo_Lista contadores 0))
                   )
                  )
                 )
              
              ;Procedimiento relacionado con la declaración de la constante "identificadoresTratamientosMasUsados"
              (concatenar_Listas_De_Texto
               (map (lambda (L) (string-append L "," (number->string (obtener_Maximo_Lista contadores 0)) "\n"))
                    (obtener_Nombres_Tratamientos_Segun_Identificadores (sort identificadoresTratamientosMasUsados <) (extraer_Registros_Tratamientos BDTratamientos))
                    )
               )
              )
            )
          )
      )
  )

#|M. (Nivel Medio) Dado el identificador de un paciente, indicar el rut, nombre y apellido de todos los médicos que lo han tratado (no incluir los que lo dieron de alta),
estos deben mostrarse  en el mismo orden en que aparecen en la tabla “Doctor”. |#
(define medicosTratantes 
  (lambda (IDPaciente)
    
    [if (not (and (number? IDPaciente) (> IDPaciente -1)))
        
        "El Identificador del paciente debe ser ingresado como un numero entero mayor o igual que 0"
        
        (let
            (
             (registrosPaciente_txt (extraer_Registros_Pacientes BDPacientes))
             (registrosDoctor_txt (extraer_Registros_Doctores BDDoctores))
             (registrosDiagnosticoPaciente_txt (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes))
             )
          
          [if (or (equal? registrosPaciente_txt null)
                  (equal? registrosDiagnosticoPaciente_txt null)
                  (equal? registrosDoctor_txt null)
                  )
              
              "Ha ocurrido un error al momento de extraer los registros desde la base de dato\n"
              
              [if (equal? (obtener_Registro_Paciente_segun_ID IDPaciente registrosPaciente_txt) null)
                  
                  "No ha sido posible encontrar el paciente en la base de datos\n"
                  
                  (let
                      (
                       (RegistrosDiagnosticoPaciente_RelacionadosConPaciente (obtener_Registros_DiagnosticoPaciente_Segun_IDPaciente IDPaciente registrosDiagnosticoPaciente_txt))
                       )
                    
                    (
                     let
                        (
                         (IdentificadoresMedicos_RelacionadosConPaciente (map (lambda (reg) (getIDDoctorDiagnostico reg)) RegistrosDiagnosticoPaciente_RelacionadosConPaciente))
                         )
                      
                      (
                       let
                          (
                           (IdentificadoresMedicos_Ordenados (sort IdentificadoresMedicos_RelacionadosConPaciente <))
                           )
                      
                        (
                         let
                            (
                             (RegistrosDoctores_RelacionadosConPaciente (map (lambda (ID) (obtener_RUT_Nombre_Apellido_Doctor_segun_Identificador_V2 ID registrosDoctor_txt) ) IdentificadoresMedicos_Ordenados))
                             )
                          (concatenar_Listas_De_Texto (map (lambda (reg) (string-append reg "\n")) RegistrosDoctores_RelacionadosConPaciente))
                          )
                        )
                      )
                    )
                  ]
              ]
          )
        ]
    )
  )


#|O. (Nivel Alto) Dado el nombre y apellido de una persona, indicar cuál es el nivel de riesgo del último tratamiento recibido.  |#
(define riesgoUltimoTratamiento 
  (lambda (nombre apellido)
    
    [if (not(string? nombre))
        
        "El nombre debe ser ingresado como una cadena de texto delimitada por comillas\n"
        
        [if (not (string? apellido))
            
            "El apellido debe ser ingresado como una cadena de texto delimitada por comillas\n"
            
            [if (or (equal? (extraer_Registros_Pacientes BDPacientes) null ) 
                    (equal? (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes) null) 
                    (equal? (extraer_Registros_TratamientoDiagnosticoPacientes BDTratamientoDiagnosticoPacientes) null)
                    (equal? (extraer_Registros_Tratamientos BDTratamientos) null)
                    )
                
                
                "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
                
                [if (= (getIDPaciente (obtener_Registro_Paciente_segun_Nombre_Apellido nombre apellido (extraer_Registros_Pacientes BDPacientes))) -1)
                    
                    "No se ha podido encontrar el nombre y el apellido del paciente en la base de datos\n"
                    
                    ;1) Se obtiene el registro de un paciente segun su nombre y apellido
                    ;2) Se obtiene el identificador del registro del paciente y se declara como una constante
                    (let
                        (
                         (IDPaciente (getIDPaciente (obtener_Registro_Paciente_segun_Nombre_Apellido nombre apellido (extraer_Registros_Pacientes BDPacientes))))
                         )
                      
                      ;3) Se obtienen los registros DiagnosticoPaciente relacionados con el identificador del paciente
                      ;4) Se obtienen los identificadores DiagnosticoPaciente relacionado con los registros obtenidos y se declara como una constante
                      (
                       let
                          (
                           (IDDiagnosticoPacientes 
                            (map getIDDiagnosticoPaciente_vvv 
                                 (obtener_Registros_DiagnosticoPaciente_Segun_IDPaciente IDPaciente (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes))
                                 )
                            )
                           )
                        
                        ;5) Se obtienen los registros Tratamientos relacionados con los identificadores DiagnosticoPaciente
                        (let 
                            (
                             (RegistrosTratamientos
                              (map (lambda (ID) (obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente ID (extraer_Registros_TratamientoDiagnosticoPacientes BDTratamientoDiagnosticoPacientes))) 
                                   IDDiagnosticoPacientes
                                   )
                              )
                             )
                          
                          ;6) Se obtienen las fechas relacionadas con los registros Tratamientos
                          (let
                              (
                               (fechasTratamientos
                                (map (lambda (registro) (getFechaInicio registro)) RegistrosTratamientos)
                                )
                               )
                            
                            ;7) Se obtiene la fecha mas reciente
                            (
                             let
                                (
                                 (fechaMasReciente (encontrar_Fecha_Mas_Reciente fechasTratamientos (car fechasTratamientos)))
                                 )
                              
                              ;8) Se obtiene el identificador del tratamiento mas reciente
                              (
                               let
                                  (
                                   (IDTratamientoReciente 
                                    (getIDTratamiento_vv (obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha fechaMasReciente (extraer_Registros_TratamientoDiagnosticoPacientes BDTratamientoDiagnosticoPacientes)))
                                    )
                                   )
                                ;9) Se obtiene el registro relacionado con el identificador del tratamiento y luego su nivel de riesgo
                                (string-append (getNivelDeRiesgo (obtener_Registro_Tratamiento_Segun_Identificador IDTratamientoReciente (extraer_Registros_Tratamientos BDTratamientos))) "\n")
                                )
                              )
                            )
                          )
                        ) 
                      )
                    ]
                ]
            ]
        ]
    )
  )
        
#|
P. (Nivel Alto) Conocer todos los pacientes diagnosticados con el diagnóstico X (nombre   del  diagnóstico) a los cuales el doctor Y (rut) les dio el Alta. Debe mostrar el 
rut, nombre y  apellido. Estos deben mostrarse en el mismo orden en que aparecen en la tabla “Paciente”, no deben repetirse resultados. 
|#
(define diagnosticoPacienteMedico 
  (lambda (nombreDiagnostico rutDoctor)
    [if (not (string? nombreDiagnostico))
        "El nombre del Diagnostico debe ser ingresado como una cadena de caracteres delimitadas por comillas\n"
        [if (not (string? rutDoctor))
            "El rut del Doctor debe ser ingresado como una cadena de caracteres delimitadas por comillas\n"
            (let
                (
                 (registrosPaciente_txt (extraer_Registros_Pacientes BDPacientes))
                 (registrosDiagnosticoPaciente_txt (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes))
                 (registrosDiagnostico_txt (extraer_Registros_Diagnosticos BDDiagnosticos))
                 (registrosDoctor_txt (extraer_Registros_Doctores BDDoctores))
                 )
              
              [if (or (equal? registrosPaciente_txt null)
                      (equal? registrosDiagnosticoPaciente_txt null)
                      (equal? registrosDiagnostico_txt null)
                      (equal? registrosDoctor_txt null)
                      )
                  
                  "Se ha producido un error en la extracción de los registros desde la base de datos\n"
                  
                  (let
                      (
                       (IDDiagnostico (getIDDiagnostico(obtener_Registros_Diagnosticos_Segun_Nombre nombreDiagnostico registrosDiagnostico_txt)))
                       (IDDoctor (getIDDoctor(obtener_Registro_Doctor_segun_RUT rutDoctor registrosDoctor_txt)))
                       )
                    
                    [if (= IDDiagnostico -1)
                        
                        "El nombre del diagnostico ingresado no pudo ser encontrado en la base de datos\n"
                        
                        [if (= IDDoctor -1)
                            
                            "El rut del doctor no pudo ser encontrado en la base de datos\n"
                            
                            (let
                                (
                                 (RegistrosComunes (obtener_Registros_DiagnosticoPaciente_Segun_IDDoctorAlta_IDDiagnostico IDDoctor IDDiagnostico registrosDiagnosticoPaciente_txt))
                                 )
                              
                              [if (equal? RegistrosComunes null)
                                  
                                  "No existen registros comunes para el Diagnostico y Doctor ingresado\n"
                                  
                                  (let
                                      (
                                       (IdentificadoresPacientesRelacionados (map (lambda (reg) (getIDPaciente_vvv reg)) RegistrosComunes))
                                       )
                                    
                                    (let
                                        (
                                         (IdentificadoresPacientesRelacionadosSinRepeticones (sort (remove-duplicates IdentificadoresPacientesRelacionados =) <)) 
                                         )
                                      (let
                                          (
                                           (registrosPacientesEstudiados (map (lambda (ID) (obtener_Registro_Paciente_segun_ID ID registrosPaciente_txt)) IdentificadoresPacientesRelacionadosSinRepeticones))
                                           )
                                        
                                        (concatenar_Listas_De_Texto
                                         (map (lambda (reg) (string-append 
                                                             (number->string (getIDPaciente reg)) ","
                                                             (getRUTPaciente reg) ","
                                                             (getEmailPaciente reg) ","
                                                             (getNombrePaciente reg) ","
                                                             (getApellidoPaciente reg) "\n"
                                                             )
                                                
                                                )
                                              
                                              registrosPacientesEstudiados
                                              )
                                         )
                                        
                                        )
                                      )
                                    )
                                  ]
                              
                              )
                            ]
                        ]
                    )
                  ]
              )
            ]
        ]
    )
  )

#|
Q. Dado el rut de un paciente, indicar el rut, nombre y apellido de todos los médicos que lo han tratado (no incluye a los que lo han dado de alta), estos deben 
mostrarse en el mismo orden en que aparecen en la tabla “Doctor”. 
|#
(define listarMedicosTratantesPaciente
  (lambda (rutPaciente)
    
    [if (not(string? rutPaciente))
        
        "El rut del paciente debe ser ingresado como una cadena de caracteres delimitada por comillas\n"
        
        [if (equal?  (extraer_Registros_Pacientes BDPacientes) null)
            
            "Ha ocurrido un error al momento de extraer los registros desde la base de datos\n"
            
            (let
                (
                 (RegistroPaciente (obtener_Registro_Paciente_segun_RUT rutPaciente (extraer_Registros_Pacientes BDPacientes)))
                 )
              
              [if (equal? RegistroPaciente null)
                
                "No ha sido posible encontrar el rut del paciente en la base de datos\n"
                
                (medicosTratantes (getIDPaciente RegistroPaciente))
                
                ]
              )
            

            ]
        ]
    )
  )
                                   
        
        
;FUNCIONES DE MODIFICACIÓN:

#| R. (Nivel Bajo) Modificar el correo electrónico de un paciente. Se asume que el correo es único.  |#
(define modificarCorreoPaciente 
  (lambda (correoAntiguo correoNuevo)
    
    [if (not (string? correoAntiguo))

        "El correo antiguo debe ser ingresado como una cadena de caracteres delimitados por comillas\n"
        
        [if (not (string? correoNuevo))
            
            "El correo nuevo debe ser ingresado como una cadena de caracteres delimitados por comillas\n"
            
            [if (string=? correoAntiguo correoNuevo)
                
                "Ambos correos ingresados son iguales. Ninguna modificación deberia ser efectuada\n"
                
                [if (equal? (obtener_Registro_Paciente_segun_Email correoAntiguo (extraer_Registros_Pacientes BDPacientes)) null)
                    
                    "No ha sido posible encontrar el correo antiguo en la base de datos\n"
                    
                    [if (not (equal? (obtener_Registro_Paciente_segun_Email correoNuevo (extraer_Registros_Pacientes BDPacientes)) null))
                        
                        "El correo nuevo ya existe en la base de datos, por lo que el cambio no puede ser realizado\n"
                    (escribirOutput
                     (string-append "identificador,rut,email,nombre,apellido,fecha de nacimiento\n"
                                    (concatenar_Listas_De_Texto 
                                     (map (lambda (reg) (string-append
                                                         (number->string (getIDPaciente reg)) ","
                                                         (getRUTPaciente reg) ","
                                                         (getEmailPaciente reg) ","
                                                         (getNombrePaciente reg) ","
                                                         (getApellidoPaciente reg) ","
                                                         (getFecha_NacimientoPaciente reg) "\n"
                                                         )
                                            )
                                          
                                          (modificar_Email_En_Registro_Paciente correoAntiguo correoNuevo (extraer_Registros_Pacientes BDPacientes))
                                          )
                                     )
                                    )
                        
                        "Base_de_Datos/Paciente.txt")
                        
                        ]
                    ]
                ]
            ]
        ]
    )
  )

#| W. (Nivel Medio) Dado   el   nombre   y   apellido   de   un   paciente,   además   del   id   de   un   tratamiento,   modificar   el  
   resultado   de   su   tratamiento   más   reciente   (puede   darse   en   caso   que   un   paciente   reciba  
   varias veces el mismo tratamiento). |#
(define modificarResultadoTratamiento 
  (lambda (nombre apellido idTratamiento nuevoResultado)
    
    [if (not (string? nombre))
        
        "El nombre del paciente debe ser ingresado como una cadena de caracteres delimitadas por comillas\n"
        
        [if (not (string? apellido))
            
            "El apellido del paciente debe ser ingresado como una cadena de caracteres delimitadas por comillas\n"
            
            [if (not (and (number? idTratamiento)(> idTratamiento -1)))
                
                "El identificador del tratamiento debe ser un numero mayor o igual que cero\n"
                
                [if (not (string? nuevoResultado))
                    
                    "El nuevo resultado debe ser una cadena de caracteres delimitadas por comillas \n"
                    
                    (let
                        (
                         (registrosPaciente_txt (extraer_Registros_Pacientes BDPacientes))
                         (registrosTratamientoDiagnosticoPaciente_txt (extraer_Registros_TratamientoDiagnosticoPacientes BDTratamientoDiagnosticoPacientes))
                         (registrosDiagnosticoPaciente_txt (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes))
                         )
                      
                      [if (or (equal? registrosPaciente_txt null) (equal? registrosTratamientoDiagnosticoPaciente_txt null) (equal? registrosDiagnosticoPaciente_txt null) )
                          
                          "Se ha producido un error al momento de extraer los registros desde la base de datos\n"
                          
                          (let
                              (
                               (registroPaciente (obtener_Registro_Paciente_segun_Nombre_Apellido nombre apellido registrosPaciente_txt))
                               )
                            
                            [if (equal? registroPaciente null)
                                
                                "No se ha podido encontrar el nombre y apellido del paciente en la base de datos\n"
                                
                                (let
                                    (
                                     (registrosDiagnosticoPaciente_RelacionadosConPaciente (obtener_Registros_DiagnosticoPaciente_Segun_IDPaciente (getIDPaciente registroPaciente) registrosDiagnosticoPaciente_txt))
                                     )
                                  
                                  [if (equal? registrosDiagnosticoPaciente_RelacionadosConPaciente null)
                                      
                                      "El paciente no se encuentra relacionado a ningun registro DiagnosticoPaciente y, por lo tanto, a ningun registro Tratamiento\n"
                                      
                                      (let
                                          (
                                           (registrosTratamientosDiagnosticoPaciente_RelacionadosConPaciente 
                                            (map 
                                             (lambda (registro)(obtener_Registro_TratamientoDiagnosticoPaciente_segun_IDDiagnosticoPaciente (getIDDiagnosticoPaciente_vvv registro) registrosTratamientoDiagnosticoPaciente_txt))
                                             registrosDiagnosticoPaciente_RelacionadosConPaciente
                                             )
                                            )
                                           )
                                        
                                        (let
                                            (
                                             (registrosTratamientosDiagnosticoPaciente_RelacionadosConPacienteYConTratamiento
                                              (obtener_Registros_TratamientoDiagnosticoPaciente_segun_IDTratamiento idTratamiento registrosTratamientosDiagnosticoPaciente_RelacionadosConPaciente)
                                              )
                                             )
                                          
                                          (let
                                              (
                                               (fechas (map (lambda (registro) (getFechaInicio registro)) registrosTratamientosDiagnosticoPaciente_RelacionadosConPacienteYConTratamiento))
                                               )
                                            
                                            (let
                                                (
                                                 (fechaMasReciente (encontrar_Fecha_Mas_Reciente fechas (car fechas)))
                                                 )
                                              (let
                                                  (
                                                   (registroAModificar (obtener_Registro_TratamientoDiagnosticoPaciente_segun_Fecha fechaMasReciente registrosTratamientosDiagnosticoPaciente_RelacionadosConPacienteYConTratamiento))
                                                   )
                                                
                                                (escribirOutput
                                                 (string-append "identificadorDiagnosticoPaciente,identificadorTratamiento,identificadorMedico,fechaInicio,duracionDias,resultado\n"
                                                               (concatenar_Listas_De_Texto
                                                                (map (lambda (registro) (string-append (number->string(getIDDiagnosticoPaciente_vv registro)) ","
                                                                                                       (number->string(getIDTratamiento_vv registro)) ","
                                                                                                       (number->string(getIDMedico_vv registro)) ","
                                                                                                       (getFechaInicio registro) ","
                                                                                                       (number->string(getDuracionDias registro)) ","
                                                                                                       (getResultado registro) "\n"
                                                                                                       )
                                                                       )                    
                                                                     (modificar_Resultado_Registro_TratamientoDiagnosticoPaciente nuevoResultado registroAModificar registrosTratamientoDiagnosticoPaciente_txt)
                                                                     )
                                                                )
                                                               )
                                                 
                                                 "Base_de_Datos/TratamientoDiagnosticoPaciente.txt"
                                                 )      
                                                )
                                              )
                                            )
                                          )
                                        )            
                                      ]
                                  )
                                ]
                            )
                          ]
                      )
                    ]
                ]
            ]
        ]
    )
  )
                                


                
#| Y. (Nivel Alto) Dado el correo del paciente y del médico, modificar el médico que da el alta en su última ocasión. |#
(define modificarMedicoAlta
  (lambda (emailPaciente emailMedico)
    
    [if (not (string? emailPaciente))
        
        "El correo del paciente debe ser ingresado como un string delimitado por comillas\n"
        
        [if (not (string? emailMedico))
            
            "El correo del medico debe ser ingresado como un string delimitado por comillas\n"
            
            [if (or
                 (equal? (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes) null)
                 (equal? (extraer_Registros_Pacientes BDPacientes) null)
                 (equal? (extraer_Registros_Doctores BDDoctores) null)
                 )
                
                "Se ha producido un error al extraer los registros desde la base de datos\n"
               
                (let 
                    (
                     (registroPaciente (obtener_Registro_Paciente_segun_Email emailPaciente (extraer_Registros_Pacientes BDPacientes)))
                     (registroDoctor (obtener_Registro_Doctor_segun_Email emailMedico (extraer_Registros_Doctores BDDoctores)))
                     )
                  
                  [if (equal? registroPaciente null)

                      "No se ha podido encontrar el email del paciente en la base de datos\n"
                   
                      [if (equal? registroDoctor null)
                       
                          "No se ha podido encontrar el email del doctor en la base de datos\n"
                       
                          (let
                              (
                               (IDPaciente (getIDPaciente registroPaciente))
                               (IDDoctor (getIDDoctor registroDoctor))
                               )
                            
                            (let
                                (
                                 (registrosDiagnosticoPaciente (obtener_Registros_DiagnosticoPaciente_Segun_IDPaciente IDPaciente (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes)))
                                 )
                              
                              [if (equal? registrosDiagnosticoPaciente null)
                                  
                                  "No se ha podido encontrar el paciente dentro de los registros de vinculación Diagnostico - Paciente \n"
                                  
                                  (
                                   let
                                      (
                                       (fechasAlta (map (lambda (L) (getFechaAlta L)) registrosDiagnosticoPaciente))
                                       )
                                    
                                     (
                                      let
                                         (
                                          (fechaAltaMasReciente (encontrar_Fecha_Mas_Reciente fechasAlta (car fechasAlta)))
                                          )
                                       
                                       (let
                                           (
                                            (registroDiagnosticoPacienteParaModificar (obtener_Registros_DiagnosticoPaciente_Segun_FechaAlta fechaAltaMasReciente registrosDiagnosticoPaciente))
                                            )
                                         
                                         ;Suponiendo que hay más de un registro con la fecha de alta más reciente
                                         (escribirOutput 
                                          (string-append "identificadorDiagnosticoPaciente,identificadorPaciente,identificadorDiagnostico,fechaDiagnostico,identificadorDoctorDiagnostico,estadoDiagnostico,fechaAlta,identificadorDoctorAlta,detalleAlta\n" 
                                                         (concatenar_Listas_De_Texto
                                                          (map (lambda (reg)
                                                                 (string-append (number->string(getIDDiagnosticoPaciente_vvv reg)) ","
                                                                                (number->string(getIDPaciente_vvv reg)) ","
                                                                                (number->string(getIDDiagnostico_vvv reg)) ","
                                                                                (getFechaDiagnostico reg) ","
                                                                                (number->string(getIDDoctorDiagnostico reg)) ","
                                                                                (getEstadoDiagnostico reg) ","
                                                                                (getFechaAlta reg) ","
                                                                                (number->string(getIDDoctorAlta reg)) ","
                                                                                (getDetalleAlta reg) "\n"
                                                                                )
                                                                 )                 
                                                               (modificar_IDDoctorAlta_Registro_DiagnosticoPaciente_Segun_Registro IDDoctor registroDiagnosticoPacienteParaModificar (extraer_Registros_DiagnosticoPacientes BDDiagnosticosPacientes))
                                                               )
                                                          )
                                                         )
                                          
                                          "Base_de_Datos/DiagnosticoPaciente.txt")
                                                  
                                         )
                                       )   
                                    )
                                  ]
                              )
                            )
                          ]
                      ]
                  ) 
               ]
            ]
        ]
    )
  )

(define concatenarRegistro
  (lambda ( hecho registro contador)
    (if (= contador 0)
        (string-append hecho "(" (concatenarRegistro hecho registro (+ contador 1)) ").\n")
                       
        (if (= (length registro) 1)
            ;Unico elemento
            (if (string=? (car registro) "muy alto")
                
                (string-append  "''"(car registro)"''")
             
                (car registro)
                
                )
           
            ;Mas elementos      
            ;(if (= contador 3)
                
                
            ;(string-append "'"(car registro)"', " (concatenarRegistro hecho (cdr registro) (+ contador 1)))
                                      
                (string-append (car registro) "," (concatenarRegistro hecho (cdr registro) (+ contador 1)))
                     
                     
                
                
            )
        )
    )
  )

(define concatenarRegistros
  (lambda (registros hecho)
    (if (= (length registros) 1)
        ;Unico registro
        (concatenarRegistro hecho (car registros) 0)
        ;Mas elementos
        (string-append (concatenarRegistro hecho (car registros) 0) (concatenarRegistros (cdr registros) hecho))
        )
    )
  )


 (escribirOutput (concatenarRegistros BDTratamientos "tratamiento") "Tratamiento.txt")
; (escribirOutput (concatenarRegistros BDTratamientos "tratamiento") "Tratamiento.txt")
; (escribirOutput (concatenarRegistros BDDoctores "doctor") "Doctor.txt")
; (escribirOutput (concatenarRegistros BDDiagnosticosPacientes "diagnostico_paciente") "DiagnosticoPaciente.txt")
; (escribirOutput (concatenarRegistros BDTratamientoDiagnosticoPacientes "tratamiento_diagnostico_paciente") "TratamientoDiagnosticoPaciente.txt")
; (escribirOutput (concatenarRegistros BDTratamientoDiagnosticos "tratamiento_diagnostico") "TratamientoDiagnostico.txt")
; (escribirOutput (concatenarRegistros BDDiagnosticos "diagnostico") "Diagnostico.txt")




