;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Logica) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

; =====================================
; Funciones auxiliares de construccion
; =====================================

; Verifica si una dimension del tablero esta entre 4 y 10.
; Entrada: un numero.
; Salida: #t si la dimension es valida y #f si no lo es.
(define (DimensionValida? dimension)
  (cond
    ((< dimension 4) #f)
    ((> dimension 10) #f)
  (else
    #t)))

; Crea una fila llena de ceros.
; Entrada: la cantidad de columnas.
; Salida: una lista con esa cantidad de ceros.
(define (CrearFila columnas)
  (cond
    ((equal? columnas 0) null)
  (else
    (cons 0 (CrearFila (- columnas 1))))))

; Crea el tablero vacio de forma recursiva.
; Entrada: la cantidad de filas y la cantidad de columnas.
; Salida: una lista de filas llena de ceros.
(define (CrearTableroAux filas columnas)
  (cond
    ((equal? filas 0) null)
  (else
    (cons (CrearFila columnas)
          (CrearTableroAux (- filas 1) columnas)))))

; Crea un tablero vacio si las dimensiones son validas.
; Entrada: la cantidad de filas y la cantidad de columnas.
; Salida: el tablero vacio o null si las dimensiones no son validas.
(define (CrearTablero filas columnas)
  (cond
    ((DimensionValida? filas)
     (cond
       ((DimensionValida? columnas)
        (CrearTableroAux filas columnas))
     (else
       null)))
  (else
    null)))

; Crea una posicion con fila y columna.
; Entrada: el numero de fila y el numero de columna.
; Salida: una lista con la fila y la columna.
(define (CrearPosicion fila columna)
  (cons fila (cons columna null)))

; Obtiene la fila de una posicion.
; Entrada: una posicion.
; Salida: el numero de fila.
(define (ObtenerFilaPosicion posicion)
  (car posicion))

; Obtiene la columna de una posicion.
; Entrada: una posicion.
; Salida: el numero de columna.
(define (ObtenerColumnaPosicion posicion)
  (car (cdr posicion)))

; Calcula la cantidad de elementos de una lista.
; Entrada: una lista.
; Salida: la cantidad de elementos de la lista.
(define (Largo lista)
  (cond
    ((null? lista) 0)
  (else
    (+ 1 (Largo (cdr lista))))))

; Obtiene el elemento que esta en una posicion dada de una lista.
; Entrada: una lista y un indice.
; Salida: el elemento en ese indice o null si no existe.
(define (ObtenerElemento lista indice)
  (cond
    ((null? lista) null)
    ((equal? indice 0) (car lista))
  (else
    (ObtenerElemento (cdr lista) (- indice 1)))))

; Busca las posiciones vacias de una fila.
; Entrada: una fila, el numero de fila y el numero de columna actual.
; Salida: una lista de posiciones vacias.
(define (BuscarVaciosFila fila numeroFila numeroColumna)
  (cond
    ((null? fila) null)
    ((equal? (car fila) 0)
     (cons (CrearPosicion numeroFila numeroColumna)
           (BuscarVaciosFila (cdr fila) numeroFila (+ numeroColumna 1))))
  (else
    (BuscarVaciosFila (cdr fila) numeroFila (+ numeroColumna 1)))))

; Busca las posiciones vacias de todo el tablero.
; Entrada: el tablero y el numero de fila actual.
; Salida: una lista con todas las posiciones vacias del tablero.
(define (BuscarVaciosTableroAux tablero numeroFila)
  (cond
    ((null? tablero) null)
  (else
    (append
     (BuscarVaciosFila (car tablero) numeroFila 0)
     (BuscarVaciosTableroAux (cdr tablero) (+ numeroFila 1))))))

; Busca todas las posiciones vacias del tablero.
; Entrada: un tablero.
; Salida: una lista con todas las posiciones vacias.
(define (BuscarVaciosTablero tablero)
  (BuscarVaciosTableroAux tablero 0))

; Reemplaza un elemento en una lista en una posicion dada.
; Entrada: una lista, un indice y un valor.
; Salida: una nueva lista con el valor reemplazado.
(define (ReemplazarEnLista lista indice valor)
  (cond
    ((null? lista) null)
    ((equal? indice 0)
     (cons valor (cdr lista)))
  (else
    (cons (car lista)
          (ReemplazarEnLista (cdr lista) (- indice 1) valor)))))

; Reemplaza un valor dentro del tablero.
; Entrada: un tablero, una fila, una columna y un valor.
; Salida: un nuevo tablero con el valor colocado en esa posicion.
(define (ReemplazarEnTablero tablero fila columna valor)
  (cond
    ((null? tablero) null)
    ((equal? fila 0)
     (cons (ReemplazarEnLista (car tablero) columna valor)
           (cdr tablero)))
  (else
    (cons (car tablero)
          (ReemplazarEnTablero (cdr tablero) (- fila 1) columna valor)))))

; Inserta un valor en una posicion dada del tablero.
; Entrada: un tablero, una posicion y un valor.
; Salida: un nuevo tablero con el valor insertado.
(define (InsertarValorEnPosicion tablero posicion valor)
  (ReemplazarEnTablero tablero
                       (ObtenerFilaPosicion posicion)
                       (ObtenerColumnaPosicion posicion)
                       valor))

; Escoge una posicion vacia al azar.
; Entrada: una lista de posiciones vacias.
; Salida: una posicion aleatoria.
(define (EscogerPosicionAleatoria vacios)
  (ObtenerElemento vacios (random (Largo vacios))))

; Inserta una ficha en una posicion vacia aleatoria.
; Entrada: un tablero, una lista de vacios y un valor.
; Salida: un nuevo tablero con la ficha insertada.
(define (InsertarFichaAleatoriaAux tablero vacios valor)
  (cond
    ((null? vacios) tablero)
  (else
    (InsertarValorEnPosicion tablero
                             (EscogerPosicionAleatoria vacios)
                             valor))))

; Inserta una ficha con un valor dado en una casilla vacia aleatoria.
; Entrada: un tablero y un valor.
; Salida: un nuevo tablero con la ficha insertada.
(define (InsertarFichaAleatoria tablero valor)
  (InsertarFichaAleatoriaAux tablero
                             (BuscarVaciosTablero tablero)
                             valor))

; Genera el valor de una ficha nueva.
; Entrada: nada.
; Salida: 2 o 4.
(define (GenerarValorFicha)
  (cond
    ((equal? (random 10) 0) 4)
  (else
    2)))

; Inserta una ficha nueva aleatoria en el tablero.
; Entrada: un tablero.
; Salida: un nuevo tablero con una ficha 2 o 4 en una posicion vacia.
(define (InsertarFichaNueva tablero)
  (InsertarFichaAleatoria tablero (GenerarValorFicha)))

; Crea el tablero inicial del juego con dos fichas de valor 2.
; Entrada: la cantidad de filas y la cantidad de columnas.
; Salida: un tablero inicial listo para probar.
(define (CrearTableroInicial filas columnas)
  (InsertarFichaAleatoria
   (InsertarFichaAleatoria
    (CrearTablero filas columnas)
    2)
   2))

; =========================
; Logica de movimientos
; =========================

; Quita todos los ceros de una fila.
; Entrada: una fila.
; Salida: una nueva fila sin ceros.
(define (QuitarCeros fila)
  (cond
    ((null? fila) null)
    ((equal? (car fila) 0)
     (QuitarCeros (cdr fila)))
  (else
    (cons (car fila) (QuitarCeros (cdr fila))))))

; Combina los valores iguales consecutivos de una fila ya sin ceros.
; Entrada: una fila sin ceros.
; Salida: una nueva fila con las combinaciones hechas.
(define (CombinarFila fila)
  (cond
    ((null? fila) null)
    ((null? (cdr fila)) fila)
    ((equal? (car fila) (car (cdr fila)))
     (cons (+ (car fila) (car (cdr fila)))
           (CombinarFila (cdr (cdr fila)))))
  (else
    (cons (car fila)
          (CombinarFila (cdr fila))))))

; Completa una fila con ceros al final hasta que tenga el largo original.
; Entrada: una fila y el largo original que debe tener.
; Salida: una fila completada con ceros.
(define (CompletarConCeros fila largoOriginal)
  (append fila (CrearFila (- largoOriginal (Largo fila)))))

; Resuelve el movimiento de una fila hacia la izquierda.
; Entrada: una fila.
; Salida: una nueva fila movida hacia la izquierda.
(define (MoverFilaIzquierda fila)
  (CompletarConCeros
   (CombinarFila (QuitarCeros fila))
   (Largo fila)))

; Invierte el orden de una lista.
; Entrada: una lista.
; Salida: una nueva lista invertida.
(define (InvertirLista lista)
  (cond
    ((null? lista) null)
  (else
    (append (InvertirLista (cdr lista))
            (cons (car lista) null)))))

; Resuelve el movimiento de una fila hacia la derecha.
; Entrada: una fila.
; Salida: una nueva fila movida hacia la derecha.
(define (MoverFilaDerecha fila)
  (InvertirLista
   (MoverFilaIzquierda
    (InvertirLista fila))))

; Aplica el movimiento hacia la izquierda a todas las filas del tablero.
; Entrada: un tablero.
; Salida: un nuevo tablero movido hacia la izquierda.
(define (MoverIzquierda tablero)
  (cond
    ((null? tablero) null)
  (else
    (cons (MoverFilaIzquierda (car tablero))
          (MoverIzquierda (cdr tablero))))))

; Aplica el movimiento hacia la derecha a todas las filas del tablero.
; Entrada: un tablero.
; Salida: un nuevo tablero movido hacia la derecha.
(define (MoverDerecha tablero)
  (cond
    ((null? tablero) null)
  (else
    (cons (MoverFilaDerecha (car tablero))
          (MoverDerecha (cdr tablero))))))

; Obtiene la primera columna de un tablero.
; Entrada: un tablero.
; Salida: una lista con la primera columna.
(define (PrimeraColumna tablero)
  (cond
    ((null? tablero) null)
  (else
    (cons (car (car tablero))
          (PrimeraColumna (cdr tablero))))))

; Quita la primera columna de un tablero.
; Entrada: un tablero.
; Salida: un nuevo tablero sin la primera columna.
(define (QuitarPrimeraColumna tablero)
  (cond
    ((null? tablero) null)
  (else
    (cons (cdr (car tablero))
          (QuitarPrimeraColumna (cdr tablero))))))

; Transpone un tablero intercambiando filas por columnas.
; Entrada: un tablero.
; Salida: un nuevo tablero transpuesto.
(define (Transponer tablero)
  (cond
    ((null? tablero) null)
    ((null? (car tablero)) null)
  (else
    (cons (PrimeraColumna tablero)
          (Transponer (QuitarPrimeraColumna tablero))))))

; Resuelve el movimiento del tablero hacia arriba.
; Entrada: un tablero.
; Salida: un nuevo tablero movido hacia arriba.
(define (MoverArriba tablero)
  (Transponer
   (MoverIzquierda
    (Transponer tablero))))

; Resuelve el movimiento del tablero hacia abajo.
; Entrada: un tablero.
; Salida: un nuevo tablero movido hacia abajo.
(define (MoverAbajo tablero)
  (Transponer
   (MoverDerecha
    (Transponer tablero))))

; Aplica un movimiento segun la direccion indicada.
; Entrada: un tablero y una direccion.
; Salida: un nuevo tablero con el movimiento aplicado.
(define (MoverTablero tablero direccion)
  (cond
    ((equal? direccion "izquierda") (MoverIzquierda tablero))
    ((equal? direccion "derecha") (MoverDerecha tablero))
    ((equal? direccion "arriba") (MoverArriba tablero))
    ((equal? direccion "abajo") (MoverAbajo tablero))
  (else
    tablero)))

; ==========================
; Validaciones del juego
; ==========================

; Compara dos filas posicion por posicion.
; Entrada: dos filas.
; Salida: #t si ambas filas son iguales y #f si no lo son.
(define (FilaIgual? fila1 fila2)
  (cond
    ((null? fila1)
     (cond
       ((null? fila2) #t)
     (else
       #f)))
    ((null? fila2) #f)
    ((equal? (car fila1) (car fila2))
     (FilaIgual? (cdr fila1) (cdr fila2)))
  (else
    #f)))

; Compara dos tableros fila por fila.
; Entrada: dos tableros.
; Salida: #t si ambos tableros son iguales y #f si no lo son.
(define (TableroIgual? tablero1 tablero2)
  (cond
    ((null? tablero1)
     (cond
       ((null? tablero2) #t)
     (else
       #f)))
    ((null? tablero2) #f)
    ((FilaIgual? (car tablero1) (car tablero2))
     (TableroIgual? (cdr tablero1) (cdr tablero2)))
  (else
    #f)))

; Determina si un movimiento realmente cambia el tablero.
; Entrada: un tablero y una direccion.
; Salida: #t si el tablero cambia y #f si queda igual.
(define (MovimientoValido? tablero direccion)
  (cond
    ((TableroIgual? tablero (MoverTablero tablero direccion)) #f)
  (else
    #t)))

; Revisa si una fila contiene un valor dado.
; Entrada: una fila y un valor.
; Salida: #t si el valor aparece en la fila y #f si no aparece.
(define (FilaContieneValor? fila valor)
  (cond
    ((null? fila) #f)
    ((equal? (car fila) valor) #t)
  (else
    (FilaContieneValor? (cdr fila) valor))))

; Revisa si el tablero contiene un valor dado.
; Entrada: un tablero y un valor.
; Salida: #t si el valor aparece en el tablero y #f si no aparece.
(define (TableroContieneValor? tablero valor)
  (cond
    ((null? tablero) #f)
    ((FilaContieneValor? (car tablero) valor) #t)
  (else
    (TableroContieneValor? (cdr tablero) valor))))

; Revisa si el tablero ya tiene una ficha 2048.
; Entrada: un tablero.
; Salida: #t si existe una ficha 2048 y #f si no existe.
(define (LlegoA2048? tablero)
  (TableroContieneValor? tablero 2048))

; Revisa si una fila tiene dos valores iguales consecutivos.
; Entrada: una fila.
; Salida: #t si existe una combinacion posible y #f si no existe.
(define (FilaTieneCombinacion? fila)
  (cond
    ((null? fila) #f)
    ((null? (cdr fila)) #f)
    ((equal? (car fila) (car (cdr fila))) #t)
  (else
    (FilaTieneCombinacion? (cdr fila)))))

; Revisa si alguna fila del tablero tiene una combinacion posible.
; Entrada: un tablero.
; Salida: #t si existe una combinacion horizontal y #f si no existe.
(define (TableroTieneCombinacionHorizontal? tablero)
  (cond
    ((null? tablero) #f)
    ((FilaTieneCombinacion? (car tablero)) #t)
  (else
    (TableroTieneCombinacionHorizontal? (cdr tablero)))))

; Revisa si el tablero tiene al menos una casilla vacia.
; Entrada: un tablero.
; Salida: #t si hay vacios y #f si no hay vacios.
(define (HayVacios? tablero)
  (cond
    ((null? (BuscarVaciosTablero tablero)) #f)
  (else
    #t)))

; Revisa si todavia existe algun movimiento posible.
; Entrada: un tablero.
; Salida: #t si todavia se puede mover y #f si ya no se puede mover.
(define (HayMovimientosPosibles? tablero)
  (cond
    ((HayVacios? tablero) #t)
    ((TableroTieneCombinacionHorizontal? tablero) #t)
    ((TableroTieneCombinacionHorizontal? (Transponer tablero)) #t)
  (else
    #f)))

; Revisa si el jugador ya perdio.
; Entrada: un tablero.
; Salida: #t si ya perdio y #f si no ha perdido.
(define (Perdio? tablero)
  (cond
    ((HayMovimientosPosibles? tablero) #f)
  (else
    #t)))

; =============================
; Resultado de la simulacion
; =============================

; Crea el resultado de una simulacion de movimiento.
; Entrada: un tablero, un valor de ganado y un valor de perdido.
; Salida: una lista con el tablero, si gano y si perdio.
(define (CrearResultadoMovimiento tablero gano perdio)
  (cons tablero
        (cons gano
              (cons perdio null))))

; Obtiene el tablero del resultado de una simulacion.
; Entrada: un resultado de movimiento.
; Salida: el tablero resultante.
(define (ObtenerTableroResultado resultado)
  (car resultado))

; Obtiene si ya gano del resultado de una simulacion.
; Entrada: un resultado de movimiento.
; Salida: #t si ya llego a 2048 y #f si no.
(define (ObtenerGanoResultado resultado)
  (car (cdr resultado)))

; Obtiene si ya perdio del resultado de una simulacion.
; Entrada: un resultado de movimiento.
; Salida: #t si ya perdio y #f si no.
(define (ObtenerPerdioResultado resultado)
  (car (cdr (cdr resultado))))

; Construye el resultado final de un tablero ya actualizado.
; Entrada: un tablero.
; Salida: una lista con el tablero, si gano y si perdio.
(define (ConstruirEstadoJuego tablero)
  (CrearResultadoMovimiento
   tablero
   (LlegoA2048? tablero)
   (Perdio? tablero)))

; ================================
; Funciones para usar desde la GUI
; ================================

; Estas funciones son las que la GUI deberia llamar directamente:
; CrearTablero
; CrearTableroInicial
; MovimientoValido?
; SimularMovimiento
; ObtenerTableroResultado
; ObtenerGanoResultado
; ObtenerPerdioResultado

; Aplica un movimiento y devuelve el tablero resultante.
; Entrada: un tablero y una direccion.
; Salida: el tablero despues del movimiento.
(define (AplicarMovimiento tablero direccion)
  (MoverTablero tablero direccion))

; Aplica un movimiento y solo inserta una ficha nueva si el tablero cambio.
; Entrada: un tablero y una direccion.
; Salida: un nuevo tablero con el movimiento aplicado y, si hubo cambio, una ficha nueva.
(define (AplicarMovimientoConFicha tablero direccion)
  (cond
    ((MovimientoValido? tablero direccion)
     (InsertarFichaNueva (AplicarMovimiento tablero direccion)))
  (else
    tablero)))

; Simula un movimiento, inserta ficha si el tablero cambia y revisa victoria y derrota.
; Entrada: un tablero y una direccion.
; Salida: una lista con el tablero resultante, si gano y si perdio.
(define (SimularMovimiento tablero direccion)
  (ConstruirEstadoJuego
   (AplicarMovimientoConFicha tablero direccion)))
