#lang racket

; Este módulo define la lógica del juego 2048, incluyendo las funciones para manipular el estado del tablero, calcular movimientos, y determinar condiciones de victoria o derrota.
(provide
 *side*
 *tile-that-wins*
 nonzero?
 pad-right
 combine
 combine-total
 slide-left
 slide-right
 moves-row-left
 reverse-moves
 transpose-moves
 moves-row-right
 add-row-coord
 transpose
 left
 right
 up
 down
 score-increment
 moves-grid-action
 moves-grid-left
 moves-grid-right
 moves-grid-up
 moves-grid-down
 chop
 count-zeros
 index-of-nth-zero
 replace-nth-zero
 new-tile
 initial-state
 initial-state-mxn
 finished?
 won-game?)

; Definimos constantes para el tamaño del tablero y el valor del tile que representa la victoria
(define *side* 4)
(define *tile-that-wins* 2048)

; La función nonzero? devuelve #t si el número no es cero, y #f si es cero, lo que es útil para filtrar los tiles vacíos del tablero
(define (nonzero? x) (not (zero? x)))

; La función pad-right toma una lista, un valor de padding y un número n, y devuelve una nueva lista que es la original seguida de suficientes elementos de padding para alcanzar una longitud de n
(define (pad-right lst padding n)
  (append lst (make-list (- n (length lst)) padding)))

; La función combine toma una lista de números y combina los elementos adyacentes iguales, sumándolos y dejando un espacio vacío (cero) después de cada combinación
(define (combine lst)
  (cond [(<= (length lst) 1) lst]
        [(= (first lst) (second lst))
         (cons (* 2 (first lst)) (combine (drop lst 2)))]
        [else (cons (first lst) (combine (rest lst)))]))

; La función combine-total es similar a combine, pero en lugar de devolver la lista combinada, devuelve la suma total de los valores combinados, lo que se utiliza para calcular el puntaje incrementado después de un movimiento
(define (combine-total lst)
  (cond [(<= (length lst) 1) 0]
        [(= (first lst) (second lst))
         (+ (* 2 (first lst)) (combine-total (drop lst 2)))]
        [else (combine-total (rest lst))]))

; La función slide-left toma una fila del tablero y la desliza hacia la izquierda, combinando los tiles iguales y llenando los espacios vacíos con ceros a la derecha
(define (slide-left row)
  (pad-right (combine (filter nonzero? row)) 0 (length row)))

; La función slide-right hace lo mismo pero desliza hacia la derecha, lo que se logra invirtiendo la fila, aplicando slide-left, y luego invirtiendo el resultado nuevamente
(define (slide-right row)
  (reverse (slide-left (reverse row))))

; La función moves-row-left toma una fila y devuelve una lista de movimientos posibles al deslizar hacia la izquierda, incluyendo las coordenadas de los tiles que se moverían o combinarían
(define (moves-row-left row [last #f] [i 0] [j -1])
  (if (null? row)
      null
      (let ([head (first row)])
        (cond [(zero? head) (moves-row-left (rest row) last (add1 i) j)]
              [(equal? last head)
               (cons (list head i j)
                     (moves-row-left (rest row) #f (add1 i) j))]
              [else (cons (list head i (add1 j))
                          (moves-row-left (rest row) head (add1 i) (add1 j)))]))))

; La función reverse-moves toma una lista de movimientos y un número n, y devuelve una nueva lista de movimientos con las coordenadas invertidas, lo que es útil para calcular los movimientos al deslizar hacia la derecha o hacia abajo
(define (reverse-moves moves n)
  (define (flip i) (- n i 1))
  (map (lambda (m)
         (match-define (list a b c) m)
         (list a (flip b) (flip c)))
       moves))

; La función transpose-moves toma una lista de movimientos y devuelve una nueva lista de movimientos con las coordenadas transpuestas, lo que es útil para calcular los movimientos al deslizar hacia arriba o hacia abajo
(define (transpose-moves moves)
  (for/list ([m moves])
    (match-define (list v (list a b) (list c d)) m)
    (list v (list b a) (list d c))))

; La función moves-row-right calcula los movimientos al deslizar hacia la derecha, lo que se logra invirtiendo la fila, aplicando moves-row-left, y luego invirtiendo las coordenadas de los movimientos resultantes
(define (moves-row-right row [n (length row)])
  (reverse-moves (moves-row-left (reverse row)) n))

; La función add-row-coord toma un índice de fila i y una lista de movimientos, y devuelve una nueva lista de movimientos con las coordenadas de fila actualizadas para cada movimiento, lo que es útil para calcular los movimientos en el contexto del tablero completo
(define (add-row-coord i rows)
  (for/list ([r rows])
    (match-define (list a b c) r)
    (list a (list i b) (list i c))))

; La función transpose toma una lista de listas (una matriz) y devuelve su transpuesta, lo que es útil para calcular los movimientos al deslizar hacia arriba o hacia abajo aplicando las mismas funciones que para izquierda y derecha
(define (transpose lsts)
  (apply map list lsts))

; Las funciones left, right, up y down aplican las funciones de deslizamiento correspondientes a cada fila o columna del tablero para calcular el nuevo estado del tablero después de un movimiento en esa dirección
(define (left grid)
  (map slide-left grid))

; La función right hace lo mismo pero para deslizar hacia la derecha
(define (right grid)
  (map slide-right grid))

; La función up calcula el nuevo estado del tablero al deslizar hacia arriba, lo que se logra transponiendo el tablero, aplicando left, y luego transponiendo el resultado nuevamente
(define (up grid)
  ((compose transpose left transpose) grid))

; La función down hace lo mismo pero para deslizar hacia abajo, aplicando right en lugar de left
(define (down grid)
  ((compose transpose right transpose) grid))

; La función score-increment calcula el incremento de puntaje después de un movimiento, sumando los valores combinados en cada fila del tablero utilizando combine-total para cada fila y luego sumando esos valores para obtener el total del incremento de puntaje
(define (score-increment grid)
  (apply + (map (lambda (row)
                  (combine-total (filter nonzero? row)))
                grid)))

; La función moves-grid-action toma un tablero y una función de movimiento (como moves-row-left) y devuelve una lista de todos los movimientos posibles en el tablero para esa acción, incluyendo las coordenadas de los tiles que se moverían o combinarían
(define (moves-grid-action grid action)
  (apply append
         (for/list ([row grid]
                    [i (in-naturals)])
           (add-row-coord i (action row)))))

; Las funciones moves-grid-left, moves-grid-right, moves-grid-up y moves-grid-down calculan los movimientos posibles para cada dirección de deslizamiento utilizando moves-grid-action con la función de movimiento correspondiente
(define (moves-grid-left grid)
  (moves-grid-action grid moves-row-left))

; La función moves-grid-right calcula los movimientos al deslizar hacia la derecha utilizando moves-row-right
(define (moves-grid-right grid)
  (moves-grid-action grid moves-row-right))

; La función moves-grid-up calcula los movimientos al deslizar hacia arriba utilizando moves-row-left y luego transponiendo las coordenadas de los movimientos resultantes
(define (moves-grid-up grid)
  ((compose transpose-moves moves-grid-left transpose) grid))

; La función moves-grid-down calcula los movimientos al deslizar hacia abajo utilizando moves-row-right y luego transponiendo las coordenadas de los movimientos resultantes
(define (moves-grid-down grid)
  ((compose transpose-moves moves-grid-right transpose) grid))

; La función chop toma una lista y un número n, y devuelve una lista de listas, donde cada sublista tiene a lo sumo n elementos, lo que es útil para convertir la representación lineal del tablero en una matriz para facilitar las operaciones de deslizamiento
(define (chop lst [n *side*])
  (if (<= (length lst) n)
      (list lst)
      (cons (take lst n) (chop (drop lst n) n))))

; La función count-zeros cuenta el número de ceros en el estado del tablero, lo que es útil para determinar cuántos espacios vacíos hay disponibles para colocar nuevos tiles después de cada movimiento
(define (count-zeros state)
  (length (filter zero? state)))

; La función index-of-nth-zero toma una lista y un número n, y devuelve el índice de la n-ésima aparición de cero en la lista, o #f si no hay suficientes ceros, lo que es útil para determinar dónde colocar un nuevo tile después de un movimiento
(define (index-of-nth-zero lst n)
  (cond [(null? lst) #f]
        [(zero? (first lst))
         (if (zero? n)
             0
             (add1 (index-of-nth-zero (rest lst) (sub1 n))))]
        [else (add1 (index-of-nth-zero (rest lst) n))]))

; La función replace-nth-zero toma una lista, un número n y un valor val, y devuelve una nueva lista donde la n-ésima aparición de cero ha sido reemplazada por val, lo que es útil para actualizar el estado del tablero después de colocar un nuevo tile
(define (replace-nth-zero lst n val)
  (let ([i (index-of-nth-zero lst n)])
    (append (take lst i) (cons val (drop lst (add1 i))))))

; La función new-tile genera un nuevo tile para colocar en el tablero después de cada movimiento, con una probabilidad del 90% de ser un 2 y del 10% de ser un 4, lo que es consistente con las reglas del juego original
(define (new-tile)
  (if (> (random) 0.9) 4 2))

(define (initial-state [side *side*])
  (shuffle (append (list (new-tile) (new-tile))
                   (make-list (- (sqr side) 2) 0))))

; La función initial-state-mxn genera un estado inicial para un tablero de tamaño MxN, asegurándose de que los valores de M y N estén entre 4 y 10, y colocando dos tiles nuevos en el tablero con el resto de los espacios vacíos llenos de ceros
(define (initial-state-mxn rows cols)
  (unless (and (integer? rows)
               (integer? cols)
               (<= 4 rows 10)
               (<= 4 cols 10))
    (error 'initial-state-mxn "rows y cols deben estar entre 4 y 10"))
  (shuffle (append '(2 2)
                   (make-list (- (* rows cols) 2) 0))))

; La función finished? toma el estado del tablero y determina si el juego ha terminado, lo que ocurre cuando no hay movimientos posibles en ninguna dirección, lo que se verifica comparando el estado actual del tablero con el resultado de aplicar cada función de movimiento (left, right, up, down) y asegurándose de que no cambie
(define (finished? state [n *side*])
  (let ([grid (chop state n)])
    (for/and ([op (list left right up down)])
      (equal? grid (op grid)))))

; La función won-game? toma el estado del tablero y determina si el juego ha ganado, lo que ocurre cuando el valor máximo en el tablero es igual al tile que representa la victoria, lo que se verifica utilizando apply max para encontrar el valor máximo en la lista de tiles y comparándolo con *tile-that-wins*
(define (won-game? state)
  (= (apply max state) *tile-that-wins*))
