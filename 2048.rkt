#lang racket

;; 2048 en Racket
(require 2htdp/image
         racket/string
         (rename-in 2htdp/universe
                    [left left-arrow]
                    [right right-arrow]
                    [up up-arrow]
                    [down down-arrow])
         "Logica.rkt")

(define *time-limit* #f)
(define *amber-alert* 60)
(define *red-alert* 10)
(define *magnification* 2)

;; Colores y gráficos
(define *text*
  '((0 "")
    (2 "2")))

(define *grid-color* (color #xbb #xad #xa0))
(define *default-tile-bg-color* (color #x3c #x3a #x32))
(define *default-tile-fg-color* 'white)

(define *tile-bg-colors*
  (map (lambda (x)
         (match-define (list n r g b) x)
         (list n (color r g b)))
       '((0 #xcc #xc0 #xb3)
         (2 #xee #xe4 #xda)
         (4 #xed #xe0 #xc8)
         (8 #xf2 #xb1 #x79)
         (16 #xf5 #x95 #x63)
         (32 #xf6 #x7c #x5f)
         (64 #xf6 #x5e #x3b)
         (128 #xed #xcf #x72)
         (256 #xed #xcc #x61)
         (512 #xed #xc8 #x50)
         (1024 #xed #xc5 #x3f)
         (2048 #xed #xc2 #x2e))))

(define *tile-fg-colors*
  '((0 dimgray)
    (2 dimgray)
    (4 dimgray)
    (8 white)
    (16 white)
    (32 white)
    (64 white)
    (128 white)
    (256 white)
    (512 white)
    (1024 white)
    (2048 white)))

(define *text-size* 30)
(define *max-text-width* 40)
(define *tile-side* 50)
(define *grid-spacing* 5)

(define-syntax define-memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       (let ([results (make-hash)])
         (lambda (args ...)
           ((lambda vals
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              (hash-ref results vals))
            args ...))))]))

; Funciones para gráficos y animaciones
; La función grid-width calcula el ancho total del tablero dado el número de columnas
(define (grid-width cols)
  (+ (* cols *tile-side*)
    (* (add1 cols) *grid-spacing*)))

; La función grid-height es igual a grid-width pero con rows en lugar de cols
(define (grid-height rows)
  (+ (* rows *tile-side*)
    (* (add1 rows) *grid-spacing*)))

; La función square/ij obtiene el valor de la casilla en la posición (i, j) del estado del juego
(define (square/ij state cols i j)
  (list-ref state (+ (* cols i) j)))

; La función combine-total suma los valores de las casillas combinadas en un movimiento
(define (interpolate k a b)
  (+ (* (- 1 k) a)
     (* k b)))

; La función count-zeros cuenta el número de ceros en una lista, lo que es útil para determinar dónde colocar un nuevo tile
(define (lookup key lst default)
  (let ([value (assoc key lst)])
    (if value (second value) default)))

; La función make-tile crea una imagen de un tile con el número dado
(define (plain-tile n)
  (square *tile-side*
          'solid
          (lookup n *tile-bg-colors* *default-tile-bg-color*)))

; La función tile-text crea una imagen de texto para el número en el tile
(define (tile-text n)
  (let* ([t (text (lookup n *text* (number->string n))
                  *text-size*
                  (lookup n *tile-fg-colors* *default-tile-fg-color*))]
         [side (max (image-width t) (image-height t))])
    (scale (if (> side *max-text-width*) (/ *max-text-width* side) 1) t)))

; La función make-tile combina el fondo del tile con el texto para crear la imagen final del tile
(define-memoized (make-tile n)
  (overlay
   (tile-text n)
   (plain-tile n)))

; La función place-tile/ij coloca un tile en la posición (i, j) sobre la imagen del tablero
(define (place-tile/ij tile i j grid-image)
  (define (pos k)
    (+ (* (add1 k) *grid-spacing*)
       (* k *tile-side*)))
  (underlay/xy grid-image (pos j) (pos i) tile))

; La función state->image convierte el estado del juego en una imagen del tablero con los tiles correspondientes
(define (state->image state rows cols)
  (for*/fold ([im (rectangle (grid-width cols) (grid-height rows) 'solid *grid-color*)])
    ([i (in-range rows)]
     [j (in-range cols)])
    (place-tile/ij (make-tile (square/ij state cols i j))
                   i j
                   im)))

; Funciones para la lógica del juego, como movimientos, combinaciones, generación de nuevos tiles
(define (empty-grid-image rows cols)
  (state->image (make-list (* rows cols) 0) rows cols))

; La función moves->frame toma una lista de movimientos y genera una imagen del tablero para cada paso de la animación
(define (moves->frame moves rows cols k)
  (for*/fold ([grid (empty-grid-image rows cols)])
    ([m moves])
    (match-define (list value (list i1 j1) (list i2 j2)) m)
    (place-tile/ij (make-tile value)
                   (interpolate k i1 i2) (interpolate k j1 j2)
                   grid)))

; La función animate-moving-tiles genera una lista de funciones que crean las imágenes para la animación de los tiles moviéndose
(define (animate-moving-tiles state rows cols op)
  (let ([grid (chop state cols)])
    (build-list 9 (lambda (i)
                    (lambda ()
                      (moves->frame (op grid) rows cols
                                    (* 0.1 (add1 i))))))))

; La función moves-grid-action aplica una función de movimiento a cada fila del grid y luego combina los resultados en una sola lista
(define (animate-appearing-tile state rows cols value index)
  (let ([start (state->image state rows cols)]
        [tile (make-tile value)]
        [i (quotient index cols)]
        [j (remainder index cols)])
    (build-list 4 (lambda (m)
                    (lambda ()
                      (place-tile/ij (overlay
                                      (scale (* 0.2 (add1 m)) tile)
                                      (plain-tile 0))
                                     i j
                                     start))))))

(define-struct world (state score winning-total rows cols frames start-time) #:transparent)

; Funciones para manejar la lógica del juego, como verificar si el juego ha terminado, calcular el puntaje, manejar las entradas del usuario
(define (out-of-time? start-time)
  (and *time-limit* (< (+ start-time *time-limit*) (current-seconds))))

; La función finished? verifica si el juego ha terminado, lo que ocurre cuando no hay movimientos posibles
(define (game-over? w)
  (match-define (world state score wt rows cols frames start-time) w)
  (and (null? frames)
       (or (finished? state cols)
           (and wt (equal? (apply + (flatten state)) wt))
           (out-of-time? (world-start-time w)))))

; La función won-game? verifica si el jugador ha ganado
(define (key->ops a-key)
  (cond
    [(key=? a-key "left") (list left moves-grid-left)]
    [(key=? a-key "right") (list right moves-grid-right)]
    [(key=? a-key "up") (list up moves-grid-up)]
    [(key=? a-key "down") (list down moves-grid-down)]
    [else (list #f #f)]))

; La función change maneja la entrada del usuario, aplicando el movimiento correspondiente y actualizando el estado del juego
(define (change w a-key)
  (match-let ([(list op moves-op) (key->ops a-key)]
              [(world st score wt rows cols frames start-time) w])
    (cond [(out-of-time? start-time) w]
          [op
           (let* ([grid (chop st cols)]
                  [slide-state (flatten (op grid))])
             (if (equal? slide-state st)
                 w
                 (let* ([replace (random (count-zeros slide-state))]
                        [index (index-of-nth-zero slide-state replace)]
                        [value (new-tile)]
                        [new-state (replace-nth-zero slide-state replace value)]
                        [horizontal? (member a-key (list "left" "right"))])
                   (make-world new-state
                       (+ score (score-increment
                             (if horizontal? grid (transpose grid))))
                       (cond [wt wt]
                         [(won-game? new-state)
                          (apply + (flatten new-state))]
                         [else #f])
                       rows
                       cols
                       (append frames
                           (animate-moving-tiles st rows cols moves-op)
                           (animate-appearing-tile slide-state rows cols value index))
                       start-time))))]
          [else w])))

; Funciones para manejar la animación
(define (banner txt state rows cols [color 'black])
  (let ([b-text (text txt 30 color)])
    (overlay
     b-text
     (rectangle (* 1.2 (image-width b-text))
                (* 1.4 (image-height b-text))
                'solid 'white)
     (state->image state rows cols))))

; La función number->time-string convierte un número de segundos en una cadena de texto con formato de tiempo
(define (number->time-string s)
  (define hrs (quotient s 3600))
  (define mins (quotient (remainder s 3600) 60))
  (define secs (remainder s 60))
  (define (xx n)
    (cond [(<= n 0) "00"]
          [(<= n 9) (format "0~a" n)]
          [else (remainder n 60)]))
  (if (>= s 3600)
      (format "~a:~a:~a" hrs (xx mins) (xx secs))
      (format "~a:~a" mins (xx secs))))

; La función time-remaining calcula el tiempo restante dado un tiempo de inicio y el límite de tiempo
(define (time-remaining start)
  (+ *time-limit* start (- (current-seconds))))

; La función time-elapsed calcula el tiempo transcurrido desde un tiempo de inicio
(define (time-elapsed start)
  (- (current-seconds) start))

; La función show-world dibuja el estado actual del juego, incluyendo el tablero, el puntaje y el tiempo
(define (show-world w)
  (match-define (world state score wt rows cols frames start-time) w)
  (let* ([board (if (null? frames)
        (cond [(finished? state cols) (banner "Fin del juego :(" state rows cols)]
          [(out-of-time? start-time) (banner "Out of Time" state rows cols 'red)]
          [(equal? (apply + (flatten state)) wt) (banner "Ganaste! :D" state rows cols)]
          [else (state->image state rows cols)])
                    ((first frames)))]
         [score-text (text (format "Score: ~a" score) 16 'dimgray)]
         [seconds ((if *time-limit* time-remaining time-elapsed) start-time)]
         [time-text (text (format "Time: ~a" (number->time-string seconds))
                          16
                          (cond [(or (> seconds *amber-alert*) (not *time-limit*)) 'gray]
                                [(> seconds *red-alert*) 'orange]
                                [else 'red]))])
    (scale *magnification*
           (above
            board
            (rectangle 0 5 'solid 'white)
            (beside
             score-text
             (rectangle (- (image-width board)
                           (image-width score-text)
                           (image-width time-text)) 0 'solid 'white)
             time-text)))))

; La función advance-frame actualiza el estado del juego para la siguiente imagen en la animación, eliminando el primer frame de la lista de frames
(define (advance-frame w)
  (match-define (world state score wt rows cols frames start-time) w)
  (if (null? frames)
      w
      (make-world state score wt rows cols (rest frames) start-time)))

; La función parse-size analiza la entrada del usuario para determinar el tamaño del tablero, aceptando formatos como "4x4" o "8"
(define (parse-size input)
  (define trimmed (string-trim input))
  (cond
    [(regexp-match #px"^([0-9]+)\\s*[xX]\\s*([0-9]+)$" trimmed)
     => (lambda (match)
          (let ([rows (string->number (second match))]
                [cols (string->number (third match))])
            (and rows cols (list rows cols))))]
    [(regexp-match #px"^[0-9]+$" trimmed)
     => (lambda (match)
          (let ([n (string->number (first match))])
            (and n (list n n))))]
    [else #f]))

; La función make-winning-state genera un estado inicial para la demo de victoria, con el tile que gana ya presente en el tablero
(define (make-winning-state rows cols)
  (append (list *tile-that-wins*)
          (make-list (- (* rows cols) 1) 0)))

; La función prompt-demo-mode pregunta al usuario si desea mostrar la demo de victoria, aceptando respuestas como "s", "si", "y", "yes" para sí, y "n", "no", "" para no
(define (prompt-demo-mode)
  (let loop ()
    (display "Mostrar demo de victoria? (s/n): ")
    (flush-output)
    (define answer (string-downcase (string-trim (or (read-line) ""))))
    (cond [(member answer '("s" "si" "y" "yes")) #t]
          [(member answer '("n" "no" "")) #f]
          [else
           (begin
             (displayln "Respuesta invalida. Escriba s o n.")
             (loop))])))

; La función prompt-size pregunta al usuario por el tamaño del tablero, asegurándose de que la entrada sea válida y esté en el formato correcto
(define (prompt-size)
  (let loop ()
    (display "Ingrese tamano MxN (4x4 a 10x10, ej 8x10): ")
    (flush-output)
    (define parsed (parse-size (read-line)))
    (cond
      [(and parsed (list? parsed) (= (length parsed) 2))
       (let ([rows (first parsed)]
             [cols (second parsed)])
         (if (and (<= 4 rows 10)
                  (<= 4 cols 10))
             (values rows cols)
             (begin
               (displayln "Entrada invalida. Use formato MxN con valores entre 4 y 10.")
               (loop))))]
      [else
       (begin
         (displayln "Entrada invalida. Use formato MxN con valores entre 4 y 10.")
         (loop))])))

; La función start es el punto de entrada del programa, que configura el estado inicial del juego y comienza la animación con big-bang
(define (start)
  (define-values (rows cols) (prompt-size))
  (define demo? (prompt-demo-mode))
  (define initial-state (if demo?
                            (make-winning-state rows cols)
                            (initial-state-mxn rows cols)))
  (define initial-winning-total (and demo? *tile-that-wins*))
  (big-bang (make-world initial-state
                        0 initial-winning-total rows cols null (current-seconds))
            (to-draw show-world)
            (on-key change)
            (on-tick advance-frame 0.01)
            (stop-when game-over? show-world)
            (name "2048 - Racket")))

(start)
