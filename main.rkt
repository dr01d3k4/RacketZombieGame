#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs

;; vector2 : int x int -> vector2
(struct vector2 (x y) #:transparent)

;; entity : vector2 -> entity
(struct entity (position) #:transparent)

;; board : int x int x (list (list cell-type)) -> board
(struct board (width height cells) #:transparent)

;; game : board x entity x entity x (list entity) x float x int x (list entity) -> game
(struct game (board player goal enemies enemy-move-chance ammo ammo-drops) #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

;; Cell types
(define CELL-FLOOR 0)
(define CELL-WALL 1)

;; Cell chars
(define FLOOR-CHAR " ")
(define WALL-CHAR "#")
(define PLAYER-CHAR "P")
(define GOAL-CHAR "G")
(define ENEMY-CHAR "Z")
(define PLAYER-GOAL-CHAR "!")
(define PLAYER-DEAD-CHAR "X")
(define AMMO-DROP-CHAR "A")

;; (width, height, wall-count, enemy-count, enemy-move-chance, starting-ammo, ammo-drops)
(define DIFFICULTY-CONFIG '((12 12 10  5 0.2 2 10)
                            (16 16 20 10 0.4 2 6)
                            (20 20 30 15 0.6 0 3)
                            (30 30 40 20 0.8 0 1)
                            (150 30 200 100 0.9 0 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; main : void -> void
(define (main [show-intro #t])
  (begin
    (if show-intro (intro) '())
    (define difficulty (ask-for-difficulty))
    (newline)
    (start-game-loop difficulty)
    (flush-output)
    (newline)
    (displayln "Play again? (y/n)")
    (define play-again (prompt))
    (if (eq? play-again 'y)
        (main #f)
        '())))


;; intro : void -> void
(define (intro)
  (begin
    (displayln "Welcome to the game!")
    (newline)
    (displayln "Get accross the world avoiding the zombies!")
    (displayln "P = you, G = goal, Z = zombie, # = wall")
    '()))


;; ask-for-difficulty : void -> int
(define (ask-for-difficulty)
  (newline)
  (display "Enter difficulty (1-")
  (display (length DIFFICULTY-CONFIG))
  (displayln ")")
  (displayln "(Higher difficulty means a bigger world, but with faster zombies)")
  (define difficulty (prompt))
  (if (number? difficulty)
      (if (and (>= difficulty 1) (<= difficulty (length DIFFICULTY-CONFIG)))
          (- difficulty 1)
          (begin
            (displayln "Number out of range")
            (ask-for-difficulty)))
      (begin
        (displayln "That's not a number")
        (ask-for-difficulty))))


;; start-game-loop : int -> void
(define (start-game-loop difficulty)
  (game-loop (create-game-for-difficulty difficulty) 1))


;; create-game-for-difficulty : int -> game
(define (create-game-for-difficulty difficulty)
  (let ([config (list-ref DIFFICULTY-CONFIG difficulty)])
    (create-game config)))


;; game-loop : game x int -> void
(define (game-loop current-game move-number)
  (begin
    (newline)
    (display "Move #")
    (displayln move-number)
    (display "Ammo: ")
    (displayln (game-ammo current-game))
    (newline)
    (print-game (game->string current-game))
    (newline)
    (define move (get-move))
    (cond [(eq? move 'quit) current-game]
          [else (begin
                  (define updated-game (run-game-step current-game move))
                  (cond [(eq? updated-game 'blocked-move) (blocked-move current-game move-number)]
                        [(game-over? updated-game) (display-game-over updated-game)]
                        [else (game-loop updated-game (+ move-number 1))]))])))


;; blocked-move : game x int -> game
(define (blocked-move current-game move-number)
  (begin
    (newline)
    (displayln "There is a wall in your way")
    (game-loop current-game move-number)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting move

;; get-move : void -> move
(define (get-move)
  (begin
    (displayln "Enter move (wasd to move, q to quit)")
    (define move (eval-player-move (prompt)))
    (cond [(eq? move 'unknown) (move-not-recognised)]
          [(eq? move 'quit) (confirm-quit)]
          [else (display-move-info move)])))


;; move-not-recognised : void -> move
(define (move-not-recognised)
  (begin
    (displayln "Move not recognised. Try again.")
    (newline)
    (get-move)))


;; confirm-quit : void -> move
(define (confirm-quit)
  (begin
    (displayln "Are you sure? (y/n)")
    (define input (prompt))
    (cond [(eq? input 'y) 'quit]
          [else (begin (newline) (get-move))])))


;; display-move-info move -> move
(define (display-move-info move)
  (begin
    (display "Moving ")
    (displayln move)
    move))


;; eval-player-move : symbol -> move
(define (eval-player-move move)
  (cond [(eq? move 'w) 'up]
        [(eq? move 's) 'down]
        [(eq? move 'a) 'left]
        [(eq? move 'd) 'right]
        [(eq? move 'q) 'quit]
        [else 'unknown]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game logic

;; run-game-step : game x move -> game
(define (run-game-step current-game move)
  (let ([new-player (attempt-move-entity (game-board current-game) (game-player current-game) (move->direction move))]
        [enemy-move-chance (game-enemy-move-chance current-game)])
    (if (entity-on-entity? new-player (game-player current-game))
        'blocked-move
        (let* ([new-enemies (map (curryr move-enemy-if-random
                                         enemy-move-chance
                                         new-player
                                         (game-board current-game)
                                         (game-enemies current-game))
                                 (game-enemies current-game))]
               [player-colliding-zombie (any-entity-on-entity? new-enemies new-player)]
               [player-colliding-ammo-drop (any-entity-on-entity? (game-ammo-drops current-game) new-player)]
               [has-ammo (> (game-ammo current-game) 0)]
               
               [ammo-after-zombie (if (and player-colliding-zombie has-ammo)
                                      (- (game-ammo current-game) 1)
                                      (game-ammo current-game))]
               [ammo (if player-colliding-ammo-drop
                         (+ ammo-after-zombie 1)
                         ammo-after-zombie)]
               [enemies-dead-removed (if (and player-colliding-zombie has-ammo)
                                         (remove-colliding-entity new-enemies new-player)
                                         new-enemies)]
               [ammo-drops (if player-colliding-ammo-drop
                               (remove-colliding-entity (game-ammo-drops current-game) new-player)
                               (game-ammo-drops current-game))])
          (game (game-board current-game)
                new-player
                (game-goal current-game)
                enemies-dead-removed
                enemy-move-chance
                ammo
                ammo-drops)))))


;; remove-colliding-entity : (list entity) x entity -> (list entity)
(define (remove-colliding-entity entities ent)
  (cond [(empty? entities) '()]
        [(entity-on-entity? (car entities) ent) (remove-colliding-entity (cdr entities) ent)]
        [else (cons (car entities) (remove-colliding-entity (cdr entities) ent))]))


;; move->direction : move -> vector2
(define (move->direction move)
  (cond [(eq? move 'up) (vector2 0 -1)]
        [(eq? move 'down) (vector2 0 1)]
        [(eq? move 'left) (vector2 -1 0)]
        [(eq? move 'right) (vector2 1 0)]))


;; move-enemy-if-random : entity x entity x board -> entity
(define (move-enemy-if-random enemy chance player b enemies)
  (if (< (random) chance)
      (move-enemy-towards-player enemy player b enemies)
      enemy))


;; move-enemy-towards-player : entity x entity x board -> entity
(define (move-enemy-towards-player enemy player b enemies)
  (let* ([diff (vector2- (entity-position player) (entity-position enemy))]
         [dx (vector2-x diff)]
         [dy (vector2-y diff)]
         [vec (cond [(and (= dx 0) (= dy 0)) (vector2 0 0)]
                    [(> (abs dx) (abs dy)) (vector2 (/ dx (abs dx)) 0)]
                    [else (vector2 0 (/ dy (abs dy)))])])
    (if (any-entity-on-point? enemies (entity-position (move-entity enemy vec)))
        enemy
        (attempt-move-entity b enemy vec))))


;; is-cell-solid? : board x vector2 -> bool
(define (is-cell-solid? b point)
  (let* ([width (board-width b)]
         [height (board-height b)]
         [cells (board-cells b)]
         [x (vector2-x point)]
         [y (vector2-y point)]
         [in-bounds (is-point-in-bounds? width height point)])
    (if in-bounds
        (= (list-ref (list-ref cells y) x) CELL-WALL)
        #t)))


;; is-point-in-bounds? : int x int x vector2 -> bool
(define (is-point-in-bounds? width height point)
  (let ([x (vector2-x point)]
        [y (vector2-y point)])
    (and (>= x 0)
         (>= y 0)
         (< x width)
         (< y height))))


;; attempt-move-entity : entity x vector2 -> entity
(define (attempt-move-entity b ent vec)
  (let ([new-ent (move-entity ent vec)])
    (if (is-cell-solid? b (entity-position new-ent))
        ent
        new-ent)))


;; move-entity : entity x vector2 -> vector2
(define (move-entity ent vec)
  (entity (vector2+ (entity-position ent) vec)))


;; game-over? : game -> bool
(define (game-over? current-game)
  (or (player-on-goal? current-game)
      (player-dead? current-game)))


;; player-on-goal? : game -> bool
(define (player-on-goal? current-game)
  (entity-on-entity? (game-goal current-game) (game-player current-game)))


;; player-dead : game -> bool
(define (player-dead? current-game)
  (any-entity-on-entity? (game-enemies current-game) (game-player current-game)))


;; entity-on-point? : entity x vector2 -> bool
(define (entity-on-point? ent point)
  (vector2-equal? (entity-position ent) point))


;; any-entity-on-point : (list entity) x vector2 -> bool
(define (any-entity-on-point? ents point)
  (ormap (curryr entity-on-point? point) ents))


;; entity-on-entity? : entity x entity -> bool
(define (entity-on-entity? e1 e2)
  (entity-on-point? e1 (entity-position e2)))


;; any-entity-on-entity? : (list entity) x entity -> bool
(define (any-entity-on-entity? ents e)
  (any-entity-on-point? ents (entity-position e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render

;; game-over-text : game -> string
(define (game-over-text current-game)
  (cond [(player-on-goal? current-game) "You win!"]
        [(player-dead? current-game) "You were eaten!"]))


;; display-game-over : game -> game
(define (display-game-over current-game)
  (begin
    (newline)
    (print-game (game->string current-game))
    (newline)
    (displayln (game-over-text current-game))
    current-game))


;; cell-type->char : cell-type -> char
(define (cell-type->char cell-type)
  (cond [(eq? cell-type CELL-FLOOR) FLOOR-CHAR]
        [(eq? cell-type CELL-WALL) WALL-CHAR]))


;; cell->char : cell-type x vector2 x (vector2 -> bool) x (vector2 -> bool) -> (vector2 -> bool) -> (vector2 -> bool) -> char
(define (cell->char cell-type cell-loc is-player? is-goal? is-enemy? is-ammo?)
  (cond [(is-player? cell-loc) (cond [(is-goal? cell-loc) PLAYER-GOAL-CHAR]
                                     [(is-enemy? cell-loc) PLAYER-DEAD-CHAR]
                                     [else PLAYER-CHAR])]
        [(is-goal? cell-loc) GOAL-CHAR]
        [(is-enemy? cell-loc) ENEMY-CHAR]
        [(is-ammo? cell-loc) AMMO-DROP-CHAR]
        [else (cell-type->char cell-type)]))


;; game->string : game -> string
(define (game->string g)
  (let* ([board (game-board g)]
         [width (board-width board)]
         [height (board-height board)]
         [cells (board-cells board)]
         [player (game-player g)]
         [goal (game-goal g)]
         [enemies (game-enemies g)]
         [ammo-drops (game-ammo-drops g)]
         [is-player? (curry entity-on-point? player)]
         [is-goal? (curry entity-on-point? goal)]
         [is-enemy? (curry any-entity-on-point? enemies)]
         [is-ammo? (curry any-entity-on-point? ammo-drops)])
    (for/list ([y height])
      (string-join
       (for/list ([x width])
         (cell->char (list-ref (list-ref cells y) x)
                     (vector2 x y)
                     is-player?
                     is-goal?
                     is-enemy?
                     is-ammo?)) ""))))


;; print-game : string -> void
(define (print-game game-string)
  (map displayln game-string)
  '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board Generation

;; make-empty-board : int x int -> board
(define (make-empty-board width height)
  (board width height
         (for/list ([y height])
           (for/list ([x width])
             (cond [(= x 0) CELL-WALL]
                   [(= x (- width 1)) CELL-WALL]
                   [(= y 0) CELL-WALL]
                   [(= y (- height 1)) CELL-WALL]
                   [else CELL-FLOOR])))))


;; path-from-x-to-y? : board x point x point -> bool
(define (path-from-x-to-y? b x y)
  (eq? #t (get-in-matrix (build-accessibility-matrix b y) x)))


;; add-random-wall-to-board : board x point x point -> board
(define (add-random-wall-to-board max-tries b p g)
  (if (<= max-tries 0)
      b
      (let ([x (+ (random (- (board-width b) 2)) 1)]
            [y (+ (random (- (board-height b) 2)) 1)])
        (if (is-cell-solid? b (vector2 x y))
            (add-random-wall-to-board (- max-tries 1) b p g)
            (let ([new-board (board (board-width b) (board-height b) (change-matrix-element (board-cells b) (vector2 x y) CELL-WALL))])
              (if (path-from-x-to-y? new-board p g)
                  new-board
                  (add-random-wall-to-board (- max-tries 1) b p g)))))))


;; add-multiple-random-walls-to-board : int x board x point x point -> board
(define (add-multiple-random-walls-to-board wall-count b p g)
  (for/fold ([bd b])
    ([i (in-range wall-count)])
    (add-random-wall-to-board 100 bd p g)))


;; calculate-random-enemy-position : int x board x entity x entity -> point
(define (calculate-random-enemy-position max-tries b p g)
  (if (<= max-tries 0)
      '()
      (let ([x (+ (random (- (board-width b) 2)) 1)]
            [y (+ (random (- (board-height b) 2)) 1)])
        (if (or (is-cell-solid? b (vector2 x y))
                (vector2-equal? (vector2 x y) p)
                (vector2-equal? (vector2 x y) g))
            (calculate-random-enemy-position (- max-tries 1) b p g)
            (vector2 x y)))))


;; generate-list-of-enemies : int x board x point x point -> (list entity)
(define (generate-list-of-enemies enemy-count b p g)
  (for/fold ([enemies '()])
    ([i (in-range enemy-count)])
    (let* ([pos (calculate-random-enemy-position 100 b p g)]
           [ent (entity pos)])
      (if (member ent enemies)
          enemies
          (cons ent enemies)))))


;; calculate-random-ammo-position : int x board x (list entity) x point x point -> point
(define (calculate-random-ammo-position max-tries b e p g)
  (if (<= max-tries 0)
      '()
      (let ([x (+ (random (- (board-width b) 2)) 1)]
            [y (+ (random (- (board-height b) 2)) 1)])
        (if (or (is-cell-solid? b (vector2 x y))
                (vector2-equal? (vector2 x y) p)
                (vector2-equal? (vector2 x y) g)
                (any-entity-on-point? e (vector2 x y)))
            (calculate-random-ammo-position (- max-tries 1) b e p g)
            (vector2 x y)))))


;; generate-ammo-drops : int x board x (list entity) x point x point -> (list entity)
(define (generate-ammo-drops ammo-count b e p g)
  (for/fold ([ammo-drops '()])
    ([i (in-range ammo-count)])
    (let* ([pos (calculate-random-ammo-position 100 b e p g)]
           [ammo (entity pos)])
      (if (member ammo ammo-drops)
          ammo-drops
          (cons ammo ammo-drops)))))


;; create-game : difficulty-config -> game
(define (create-game config)
  (let* ([w (first config)]
         [h (second config)]
         [wall-count (third config)]
         [enemy-count (fourth config)]
         [enemy-move-chance (fifth config)]
         [starting-ammo (sixth config)]
         [ammo-drop-count (seventh config)]
         [p (vector2 1 1)]
         [g (vector2 (- w 2) (- h 2))]
         [b (add-multiple-random-walls-to-board wall-count (make-empty-board w h) p g)]
         [e (generate-list-of-enemies enemy-count b p g)]
         [a (generate-ammo-drops ammo-drop-count b e p g)])
    (game b (entity p) (entity g) e enemy-move-chance starting-ammo a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board Accessibility + Flood Fill

;; build-accessibility-matrix : board x vector2 -> matrix
(define (build-accessibility-matrix b start-point)
  (if (is-point-in-bounds? (board-width b) (board-height b) start-point)
      (let ([matrix (map (lambda r
                           (map (lambda c
                                  (if (= (car c) CELL-WALL) #f 0))
                                (car r)))
                         (board-cells b))])
        (flood-fill b matrix start-point))
      '()))


;; get-in-matrix : matrix x point -> value
(define (get-in-matrix matrix point)
  (list-ref (list-ref matrix (vector2-y point)) (vector2-x point)))


;; in-board-bounds : board x point -> bool
(define (in-board-bounds? b point)
  (is-point-in-bounds? (board-width b) (board-height b) point))


;; is-visited : matrix x point -> bool
(define (is-visited? matrix point)
  (not (eq? 0 (get-in-matrix matrix point))))


;; change-list-element : list x int x value -> list
(define (change-list-element lst index value)
  (append (take lst index) (cons value (list-tail lst (+ index 1)))))


;; change-matrix-element : matrix x point x value -> matrix
(define (change-matrix-element matrix point value)
  (let ([x (vector2-x point)]
        [y (vector2-y point)])
    (change-list-element matrix y (change-list-element (list-ref matrix y) x value))))


;; flood-onto : board x matrix x point -> matrix
(define (flood-onto b matrix point)
  (if (is-visited? matrix point)
      matrix
      (flood-fill b matrix point)))


;; flood-fill : board x matrix x vector2 -> matrix
(define (flood-fill b matrix point)
  (if (and (in-board-bounds? b point)
           (not (is-visited? matrix point)))
      (for/fold ([m (change-matrix-element matrix point #t)])
        ([p (list (vector2 -1 0) (vector2 1 0) (vector2 0 -1) (vector2 0 1))])
        (flood-onto b m (vector2+ point p)))
      matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util

;; vector2-equal : vector2 x vector2 -> bool
(define (vector2-equal? v1 v2)
  (and (= (vector2-x v1) (vector2-x v2))
       (= (vector2-y v1) (vector2-y v2))))


;; vector2+ : vector2 x vector2 -> vector2
(define (vector2+ v1 v2)
  (vector2 (+ (vector2-x v1) (vector2-x v2))
           (+ (vector2-y v1) (vector2-y v2))))


;; vector2- : vector2 x vector2 -> vector2
(define (vector2- v1 v2)
  (vector2 (- (vector2-x v1) (vector2-x v2))
           (- (vector2-y v1) (vector2-y v2))))


;; prompt : void -> symbol
(define (prompt)
  (begin
    (display "> ")
    (flush-output)
    (read)))

(main)