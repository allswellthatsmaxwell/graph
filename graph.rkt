#lang racket
(require graph)
(require racket/set)
(require errortrace)

(struct vertex (id [color #:auto #:mutable])
  #:auto-value "white"
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (equal?-recur (vertex-id a) (vertex-id b)))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (hash-recur (vertex-id a)))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (hash2-recur (expt (vertex-id a) (vertex-id a))))])

; (: read-graph (-> String Graph))
; the first line in path is either "directed" or something else
; anything but "directed" is taken to mean the graph is undirected.
; the rest is a series of edges:
; 1 2
; 4 5
; ...
; returns a graph made up of the edges (and nodes, implicitly) in path.
(define (read-graph path)
  (let* ([info (file->list path)]
         [type (car info)]
         [vertices (cddr info)]
         [edge-list (letrec
                        ([make-pairs
                          (lambda (vertices)
                            (cond
                              [(null? vertices) '()]
                              [else
                               (cons (list (vertex (car vertices))
                                           (vertex (car (cdr vertices))))
                                     (make-pairs (cddr vertices)))]))])
                      (make-pairs vertices))])
    (if (equal? type "directed")
        (unweighted-graph/directed edge-list)
        (unweighted-graph/undirected edge-list))))

; (: unnest (-> List List))
(define (unnest l)
  (if (null? l)
      '()
      (append (car l) (unnest (cdr l)))))

; (: get-complete-graph (-> Integer Graph))
(define (get-complete-graph n)
  (unweighted-graph/undirected
   (unnest (for/list ([i (in-range 0 n)])
             (for/list ([j (in-range i n)])
               (list (vertex i) (vertex j)))))))

; (: graph-complement/undirected (-> Graph Graph))
(define (graph-complement/undirected g)
  (let*
      ([vertices (get-vertices g)]
       [complete-graph (get-complete-graph (- (length (get-vertices g)) 1))]
       [complement (remove-self-loops (subtract-graph complete-graph g))])
    ; the above process disappears fully connected nodes in g. Put them
    ; into complement:
    (for-each
     (lambda (v) (not (has-vertex? complement v)) (add-vertex! complement v))
     (get-vertices g))
    complement))

; (: remove-self-loops (-> Graph Graph))
(define (remove-self-loops g)
  (unweighted-graph/undirected
   (filter (lambda (e) (not (equal? (car e) (car (cdr e)))))
           (get-edges g))))

; (: make-random-graph/undirected (-> Integer Real Graph))
; returns the edge set of a random undirected graph with n nodes,
; with probability p that there is an edge between two given nodes.
(define (make-random-graph/undirected n p)
  (letrec ([remove-edges
            (lambda (edges)
              (cond [(null? edges) '()]
                    [(> (random) (- 1 p))
                     (append (list (car edges)) (remove-edges (cdr edges)))]
                    [else (remove-edges (cdr edges))]))])
    (remove-self-loops
     (unweighted-graph/undirected
      (remove-edges (get-edges (get-complete-graph n)))))))

; (: subtract-graph (-> Graph Graph Graph))
(define (subtract-graph g h)
  (unweighted-graph/undirected
   (set->list (set-subtract (list->set (get-edges g))
                            (list->set (get-edges h))))))


; (: fill-to-range (-> Graph Integer))
; if node i for 0 <= i <= n is not in g, adds node i to g.
(define (fill-to-range g n)
  (for-each (lambda (i) (not (has-vertex? g (vertex i))) (add-vertex! g (vertex i)))
            (sequence->list (in-range 0 n)))
  g)

; (: graph->edge-id-list (-> Graph List))
(define (graph->edge-id-list g)
  (map (lambda (e) (list (vertex-id (car e)) (vertex-id (car (cdr e)))))
       (get-edges g)))

; (: uniqify-upto-edge-reversal (-> List List))
(define (uniqify-upto-edge-reversal edges)
  (letrec
      ([get-uniq-edges
        (lambda (edge-set)
          (cond [(set-empty? edge-set) edge-set]
                [else (let ([e (set-first edge-set)])
                        (cond [(and (set-member? edge-set e)
                                    (set-member? edge-set (reverse e)))
                               (get-uniq-edges (set-remove edge-set e))]
                              [else (set-add (get-uniq-edges (set-remove edge-set e)) e)]))]))])
    (set->list (get-uniq-edges (list->set edges)))))

; (: print-graphviz/undirected (-> Graph None))
(define (print-graphviz/undirected g)
  (displayln "graph g {")
    (for-each
     (lambda (e)
       (display (vertex-id (car e)))
       (display " -- ")
       (display (vertex-id (car (cdr e))))
       (displayln ";"))
     (uniqify-upto-edge-reversal (get-edges g)))
    (for-each (lambda (v)
                (display (vertex-id v))
                (display " [style=filled] [fillcolor=")
                (display (vertex-color v))
                (displayln "];"))
              (get-vertices g))
    (displayln "}"))

(define g
  (let ([first-arg (vector-ref (current-command-line-arguments) 0)]
        [second-arg (with-handlers ([exn:fail? (lambda (exn) '())])
                      (vector-ref (current-command-line-arguments) 1))])
    (if (null? second-arg)
        (read-graph first-arg) ; one arg => first-arg is path to a graph file
                               ; two args => first-arg = n, second-arg = p
        (let* ([n (string->number first-arg)]
               [p (string->number second-arg)]
               [graph (make-random-graph/undirected n p)])
          (fill-to-range graph n)))))

(define color-list (file->list "colors.txt"))

(define (graph-set-colors g colors)
  (for-each (lambda (v)
              (let ([color (hash-ref colors v)])
                (set-vertex-color! v (list-ref color-list color))))
            (hash-keys colors))
  g)

(define g_c (graph-complement/undirected g))
(print-graphviz/undirected (graph-set-colors g (coloring g (length color-list))))
(print-graphviz/undirected (graph-set-colors g_c (coloring g_c (length color-list))))









;(displayln (graph->edge-id-list (get-complete-graph 3)))
;(displayln (make-random-edges/undirected 5 .5))
;(graphviz g)
;(displayln (graph->edge-id-list g))

;  (for-each (lambda (e)
;              (display (vertex-id (car e)))
;              (display " ")
;              (displayln (vertex-id (car (cdr e)))))
            ;          (
;(displayln (uniqify-upto-edge-reversal (get-edges g)))
; (graphviz g_c)
; (get-vertices (graph-set-colors g (coloring g 4)))
