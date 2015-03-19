#lang racket
(require graph)
(require racket/set)

; the first line in path is either "directed" or "undirected"
; the rest is a series of edges:
; 1 2
; 4 5
; ...

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
                               (cons (list (car vertices)
                                           (car (cdr vertices)))
                                     (make-pairs (cddr vertices)))]))])
                      (make-pairs vertices))])
    (if (equal? type "directed")
        (unweighted-graph/directed edge-list)
        (unweighted-graph/undirected edge-list))))

(define (unnest l)
  (if (null? l)
      '()
      (append (car l) (unnest (cdr l)))))


(define (graph-complement/undirected g)
  (let*
      ([vertices (get-vertices g)]
       [graph-min (argmin identity vertices)]
       [graph-max (argmax identity vertices)]
       [complete-list (for/list ([i (in-range graph-min (+ graph-max 1))])
                          (for/list ([j (in-range i (+ graph-max 1))])
                            (list i j)))])
    (unweighted-graph/undirected
     (filter (lambda (pair) (not (= (car pair) (car (cdr pair)))))
             (set->list (set-subtract (list->set (unnest complete-list))
                                      (list->set (get-edges g))))))))




; (define graph-file (vector-ref (current-command-line-arguments) 0))
(define graph-file "/home/mson/Documents/cse/graphs/gc_4_1")
(define g (read-graph graph-file))
(define g_c (graph-complement/undirected g))


(graphviz g)
(graphviz g_c)
; (displayln (get-vertices g))
; (displayln (get-edges g))
