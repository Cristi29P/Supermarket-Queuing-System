#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index closed tt et queue) #:transparent)

(define (empty-counter index)           
  (make-counter index #f 0 0 empty-queue))

(define (update f counters index)
  (map (lambda (x) (if (= (counter-index x) index)
                       (f x)
                       x
                       )) counters))

(define tt+
  (lambda (x)
    (lambda (y) (match y [(counter index closed tt et t) (struct-copy counter y [tt (+ tt x)])])
      )))

(define et+
  (lambda (x)
    (lambda (y) (match y [(counter index closed tt et t) (struct-copy counter y [et (+ et x)])])
      )))

(define (add-to-counter name items)     
  (λ (C)                                
    (match C
      [(counter index closed tt et queue)
       (struct-copy counter C
                    [tt (+ tt items)]
                    [et (if (queue-empty? queue) (+ et items) et)]
                    [queue (enqueue (cons name items) queue)]
                    )])))

(define (driver f counters)
  (foldl f (first counters) (rest counters)))

(define (min-tt counters)
  (match (driver (lambda (x y) (if (< (counter-tt x) (counter-tt y)) x y)) counters)
    [(counter index _ tt _ _) (cons index tt)]))

(define (min-et counters)
  (match (driver (lambda (x y) (if (< (counter-et x) (counter-et y)) x y)) counters)
    [(counter index _ _ et _) (cons index et)]))

(define (remove-first-from-counter C)
  (match C
    [(counter index closed tt et queue)
     (struct-copy counter C
                  [tt (if (= (queue-size queue) 1) 0 (- tt et))]
                  [queue (if (= (queue-size queue) 1) empty-queue (dequeue queue))]
                  [et (if (= (queue-size queue) 1) 0 (cdr (top (dequeue queue))))])]))

(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
      [(counter index closed tt et queue)
       (struct-copy counter C
                    [tt (if (> tt minutes) (- tt minutes) 0)]
                    [et (if (> et minutes) (- et minutes) 0)]
                    )])))

(define (clients? counters)
  (ormap (lambda (C)
           (not (null? (counter-queue C))))
         counters))

(define (average-tt counters)
  (/ (foldl (lambda (x acc) (+ (counter-tt x) acc)) 0 counters) (length counters)))

(define (create-new-counters fast slow len ttmed)
  (if (<= (average-tt (filter (λ(x) (false? (counter-closed x))) (append fast slow))) ttmed)
      slow
      (create-new-counters fast (append slow (list (empty-counter (add1 len)))) (add1 len) ttmed)))

(define (update-all f counters)
  (map (lambda (x) (f x)) counters))

(define (take-order counters)
  (foldr (lambda (x acc) (if (= (counter-et x) 0)
                             (if (queue-empty? (counter-queue x))
                                 acc
                                 (cons (cons (counter-index x) (car (top (counter-queue x)))) acc) )
                             acc
                             )) '() counters))

(define (remove-from counters)
  (foldr (lambda (x acc) (if (= (counter-et x) 0)
                             (if (queue-empty? (counter-queue x))
                                 (cons x acc)
                                 (cons (remove-first-from-counter x) acc))
                             (cons x acc))) '() counters))

(define (passing-minutes minutes orders fast slow)
  (if (= minutes 0)
      (append (list orders) (list fast) (list slow))
      (let* ((fast1 (update-all (pass-time-through-counter 1) fast))
             (slow1 (update-all (pass-time-through-counter 1) slow))
             (orders (append orders (take-order (append fast1 slow1))))
             (fast2 (remove-from fast1))
             (slow2 (remove-from slow1)))
        (passing-minutes (sub1 minutes) orders fast2 slow2))))


(define (close-counter counters index)
  (map (lambda (x) (if (= (counter-index x) index)
                       (match x
                         [(counter _ closed _ _ _)
                          (struct-copy counter x [closed #t])])
                       x
                       )) counters))


(define (final-printer counters)
  (foldr (lambda (x acc) (if (queue-empty? (counter-queue x))
                             acc
                             (cons (cons (counter-index x) (counter-queue x)) acc))) '() counters))



(define (helper order requests fast-counters slow-counters)
 (if (null? requests)
      (append (list order) (final-printer (append fast-counters slow-counters)))
      (match (car requests)
        [(list 'delay index minutes) (helper order (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                                             (update (et+ minutes) (update (tt+ minutes) slow-counters index) index))]
        [(list 'ensure average) (helper order (cdr requests) fast-counters (create-new-counters fast-counters slow-counters
                                                                                                (length (append fast-counters slow-counters)) average))]
        [(list 'close index) (helper order (cdr requests) (close-counter fast-counters index) (close-counter slow-counters index))]
        [(list name n-items) (if (<= n-items ITEMS)
                                 (helper order (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (filter (λ(x) (false? (counter-closed x))) (append fast-counters slow-counters)))))
                                         (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (λ(x) (false? (counter-closed x))) (append fast-counters slow-counters))))))                                 
                                 (helper order (cdr requests) fast-counters
                                         (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (λ(x) (false? (counter-closed x))) slow-counters)))))
                                 )]
        [minute (let* ((result (passing-minutes minute order fast-counters slow-counters))
                       (order (car result))
                       (faster (cadr result))
                       (slower (last result)))
                  (helper order (cdr requests) faster slower))])))
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă, nu poate primi clienti noi                (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve requests fast-counters slow-counters)
 (helper '() requests fast-counters slow-counters))