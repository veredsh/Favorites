;;;Signature: cse(lst)
;;;Purpose: largest common sub-expressions elimination.
;;;Type: LIST(symbol)->LIST(symbol)
;; add gen gym
;; add sort in dict 

(define get-apps
  (lambda(lst)
    (letrec ((get-apps$ (lambda(lst dict cont)
                          (letrec ((const? (lambda(exp)
                                             (and (list? exp) (equal? (car exp) 'quote)))))
                            (cond ((null? lst) (cont dict))
                                  ((and (list? (car lst)) (not (const? (car lst)))) 
                                   (insert-to-dict$ (car lst)
                                                    dict
                                                    (lambda(new-dict)
                                                      (get-apps$ (car lst) 
                                                                 new-dict
                                                                 (lambda(res-car)
                                                                   (get-apps$ (cdr lst) res-car cont))))))
                                  (else (get-apps$ (cdr lst) dict cont)))))))
      (get-apps$ lst (list (list (gensym) lst 1)) (lambda(x)x)))))

  
(define sub-exp?
  (lambda(son faja)
    (letrec ((sub-exp$ (lambda(son faja cont)
                         (cond ((null? faja) (cont #f))
                               ((list? (car faja))(if (equal? son (car faja))
                                                      (cont #t)
                                                      (sub-exp$ son 
                                                                (car faja) 
                                                                (lambda(res-car)                                                                             (if res-car 
                                                                   (cont #t)
                                                                   (sub-exp$ son 
                                                                             (cdr faja)
                                                                             cont))))))
                               (else (sub-exp$ son (cdr faja) cont))))))
    (sub-exp$ son faja (lambda(x)x)))))

;;assume son is sub expression of faja
;;get only the expressions of faja
(define replace-sub-exp$
  (lambda(son faja succ)
    (cond ((null? faja) (succ (list)))
          ((list? (car faja)) (if (equal? (car faja) (cadr son))
                                  (replace-sub-exp$ son 
                                                    (cdr faja)
                                                    (lambda(res-cdr)
                                                      (succ (cons (car son) res-cdr))))
                                  (replace-sub-exp$ son 
                                                    (car faja) 
                                                    (lambda(res-car)
                                                      (replace-sub-exp$ son
                                                                        (cdr faja)
                                                                        (lambda(res-cdr)
                                                                          (succ (cons res-car
                                                                                      res-cdr))))))))
                                  (else (replace-sub-exp$ son 
                                                          (cdr faja) 
                                                          (lambda(res-cdr)
                                                            (succ (cons (car faja) res-cdr))))))))

(define insert-to-dict$
  (lambda(element dict cont)
    (cond ((null? dict) (cont (cons (list (gensym) element 1) (list)))) ;;element not in dict
          ((equal? (cadar dict) element) ;;already in dict 
           (cont (append (cdr dict) (list (list (caar dict) element (+ (caddar dict) 1))))))
          (else (insert-to-dict$ element (cdr dict) (lambda(res-cdr)(cont (cons (car dict)
                                                                                res-cdr))))))))

(define gcse
  (lambda(dict)
    (letrec ((gcse-helper$ 
              (lambda(son fajas success fail)
                (cond ((null? fajas) (success (list)))
                      ((sub-exp? (cadr son) (cadar fajas))
                       (let ((faja (car fajas)))
                        (if (eq? (caddr son) (caddr faja)) ;equal counters not gcse
                            (fail) ;not grater common sub expression
                            (replace-sub-exp$ son
                                              (cadr faja)
                                              (lambda(new-faja)
                                                (gcse-helper$ son 
                                                              (cdr fajas)
                                                              (lambda(res-cdr)
                                                                (success (cons (list (car faja) ;;same gensym 
                                                                                  new-faja
                                                                                  (caddr faja)) ;;same counter
                                                                            res-cdr)))
                                                              fail))))))
                      (else (gcse-helper$ son 
                                          (cdr fajas) 
                                          (lambda(res-cdr)
                                            (success (cons (car fajas) res-cdr)))
                                          fail)))))
             (find-gcses$ 
              (lambda(dict cont)
                (cond ((null? dict) (cont (list)))
                      (else (gcse-helper$ (car dict) 
                                          (cdr dict) 
                                          (lambda(res-help)
                                            (find-gcses$ res-help 
                                                        (lambda(res-find) 
                                                          (cont (cons (car dict) res-find)))))
                                          (lambda()
                                            (find-gcses$ (cdr dict) cont))))))))
      (find-gcses$ (reverse dict) (lambda(x)x)))))


(define cse
  (lambda(exp)
    (letrec ((make-let* (lambda(dict body)
                          `(let* (,@(append (map (lambda(dic)(list (car dic) (cadr dic))) dict))) ,body)))
             (remove-last (lambda(lst)
                            (cond ((null? (cdr lst)) (list))
                                  (else (cons (car lst) (remove-last (cdr lst)))))))
             (find-in-dict (lambda(exp dict)
                             (cond ((null? dict) (list)) ;;not in dict
                                   ((equal? exp (cadar dict)) (caar dict)) 
                                   (else (find-in-dict exp (cdr dict)))))))
      (let* ((dict (get-apps exp)) ;;make dictionary out of applications
             (first-in-dict (car dict)) ;;get the original expression from dict
             (cses (filter (lambda(element)(not (eq? (caddr element) 1))) dict))) 
        ;;cse is unneeded if all applications appear only once
        
        (if (null? cses)
            exp ;;no common sub-expression to eliminate
            (let* ((new-dict (gcse (cons first-in-dict cses))) ;;leave only grater common sub-expressions in dict
                   (final-dict (remove-last new-dict)) 
                   ;;remove last in dict (the original expression after replacing symbols
                   (body (cadr (list-ref new-dict (- (length new-dict) 1))))) 
              ;;the original expression after replacing symbols
              (make-let* final-dict body)))))))


(get-apps '(+ (* (- x y) (* x x))
(* x x)
(foo (- x y))
(goo (* (- x y) (* x x)))))
                                         