;;1.9
   (define removeall
    (lambda (s los)
      (if (null? los)
        '()
        (if (eqv? (car los) s)
          (removeall s (cdr los))
          (cons (car los) (removeall s (cdr los)))))))

;;1.12
   (define substinline
    (lambda (new old slist)
      (if (null? slist)
        '()
        (cons   
          (if (symbol? (car slist)) 
            (if (eqv? (car slist) old) new (car slist))
            (substinline new old (car slist)))
          (substinline new old (cdr slist))))))
          
(equal?? (substinline 'a 'b '((b c) (b () d))) '((a c) (a () d)))          

;;1.13
   (define substmap
     (lambda (new old slist)
       (map (lambda (sexp) (subst-in-s-expmap new old sexp)) ;; map includes the '() case
            slist)))
   
   
   (define subst-in-s-expmap
     (lambda (new old sexp)
       (if (symbol? sexp)
         (if (eqv? sexp old) new sexp)
         (substmap new old sexp))))
         
   (equal?? (substmap 'a 'b '((b c) (b () d))) '((a c) (a () d))) 
   
;;1.15  
;(duple n x) returns a list containing n copies of x.
;>(duple 2 3)
;(3 3)
; > (duple 4 '(ha ha))
; ((ha ha) (ha ha) (ha ha) (ha ha))
; > (duple 0 '(blah))
; '()

;duple: int × Sym → Lst(Sym)
; usage: takes an argument item and returns a list of containing n copies of item.
(define duple
     (lambda (n x)
       (if (eqv? n 0)
           '()
           (cons x (duple (- n 1) x)))))
> (duple 2 3)

;;1.18

   (define substswap
     (lambda (s1 s2 slist)
       (map (lambda (sexp) (subst-in-s-expswap s1 s2 sexp))
            slist)))
   
   
   (define subst-in-s-expswap
     (lambda (s1 s2 sexp)
       (if (symbol? sexp)
         (cond ((eqv? sexp s2) s1)
               ((eqv? sexp s1) s2)
               (else sexp))
         (substswap s1 s2 sexp))))
         
   (equal?? (substswap 'a 'd '((d c) (b () d))) '((a c) (a () d))) 
   
