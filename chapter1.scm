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
   
