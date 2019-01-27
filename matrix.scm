
   ;;some functions on matrices

 

  (define (evaluate expr)
        (eval expr user-initial-environment))

 (define (eval-bool-ls lsb)
      (evaluate (cons 'and lsb)))

 (define (list-of-nr? ls) 
      (eval-bool-ls 
           (map number? ls))) 

 ;;compares the length of the each line in a matrix
 ;; with the length of the first line
 (define (is-matrix? ls)
    (if (not (null? ls))
        (let ((len (length (car ls)))   
              (res-l (cdr ls))
              (b-ls (map list-of-nr? ls )))
        (and (eval-bool-ls b-ls)   
            (fold-left    (lambda (a b) (and a (=  len (length b) )))   #t res-l )))
        #f))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;identity matrix of n size

 (define (identity-matrix n_size)
 
  (define matrix '())

  (define ind 0)
 
   (define (add-line)
      (let* ((vec (make-vector n_size 0))
             (nl (vector-set! vec ind 1))
             (__ (set! ind (1+ ind))))
          (set! matrix (append matrix  (list vec)))))
          
   (define (loop fn  times)
         (if (= times 0)
             matrix
             (begin 
                 (fn)
                 (loop fn (- times 1)))))
  (map vector->list
    (loop add-line n_size )
     )
   )  

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (zip-with l1 l2 op)
       (if (null? l1)
           '()
            (cons
             (op (car l1) (car l2))
             (zip-with (cdr l1) (cdr l2) op))))

 ;;fundamental operations on matrices
 ;; interchange any two rows
 ;;fast swap -- dependent on the i2
 (define (swap i1 i2 matrix)
      (define (check-ind)
       (let ((ml (- (length matrix) 1)))
             (or (> i1 ml) (> i2 ml))
              ))
      (define t1 '())
      (define t2  '())
      (define (find-var i1 i2 matrix)
        (cond  
            ((= i1 0) 
             (begin (set! t1 (car matrix))
                    (find-var (- i1 1) (- i2 1) (cdr matrix))))
            ((= i2 0) 
             (begin (set! t2 (car matrix))))
            (else (find-var (- i1 1) (- i2 1) (cdr matrix)))))
     (if (check-ind)
         (error "incorrect indeces")
         (if (> i1 i2)
              (swap i2 i1 matrix )            
              (begin (find-var i1 i2 matrix)
                     (list-set! matrix i1 t2)
                     (list-set! matrix i2 t1)))))

  (define (interchange-rows i1 i2 matrix)
           (if (is-matrix? matrix)
               (swap i1 i2 matrix)
               (error "not a matrix")))
  
 ;;multiply row by scalar
 (define (*row ind matrix scalar)
     (list-set! matrix ind
               (map (lambda (e) (* e scalar))
                    (list-ref matrix ind)))))

 ;;add row by another row multiplied by a scalar
    (define (+adr i1 i2 scalar matrix)
           (let* ((r1 (list-ref matrix i1))
                  (r1m (map (lambda (e) (* e scalar)) r1))
                  (r2  (list-ref matrix i2)))
               (list-set! matrix i2 (zip-with r1m r2 +)))) 

;; interchange two columns
 (define (interchange-columns i1 i2 matrix)
      (for-each (lambda (r) (swap i1 i2 r)) matrix))   

;;multiply column by scalar  
 (define (*col-s matrix ind scalar)
     (for-each (lambda (r) 
                (list-set! r ind
                  (* scalar (list-ref r ind))))
               matrix ))  
;;add columns by another column multiplied by a scalar

 (define (+adc i1 i2 matrix scalar)
    (define (add r)
       (list-set! r i2 (+ (* scalar (list-ref r i1)) (list-ref r i2))))
   (for-each (lambda (row) (add row)) matrix))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;check if row-echalon form

   (define (zero-or-null row)
                   (cond ((null? row) #t)
                         ((zero? (car row))
                          (and #t (zero-or-null (cdr row))))
                         (else #f)))
                                  

 (define (count-rows matrix p-ind ind)
    (let ((z (car p-ind))
          (nz (cdr p-ind))
          (l  (- (length matrix) 1))
          (ap (lambda (ls e) (append ls (list e))))
          (next (1+ ind)))
     (if  (null? matrix)
          p-ind
          (cond ((zero-or-null (car matrix))
                 (count-rows (cdr matrix) (cons (ap z ind) nz) next ))
                (else 
                 (count-rows (cdr matrix) (cons z (ap nz ind)) next ))))))
 ;;defining error messages
  (define (first-condition-failed)
       (error ""))
  (define (second-condition-failed)
       (error ""))
  (define (third-condition-failed)
       (error ""))
  (define (forth-condition-failed)
       (error ""))
  (define (check-conditions matrix)
        (let* ((p (count-rows matrix (cons '() '()) 0))
               (z (car p))
               (nz (cdr p))
               (le (last nz)))
      
     (define (first-condition)
          (if (null? z)
              #t
              (> (car z) le)))
      
      
      (define (get-rows ls-ind)
                   (if (null? ls-ind)
                       '()
                       (cons (list-ref matrix (car ls-ind))
                             (get-rows (cdr ls-ind)))))
     (define matr  (get-rows nz))  

       (define (find-unity-ind row i)
              (if (= (car row) 1)
                  i
                  (find-unity-ind (cdr row) (1+ i))))
     
      (define (second-condition)
            (define (check-if-unit ls)
                  (cond ((null? ls) #t)
                        ((= (car ls) 0)
                         (check-if-unit (cdr ls)))
                        ((= (car ls) 1)
                         #t)
                        ((not (= (car ls) 1))
                         #f)))  
             (eval-bool-ls
                    (map check-if-unit 
                         matr)))

      (define (third-condition)
         (define (get-elems ci mt)
                  (if (null? mt)
                      '()
                      (cons (list-ref (car mt) ci)
                            (get-elems ci (cdr mt)))))
          ;;calculating column index                 
       
         (define (gather-all-sb mat)
               (let* ((ci '())
                      (m  mat))
                (map
                  (lambda (row)         
                       (begin (set! m (cdr m))
                              (set! ci (find-unity-ind row 0))
                              (get-elems ci m)))
                  mat)))
       
         (eval-bool-ls  
          (map zero-or-null (gather-all-sb matr))))

        (define (find-unity-ind2 row)
                (find-unity-ind row 0))
                  
       (define (forth-condition)
            (let ((inds (map find-unity-ind2 matr)))
               (define (in-order? ls)
                     (cond ((null? ls) #t)
                           ((= (length ls) 1) #t)
                           ((= (length ls) 2) (< (car ls) (cadr ls)))
                           (else
                  (and  
                    (< (car ls)
                       (apply min (cdr ls)))    
                    (in-order? (cdr ls))))))      
               (in-order? inds)))

      ;;checking all the conditions
      (if (not (first-condition))
         (first-condition-failed) 
         (if (not (second-condition))
             (second-condition-failed)
             (if (not (third-condition))
                 (third-condition-failed)
                 (if (not (forth-condition))
                     (forth-condition-failed)
                     (display "matrix is in row-form echelon!\n")
                   ))))                  
           ))


;;transform a matrix into row-echelon form


 (define (search-in-ls criterion ls ind) ;;until criterion is met
       (cond ((null? ls) -1)
             ((criterion (car ls)) ind)
             (else 
                (search-in-ls criterion (cdr ls) (1+ ind) ))))

 (define (search-nz  sri matrix)
      (if (= sri (length matrix))
          'done_nz
           (let* ((mat (drop matrix sri))
                  (temp-ci (search-in-ls (lambda (x) (not (= x 0))) (car mat)   0)))
              (if (not (= temp-ci -1))
                  (list sri temp-ci)
                  (search-nz (1+ sri) matrix)))))
               
    (define (col ind m)
          (map (lambda (row)
                  (list-ref row ind))
               m))
     
    ;;

    (define (row-form-ech r matrix)
    (if (= r  (length matrix) )
        'done
        (let* ( 
               (prc (search-nz r matrix))
               (ri (car prc))
               (ci (cadr prc))
               (cln (col ci matrix)))
           (begin (if (not (= ri r))
                  (interchange-rows r ri matrix))
                  (let* ((pivot (list-ref (list-ref matrix r) ci))
                         (cln2 (drop cln (1+ r)))) ;;? 
                      (if (not (= pivot 1))
                          (*row r matrix (/ 1 pivot)))
                      (define (reduce-to-zero column ind)
                          (cond ((null? column)
                                 (row-form-ech (1+ r) matrix))
                                ((zero? (car column))
                                 (reduce-to-zero (cdr column) (1+ ind)))
                                (else 
                                  (let* ((new-ri (+ r ind 1))
                                         (n (list-ref matrix new-ri))
                                         (v (list-ref n ci)))
                                      (begin (+adr r new-ri (* -1 v) matrix) 
                                             (reduce-to-zero (cdr column) (1+ ind))))))) 
                   (reduce-to-zero cln2 0))))))                     
                                             
 
  ;;searching for an element in a matrix
 (define (search elem mat)
   (define colnr (- (length (car mat)) 1))
   (define rownr (- (length mat) 1))
  
   (define row-ind 0)

   (define col-ind 0)
 
   (define search-results '())

   (define (copy-indeces vals)
          (set! search-results
                (append search-results 
                       (list vals))))

    (define (search-next e ri ci) ;;mutual recursive functions
           (if (= ri -1)                         
               search-results
               (if (and (= ri rownr) (= ci colnr))
                   (begin
                     (copy-indeces (cons ri ci))
                     (search-next e -1 -1))
                   (let* ((next-ci (if (<= ci (- colnr 1)) 
                                       (1+ ci)   
                                        0))
                          (next-ri (if (= ci colnr)
                                       (1+ ri)
                                       ri)))
                     (begin
                       (copy-indeces (cons ri ci))
                       (search-through-lines e next-ri next-ci))
                     ))))     ;; calls search-through-lines with next indeces
 
   ;;main function which performs the search in the matrix
  (define (search-through-lines e ri ci)
        (let* ((sl (list-ref mat ri))   
               (sc (list-ref sl ci)))
           (if (= e sc)
               (search-next e ri ci)   ;;continues the search if a match is found
               (if (not (= ci colnr))  ;;calls search-next
                    (begin
                        (search-through-lines e ri (1+ ci)))         
                    (if (not (= ri rownr))
                        (begin
                            (search-through-lines e (1+ ri) 0)
                                 )
                        (search-next  e -1 -1) )))))
           (search-through-lines elem row-ind col-ind)
      )    
    ;;memoization
 (define (memoized-search ls-elem matrix)

   
  (define (memo e)
    (let ((already-searched? #f)
          (result     #f))
        (lambda ()
          (if (not already-searched?)
              (let ((res (search e matrix)))
                 (begin (set! already-searched? #t)
                        (set! result res)
                        result))
              result))))
    (map (lambda (e) ((memo e)) ) ls-elem))

;;;;;;;;

;;multiplying matrix by a scalar
 (define (mult-m-s matrix scal)
  (if (is-matrix? matrix)
    (map (lambda (line)
            (map (lambda (e) (* scal e)) 
                 line))
         matrix)
    (display "not a matrix\n\n")))

   (define (sum-of-products l1 l2)
         (apply + (zip-with l1 l2 *))
      )

;; ading two matrices -- list represented
 (define (lines-addition l1 l2 )
             (zip-with  l1 l2 +))
             


 (define (add-matrices m1 m2)
     (if (addable m1 m2)
       (if (null? m1)
           '()
            (cons (lines-addition (car m1) (car m2))
                  (add-matrices (cdr m1) (cdr m2))))
       'matrices_not_addable
       ))

  (define (addable m1 m2)
     (if  (and (is-matrix? m1)
               (is-matrix? m2)
               (= (length m1)
                  (length m2)))))




;;multiplying two matrices
(define (multipliable? m1 m2)
     (= (length (car m1))
        (length m2 )))


(define (mult-matrices m1 m2)
     (if (null? m1)
         '()  
          (cons  (mult-line-by-colmns (car m1) m2)
                 (mult-matrices (cdr m1) m2))))

 (define (mult-line-by-colmns  ls mat)
      (if (null? (car mat))
          '()     
          (cons (line-by-column ls (map car mat))
                (mult-line-by-colmns ls (map cdr mat)))))

 
(define (line-by-column ln col)
     (sum-of-products ln col))   
               

(define (*m m1 m2)
 (if (not (and (is-matrix? m1) (is-matrix? m2))) 
     'error_not_matrices
     (if (not (multipliable? m1 m2))
         'matrices_are_not_multipliable
          (mult-matrices m1 m2))))


;;create a matrix as a list of vectors and fill it
;;matrix can be represented later as a list of lists
(define (getnumber)
    (letrec ((loop  (lambda ()
                         (let ((c (read-char)))
                               (cond ((char=? c #\newline)'())
                                     ((char-numeric? c ) (cons c (loop)))
                                     (else (loop)))))))
             (string->number (list->string (loop)))))

 (define (create-matrix col-nr ln-number)
        (let ((line (make-vector col-nr)))
           (make-lines line ln-number))) 

 (define (make-lines line ln-number)
      (if (= ln-number 0)
           '()
            (cons line (make-lines line (- ln-number 1)))))
         

 (define (fill-matrix mat)
     (let ((ln-len  (vector-length (car mat))))
      (define (vector-line-fill! vl cn)
        (if (< cn ln-len)
            (begin  (vector-set! vl cn (getnumber))              
                    (vector-line-fill! vl (1+ cn)))
            (display "line filled\n\n")))             
      (for-each (lambda (line)
                    (vector-line-fill! line 0))
                 mat)))
                    
 (define (lvs-to-lls matrix-v)
      (map vector->list matrix-v)) 


;;transpose  a matrix   -- represented as a list of lists
 (define (transpose matrix)
      (if (null? (car matrix))
          '()
           (cons (map car matrix)
                 (transpose (map cdr matrix)))))
                 

;;print-matrix
 (define (print-matrix m)
      (define (print-space)
        (begin (newline)
               (display (ascii->char 9))
               (display (ascii->char 9))))
              
      (define (print-line ln)
             (begin 
              (print-space)
              (for-each (lambda (e)
                      (begin
                         (display e)
                         (display "  "))) ln)                    
             ))
    (begin
     (for-each (lambda (line)
                    (print-line line))
               m)
          (newline) ))

 (define (print-matrices ls-mat)
       (for-each print-matrix ls-mat))

;;first diagonal of a matrix represented as a list of vectors

  (define (is-square-matrix? matrix)
      (=
          (length (car matrix))
          (length matrix )))


 (define (major-diag2 matrix counter fn )
    (let* ( (l1 (length (car matrix)))
            (l2 (length matrix))           ;;continuation-passing-style
            (dif (- l1 l2 ))               ;; counter is an index of retrieval for list-ref
            (len   (if (> dif 0)          
                       (-  l1   (1+ dif)) 
                       (- l1 1))))
       (if (> counter len)
           (fn (list (list-ref (car matrix) counter )))
           (major-diag2 (cdr matrix) (1+ counter)
                        (lambda (newl)
                           (fn
                             (cons   (list-ref (car matrix) counter) 
                                   newl))))))))

 (define (major-diagonal matrix)
             (major-diag2 matrix 0 (lambda (x) x))) 

;;;;;
 (define (minor-diagonal matrix)
     (let ((m 
   (call-with-current-continuation   
    (lambda (cont)         ;; breaking the fold using a continuation 
      (fold-left
        (lambda (ac line)
           (let*  ((i-ac (cdr ac)) 
                   (l-ac (append (car ac)
                    (if (< i-ac 0)
                        (cont (car ac))
                        (list
                         (list-ref line i-ac)))))
                   (new-ind (- i-ac 1)) )
                        (cons l-ac new-ind)))
                 (cons '() (- (length (car matrix)) 1) ) 
                 matrix)))))
        (if (list? (car m))
            (car m)
            m)))
         
;;matrices are equal?


    (define (equal-part? e1 e2 proc)
       (cond ((or (and  (null? e1) (not (null? e2)))
                    (and (not (null? e1)) (null? e2)))
                     #f)
                ((and (null? e1) (null? e2) ) #t)
                (else (and (proc (car e1) (car e2))
                           (equal-part? (cdr e1) (cdr e2) proc)))))       
    
  (define (equal-line? l1 l2)
         (equal-part? l1 l2 = ))

 (define (equal-mt? m1 m2)
       (if (and (is-matrix? m1) 
                (is-matrix? m2))
          (equal-part? m1 m2 equal-line?))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;determinant
  
   (define (mult-by-sign l starting-seq)
       (define (alter-seq start)
           (cons-stream start 
                        (alter-seq (* -1 start))))
                       ;;creates an infinite alternatig sequence
                       ;; 1 -1 1 -1 1 -1 1 -1 ......
       (define (mult-ls-by-seq l sq)
               (if (null? l)
                   '()
                   (let ((x (head sq))
                         (rest (tail sq)))
                      (cons (* (car l) x)
                            (mult-ls-by-seq (cdr l) rest)))))    
        (mult-ls-by-seq l (alter-seq starting-seq)) )

 ;;

 (define (is-n-mat? matrix o)
      (and (is-square-matrix? matrix)
           (=  (length  matrix) o)))
 
 (define (det-ord-2 matrix)
      (- (* (caar matrix) (cadar (cdr matrix)))
         (* (cadar matrix) (caadr matrix))))

 
 (define (delete-column matrix ind)
      (map (lambda (line)
                 (del-i line ind))
           matrix))
               
 (define (del-i l i)
           (if (> i (- (length l) 1))
               (error "index too big")
               (if (= i 0)
                   (cdr l)
                   (cons (car l) (del-i (cdr l) (- i 1))))))
       
   


 (define (all-minors matrix)
    (letrec ((cut-m (cdr matrix))
             (len (- (length (car matrix)) 1))
             (loop (lambda (s p m e)            ;;s - starting index of iteration, p - a procedure with two arguments
                      (if (> s e)               ;;m - matrix , e - end of the loop  compared with starting index
                          '()
                           (cons (p m s)      
                                 (loop (+ s 1) p m e))))))
      (map  (lambda (mat)    
                     (loop 0 delete-column mat len))  ;; List matrices -> List of matrix minors
            (loop 0  del-i matrix   (- (length matrix) 1)))))  ;; matrix -> List of matrices

     (define (first-line-minors matrix)
               (car (all-minors matrix)))

(define (determinant matrix)
    (cond ((is-n-mat? matrix 3) (det-ord-3 matrix))
          ((is-n-mat? matrix 2) (det-ord-2 matrix))
          (else   (let*  ((line  (mult-by-sign (car matrix) 1))
                          (det-list (map determinant 
                                       (first-line-minors matrix))))
                     (sum-of-products line det-list )))))
 


 ;;triangle rule for 3 X 3
  (define (det-ord-3 matrix )
      (- (+  (apply * (major-diagonal matrix))
             (* (caadr matrix)  (car (cdr (caddr matrix)))
                (caddar matrix) )
             (* (cadar matrix)   (caddr (cadr matrix))
                (caaddr matrix) ) )
         (+ (apply * (minor-diagonal matrix))
            (*  (caddr (cadr matrix))  (cadr (caddr matrix))
                (caar matrix) )
            (*  (cadar matrix) (caadr matrix)
                (caddr (caddr matrix)) )))) 


;;;Cramer's rule

    (define (insert n  pos line)
        (cond ((> pos (- (length line) 1))
               (display "index too big"))
              ((= pos 0) (cons n (cdr line)))
              (else (cons (car line) 
                          (insert n (- pos 1) (cdr line))))))
            

 (define (replace-column vec matrix pos)
     (if (not (= (length vec) (length matrix)))
         (error "lengths do not match")
         (if (null? vec)
             '()
            (cons (insert (car vec) pos (car matrix))
                  (replace-column (cdr vec) (cdr matrix) pos)))))      
             
 (define (insert-at-all-pos vec matrix)
       (let ((start-pos 0)
             (end-pos  (- (length (car matrix)) 1) ))
          (define (loop s e)
              (if (> s e)
                  '()
                  (cons 
                    (replace-column vec matrix s)
                    (loop (+ s 1) e))))
              (loop start-pos end-pos)))

  (define (find-solutions vec matrix)
         (let ((det (determinant matrix))
               (det-s (map determinant 
                           (insert-at-all-pos vec matrix))))
            (if (not (= det 0))
                (map (lambda (d) (/ d det))
                     det-s)
                (display "the determinant of matrix of the system is zero"))))


;;;;;;;;;martrix inverse
 
 (define (inverse matrix)
          (let* ((det-A (determinant matrix))
                 (trans-m (transpose matrix))
                 (list-of-det (map (lambda (l-m)
                                      (map determinant l-m))
                                   (all-minors trans-m)))
                 (adjugate-m  (car (fold-left 
                                     (lambda (a b)
                                       (let ((ac (car a))
                                             (i  (cdr a )))
                                         (cons
                                           (append ac (list (mult-by-sign b  i )))
                                           (if (= i -1) 1 -1))))
                                      (cons '() 1)          
                                      list-of-det))))
            (if (not (= det-A 0))
                (mult-m-s adjugate-m (/ 1 det-A) )
                (display "the determinant is zero"))))                               
      ))
                     

 (define (invertible? matrix)
            (let* ((s (length matrix))
                   (i-m (identity-matrix s))
                   (inv (iverse matrix)))
                (equal-mt? (*m matrix inv)
                        i-m ) ))
