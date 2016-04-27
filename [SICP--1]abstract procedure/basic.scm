;在终端输入(cf "filename")进行编译
;之后再输入(load "filename")进行加载

;在终端中进入error后输入(RESTART 1)恢复到正常
;每次打开后要先分屏再开一个终端窗口以方面调试，方法Tools->SublimeREPL->scheme
;＊注意打开SublimeREPL时要先把光标放到代码编辑区，这样终端的工作目录才会定位到代码所在的目录下
;每次打开都要为终端窗口设置syntex为scheme,以显示颜色和提示,方法View->syntex->scheme



;define a function
(define (myadd x y)
	(+ x y))

;if
(define (fact x)
	(if (= x 1)
		1
		(* x (fact (- x 1)))))

;call a function
(myadd 12 12)


; cond 
; 线性递归
(define (Ack x y)
	(cond 	((= y 0) 0)
	      	((= x 0) (* 2 y))
	      	((= y 1) 2)
	      	(else (Ack (- x 1) (Ack x (- y 1))))))

;(Ack 1 10)

;树形递归

(define (fib n)
  	(cond 	((= n 0) 0) 
  			((= n 1) 1) 
  			(else (+ 
  				(fib (- n 1))
  				(fib (- n 2))))))


;树形递归改迭代
(define (fib_iter a b n)
  	(if (= n 0) a 
  		(fib_iter b (+ a b) (- n 1))))

(define (fib2 n)
  	(fib_iter 0 1 n))

;exercise 1.11
(define (f n)
	(cond ((< n 3) n)
	      (else (+
	      		(f (- n 1))
	      		(* 2 (f (- n 2)))
	      		(* 3 (f (- n 3)))))))











