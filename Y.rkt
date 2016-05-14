#lang racket

;Y combinator tutorial

;reference:

;ChineseTranslation:
;http://shellfly.org/blog/2015/01/07/yi-the-y-combinator-slight-return/

;English
;http://mvanier.livejournal.com/2897.html



;-----------------------------------------------------
;先写一个阶乘函数

(define (fac n)
	(if (= n 0) 
		1
		(* n (fac (- n 1)))))

;事实上执行时所有函数体都会变成匿名函数：
(define fac
	(lambda (n) 
		(if (= n 0) 
		1
		(* n (fac (- n 1))))))


;尝试不使用显示递归

;假设有个函数能够计算factorial,将其作为参数:
(define almost-fac
	(lambda (f)
		(lambda (n) 
			(if (= n 0) 
				1
				(* n (f (- n 1)))))))



;定义一个identity函数
(define identity (lambda (x) x))


;部分正确的阶乘函数
(define fact0 (almost-fac identity))	;right for 0
(define fact1 (almost-fac fact0))		;right for 0,1
(define fact2 (almost-fac fact1))		;right for 0,1,2
(define fact3 (almost-fac fact2))		;right for 0,1,2,3
(define fact4 (almost-fac fact3))		;right for 0,1,2,3,4
;...


;因此
(define fact 			;right for 0,1,2,3....
	(almost-fac
		(almost-fac
			(almost-fac
				;...infinity...
				(almost-fac identity)))))


;上面的程序无法写出来，因为它无限长
;但是可以看出(almost-fac fact) = fact，即almost-fac的不动点就是fact
;如果能够将almost-fact的不动点求出来，就能求出fact了
;Y combinator就是用来求函数不动点的的组合子(高阶函数)

;Y满足：
; (Y f) = f (Y f)
;先这样定义它
;(define (Y f) (f (Y f)))

;上面这个定义能够在惰性求值语言里work，但在严格求值的语言里，会把
;(Y f)无限展开，从而死循环:
;	(Y f)
;=>	(f (Y f))
;=>	(f (f (Y f)))
;=> (f (f (f (Y f))))
;=> ...


;严格求值语言里的Y combinator
;在严格求值语言里,在把lambda表达式应用到一个参数之前,
;函数体里的lambda表达式永远不会执行
;因此考虑把(Y f)包装在lambda表达式里,用lambda来推迟了(Y f)的执行
;从而避免死循环

;因为(Y f)本质是一个接受一个参数的函数
;所以: (Y f) = (lambda (x) ((Y f) x))

;给出新定义
(define Y
	(lambda (f)
		(f (lambda (x) ((Y f) x)))))



;这样就可以求almost-fac的不动点了,也就是真正的fact函数
(define fact (Y almost-fac))







;----------------------------------------------------------
;前面的Y定义虽然能work但它并不是真正的combinator,因为combinator
;的定义是：没有自由变量的lambda项，而上面的定义中递归调用了Y,而Y是自由
;变量。下面推导真正的Y combinator:


;---------首先推导惰性求值语言里的 Y combinator


;我们让part-fac通过传来的参数调用自己，即把自己传给自己来实现递归
(define (part-fac self n)
	(if (= n 0)
		1
		(* n (self self (- n 1)))))

;这样就能够不用显示递归调用实现递归了

(define fact
	(lambda (x) (part-fac part-fac x)))


;我们对上面的函数进行变换，构造出Y
;part-fac可以写成:
(define (part-fac self)
	(lambda (n)
		(if (= n 0)
			1
			(* n ((self self) (- n 1))))))


;进一步变换
(define (part-fac self)
	(lambda (f)
		(lambda (n)
      		(if (= n 0)
          		1
          		(* n (f (- n 1))))) 
		(self self)))
;;;注意严格求值语言会在(self self)处陷入死循环，惰性
;;;求值语言则不会


;==>

(define (part-fac self)
	(almost-fac (self self)))

;==>

(define part-fac
	(lambda (x) (almost-fac (x x))))


;再写出fact的定义：
(define fact (part-fac part-fac))


;==>
(define fact
	((lambda (x) (almost-fac (x x)))
	 (lambda (x) (almost-fac (x x)))))


;==>
(define fact
	(lambda (f)
		((lambda (x) (f (x x)))
	 	 (lambda (x) (f (x x)))))
	almost-fac)

;把中间的匿名函数提取出来
(define func
	(lambda (f)
		((lambda (x) (f (x x)))
	 	 (lambda (x) (f (x x))))))

;fact的定义就变成了
(define fact (func almost-fac))

;!!!!!可以看出刚刚提取出的func就是Y combinator
;因此
(define Y
	(lambda (f)
		((lambda (x) (f (x x)))
	 	 (lambda (x) (f (x x))))))

;这个Y是真正的Y combinator,它是一个没有自由变量的lambda项





;---------下面推导严格求值语言里的Y combinator

;在上面的推倒过程中到下面这步就无法在严格求值的语言中使用了
(define (part-fac self)
	(lambda (f)
		(lambda (n)
      		(if (= n 0)
          		1
          		(* n (f (- n 1))))) 
		(self self)))

;由于(self self)是一个接受一个参数的函数，与之前的做法类似，我们用lambda
;函数将它包住:

;(self self) = (lambda (x) ((self self) x))

;因此
(define (part-fac self)
	(lambda (f)
		(lambda (n)
      		(if (= n 0)
          		1
          		(* n (f (- n 1))))) 
		(lambda (x) ((self self) x))))
;这样(self self)就不会陷入死循环了


;==>
(define (part-fac self)
	(almost-fac (lambda (x) ((self self) x))))


;==>
(define part-fac
	(lambda (f) (almost-fac (lambda (x) ((f f) x))))


;再写出fact的定义：
(define fact (part-fac part-fac))


;==>
(define fact
	((lambda (f) (almost-fac (lambda (x) ((f f) x))))
	 (lambda (f) (almost-fac (lambda (x) ((f f) x))))))


;==>
(define fact
	(lambda (g)
		((lambda (f) (g (lambda (x) ((f f) x))))
	 	 (lambda (f) (g (lambda (x) ((f f) x))))))
	almost-fac)

;把中间的匿名函数提取出来
(define func
	(lambda (g)
		((lambda (f) (g (lambda (x) ((f f) x))))
	 	 (lambda (f) (g (lambda (x) ((f f) x)))))))

;fact的定义就变成了
(define fact (func almost-fac))


;与前面一样,这里的 func 就是Y combinator,而且是可以在严格求值环境
;工作的版本
;因此

(define Y
	(lambda (g)
		((lambda (f) (g (lambda (x) ((f f) x))))
	 	 (lambda (f) (g (lambda (x) ((f f) x)))))))





;总结
;-----------------------------------------------------------
;从前面的结论可以看出，我们不需要任何显示递归调用就实现了递归函数，
;这说明一个编程语言只要支持高阶函数就能够实现递归！！！
;而且我们甚至不用定义任何有名字的函数，只用匿名函数就能实现递归！！！


;比如我们把前面的代码稍加改动就得到下面这段代码，它不含任何函数名
;就能够实现递归计算阶乘

(((lambda (g)
	((lambda (f) (g (lambda (x) ((f f) x))))
	 (lambda (f) (g (lambda (x) ((f f) x))))))
  (lambda (f)
	(lambda (n) 
		(if (= n 0) 
			1
			(* n (f (- n 1))))))) 
 12)

;它的计算结果为12的阶乘:479001600


;----------
;上面只是以阶乘函数来做例子，事实上任意的递归函数，都可以简单地
;按照上面的做法改成 almost-XXX 的形式，然后通过Y来转为没有显示递归调用
;的递归函数



