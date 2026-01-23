(import (chicken foreign)
        (chicken process-context))

;; riscv-csc-options
;; riscv-csc -L -lgpiod -I. -o run-gpio gpio.scm gpio.c

;; Inject C prototype so Chicken knows about gpio
#> extern int gpio(int,int); <#

;; Now define the foreign lambda
(define gpio
  (foreign-lambda int "gpio" int int))

(let ([args (map string->number (command-line-arguments))])
  (case (length args)
    ((0) (gpio 18 0))
    ((1) (gpio 18 (car args)))
    ((2) (gpio (car args) (cadr args)))))
