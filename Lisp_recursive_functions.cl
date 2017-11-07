Factorial

(defun fact(n)
(if(<= n 1)1
(* n (fact(- n 1)))))

Generate Fibonacci Sequence

(defun fib(n)(fib-recurse n 1 1))

(defun fib-recurse(n b a)
(if (zerop n) nil
(cons b (fib-recurse(- n 1) a (+ b a)))))

Number of not numbers in list

(defun notnums (n)
(cond
((endp n) 0)
((listp (first n)) (+ (notnums (first n)) (notnums (rest n))))
((numberp (first n)) (notnums (rest n)))
(T (+ 1 (notnums (rest n))))
))

Check if prime number

(defun isprime (n)
(cond
((= n 2) T)
((< n 2) NIL)
((evenp n) NIL)
(T (checkifprime n (- n 1)))))

(defun checkifprime(n d)
(cond
((= 1 d) T)
((= 0 (mod n d)) NIL)
(T (checkifprime n (- d 1)))))

Check if valid phone number (area code followed by exchange, followed by 4 digits)

(defun hfxnum(n)
(cond
((not (= 10 (list-length n)))NIL)
((and(=(list-to-number n 0 1 2)902) (check-902-exch (list-to-number n 3 4 5))))
((and(=(list-to-number n 0 1 2)782) (check-782-exch (list-to-number n 3 4 5))))
))

(defun list-to-number(n a b c)
(parse-integer
(concatenate 'string
(write-to-string (nth a n))
(write-to-string (nth b n))
(write-to-string(nth c n))
)))

(defun check-782-exch (n)
(not(null(intersection (list n) '(486 510 610 845)))))

(defun check-902-exch (n)
(not(null(intersection (list n) '(209 210 219 220 221 222 223 225 229 233 237 240 244 252 266 268 292 293 329 332 333 334 337 342 344 346 347 359 377 384 399 401 402 403 404 405 406 407 412 414 415 420 421 422 423 424 425 426 427 428 429 430 431 440 441 442 443 444 445 446 448 449 450 451 452 453 454 455 456 457 458 459 470 471 473 474 475 476 477 478 479 480 482 483 484 486 487 488 489 490 491 492 493 494 495 496 497 498 499 501 503 520 536 551 558 559 568 571 576 579 580 593 700 701 702 703 704 705 706 707 708 714 717 718 719 720 721 722 772 789 797 800 801 802 809 817 818 827 830 868 873 876 877 878 880 889 891 949 981 982 989 997 998 999)))))

Check if element is in list (not using member function)

(defun isthere(n the-list)
(cond
((endp the-list)nil)
((eq n (first the-list))T)
(T (isthere n (rest the-list)))
))

Superset

(defun flatten (n)
(cond
((null n) nil)
((endp n)n)
((atom (car n)) (append (list (car n)) (flatten (cdr n))))
(T (append(flatten (car n)) (flatten(cdr n))))
))

(defun superset (a b)(remove-duplicates (remove nil(flatten (list a b)))))

Palindrome

(defun palindrome(n)(equal n (reverse n)))

Infix Expression

(defun infix (n)
(if (atom n) n
(eval (list (second n)(infix (first n))(infix (third n))))
))

Flatten a list of any complexity

(defun flatten (n)
(cond
((null n) nil)
((endp n)n)
((atom (car n)) (append (list (car n)) (flatten (cdr n))))
(T (append(flatten (car n)) (flatten(cdr n))))
))
