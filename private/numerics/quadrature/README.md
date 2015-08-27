quadrature
==========

- `defint.rkt` definite integrals
- `quadrature.rkt` interface / main file (?)
- `rational.rkt` ???


##### Problem with rational interpolation

Date: Sun, 30 Nov 2008 16:47:59 -0500
From: Jack Wisdom <wisdom@MIT.EDU>
To: gjs@mit.edu
Subject: quadrature

The problem seems to occur when the rational-interpolation
gets a zero denominator.  Try setting zd-wallp? to #t.

I do not yet fully understand it, but if we quit the
rational-interpolation instead of using the "zero denominator fix of
BS and Henon", then the quadrature seems to work, in this case.

So replace rational-interpolation with

(define (rational-interpolation dt c dx-list dx-new eps)
  (if (null? dt)
      '()
      (let* ((dt1 (car dt))
	     (w (flo:- c dt1))
	     (b1 (flo:* (flo:/ (car dx-list) dx-new) dt1))
	     (den (flo:- b1 c)))
	(if (flo:= den 0.0)
	    '()
	    (let* ((b (flo:/ w den))
		   (new-d (flo:* c b)))
	      (cons new-d
		    (rational-interpolation (cdr dt)
					    (flo:* b1 b)
					    (cdr dx-list)
					    dx-new
					    eps)))))))

I'll keep looking at it.
