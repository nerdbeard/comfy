;;; comfy.el --- COMFY-65 language for 6502

;; Copyright (C) 2019 Mike Pelletier

;; Author: Mike Pelletier <mike@mkp.ca>
;;	  Henry G. Baker
;; Maintainer: Mike Pelletier <mike@mkp.ca>
;; Created: Nov 1976
;; Keywords: languages 6502
;; Version: 0.0.0

;;;; LICENSE
;; Copyright � 1998 Association for Computing Machinery, Inc. Permission
;; to include in application software or to make digital or hard copies
;; of part or all of this work is subject to the following licensing
;; agreement.  

;; ACM Software License Agreement

;; All software, both binary and source published by the Association for
;; Computing Machinery (hereafter, Software) is copyrighted by the
;; Association (hereafter, ACM) and ownership of all right, title and
;; interest in and to the Software remains with ACM. By using or copying
;; the Software, User agrees to abide by the terms of this Agreement.

;; Noncommercial Use

;; The ACM grants to you (hereafter, User) a royalty-free, nonexclusive
;; right to execute, copy, modify and distribute both the binary and
;; source code solely for academic, research and other similar
;; noncommercial uses, subject to the following conditions:

;;    1. User acknowledges that the Software is still in the development
;;    stage and that it is being supplied "as is," without any support
;;    services from ACM. Neither ACM nor the author makes any
;;    representations or warranties, express or implied, including,
;;    without limitation, any representations or warranties of the
;;    merchantability or fitness for any particular purpose, or that the
;;    application of the software, will not infringe on any patents or
;;    other proprietary rights of others.  

;;    2. ACM shall not be held liable for direct, indirect, incidental or
;;    consequential damages arising from any claim by User or any third
;;    party with respect to uses allowed under this Agreement, or from
;;    any use of the Software.
   
;;    3. User agrees to fully indemnify and hold harmless ACM and/or the
;;    author(s) of the original work from and against any and all claims,
;;    demands, suits, losses, damages, costs and expenses arising out of
;;    the User's use of the Software, including, without limitation,
;;    arising out of the User's modification of the Software.  

;;    4. User
;;    may modify the Software and distribute that modified work to third
;;    parties provided that: (a) if posted separately, it clearly
;;    acknowledges that it contains material copyrighted by ACM (b) no
;;    charge is associated with such copies, (c) User agrees to notify
;;    ACM and the Author(s) of the distribution, and (d) User clearly
;;    notifies secondary users that such modified work is not the
;;    original Software.  

;;    5. User agrees that ACM, the authors of the
;;    original work and others may enjoy a royalty-free, non-exclusive
;;    license to use, copy, modify and redistribute these modifications
;;    to the Software made by the User and distributed to third parties
;;    as a derivative work under this agreement.  

;;    6. This agreement will terminate immediately upon User's breach of,
;;    or non-compliance with, any of its terms. User may be held liable
;;    for any copyright infringement or the infringement of any other
;;    proprietary rights in the Software that is caused or facilitated by
;;    the User's failure to abide by the terms of this agreement.

;;    7. This agreement will be construed and enforced in accordance with
;;    the law of the state of New York applicable to contracts performed
;;    entirely within the State. The parties irrevocably consent to the
;;    exclusive jurisdiction of the state or federal courts located in
;;    the City of New York for all disputes concerning this agreement.

;; Commercial Use

;; Any User wishing to make a commercial use of the Software must contact
;; ACM at permissions@acm.org to arrange an appropriate
;; license. Commercial use includes (1) integrating or incorporating all
;; or part of the source code into a product for sale or license by, or
;; on behalf of, User to third parties, or (2) distribution of the binary
;; or source code to third parties for use with a commercial product sold
;; or licensed by, or on behalf of, User.

;; Revised 6/98
;;; Library functions

(defun comfy-subseq (v i)
  ;; XXX NOT CALLED -- is this any different than seq-subseq?
  (let* ((nv (make-vector (- (length v) i) nil))
         (j 0))
    (while (< j (length nv))
      (aset nv j (elt v (+ i j)))
      (setq j (1+ j)))
    nv))

(defun comfy--sublis (a e)
  ;; Works like CL's sublis
  (cond ((atom e)
         (let* ((binding (assq e a)))
           (cond (binding (cdr binding))
                 (t e))))
        (t (cons (comfy--sublis a (car e))
                 (comfy--sublis a (cdr e))))))

(defun comfy--put (symbol propname value)
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (put symbol propname value)))

(defun comfy--get (symbol propname)
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (get symbol propname)))

;;; COMFY-65 instructions

;;;; Basic test instructions.
(comfy--put 'c=1\? 'test 176)       ;; test carry=1.
(comfy--put 'c=0\? 'test 144)       ;; test carry=0.
(comfy--put 'llt 'test 144)         ;; logically less than.
(comfy--put 'lge 'test 176)         ;; logically greater than or equal.
(comfy--put '=\? 'test 240)         ;; equal.
(comfy--put '~=\? 'test 208)        ;; not equal.
(comfy--put '=0\? 'test 240)        ;; equals zero.
(comfy--put '~=0\? 'test 208)       ;; not equal to zero.
(comfy--put 'v=1\? 'test 112)       ;; test overflow=1.
(comfy--put 'v=0\? 'test 80)        ;; test overflow=0.
(comfy--put '<\? 'test 48)          ;; test arithmetic less than.
(comfy--put '>=\? 'test 16)         ;; test arithmetic greater than or equal.
(comfy--put '<0\? 'test 48)         ;; test arithmetic less than zero.
(comfy--put '>=0\? 'test 16)        ;; test airthmetic greater than or equal to zero.

;;;; Group 0.
(comfy--put '\? 'skeleton 32)       ;; test.
(comfy--put 'stj 'skeleton 152)     ;; store j.
(comfy--put 'lj 'skeleton 168)      ;; load j.
(comfy--put 'cj 'skeleton 200)      ;; compare j.
(comfy--put 'ci 'skeleton 232)      ;; compare i.
;;;; Group 1.
(comfy--put 'lor 'skeleton 17)      ;; logical or.
(comfy--put 'land 'skeleton 49)     ;; logical and.
(comfy--put 'lxor 'skeleton 81)     ;; logical xor.
(comfy--put '+ 'skeleton 113)       ;; add with carry.
(comfy--put 'st 'skeleton 145)      ;; store accumulator.
(comfy--put 'l 'skeleton 177)       ;; load accumulator.
(comfy--put 'c 'skeleton 209)       ;; compare accumulator.
(comfy--put '- 'skeleton 241)       ;; subtract with borrow.
;;;; Group 2.
(comfy--put 'asl 'skeleton 10)      ;; arithmetic shift left.
(comfy--put 'rl 'skeleton 42)       ;; rotate left.
(comfy--put 'lsr 'skeleton 74)      ;; logical shift right.
(comfy--put 'rr 'skeleton 106)      ;; rotate right.
(comfy--put 'sti 'skeleton 138)     ;; store i.
(comfy--put 'li 'skeleton 170)      ;; load i.
(comfy--put '1- 'skeleton 194)      ;; decrement.
(comfy--put '1+ 'skeleton 226)      ;; increment.
;;;; random instructions.
(comfy--put 'trap 'skeleton 0)      ;; programmed break.
(comfy--put 'save 'skeleton 8)      ;; push processor state onto stack.
(comfy--put 'restore 'skeleton 40)  ;; restore processor state from stack.
(comfy--put 'push 'skeleton 72)     ;; push accumulator onto stack.
(comfy--put 'pop 'skeleton 104)     ;; pop accumulator from stack.
(comfy--put 'c=0 'skeleton 24)      ;; clear carry.
(comfy--put 'c=1 'skeleton 56)      ;; set carry.
(comfy--put 'seb 'skeleton 24)      ;; set borrow.
(comfy--put 'clb 'skeleton 56)      ;; clear borrow.
(comfy--put 'v=0 'skeleton 184)     ;; clear overflow.
(comfy--put 'enable 'skeleton 88)   ;; enable interrupts.
(comfy--put 'disable 'skeleton 120) ;; disable interrupts.
(comfy--put 'binary 'skeleton 216)  ;; set binary mode.
(comfy--put 'decimal 'skeleton 248) ;; set decimal mode.
(comfy--put 'i+1 'skeleton 232)     ;; increment i.
(comfy--put 'j+1 'skeleton 200)     ;; increment j.
(comfy--put 'i-1 'skeleton 202)     ;; decrement i.
(comfy--put 'j-1 'skeleton 136)     ;; decrement j.
(comfy--put 'nop 'skeleton 234)     ;; no operation.

(defvar comfy-jmp 76)
(setq comfy-jmp 76)
(defvar comfy-jsr 32)
(setq comfy-jsr 32)
(comfy--put 'return 'jump 96)
(comfy--put 'resume 'jump 64)

;;; Compiler

(defun make-comfy (&optional env-size)
  (let ((env-size (or env-size #x10000)))
    (cons (1- env-size) (make-vector env-size 0))))

(defmacro comfy-f (state) `(car ,state))
(defmacro comfy-mem (state) `(cdr ,state))

(defun comfy-gen (state obj)
  ;; place one character "obj" into the stream.
  (assert (numberp obj))
  (assert (>= obj #x00))
  (assert (<= obj #xff))
  (setf (comfy-f state) (1- (comfy-f state)))
  (aset (comfy-mem state) (comfy-f state) obj)
  (comfy-f state))

(defun comfy-testp (e)
  ;; predicate to tell whether "e" is a test.
  (and (symbolp e) (comfy--get e 'test)))

(defun comfy-actionp (e)
  ;; predicate to tell whether "e" is an action.
  (and (symbolp e) (not (comfy--get e 'test))))

(defun comfy-jumpp (e)
  ;; predicate to tell whether "e" is a jump-type action.
  (and (symbolp e) (comfy--get e 'jump)))

(defun comfy-macrop (x)
  (and (symbolp x) (comfy--get x 'cmacro)))

(defun comfy-ra (state b a)
  ;; replace the absolute address at the instruction "b"
  ;; by the address "a".
  (let* ((ha (lsh a -8))
         (la (logand a 255))
         (mem (comfy-mem state)))
    (aset mem (1+ b) la)
    (aset mem (+ b 2) ha))
  b)

(defun comfy-inv (op)
  ;; invert the condition for a branch.
  ;; invert bit 5 (counting from the right).
  (logxor op 32))

(defun comfy-genbr (state win)
  ;; generate an unconditional jump to "win".
  (comfy-gen state 0) (comfy-gen state 0) (comfy-gen state comfy-jmp)
  (comfy-ra state (comfy-f state) win))

(defun comfy-8bitp (n)
  ;; t when -128 <= n <= 127; ie when n is an integer that can be
  ;; represented by a signed byte
  (let* ((m (logand n -128)))
    (or (= 0 m) (= -128 m))))

(defun comfy-genbrc (state cond win lose)
  ;; generate an optimized conditional branch
  ;; on condition c to "win" with failure to "lose".
  (let* ((w (- win (comfy-f state))) (l (- lose (comfy-f state)))) ;; Normalize to current point.
    (cond ((= w l) win)
          ((and (= l 0) (comfy-8bitp w))
           (comfy-gen state w) (comfy-gen state cond))
          ((and (= w 0) (comfy-8bitp l))
           (comfy-gen state l) (comfy-gen state (comfy-inv cond)))
          ((and (comfy-8bitp l) (comfy-8bitp (- w 2)))
           (comfy-gen state l)
           (comfy-gen state (comfy-inv cond))
           (comfy-gen state (- w 2))
           (comfy-gen state cond))
          ((and (comfy-8bitp w) (comfy-8bitp (- l 2)))
           (comfy-gen state w)
           (comfy-gen state cond)
           (comfy-gen state (- l 2))
           (comfy-gen state (comfy-inv cond)))
          ((comfy-8bitp (- l 3))
           (comfy-genbrc state cond (comfy-genbr state win) lose))
          (t
           (comfy-genbrc state cond win (comfy-genbr state lose))))))

(defun comfy-ogen (state op a)
  ;; put out address and op code into stream.
  ;; put out only one byte address, if possible.
  (let* ((ha (lsh a -8))
         (la (logand a 255)))
    (cond ((= ha 0) (comfy-gen state la) (comfy-gen state op))
          (t (comfy-gen state ha) (comfy-gen state la) (comfy-gen state (+ op 8))))))

(defun comfy-skeleton (op)
  ;; return the skeleton of the op code "op".
  ;; the "skeleton" property of op contains either
  ;; the code for "accumulator" (groups 0,2) or "immediate" (1) addressing.
  (logand (comfy--get op 'skeleton) 227))

(defun comfy-emit (state i win)
  ;; place the unconditional instruction "i" into the stream with
  ;; success continuation "win".
  (cond ((not (= win (comfy-f state)))
         (comfy-emit state i (comfy-genbr state win)))
        ;; atom is a single character instruction.
        ((symbolp i)
         (comfy-gen state (comfy--get i 'skeleton)))
        ;; no op code indicates a subroutine call.
        ((null (cdr i))
         (comfy-gen state 0)
         (comfy-gen state 0)
         (comfy-gen state comfy-jsr)
         (comfy-ra state (comfy-f state) (eval (car i))))
        ;; "a" indicates the accumulator.
        ((eq (cadr i) 'a)
         (comfy-emit state (car i) win))
        ;; "s" indicates the stack.
        ((eq (cadr i) 's)
         (comfy-gen state (+ (comfy-skeleton (car i)) 24)))
        ;; length=2 indicates absolute addressing.
        ((= (length i) 2)
         (comfy-ogen (+ (comfy-skeleton (car i)) 4)
                     (eval (cadr i))))
        ;; "i" indicates absolute indexed by i.
        ((eq (cadr i) 'i)
         (comfy-ogen (+ (comfy-skeleton (car i)) 20) (eval (cadr (cdr i)))))
        ;; "j" indicates absolute indexed by j.
        ;; this cannot be optimized for page zero addresses.
        ((eq (cadr i) 'j)
         (comfy-gen state 0)
         (comfy-gen state 0)
         (comfy-gen state (+ (comfy-skeleton (car i)) 24))
         (comfy-ra state (comfy-f state) (eval (cadr (cdr i)))))
        ;; "\#" indicates immediate operand.
        ((eq (cadr i) '\#)
         (comfy-ogen (- (comfy--get (car i) 'skeleton) 8)
                     (logand (eval (cadr (cdr i))) 255)))
        ;; "i@" indicates index by i, the indirect.
        ((eq (cadr i) 'i@)
         (comfy-ogen (comfy-skeleton (car i))
                     (logand (eval (cadr (cdr i))) 255)))
        ;; "@j" indicates indirect, then index by j.
        ((eq (cadr i) '@j)
         (comfy-ogen (+ (comfy-skeleton (car i)) 16)
                     (logand (eval (cadr (cdr i))) 255)))))

(defun comfy-compile (state e win lose)
  ;; comfy-compile expression e with success continuation "win" and
  ;; failure continuation "lose".
  ;; "win" an "lose" are both addresses of stuff higher in memory.
  (cond ((numberp e) ; allow constants.
         (comfy-gen state e))
        ((comfy-macrop e)
         (comfy-compile state (apply (comfy--get e 'cmacro) (list e)) win lose))
        ((comfy-jumpp e) ; must be return or resume.
         (comfy-gen state (comfy--get e 'jump)))
        ((comfy-actionp e) ; single byte instruction.
         (comfy-emit state e win))
        ((comfy-testp e) ; test instruction
         (comfy-genbrc state (comfy--get e 'test) win lose))
        ((eq (car e) 'not)
         (comfy-compile state (cadr e) lose win))
        ((eq (car e) 'seq)
         (cond ((null (cdr e)) win)
               (t (comfy-compile state (cadr e)
                                 (comfy-compile state (cons 'seq (cddr e)) win lose)
                                 lose))))
        ((eq (car e) 'loop)
         (let* ((l (comfy-genbr state 0))
                (r (comfy-compile state (cadr e) l lose)))
           (comfy-ra state l r)
           r))
        ((numberp (car e)) ; duplicate n times.
         (cond ((zerop (car e)) win)
               (t (comfy-compile state (cons (1- (car e)) (cdr e))
                                 (comfy-compile state (cadr e) win lose)
                                 lose))))
        ((eq (car e) 'if) ; if-then-else.
         (comfy-compile state (cadr e)
                        (comfy-compile state (cadr (cdr e)) win lose)
                        (comfy-compile state (cadddr e) win lose)))
        ((eq (car e) 'while) ; do-while.
         (let* ((l (comfy-genbr state 0))
                (r (comfy-compile state (cadr e)
                                  (comfy-compile state (cadr (cdr e)) l lose)
                                  win)))
           (comfy-ra state l r)
           r))
        ;; allow for COMFY macros !
        ((comfy-macrop (car e))
         (comfy-compile state (apply (comfy--get (car e) 'cmacro) (list e)) win lose))
        (t
         (comfy-emit state e win))))

;;; Language

(comfy--put
 'alt
 'cmacro
 '(lambda (e)
    ;; define the dual of "seq" using DeMorgan's law.
    (list 'not
          (cons 'seq
                (mapcar '(lambda (e) (list 'not e))
                        (cdr e))))))

(comfy--put
 'call
 'cmacro
 '(lambda (e)
    (let* ((p (cadr e)) (pl (cddr e)))
      (comfy--sublis (list (cons 'pushes (comfy-genpush pl))
                           (cons 'p p)
                           (cons 'n (length pl)))
                     '(seq (seq . pushes)
                           (p)
                           (li s)
                           (land ii)
                           (sti s))))))

(comfy--put
 'lambda
 'cmacro
 '(lambda (e)
    (let* ((pl (cadr e))
           (body (cddr e)))
      (comfy--sublis (list (cons 'body body)
                           (cons 'xchs (comfy-genxchs pl))
                           (cons 'moves (comfy-genmoves pl)))
                     '(seq (li s)
                           (seq . xchs)
                           (seq . body)
                           (li s)
                           (seq . moves)
                           (return))))))

(defun comfy-genxchs (pl)
  (cond ((null pl) pl)
        (t (cons (list 'xch (list 'i (+ 258 (length pl))) (list (car pl)))
                 (comfy-genxchs (cdr pl))))))

(defun comfy-genmoves (pl)
  (cond ((null pl) nil)
        (t (cons (list 'move (list 'i (+ 258 (length pl))) (list (car pl)))
                 (comfy-genmoves (cdr pl))))))

(defun comfy-genpush (pl)
  (cond ((null pl) pl)
        (t (let* ((p (car pl)))
             (append (` ((l (, p)) push)) (comfy-genpush (cdr pl)))))))

(defun comfy-match (p e f alist)
  ;; f is a function which is executed if the comfy-match fails.
  ;; f had better not return.
  (cond ((comfy-constantp p)
         (cond ((eq p e) alist)
               (t (funcall f))))
        ((comfy-variablep p) (cons (cons (cadr p) e) alist))
        ((eq (car p) 'quote) (cond ((eq (cadr p) e) alist)
                                   (t (funcall f))))
        ((comfy-predicate p) (cond ((funcall (cadr p) e) alist)
                             (t (funcall f))))
        ((atom e) (funcall f))
        (t (comfy-match (car p)
                  (car e)
                  f
                  (comfy-match (cdr p)
                         (cdr e)
                         f
                         alist)))))

(defun comfy-predicate (x)
  (and (consp x) (eq (car x) 'in)))

(defun comfy-constantp (x) (atom x))

(defun comfy-variablep (x)
  (and (consp x) (eq (car x) '\,)))

(defmacro comfy-cases (&rest a)
  (` (quote
      (, (catch 'comfy-cases
           (comfy-fapplyl (cdr a)
			  (eval (car a))
			  '(lambda () (throw 'comfy-cases nil))))))))

(defun comfy-fapplyl (fl a fail)
  ;; "fail" is a function which is executed if comfy-fapplyl fails.
  ;; "fail" had better not return.
  (cond ((null fl) (funcall fail))
        (t (catch 'comfy-fapplyl
             ((comfy-f c)apply (car fl) a
              '(lambda ()
                 (throw 'comfy-fapplyl
                        (comfy-fapplyl (cdr fl) a fail))))))))

(defun comfy-fapply (f a fail)
  (let* ((alist (comfy-match (cadr f) a fail nil)))
    (apply (cons 'lambda
                 (cons (mapcar 'car alist)
                       (cddr f)))
           (mapcar 'cdr alist))))

(defmacro comfy-define (&rest a)
  (let* ((ind (car a))
         (patt (cadr a))
         (body (cddr a))
         (where (cond ((atom patt) patt)
                      ((atom (car patt)) (car patt)))))
    (or (comfy--get where ind) (comfy--put where ind '(lambda (e) (comfy-cases e))))
    (comfy--put
     where
     ind
     (` (lambda (e)
          (, (append (` (comfy-cases e (, (append (` (lambda (, patt))) body))))
                     (cddr (cadr (cdr (comfy--get where ind)))))))))
    nil))

(comfy--put 'star 'cmacro nil)

(comfy-define cmacro (star . (, body))
              (` (not (loop (, (append '(seq) body))))))

(comfy--put 'i2 'cmacro nil)

(comfy-define cmacro (i2 (, p))
              (` (seq (1+ (, p))
                      (if =0\? (1+ (1+ (, p)))
                        (seq)))))

(comfy--put 'move 'cmacro nil)

(comfy-define cmacro (move (, x) (, y))
              (` (seq (, (append '(l) x))
                      (, (append '(st) y)))))

(comfy--put 'prog 'cmacro nil)

(comfy-define cmacro (prog ((, v)) . (, body))
              (` (seq push
                      (li s)
                      (move ((, v)) (i 257))
                      (, (append '(seq) body))
                      (li s)
                      (move (i 257) ((, v)))
                      i-1
                      (sti s))))

(comfy--put 'fori 'cmacro nil)

(comfy-define cmacro (fori (, from) (, to) . (, body))
              (` (seq (, (append '(li) from))
                      (while (seq (, (append '(ci) to)) llt)
                        (seq (, (append '(seq) body)) i+1)))))

(comfy--put 'forj 'cmacro nil)

(comfy-define cmacro (forj (, from) (, to) . (, body))
              (` (seq (, (append '(lj) from))
                      (while (seq (, (append '(cj) to)) llt)
                        (seq (, (append '(seq) body)) j+1)))))

(comfy--put 'for 'cmacro nil)

(comfy-define cmacro (for (, v) (, from) (, to) . (, body))
              (` (seq (, (append '(l) from)) (, (append '(st) v))
                      (while (seq (, (append '(c) to)) llt)
                        (seq (, (append '(seq) body))
                             (, (append '(1+) v))
                             (, (append '(l) v)))))))

;;; Feature

(provide 'comfy)
