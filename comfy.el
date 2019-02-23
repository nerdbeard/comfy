;;; comfy.el --- COMFY-65 language for 6502

;; Copyright (C) 2019 Mike Pelletier

;; Author: Mike Pelletier <mike@mkp.ca>
;;	  Henry G. Baker
;; Maintainer: Mike Pelletier <mike@mkp.ca>
;; Created: Nov 1976
;; Keywords: languages 6502
;; Version: 0.0.1alpha

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
;;; Commentary:
"COMFY-65 is a ‘medium level’ language for programming on the MOS
Technology 6502 microcomputer [MOSTech76].

Please see the accompanying readme.org or
http://home.pipeline.com/~hbaker1/sigplannotices/sigcol04.pdf for
more information."
;;; Code:
;;;; Library functions
;; This pair of functions prevent namespace polution.  Comfy uses a
;; lot of common symbols that we don't really want to clutter up with
;; our properties.

(defun comfy--put (symbol propname value)
  "Prepend a namespace to SYMBOL before setting PROPNAME to VALUE."
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (put symbol propname value)))

(defun comfy--get (symbol propname)
  "Prepend a namespace to SYMBOL before getting PROPNAME."
  (let ((symbol (intern (concat "comfy-" (symbol-name symbol)))))
    (get symbol propname)))
;;;; COMFY-65 instructions
;;;;; Basic test instructions.
(comfy--put 'c=1?    'test     #xB0) ;; test carry=1.
(comfy--put 'c=0?    'test     #x90) ;; test carry=0.
(comfy--put 'llt     'test     #x90) ;; logically less than.
(comfy--put 'lge     'test     #xB0) ;; logically greater than or equal.
(comfy--put '=?      'test     #xF0) ;; equal.
(comfy--put '~=?     'test     #xD0) ;; not equal.
(comfy--put '=0?     'test     #xF0) ;; equals zero.
(comfy--put '~=0?    'test     #xD0) ;; not equal to zero.
(comfy--put 'v=1?    'test     #x70) ;; test overflow=1.
(comfy--put 'v=0?    'test     #x50) ;; test overflow=0.
(comfy--put '<?      'test     #x30) ;; test arithmetic less than.
(comfy--put '>=?     'test     #x10) ;; test arithmetic greater than or equal.
(comfy--put '<0?     'test     #x30) ;; test arithmetic less than zero.
(comfy--put '>=0?    'test     #x10) ;; test " greater than or equal to zero.
;;;;; Group 0.
(comfy--put '\?      'skeleton #x20) ;; test.
(comfy--put 'stj     'skeleton #x98) ;; store j.
(comfy--put 'lj      'skeleton #xA8) ;; load j.
(comfy--put 'cj      'skeleton #xC8) ;; compare j.
(comfy--put 'ci      'skeleton #xE8) ;; compare i.
;;;;; Group 1.
(comfy--put 'lor     'skeleton #x11) ;; logical or.
(comfy--put 'land    'skeleton #x31) ;; logical and.
(comfy--put 'lxor    'skeleton #x51) ;; logical xor.
(comfy--put '+       'skeleton #x71) ;; add with carry.
(comfy--put 'st      'skeleton #x91) ;; store accumulator.
(comfy--put 'l       'skeleton #xB1) ;; load accumulator.
(comfy--put 'c       'skeleton #xD1) ;; compare accumulator.
(comfy--put '-       'skeleton #xF1) ;; subtract with borrow.
;;;;; Group 2.
(comfy--put 'asl     'skeleton #x0A) ;; arithmetic shift left.
(comfy--put 'rl      'skeleton #x2A) ;; rotate left.
(comfy--put 'lsr     'skeleton #x4A) ;; logical shift right.
(comfy--put 'rr      'skeleton #x6A) ;; rotate right.
(comfy--put 'sti     'skeleton #x8A) ;; store i.
(comfy--put 'li      'skeleton #xAA) ;; load i.
(comfy--put '1-      'skeleton #xC2) ;; decrement.
(comfy--put '1+      'skeleton #xE2) ;; increment.
;;;;; random instructions.
(comfy--put 'trap    'skeleton #x00) ;; programmed break.
(comfy--put 'save    'skeleton #x08) ;; push processor state onto stack.
(comfy--put 'restore 'skeleton #x28) ;; restore processor state from stack.
(comfy--put 'push    'skeleton #x48) ;; push accumulator onto stack.
(comfy--put 'pop     'skeleton #x68) ;; pop accumulator from stack.
(comfy--put 'c=0     'skeleton #x18) ;; clear carry.
(comfy--put 'c=1     'skeleton #x38) ;; set carry.
(comfy--put 'seb     'skeleton #x18) ;; set borrow.
(comfy--put 'clb     'skeleton #x38) ;; clear borrow.
(comfy--put 'v=0     'skeleton #xB8) ;; clear overflow.
(comfy--put 'enable  'skeleton #x58) ;; enable interrupts.
(comfy--put 'disable 'skeleton #x78) ;; disable interrupts.
(comfy--put 'binary  'skeleton #xD8) ;; set binary mode.
(comfy--put 'decimal 'skeleton #xF8) ;; set decimal mode.
(comfy--put 'i+1     'skeleton #xE8) ;; increment i.
(comfy--put 'j+1     'skeleton #xC8) ;; increment j.
(comfy--put 'i-1     'skeleton #xCA) ;; decrement i.
(comfy--put 'j-1     'skeleton #x88) ;; decrement j.
(comfy--put 'nop     'skeleton #xEA) ;; no operation.
;;;;; Control transfer instructions.
(comfy--put 'return  'jump     #x60)
(comfy--put 'resume  'jump     #x40)
(defvar comfy-jmp #x4C)
(defvar comfy-jsr #x20)
;;;; Compiler
(defun make-comfy-image (&optional size)
  "Typical SIZE is 2^16 or #x10000."
  (let ((size (or size #x10000)))
    (cons (1- size) (make-vector size 0))))

(defmacro comfy-f (image)
  "Compiled code array pointer of IMAGE; it works its way down from the top."
  `(car ,image))

(defmacro comfy-mem (image)
  "Vector of IMAGE where the compiled code is placed."
  `(cdr ,image))

(defun comfy-gen (image obj)
  "In IMAGE, place one character OBJ into its stream."
  (assert (numberp obj))
  (assert (>= obj #x00))
  (assert (<= obj #xFF))
  (let ((f-1 (1- (comfy-f image))))
    (setf (comfy-f image) f-1)
    (aset (comfy-mem image) f-1 obj)
    f-1))

(defun comfy-testp (e)
  "Predicate to tell whether E is a test."
  (and (symbolp e) (comfy--get e 'test)))

(defun comfy-actionp (e)
  "Predicate to tell whether E is an action."
  (and (symbolp e) (not (comfy--get e 'test))))

(defun comfy-jumpp (e)
  "Predicate to tell whether E is a jump-type action."
  (and (symbolp e) (comfy--get e 'jump)))

(defun comfy-macrop (x)
  "Predicate to tell whether X is a cmacro."
  (and (symbolp x) (comfy--get x 'cmacro)))

(defun comfy-ra (image b a)
  "In IMAGE, replace the absolute address at instruction B with address A."
  (let* ((ha (lsh a -8))
         (la (logand a #xFF))
         (mem (comfy-mem image)))
    (aset mem (1+ b) la)
    (aset mem (+ b 2) ha))
  b)

(defun comfy-inv (op)
  "Invert the condition of OP for a branch."
  ;; invert bit 5 (counting from the right).
  (logxor op #x20))

(defun comfy-genbr (image win)
  "In IMAGE, generate an unconditional jump to WIN."
  (comfy-gen image 0)
  (comfy-gen image 0)
  (comfy-gen image comfy-jmp)
  (comfy-ra image (comfy-f image) win))

(defun comfy-8bitp (n)
  "True when N is an integer that can be represented by a signed byte.
That is, when -128 <= N <= 127."
  (let* ((m (logand n -128)))
    (or (= 0 m) (= -128 m))))

(defun comfy-genbrc (image cond win lose)
  "In IMAGE, generate a branch on COND to WIN with failure to LOSE.
The optimal conditional branch is generated."
  (let* ((w (- win (comfy-f image)))
         (l (- lose (comfy-f image)))) ;; Normalize to current point.
    (cond ((= w l) win)
          ((and (= l 0) (comfy-8bitp w))
           (comfy-gen image w) (comfy-gen image cond))
          ((and (= w 0) (comfy-8bitp l))
           (comfy-gen image l) (comfy-gen image (comfy-inv cond)))
          ((and (comfy-8bitp l) (comfy-8bitp (- w 2)))
           (comfy-gen image l)
           (comfy-gen image (comfy-inv cond))
           (comfy-gen image (- w 2))
           (comfy-gen image cond))
          ((and (comfy-8bitp w) (comfy-8bitp (- l 2)))
           (comfy-gen image w)
           (comfy-gen image cond)
           (comfy-gen image (- l 2))
           (comfy-gen image (comfy-inv cond)))
          ((comfy-8bitp (- l 3))
           (comfy-genbrc image cond (comfy-genbr image win) lose))
          (t
           (comfy-genbrc image cond win (comfy-genbr image lose))))))

(defun comfy-ogen (image op address)
  "In IMAGE, put out OP code and ADDRESS into stream.
Put out only one byte address, if possible."
  (let* ((ha (lsh address -8))
         (la (logand address #xFF)))
    (cond ((= ha 0)
           (comfy-gen image la)
           (comfy-gen image op))
          (t
           (comfy-gen image ha)
           (comfy-gen image la)
           (comfy-gen image (+ op 8))))))

(defun comfy-skeleton (op)
  "Return the skeleton of the op code OP.
The \"skeleton\" property of op contains either the code for
\"accumulator\" (groups 0,2) or \"immediate\" (1) addressing."
  (logand (comfy--get op 'skeleton) #xE3))

(defun comfy-emit (image i win)
  "In IMAGE, place I into the stream with success continuation WIN.
I is an unconditional instruction."
  (cond ((not (= win (comfy-f image)))
         (comfy-emit image i (comfy-genbr image win)))
        ;; atom is a single character instruction.
        ((symbolp i)
         (comfy-gen image (comfy--get i 'skeleton)))
        ;; no op code indicates a subroutine call.
        ((null (cdr i))
         (comfy-gen image 0)
         (comfy-gen image 0)
         (comfy-gen image comfy-jsr)
         (comfy-ra image (comfy-f image) (eval (car i))))
        ;; "a" indicates the accumulator.
        ((eq (cadr i) 'a)
         (comfy-emit image (car i) win))
        ;; "s" indicates the stack.
        ((eq (cadr i) 's)
         (comfy-gen image (+ (comfy-skeleton (car i)) #x18)))
        ;; length=2 indicates absolute addressing.
        ((= (length i) 2)
         (comfy-ogen image
                     (+ (comfy-skeleton (car i)) #x04)
                     (eval (cadr i))))
        ;; "i" indicates absolute indexed by i.
        ((eq (cadr i) 'i)
         (comfy-ogen image
                     (+ (comfy-skeleton (car i)) #x14)
                     (eval (cadr (cdr i)))))
        ;; "j" indicates absolute indexed by j.
        ;; this cannot be optimized for page zero addresses.
        ((eq (cadr i) 'j)
         (comfy-gen image 0)
         (comfy-gen image 0)
         (comfy-gen image (+ (comfy-skeleton (car i)) #x18))
         (comfy-ra image (comfy-f image) (eval (cadr (cdr i)))))
        ;; "\#" indicates immediate operand.
        ((eq (cadr i) '\#)
         (comfy-ogen image
                     (- (comfy--get (car i) 'skeleton) #x08)
                     (logand (eval (cadr (cdr i))) #xFF)))
        ;; "i@" indicates index by i, the indirect.
        ((eq (cadr i) 'i@)
         (comfy-ogen image
                     (comfy-skeleton (car i))
                     (logand (eval (cadr (cdr i))) #xFF)))
        ;; "@j" indicates indirect, then index by j.
        ((eq (cadr i) '@j)
         (comfy-ogen image
                     (+ (comfy-skeleton (car i)) #x10)
                     (logand (eval (cadr (cdr i))) #xFF)))))

(defun comfy-compile (image e win lose)
  "In IMAGE, compile expression E with continuations WIN and LOSE.
WIN and LOSE are both addresses of stuff higher in memory."
  (cond ((numberp e) ; allow constants.
         (comfy-gen image e))
        ((comfy-macrop e)
         (comfy-compile image (apply (comfy--get e 'cmacro) (list e))
                        win lose))
        ((comfy-jumpp e) ; must be return or resume.
         (comfy-gen image (comfy--get e 'jump)))
        ((comfy-actionp e) ; single byte instruction.
         (comfy-emit image e win))
        ((comfy-testp e) ; test instruction
         (comfy-genbrc image (comfy--get e 'test) win lose))
        ((eq (car e) 'not)
         (comfy-compile image (cadr e) lose win))
        ((eq (car e) 'seq)
         (cond ((null (cdr e)) win)
               (t (comfy-compile image (cadr e)
                                 (comfy-compile image (cons 'seq (cddr e))
                                                win lose)
                                 lose))))
        ((eq (car e) 'loop)
         (let* ((l (comfy-genbr image 0))
                (r (comfy-compile image (cadr e) l lose)))
           (comfy-ra image l r)
           r))
        ((numberp (car e)) ; duplicate n times.
         (cond ((zerop (car e)) win)
               (t (comfy-compile image (cons (1- (car e)) (cdr e))
                                 (comfy-compile image (cadr e) win lose)
                                 lose))))
        ((eq (car e) 'if) ; if-then-else.
         (comfy-compile image (cadr e)
                        (comfy-compile image (cadr (cdr e)) win lose)
                        (comfy-compile image (cadddr e) win lose)))
        ((eq (car e) 'while) ; do-while.
         (let* ((l (comfy-genbr image 0))
                (r (comfy-compile image (cadr e)
                                  (comfy-compile image (cadr (cdr e)) l lose)
                                  win)))
           (comfy-ra image l r)
           r))
        ;; allow for COMFY macros !
        ((comfy-macrop (car e))
         (comfy-compile image (apply (comfy--get (car e) 'cmacro) (list e))
                        win lose))
        (t
         (comfy-emit image e win))))
;;;; Language
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
      (sublis (list (cons 'pushes (comfy--genpush pl))
                    (cons 'p p)
                    (cons 'n (length pl)))
              '(seq (seq . pushes)
                    (p)
                    (li s)
                    ;; Next line was: (land ii) --mkp
                    (n i+1)
                    (sti s))))))

(comfy--put
 'lambda
 'cmacro
 '(lambda (e)
    (let* ((pl (cadr e))
           (body (cddr e)))
      (sublis (list (cons 'body body)
                    (cons 'xchs (comfy-genxchs pl))
                    (cons 'moves (comfy-genmoves pl)))
              '(seq (li s)
                    (seq . xchs)
                    (seq . body)
                    (li s)
                    (seq . moves)
                    (return))))))

(defun comfy-genxchs (pl)
  "Generate xch items for parameter list PL.
Supports the lambda cmacro."
  (cond ((null pl) pl)
        (t (cons (list 'xch
                       (list 'i
                             (+ #x102 (length pl)))
                       (list (car pl)))
                 (comfy-genxchs (cdr pl))))))

(defun comfy-genmoves (pl)
  "Generate move items for parameter list PL.
Supports the lambda cmacro."
  (cond ((null pl) nil)
        (t (cons (list 'move
                       (list 'i
                             (+ #x102 (length pl)))
                       (list (car pl)))
                 (comfy-genmoves (cdr pl))))))

(defun comfy--genpush (pl)
  "Generate load and push instructions for each address in list PL.
Supports the call cmacro by copying parameters to the stack."
  (cond ((null pl) pl)
        (t (let* ((p (car pl)))
             (append `((l ,p) push) (comfy-genpush (cdr pl)))))))

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

(defun comfy-constantp (x)
  (atom x))

(defun comfy-variablep (x)
  (and (consp x) (eq (car x) '\,)))

(defmacro comfy-cases (&rest a)
  `(quote
    ,(catch 'comfy-cases
       (comfy-fapplyl (cdr a)
                      (eval (car a))
                      '(lambda () (throw 'comfy-cases nil))))))

(defun comfy-fapplyl (fl a fail)
  ;; "fail" is a function which is executed if comfy-fapplyl fails.
  ;; "fail" had better not return.
  (cond ((null fl) (funcall fail))
        (t (catch 'comfy-fapplyl
             (comfy-fapply (car fl) a
                           '(lambda ()
                              (throw 'comfy-fapplyl
                                     (comfy-fapplyl (cdr fl) a fail))))))))

(defun comfy-fapply (f a fail)
  (let* ((alist (comfy-match (cadr f) a fail nil)))
    (apply (cons 'lambda
                 (cons (mapcar 'car alist)
                       (cddr f)))
           (mapcar 'cdr alist))))

(defmacro comfy-define (ind patt &rest body)
  "Place a macro in the IND property of a symbol extracted from PATT;
macro defined by BODY.

PATT can be a symbol or a list with a symbol as its first
element.  PATT is used as the parameter list of a lambda."
  (let* ((where (cond ((atom patt) patt)
                      ((atom (car patt)) (car patt)))))
    (or (comfy--get where ind)
        (comfy--put where ind '(lambda (e) (comfy-cases e))))
    (comfy--put
     where
     ind
     `(lambda (e)
        ,(append `(comfy-cases e ,(append `(lambda ,patt) body))
                 (cddr (cadr (cdr (comfy--get where ind)))))))
    nil))


(comfy--put 'star 'cmacro nil)

(comfy-define cmacro (star . ,body)
              `(not (loop ,(append '(seq) body))))

(comfy--put 'i2 'cmacro nil)

(comfy-define cmacro (i2 ,p)
              `(seq (1+ ,p)
                    (if =0? (1+ (1+ ,p))
                      (seq))))

(comfy--put 'move 'cmacro nil)

(comfy-define cmacro (move ,x ,y)
              `(seq ,(append '(l) x)
                    ,(append '(st) y)))
(comfy--put 'prog 'cmacro nil)

(comfy-define cmacro (prog (,v) . ,body)
              `(seq push
                    (li s)
                    (move (,v) (i #x101))
                    ,(append '(seq) body)
                    (li s)
                    (move (i #x101) (,v))
                    i-1
                    (sti s)))

(comfy--put 'fori 'cmacro nil)

(comfy-define cmacro (fori ,from ,to . ,body)
              `(seq ,(append '(li) from)
                    (while (seq ,(append '(ci) to) llt)
                      (seq ,(append '(seq) body) i+1))))

(comfy--put 'forj 'cmacro nil)

(comfy-define cmacro (forj ,from ,to . ,body)
              `(seq ,(append '(lj) from)
                    (while (seq ,(append '(cj) to) llt)
                      (seq ,(append '(seq) body) j+1))))

(comfy--put 'for 'cmacro nil)

(comfy-define cmacro (for ,v ,from ,to . ,body)
              `(seq ,(append '(l) from) ,(append '(st) v)
                    (while (seq ,(append '(c) to) llt)
                      (seq ,(append '(seq) body)
                           ,(append '(1+) v)
                           ,(append '(l) v)))))
;;;; Feature
(provide 'comfy)
;;; comfy.el ends here
