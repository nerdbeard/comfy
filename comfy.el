(defun sublis (a e)
  (cond ((atom e)
         (let* ((binding (assq e a)))
           (cond (binding (cdr binding))
                 (t e))))
        (t (cons (sublis a (car e))
                 (sublis a (cdr e))))))

(defun cadr (x) (car (cdr x)))

(defun cddr (x) (cdr (cdr x)))

(defun caddr (x) (car (cdr (cdr x))))

(defun cadddr (x) (car (cdr (cdr (cdr x)))))

;;; Basic test instructions.
(put 'c=1\? 'test 176)       ;;; test carry=1.
(put 'c=0\? 'test 144)       ;;; test carry=0.
(put 'llt 'test 144)         ;;; logically less than.
(put 'lge 'test 176)         ;;; logically greater than or equal.
(put '=\? 'test 240)         ;;; equal.
(put '~=\? 'test 208)        ;;; not equal.
(put '=0\? 'test 240)        ;;; equals zero.
(put '~=0\? 'test 208)       ;;; not equal to zero.
(put 'v=1\? 'test 112)       ;;; test overflow=1.
(put 'v=0\? 'test 80)        ;;; test overflow=0.
(put '<\? 'test 48)          ;;; test arithmetic less than.
(put '>=\? 'test 16)         ;;; test arithmetic greater than or equal.
(put '<0\? 'test 48)         ;;; test arithmetic less than zero.
(put '>=0\? 'test 16)        ;;; test airthmetic greater than or equal to zero.

;;; Group 0.
(put '\? 'skeleton 32)       ;;; test.
(put 'stj 'skeleton 152)     ;;; store j.
(put 'lj 'skeleton 168)      ;;; load j.
(put 'cj 'skeleton 200)      ;;; compare j.
(put 'ci 'skeleton 232)      ;;; compare i.
;;; Group 1.
(put 'lor 'skeleton 17)      ;;; logical or.
(put 'land 'skeleton 49)     ;;; logical and.
(put 'lxor 'skeleton 81)     ;;; logical xor.
(put '+ 'skeleton 113)       ;;; add with carry.
(put 'st 'skeleton 145)      ;;; store accumulator.
(put 'l 'skeleton 177)       ;;; load accumulator.
(put 'c 'skeleton 209)       ;;; compare accumulator.
(put '- 'skeleton 241)       ;;; subtract with borrow.
;;; Group 2.
(put 'asl 'skeleton 10)      ;;; arithmetic shift left.
(put 'rl 'skeleton 42)       ;;; rotate left.
(put 'lsr 'skeleton 74)      ;;; logical shift right.
(put 'rr 'skeleton 106)      ;;; rotate right.
(put 'sti 'skeleton 138)     ;;; store i.
(put 'li 'skeleton 170)      ;;; load i.
(put '1- 'skeleton 194)      ;;; decrement.
(put '1+ 'skeleton 226)      ;;; increment.
;;; random instructions.
(put 'trap 'skeleton 0)      ;;; programmed break.
(put 'save 'skeleton 8)      ;;; push processor state onto stack.
(put 'restore 'skeleton 40)  ;;; restore processor state from stack.
(put 'push 'skeleton 72)     ;;; push accumulator onto stack.
(put 'pop 'skeleton 104)     ;;; pop accumulator from stack.
(put 'c=0 'skeleton 24)      ;;; clear carry.
(put 'c=1 'skeleton 56)      ;;; set carry.
(put 'seb 'skeleton 24)      ;;; set borrow.
(put 'clb 'skeleton 56)      ;;; clear borrow.
(put 'v=0 'skeleton 184)     ;;; clear overflow.
(put 'enable 'skeleton 88)   ;;; enable interrupts.
(put 'disable 'skeleton 120) ;;; disable interrupts.
(put 'binary 'skeleton 216)  ;;; set binary mode.
(put 'decimal 'skeleton 248) ;;; set decimal mode.
(put 'i+1 'skeleton 232)     ;;; increment i.
(put 'j+1 'skeleton 200)     ;;; increment j.
(put 'i-1 'skeleton 202)     ;;; decrement i.
(put 'j-1 'skeleton 136)     ;;; decrement j.
(put 'nop 'skeleton 234)     ;;; no operation.

(defvar jmp 76)
(setq jmp 76)
(defvar jsr 32)
(setq jsr 32)
(put 'return 'jump 96)
(put 'resume 'jump 64)

;;; Numbers in Emacs Lisp are always decimal.

(defvar mem (make-vector 10 0)
  "Vector where the compiled code is placed.")
(setq mem (make-vector 100 0))

(defvar f (length mem)
  "Compiled code array pointer; it works its way down from the top.")
(setq f (length mem))

(defun init ()
  (fillarray mem 0)
  (setq f (length mem)))

(defun subseq (v i)
  (let* ((nv (make-vector (- (length v) i) nil))
         (j 0))
    (while (< j (length nv))
      (aset nv j (elt v (+ i j)))
      (setq j (1+ j)))
    nv))

(defun gen (obj)
  ;;; place one character "obj" into the stream.
  (setq f (1- f))
  (aset mem f obj)
  f)

(defun testp (e)
  ;;; predicate to tell whether "e" is a test.
  (and (symbolp e) (get e 'test)))

(defun actionp (e)
  ;;; predicate to tell whether "e" is an action.
  (and (symbolp e) (not (get e 'test))))

(defun jumpp (e)
  ;;; predicate to tell whether "e" is a jump-type action.
  (and (symbolp e) (get e 'jump)))

(defun macrop (x)
  (and (symbolp x) (get x 'cmacro)))

(defun ra (b a)
  ;;; replace the absolute address at the instruction "b"
  ;;; by the address "a".
  (let* ((ha (lsh a -8))
         (la (logand a 255)))
    (aset mem (1+ b) la)
    (aset mem (+ b 2) ha))
  b)

(defun inv (c)
  ;;; invert the condition for a branch.
  ;;; invert bit 5 (counting from the right).
  (logxor c 32))

(defun genbr (win)
  ;;; generate an unconditional jump to "win".
  (gen 0) (gen 0) (gen jmp) (ra f win))

(defun 8bitp (n)
  (let* ((m (logand n -128)))
    (or (= 0 m) (= -128 m))))

(defun genbrc (c win lose)
  ;;; generate an optimized conditional branch
  ;;; on condition c to "win" with failure to "lose".
  (let* ((w (- win f)) (l (- lose f)))   ;;; Normalize to current point.
    (cond ((= w l) win)
          ((and (= l 0) (8bitp w)) (gen w) (gen c))
          ((and (= w 0) (8bitp l)) (gen l) (gen (inv c)))
          ((and (8bitp l) (8bitp (- w 2)))
           (gen l) (gen (inv c)) (gen (- w 2)) (gen c))
          ((and (8bitp w) (8bitp (- l 2)))
           (gen w) (gen c) (gen (- l 2)) (gen (inv c)))
          ((8bitp (- l 3)) (genbrc c (genbr win) lose))
          (t (genbrc c win (genbr lose))))))

(defun ogen (op a)
  ;;; put out address and op code into stream.
  ;;; put out only one byte address, if possible.
  (let* ((ha (lsh a -8))
         (la (logand a 255)))
    (cond ((= ha 0) (gen la) (gen op))
          (t (gen ha) (gen la) (gen (+ op 8))))))

(defun skeleton (op)
  ;;; return the skeleton of the op code "op".
  ;;; the "skeleton" property of op contains either
  ;;; the code for "accumulator" (groups 0,2) or "immediate" (1) addressing.
  (logand (get op 'skeleton) 227))

(defun emit (i win)
  ;;; place the unconditional instruction "i" into the stream with
  ;;; success continuation "win".
  (cond ((not (= win f)) (emit i (genbr win)))
        ;;; atom is a single character instruction.
        ((symbolp i) (gen (get i 'skeleton)))
        ;;; no op code indicates a subroutine call.
        ((null (cdr i))
         (gen 0) (gen 0) (gen jsr) (ra f (eval (car i))))
        ;;; "a" indicates the accumulator.
        ((eq (cadr i) 'a) (emit (car i) win))
        ;;; "s" indicates the stack.
        ((eq (cadr i) 's)
         (gen (+ (skeleton (car i)) 24)))
        ;;; length=2 indicates absolute addressing.
        ((= (length i) 2)
         (ogen (+ (skeleton (car i)) 4)
               (eval (cadr i))))
        ;;; "i" indicates absolute indexed by i.
        ((eq (cadr i) 'i)
         (ogen (+ (skeleton (car i)) 20) (eval (caddr i))))
        ;;; "j" indicates absolute indexed by j.
        ;;; this cannot be optimized for page zero addresses.
        ((eq (cadr i) 'j)
         (gen 0) (gen 0) (gen (+ (skeleton (car i)) 24))
         (ra f (eval (caddr i))))
        ;;; "\#" indicates immediate operand.
        ((eq (cadr i) '\#)
         (ogen (- (get (car i) 'skeleton) 8)
               (logand (eval (caddr i)) 255)))
        ;;; "i@" indicates index by i, the indirect.
        ((eq (cadr i) 'i@)
         (ogen (skeleton (car i))
               (logand (eval (caddr i)) 255)))
        ;;; "@j" indicates indirect, then index by j.
        ((eq (cadr i) '@j)
         (ogen (+ (skeleton (car i)) 16)
               (logand (eval (caddr i)) 255)))))

(defun compile (e win lose)
  ;;; compile expression e with success continuation "win" and
  ;;; failure continuation "lose".
  ;;; "win" an "lose" are both addresses of stuff higher in memory.
  (cond ((numberp e) (gen e))           ; allow constants.
        ((macrop e)
         (compile (apply (get e 'cmacro) (list e)) win lose))
        ((jumpp e) (gen (get e 'jump))) ; must be return or resume.
        ((actionp e) (emit e win))      ; single byte instruction.
        ((testp e) (genbrc (get e 'test) win lose)) ; test instruction
        ((eq (car e) 'not) (compile (cadr e) lose win))
        ((eq (car e) 'seq)
         (cond ((null (cdr e)) win)
               (t (compile (cadr e)
                           (compile (cons 'seq (cddr e)) win lose)
                           lose))))
        ((eq (car e) 'loop)
         (let* ((l (genbr 0))
                (r (compile (cadr e) l lose)))
           (ra l r)
           r))
        ((numberp (car e))              ; duplicate n times.
         (cond ((zerop (car e)) win)
               (t (compile (cons (1- (car e)) (cdr e))
                           (compile (cadr e) win lose)
                           lose))))
        ((eq (car e) 'if)               ; if-then-else.
         (compile (cadr e)
                  (compile (caddr e) win lose)
                  (compile (cadddr e) win lose)))
        ((eq (car e) 'while)            ; do-while.
         (let* ((l (genbr 0))
                (r (compile (cadr e)
                            (compile (caddr e) l lose)
                            win)))
           (ra l r)
           r))
        ;;; allow for COMFY macros !
        ((macrop (car e))
         (compile (apply (get (car e) 'cmacro) (list e)) win lose))
        (t (emit e win))))

(put
 'alt
 'cmacro
 '(lambda (e)
    ;;; define the dual of "seq" using DeMorgan's law.
    (list 'not
          (cons 'seq
                (mapcar '(lambda (e) (list 'not e))
                        (cdr e))))))

(put
 'call
 'cmacro
 '(lambda (e)
    (let* ((p (cadr e)) (pl (cddr e)))
      (sublis (list (cons 'pushes (genpush pl))
                    (cons 'p p)
                    (cons 'n (length pl)))
              '(seq (seq . pushes)
                    (p)
                    (li s)
                    (land ii)
                    (sti s))))))

(put
 'lambda
 'cmacro
 '(lambda (e)
    (let* ((pl (cadr e))
           (body (cddr e)))
      (sublis (list (cons 'body body)
                    (cons 'xchs (genxchs pl))
                    (cons 'moves (genmoves pl)))
              '(seq (li s)
                    (seq . xchs)
                    (seq . body)
                    (li s)
                    (seq . moves)
                    (return))))))

(defun genxchs (pl)
  (cond ((null pl) pl)
        (t (cons (list 'xch (list 'i (+ 258 (length pl))) (list (car pl)))
                 (genxchs (cdr pl))))))

(defun genmoves (pl)
  (cond ((null pl) nil)
        (t (cons (list 'move (list 'i (+ 258 (length pl))) (list (car pl)))
                 (genmoves (cdr pl))))))

(defun genpush (pl)
  (cond ((null pl) pl)
        (t (let* ((p (car pl)))
             (append (` ((l (, p)) push)) (genpush (cdr pl)))))))

(defun match (p e f alist)
  ;;; f is a function which is executed if the match fails.
  ;;; f had better not return.
  (cond ((constantp p)
         (cond ((eq p e) alist)
               (t (funcall f))))
        ((variablep p) (cons (cons (cadr p) e) alist))
        ((eq (car p) 'quote) (cond ((eq (cadr p) e) alist)
                                   (t (funcall f))))
        ((predicate p) (cond ((funcall (cadr p) e) alist)
                             (t (funcall f))))
        ((atom e) (funcall f))
        (t (match (car p)
                  (car e)
                  f
                  (match (cdr p)
                         (cdr e)
                         f
                         alist)))))

(defun predicate (x)
  (and (consp x) (eq (car x) 'in)))

(defun constantp (x) (atom x))

(defun variablep (x)
  (and (consp x) (eq (car x) '\,)))

(defun constantp (x) (atom x))

(defmacro cases (&rest a)
  (` (quote
      (, (catch 'cases
           (fapplyl (cdr a)
                    (eval (car a))
                    '(lambda () (throw 'cases nil))))))))

(defun fapplyl (fl a fail)
  ;;; "fail" is a function which is executed if fapplyl fails.
  ;;; "fail" had better not return.
  (cond ((null fl) (funcall fail))
        (t (catch 'fapplyl
             (fapply (car fl) a
                     '(lambda ()
                        (throw 'fapplyl
                               (fapplyl (cdr fl) a fail))))))))

(defun fapply (f a fail)
  (let* ((alist (match (cadr f) a fail nil)))
    (apply (cons 'lambda
                 (cons (mapcar 'car alist)
                       (cddr f)))
           (mapcar 'cdr alist))))

(defmacro define (&rest a)
  (let* ((ind (car a))
         (patt (cadr a))
         (body (cddr a))
         (where (cond ((atom patt) patt)
                      ((atom (car patt)) (car patt)))))
    (or (get where ind) (put where ind '(lambda (e) (cases e))))
    (put
     where
     ind
     (` (lambda (e)
          (, (append (` (cases e (, (append (` (lambda (, patt))) body))))
                     (cddr (caddr (get where ind))))))))
    nil))

(put 'star 'cmacro nil)

(define cmacro (star . (, body))
  (` (not (loop (, (append '(seq) body))))))

(put 'i2 'cmacro nil)

(define cmacro (i2 (, p))
  (` (seq (1+ (, p))
          (if =0\? (1+ (1+ (, p)))
            (seq)))))

(put 'move 'cmacro nil)

(define cmacro (move (, x) (, y))
  (` (seq (, (append '(l) x))
          (, (append '(st) y)))))

(put 'prog 'cmacro nil)

(define cmacro (prog ((, v)) . (, body))
  (` (seq push
          (li s)
          (move ((, v)) (i 257))
          (, (append '(seq) body))
          (li s)
          (move (i 257) ((, v)))
          i-1
          (sti s))))

(put 'fori 'cmacro nil)

(define cmacro (fori (, from) (, to) . (, body))
  (` (seq (, (append '(li) from))
          (while (seq (, (append '(ci) to)) llt)
            (seq (, (append '(seq) body)) i+1)))))

(put 'forj 'cmacro nil)

(define cmacro (forj (, from) (, to) . (, body))
  (` (seq (, (append '(lj) from))
          (while (seq (, (append '(cj) to)) llt)
            (seq (, (append '(seq) body)) j+1)))))

(put 'for 'cmacro nil)

(define cmacro (for (, v) (, from) (, to) . (, body))
  (` (seq (, (append '(l) from)) (, (append '(st) v))
          (while (seq (, (append '(c) to)) llt)
            (seq (, (append '(seq) body))
                 (, (append '(1+) v))
                 (, (append '(l) v)))))))

(setq typical '(seq c=0 binary enable v=0))

(setq typ1 '(alt (seq <0\? (1+ m))
                 (1- b))
      m 100
      b 101)

(setq typ2 '(not (loop (seq <0\? (1+ m)))))

(setq typ3 '(star (seq (li s) ~=0\? pop)))

(setq typ3 '(not (loop (seq (li s) ~=0\? pop))))

(setq incr
      '(seq (li \# 0)
            c=1
            (while c=1\?
              (seq (l i n) (+ \# 0) (st i n))))
      n 32)

(setq sum
      '(seq (l \# 0)
            li
            (while (seq (ci n) ~=\?)
              (seq (+ i A) i+1)))
      A 29)

(setq mult
      '(seq (l x) (st xp)
            (l y) (st y0p)
            (l \# 0) (st y1p) (st z0) (st z1)
            (while (seq (lsr xp) ~=\?)
              (seq c=0 (l z0) (+ y0p) (st z0)
                   (l z1) (+ y1p) (st z1)
                   (asl y0p) (rl y1p)))))

(setq x 1 xp 2
      y 3 y0p 4
      y1p 5 z1 6
      z0 7)

(setq masktable
      '(compile '(seq 128 64 32 16 8 4 2 1) 0 0)
      p 8)

(setq testb
      '(seq push
            (land \# 7)
            (li a)
            pop
            (3 lsr)
            (lj a)
            (l @j p)
            (land i masktable)
            return))

(setq setb
      '(seq push
            (land \# 7)
            (li a)
            pop
            (3 lsr)
            (lj a)
            (l @j p)
            (lor i masktable)
            (st @j p)
            return))

;;; Routines for the Universal Product Code Wand.  Requires 88 bytes+table.
(setq upctable
      (compile '(seq 13 25 19 61 35 49 47 59 55 11) 0 0))

(setq
 code 11
 digit 12
 mq 13
 upcwand
 '(alt
   (seq (fori (\# 6) (\# 12)            ; complement patterns of right 6 upc digits.
              (l i code)
              (lxor \# 127)
              (st i code))
        (fori (\# 0) (\# 12)
              (l i code)
              (not
               (forj (\# 0) (\# 10)
                     (c j upctable)
                     ~=\?))             ; fail if equal.
              (stj i digit))            ; store index of upctable.
        decimal                         ; set decimal arithmetic mode.
        (l \# 0)                        ; clear ac.
        (fori (\# 0) (\# 12)            ; add up the even digits.
              (+ i digit)               ; loop control clears carry!
              i+1)                      ; only every other one.
        (st mq)                         ; save partial sum.
        c=0                             ; clear the carry.
        (2 (+ mq))                      ; multiply by 3.
        (fori (\# 1) (\# 12)            ; add up the odd digits.
              (+ i digit)               ; loop cotrol clears carry.
              i+1)                      ; only every other one.
        (lxor \# 15)                    ; select low decimal digit.
        =0\?                            ; fails if non-zero.
        return)
   (seq break                           ; signal failure.
        return)))
