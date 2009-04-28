;;; lisp-re.el --- Transform REs written in a Lisp like notation to strings

;; Copyright (C) 1999 by Detlev Zundel

;; Author: Detlev Zundel <Detlev.Zundel@stud.uni-karlsruhe.de>
;; Keywords: matching, lisp

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $Id: lisp-re.el,v 1.1 1999/12/31 18:54:54 dzu Exp $

;; The `flat string representation' for a regular expression can
;; sometimes be complicated when a notation allowing for clearer
;; grouping and more easily remembered operators could be
;; `structurally clearer'.

;; For a Lisp programmer clearly such a notation is the Lisp list
;; syntax.  The functions below allow to toy with such a notation for
;; regular expressions which was inspired by the REs in the `RScheme'
;; system.  They are basically a front end to the string
;; representation used in ELisp.  (Although one could conceive
;; different backends for other (slightly) different syntaxes, as
;; for example the Perl RE syntax)

;; REs can be specified with the following `function' symbols:
;;
;; (seq <e1> ...)     - Sequence of expressions
;; (or  <e1> ...)     - Alternatives
;; (+ <e>)            - One or more occurrences of <e>
;; (\? <e>)           - Zero or one occurrences of <e>
;; (* <e>)            - Zero or more occurrences of <e>
;; (save <e>)         - Save occurrence of <e> for reference and returning
;; (ref <num>)        - Occurrence of <num>th `save'd expression
;; (syntax <s>)       - Character of syntax <s>
;; (not-syntax <s>)   - Character of syntax other than <s>
;; (range <f> <t>)    - Range between <f> and <t>
;; (not <r>)          - Complement of range <r>
;; (cclass <c1> ..)   - Character class allowing <c1> ... (either chars
;;                     or ranges)
;;
;; If you like more verbose constructs one-or-more, zero-or-one and
;; zero-or-more can be used for the postfix operators.

;;; The following symbols can be used as expressions: (zw = zero-width)

;; any           - Any character
;; eol           - End of line (zw)
;; bol           - Beginning of line (zw)
;; bob           - Beginning of buffer (zw)
;; eob           - End of Buffer (zw)
;; wordlimit     - Beginning or end of a word (zw)
;; not-wordlimit - Not the beginning or end of a word (zw)
;; bow           - Beginning of word (zw)
;; eow           - End of word (zw)
;; wordchar      - Word character
;; non-wordchar  - Non-word character
;; point         - Position of point (zw)
;; cr            - Carriage return

;;; Also the following predefined character classes are available:
;; digit, alpha, uppercase, lowercase, hexdigit, printable and  space

;; As the RE is a normal list you can insert comments just like in any
;; other Lisp code - without the need to `extend' the syntax.

;; This notation introduces a problem with the sub-expressions in REs.
;; Because a `(one-or-more <string>)' construct needs to group the
;; <string> (if it is longer than one character) to be sensible, there
;; might be `unwanted' subexpressions, i.e. groups not resulting from
;; `save' constructs, in the string representation.  In other words
;; the mapping of the lispy REs to string REs is not injective.  (In
;; Perl syntax it could be because Perl has got the `(?' to group
;; without allowing references to the group)

;; To cope with this the `lre-compile-stringmatcher' and
;; `lre-compile-buffermatcher' functions return lambda expressions
;; that handle the actual matching and return a list of the whole
;; matched string and the `save'd parts respectively.  This insulating
;; layer makes the mapping lispy RE to lambdas injective.

;; If you do not want to use them you can still call
;; `lre-compile-string' and evaluate the value of `lre-match-list'
;; afterwards.  It is a list of the numbers of the subexpressions
;; corresponding to the `save's.

;; Examples:

;; The notorious (hopelessly inefficient but nevertheless elegant)
;; primality test via regexps can thus be written as follows:

; (defun is-prime (n)
;   (not (funcall
;       (lre-compile-stringmatcher
;        '(seq bol
;              (save any (one-or-more any))
;              (one-or-more (ref 1))
;              eol))
;       (concat (make-vector n ?1)))))

;;; Code:

(defvar lre-symbols
  '((any .".")
    (eol . "$")
    (bol . "^")
    (bob . "\\`")
    (eob . "\\'")
    (wordlimit . "\\b")
    (not-wordlimit . "\\B")
    (bow . "\\<")
    (eow . "\\>")
    (wordchar . "\\w")
    (non-wordchar . "\\W")
    (point . "\\=")
    (cr . "\\n"))
  "Symbols known by the regexp compiler.")

(defvar lre-classes
  '((digit . "[0-9]")
    (alpha . "[a-zA-Z]")
    (uppercase . "[A-Z]")
    (lowercase . "[a-z]")
    (hexdigit . "[0-9a-fA-F]")
    (printable . "[ -~]")
    (space . "[ \t]"))
  "Character classes known by the regexp compiler.")

(defvar lre-match-list nil
  "Internal list for `lre-compile'.")
(defvar lre-match-pos 0
  "Internal counter for `lre-compile'.")

(defun lre-compile-stringmatcher (exp)
  "Compile an regular expression in lisp-like format to a lambda form.
The resulting lambda form takes one string argument to match with the
regular expression.  Return nil if no match was found and a list with
n+1 entries where n is the number of `save' expressions in EXP.
The list contains the whole match and the `save'd parts of it."

  (setq lre-match-list nil
        lre-match-pos 0)
  (let ((re (cdr (lre-compile-internal exp))))
    `(lambda (string)
       (if (string-match ,re string)
           (list (match-string 0 string)
                 ,@(mapcar (lambda (num) (list 'match-string num 'string))
                           lre-match-list))))))

(defun lre-compile-buffermatcher (exp)
  "Compile an regular expression in lisp-like format to a lambda form.
The resulting lambda form takes no arguments and searches in the
current buffer from point onwards.  Return nil if no match was found
and a list with n+1 entries where n is the number of `save'
expressions in EXP.  The list contains the whole match and the `save'd
parts of it."

  (setq lre-match-list nil
        lre-match-pos 0)
  (let ((re (cdr (lre-compile-internal exp))))
    `(lambda ()
       (if (re-search-forward ,re (point-max) t)
           (list (match-string 0)
                 ,@(mapcar (lambda (num) (list 'match-string num))
                           lre-match-list))))))

(defun lre-compile-string (exp)
  "Compile the regular expression EXP in lisp-like format to a string."

  (setq lre-match-list nil
        lre-match-pos 0)
  (lre-compile-internal-string exp))

(defmacro lre-compile-macro (exp)
  "Macro version of `lre-compile-string'."

  (lre-compile-string exp))

;; Below `primary' in contrast to `compound' parts of an RE need no
;; further grouping for the postfix operators.

(defun lre-prim (exp)
  "Constructor for a primary expression EXP in lisp-re."
  (cons 'primitive exp))

(defun lre-prim-p (exp)
  "Predicate testing whether EXP is a primary expression in lisp-re."
  (eq (car exp) 'compound))

(defun lre-comp (exp)
  "Constructor for a compound expression EXP in lisp-re."
  (cons 'compound exp))

(defun lre-comp-p (exp)
  "Predicate testing whether EXP is a compund expression in lisp-re."
  (eq (car exp) 'compound))

(defun lre-comp-group-if-nec (exp)
  "Compile EXP and group if necessary.  Also adjust `saved' positions."

  (let* ((oldmatch-pos lre-match-pos)
         (parsedarg (lre-compile-internal exp)))
    (if (lre-comp-p parsedarg)
        (progn
          (lre-advance-matches oldmatch-pos)
          (setq lre-match-pos (1+ lre-match-pos))
          (concat "\\(" (cdr parsedarg) "\\)"))
      (cdr parsedarg))))

(defun lre-advance-matches (from)
  "Shift current match positions starting at FROM."

  (let ((list (nthcdr from lre-match-list)))
    (while list
      (setcar list (1+ (car list)))
      (setq   list (cdr list)))))

(defun lre-char-or-class (exp)
  "If EXP is a symbol return character class.  Return string otherwise."

  (cond
   ((symbolp exp)
    (let ((class (cdr (assoc exp lre-classes))))
      (if class
          (substring class 1 (1- (length class)))
        (error "Invalid character class: `%s'" exp))))
   ((listp exp)
    (let ((parsedarg (lre-compile-string exp)))
      (if (eq (aref parsedarg 0) (aref "[" 0))
          (substring parsedarg 1 (1- (length parsedarg)))
        (error "Invalid character class: `%s'" exp))))
   (t (char-to-string exp))))

(defun lre-compile-internal-string (exp)
  "Compile a regular expression EXP in lisp-like format to a string."

  (cdr (lre-compile-internal exp)))

(defun lre-compile-internal (exp)
  "Internal helper for `lre-compile' compiling EXP."

  (cond ((null exp)
         (lre-prim ""))
        ((symbolp exp)
         (lre-prim (or (cdr (assoc exp lre-symbols))
                       (cdr (assoc exp lre-classes))
                       (error "Invalid symbol: `%s'" exp))))
        ((integerp exp)
         (lre-prim (char-to-string exp)))
        ((stringp exp)
         (if (> (length exp) 1)
             (lre-comp exp)
           (lre-prim exp)))
        ((listp exp)
         (let ((op (car exp))
               (arg (cadr exp)))
           (cond
            ((eq op 'seq)
             (lre-comp
              (mapconcat 'lre-compile-internal-string (cdr exp) "")))
            ((eq op 'or)
             (lre-comp (mapconcat
                        'lre-compile-internal-string (cdr exp) "\\|")))
            ((eq op 'save)
             (setq lre-match-pos (1+ lre-match-pos)
                   lre-match-list (append lre-match-list
                                          (list lre-match-pos)))
             (lre-prim
              (concat "\\("
                      (lre-compile-internal-string (cons 'seq (cdr exp)))
                      "\\)")))
            ((eq op 'ref)
             (lre-prim
              (concat "\\" (or (nth (1- arg) lre-match-list)
                               (error "Invalid backward reference to `%d'"
                                      arg)))))
            ((memq op '(+ one-or-more))
             (lre-comp (concat (lre-comp-group-if-nec arg) "+")))
            ((memq op '(\? zero-or-one))
             (lre-comp (concat (lre-comp-group-if-nec arg) "?")))
            ((memq op '(* zero-or-more))
             (lre-comp (concat (lre-comp-group-if-nec arg) "*")))
            ((eq op 'syntax)
             (lre-prim (concat "\\s" arg)))
            ((eq op 'not-syntax)
             (lre-prim (concat "\\S" arg)))
            ((eq op 'range)
             (let ((from arg)
                   (to (caddr exp)))
               (lre-prim
                (concat "["
                        (if (stringp from) from (char-to-string from))
                        "-"
                        (if (stringp to) to (char-to-string to))
                        "]"))))
            ((eq op 'cclass)
             (lre-prim (concat "["
                              (mapconcat 'lre-char-or-class (cdr exp) "")
                              "]")))
            ((eq op 'not)
             (let ((class (lre-compile-internal-string arg)))
               (if (eq (aref class 0) (aref "[" 0))
                 (lre-prim (concat "[^" (substring class 1)))
                 (error "Invalid character class: `%s'" arg))))
            (t (error "Invalid regexp-function: `%s'" op)))))))

;;; lisp-re.el ends here
