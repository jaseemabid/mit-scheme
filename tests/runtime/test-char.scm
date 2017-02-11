#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Test of character abstraction

(declare (usual-integrations))

(define named-chars
  '((#\null #x00)
    (#\alarm #x07)
    (#\backspace #x08)
    (#\tab #x09)
    (#\newline #x0A)
    (#\page #x0C)
    (#\return #x0D)
    (#\escape #x1B)
    (#\space #x20)
    (#\delete #x7F)))

(define-test 'named-chars
  (lambda ()
    (for-each (lambda (entry)
		(let ((char (car entry))
		      (code (cadr entry)))
		  (assert-= code (char->integer char))
		  (assert-eq char (integer->char code))))
	      named-chars)))

(define ascii-chars
  '(#\x00 #\x01 #\x02 #\x03 #\x04 #\x05 #\x06 #\x07
    #\x08 #\x09 #\x0A #\x0B #\x0C #\x0D #\x0E #\x0F
    #\x10 #\x11 #\x12 #\x13 #\x14 #\x15 #\x16 #\x17
    #\x18 #\x19 #\x1A #\x1B #\x1C #\x1D #\x1E #\x1F
    #\x20 #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q
    #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\`
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q
    #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~ #\x7F))

(define-test 'basic-ascii
  (lambda ()
    (for-each basic-char-tests
              ascii-chars
              (iota #x80))))

(define (basic-char-tests char code)
  (assert-true (char? char))
  (assert-eq char (integer->char code))
  (assert-= code (char->integer char)))

(define-test 'valid-utf8-sequences
  (lambda ()
    (for-each (lambda (entry)
                (let ((bytes (car entry))
                      (char (cadr entry)))
		  (let ((n (bytevector-length bytes)))
		    (assert-= n
			      (initial-byte->utf8-char-length
			       (bytevector-u8-ref bytes 0)))
		    (assert-= n
			      (char-utf8-byte-length char))
		    (assert-eq char (decode-utf8-char bytes 0))
		    (do ((offset 0 (+ offset 1)))
			((not (< offset 8)))
		      (let ((bv (make-bytevector 16))
			    (m (+ offset n)))
			(assert-= m (encode-utf8-char! bv offset char))
			(assert-equal bytes (bytevector-copy bv offset m)))))))
              valid-utf8-sequences)))

(define valid-utf8-sequences
  `(
    ,@(map (lambda (char)
	     (list (bytevector (char->integer char)) char))
	   ascii-chars)

    ;; 2.1  First possible sequence of a certain length
    (#u8(#x00) #\x00000000)
    (#u8(#xC2 #x80) #\x00000080)
    (#u8(#xE0 #xA0 #x80) #\x00000800)
    (#u8(#xF0 #x90 #x80 #x80) #\x00010000)

    ;; 2.2  Last possible sequence of a certain length
    (#u8(#x7F) #\x0000007F)
    (#u8(#xDF #xBF) #\x000007FF)
    (#u8(#xEF #xBF #xBD) #\x0000FFFD)
    (#u8(#xEF #xBF #xBF) #\x0000FFFF)

    ;; 2.3  Other boundary conditions
    (#u8(#xED #x9F #xBF) #\x0000D7FF)
    (#u8(#xEE #x80 #x80) #\x0000E000)
    (#u8(#xEF #xBF #xBD) #\x0000FFFD)
    (#u8(#xF4 #x8F #xBF #xBD) #\x0010FFFD)
    (#u8(#xF4 #x8F #xBF #xBF) #\x0010FFFF)

    ;; 5.3 Noncharacter code positions
    ;; Particularly problematic noncharacters in 16-bit applications:
    (#u8(#xEF #xBF #xBE) #\xFFFE)
    (#u8(#xEF #xBF #xBF) #\xFFFF)
    ))

(define-test 'utf8-initial-byte
  (lambda ()
    (for-each (lambda (b)
                (if (memv b invalid-utf8-initial-bytes)
                    (assert-error
                     (lambda () (initial-byte->utf8-char-length b)))
                    (assert-= (cond ((< b #x80) 1)
                                    ((< b #xE0) 2)
                                    ((< b #xF0) 3)
                                    (else 4))
                              (initial-byte->utf8-char-length b))))
              (iota #x100))))

(define invalid-utf8-initial-bytes
  (append (iota (- #xc2 #x80) #x80)
          (iota (- #x100 #xf5) #xF5)))

(define-test 'invalid-known-length-utf8-sequences
  (lambda ()
    (for-each (lambda (entry)
		(let ((bytes (car entry))
		      (length (cadr entry)))
                  (let ((b0 (bytevector-u8-ref bytes 0)))
                    (if (not (memv b0 invalid-utf8-initial-bytes))
                        (assert-= length
                                  (initial-byte->utf8-char-length b0))))
		  (assert-error (lambda () (decode-utf8-char bytes 0)))))
              invalid-known-length-sequences)))

(define invalid-known-length-sequences
  `(
    ;; 3.2  Lonely start characters
    ;; 3.2.1  All 32 first bytes of 2-byte sequences (0xc0-0xdf),
    ;;        each followed by a space character:
    ,@(map (lambda (initial-byte)
             (list (bytevector initial-byte #x20) 2))
           (iota #x20 #xC0))

    ;; 3.2.2  All 16 first bytes of 3-byte sequences (0xe0-0xef),
    ;;        each followed by a space character:
    ,@(map (lambda (initial-byte)
             (list (bytevector initial-byte #x20) 3))
           (iota #x10 #xE0))

    ;; 3.2.3  All 8 first bytes of 4-byte sequences (0xf0-0xf7),
    ;;        each followed by a space character:
    ,@(map (lambda (initial-byte)
             (list (bytevector initial-byte #x20) 4))
           (iota #x08 #xF0))

    ;; 3.3  Sequences with last continuation byte missing
    ,@(map (lambda (bytes)
             (list bytes (+ (bytevector-length bytes) 1)))
           '(
             #u8(#xC0)                     ; #\x0000
             #u8(#xE0 #x80)                ; #\x0000
             #u8(#xF0 #x80 #x80)           ; #\x0000
             #u8(#xDF)                     ; #\x000007FF
             #u8(#xEF #xBF)                ; #\x0000FFFF
             #u8(#xF7 #xBF #xBF)           ; #\x001FFFFF
             ))
    ))

(define-test 'utf16-surrogates
  (lambda ()
    (for-each (lambda (cp)
		(value-assert unicode-code-point? "code point" cp)
		(value-assert (lambda (cp)
                                (not (unicode-scalar-value? cp)))
                              "non-scalar value"
                              cp)
		(let ((char (integer->char cp)))
                  (value-assert (lambda (char)
                                  (not (unicode-char? char)))
                                "non-unicode character"
                                char)
		  (assert-error
		   (lambda ()
		     (encode-utf8-char! (make-bytevector 16) 0 char)))))
              utf16-surrogates)))

(define utf16-surrogates
  (iota #x800 #xD800))

(define-test 'illegal-chars
  (lambda ()
    (for-each (lambda (cp)
		(value-assert unicode-code-point? "code point" cp)
		(value-assert unicode-scalar-value? "scalar value" cp)
		(let ((char (integer->char cp)))
                  (value-assert (lambda (char)
                                  (not (unicode-char? char)))
                                "non-unicode character"
                                char)
                  (encode-utf8-char! (make-bytevector 16) 0 char)))
              illegal-characters)))

(define illegal-characters
  `(
    ;; Other noncharacters:
    ,@(iota #x20 #xFDD0)
    ,@(append-map (lambda (plane)
		    (let ((prefix (* plane #x10000)))
		      (list (+ prefix #xFFFE)
			    (+ prefix #xFFFF))))
		  (iota #x11))
    ))

(define-test 'invalid-utf8-sequences
  (lambda ()
    (for-each (lambda (bytes)
                (assert-error (lambda () (decode-utf8-char bytes 0))))
              invalid-utf8-sequences)))

(define invalid-utf8-sequences
  `(

    ;; 2.3  Other boundary conditions
    #u8(#xF4 #x90 #x80 #x80)		; #\x00110000
    #u8(#xF7 #xBF #xBF #xBF)		; #\x001FFFFF

    ;; 3.1  Unexpected continuation bytes
    ;; (duplicated below)
    ;; ,@(map bytevector
    ;;        (iota #x20 #x80))

    ;; 2 continuation bytes: #u8(#x80 #xBF)
    ;; 3 continuation bytes: #u8(#x80 #xBF #x80)
    ;; 4 continuation bytes: #u8(#x80 #xBF #x80 #xBF)
    ;; 5 continuation bytes: #u8(#x80 #xBF #x80 #xBF #x80)
    ;; 6 continuation bytes: #u8(#x80 #xBF #x80 #xBF #x80 #xBF)
    ;; 7 continuation bytes: #u8(#x80 #xBF #x80 #xBF #x80 #xBF #x80)

    ;; 3.1.9  Sequence of all 64 possible continuation bytes (0x80-0xbf):
    ,@(map bytevector
           (iota #x40 #x80))

    ;; 3.2.4  All 4 first bytes of 5-byte sequences (0xf8-0xfb),
    ;;        each followed by a space character:
    ,@(map (lambda (initial-byte)
             (bytevector initial-byte #x20))
           (iota #x04 #xF8))

    ;; 3.2.5  All 2 first bytes of 6-byte sequences (0xfc-0xfd),
    ;;        each followed by a space character:
    ,@(map (lambda (initial-byte)
             (bytevector initial-byte #x20))
           (iota #x02 #xFC))

    ;; 3.3  Sequences with last continuation byte missing
    #u8(#xF8 #x80 #x80 #x80)		; #\x0000
    #u8(#xFC #x80 #x80 #x80 #x80)	; #\x0000
    #u8(#xFB #xBF #xBF #xBF)		; #\x03FFFFFF
    #u8(#xFD #xBF #xBF #xBF #xBF)	; #\x7FFFFFFF

    ;; 4.1  Examples of an overlong ASCII character
    #u8(#xC0 #xAF)                      ; #\x002F
    #u8(#xE0 #x80 #xAF)                 ; #\x002F
    #u8(#xF0 #x80 #x80 #xAF)            ; #\x002F
    #u8(#xF8 #x80 #x80 #x80 #xAF)       ; #\x002F
    #u8(#xFC #x80 #x80 #x80 #x80 #xAF)  ; #\x002F

    ;; 4.2  Maximum overlong sequences
    #u8(#xC1 #xBF)                      ; #\x0000007F
    #u8(#xE0 #x9F #xBF)                 ; #\x000007FF
    #u8(#xF0 #x8F #xBF #xBF)            ; #\x0000FFFF
    #u8(#xF8 #x87 #xBF #xBF #xBF)       ; #\x001FFFFF
    #u8(#xFC #x83 #xBF #xBF #xBF #xBF)  ; #\x03FFFFFF

    ;; 4.3  Overlong representation of the NUL character
    #u8(#xC0 #x80)                      ; #\x0000
    #u8(#xE0 #x80 #x80)                 ; #\x0000
    #u8(#xF0 #x80 #x80 #x80)            ; #\x0000
    #u8(#xF8 #x80 #x80 #x80 #x80)       ; #\x0000
    #u8(#xFC #x80 #x80 #x80 #x80 #x80)  ; #\x0000

    ;; 3.5  Impossible bytes
    #u8(#xFE)
    #u8(#xFF)
    #u8(#xFE #xFE #xFF #xFF)

    ;; 5.1 Single UTF-16 surrogates
    #u8(#xED #xA0 #x80)                 ; #\xD800
    #u8(#xED #xAD #xBF)                 ; #\xDB7F
    #u8(#xED #xAE #x80)                 ; #\xDB80
    #u8(#xED #xAF #xBF)                 ; #\xDBFF
    #u8(#xED #xB0 #x80)                 ; #\xDC00
    #u8(#xED #xBE #x80)                 ; #\xDF80
    #u8(#xED #xBF #xBF)                 ; #\xDFFF

    ;; 5.2 Paired UTF-16 surrogates
    ;; (#\xD800 #\xDC00 #u8(#xED #xA0 #x80 #xED #xB0 #x80))
    ;; (#\xD800 #\xDFFF #u8(#xED #xA0 #x80 #xED #xBF #xBF))
    ;; (#\xDB7F #\xDC00 #u8(#xED #xAD #xBF #xED #xB0 #x80))
    ;; (#\xDB7F #\xDFFF #u8(#xED #xAD #xBF #xED #xBF #xBF))
    ;; (#\xDB80 #\xDC00 #u8(#xED #xAE #x80 #xED #xB0 #x80))
    ;; (#\xDB80 #\xDFFF #u8(#xED #xAE #x80 #xED #xBF #xBF))
    ;; (#\xDBFF #\xDC00 #u8(#xED #xAF #xBF #xED #xB0 #x80))
    ;; (#\xDBFF #\xDFFF #u8(#xED #xAF #xBF #xED #xBF #xBF))
    ))