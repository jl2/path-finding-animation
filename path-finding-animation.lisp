;;;; path-finding-animation.lisp 
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :path-finding-animation)

(defun create-animation (n)
  "Create a path finding animation."
  (let ((lines (make-line-segments)))
    (dotimes (i n)
      (newgl:add-line-2 lines
                  :p1 (vec3-random -0.98 0.98)
                  :p2 (vec3-random -0.98 0.98)
                  :c1 (vec4-random 0.2 0.8)
                  :c2 (vec4-random 0.2 0.8)))
    lines))
