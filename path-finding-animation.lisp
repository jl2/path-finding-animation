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


(defclass path-animation (newgl:line-segments)
  ((radius :initform 20.0 :initarg :radius)
   (theta :initform 0.0 ;; (/ pi 3)
          :initarg :theta) ;; Rotation around x axis
   (gamma :initform 0.0 ;; (/ pi 3)
          :initarg :gamma) ;; Rotation around y axis
   (rotating :initform nil :initarg :rotating)))

(defun torus (tv)
  (let ((theta (vx tv))
        (gamma (vz tv)))
    (vec3 (* (+ (vy tv) (* 2 (cos theta))) (cos gamma))
          (* (+ 4 (* 2 (cos theta))) (sin gamma))
          (* (vz tv) (sin theta)))))

(defun ft (tv)
  (let ((theta (vx tv))
        (gamma (vy tv)))
    (vec3 (* (vx tv) (cos gamma) (sin theta))
          (* (vy tv) (sin gamma) (sin theta))
          (* (vz tv) (cos theta) ))))
  ;; (vec3 (vx tv)
  ;;       (vy tv)
  ;;       (sin (vz tv))))

;; (defun ft (tv)
;;   (vec3 (vx tv))
;;         (cos (* 6 (vy tv)))
;;         (* (cos (* 6 (vx tv))) (sin (* 6 (vz tv)))))

(defun make-path-animation (&key
                              (function #'torus)
                              (x-count 20)
                              (y-count 20)
                              (z-count 1)
                              (x-min (if (= 1 x-count) 0.0 -0.75))
                              (x-max (if (= 1 x-count) 0.0 0.75))
                              (y-min (if (= 1 y-count) 0.0 -0.75))
                              (y-max (if (= 1 y-count) 0.0 0.75))
                              (z-min (if (= 1 z-count) 0.0 -0.75))
                              (z-max (if (= 1 z-count) 0.0 0.75))
                              (dd (vec3 0.025 0.025 0.025)))
  (let ((lines (make-instance 'path-animation))
        (dx (/ (- x-max x-min)  x-count))
        (dy (/ (- y-max y-min)  y-count))
        (dz (/ (- z-max z-min)  z-count)))

    (loop for i below (1+ z-count)
       do
         (loop
            with zv = (+ z-min (* i dz))
            for j below (1+ y-count)
            do
              (loop
                 with yv = (+ y-min (* j dy))
                 for k below (1+ x-count)
                 do
                   (let* ((xv (+ x-min (* k dx)))

                          (tv (vec3 xv yv zv))
                          (ptv (v- tv dd))
                          (ntv (v+ tv dd))

                          (pftv (funcall function ptv))
                          ;;(ftv (funcall function tv))
                          (nftv (funcall function ntv))
                          (fdiff (v- nftv pftv)))
                     (newgl:add-line-2 lines
                                       :p1 ptv
                                       :p2 (v+ tv fdiff)
                                       :c1 (vec4 0.1 0.8 0.1 0.0)
                                       :c2 (vec4 0.1 0.8 0.1 1.0))
                     ))))
    lines))

(defmethod newgl:set-uniforms ((object opengl-object))
  (with-slots (newgl:shader-program newgl:xform newgl:aspect-ratio radius theta gamma) object
    (set-uniform newgl:shader-program "aspectRatio" newgl:aspect-ratio)
    (set-uniform newgl:shader-program "transform"
                 (m*
                  (3d-matrices:mscaling (vec3 newgl:aspect-ratio 1.0 1.0))
                  (3d-matrices:mperspective 45 newgl:aspect-ratio 0.10 200.0)

                  (3d-matrices:mrotation (vec3 1 0 0) theta)
                  (mtranslation (vec3 0.0 0.0 (- radius)))
                  (3d-matrices:mrotation (vec3 0 1 0) gamma)
                  newgl:xform))))


(defmethod newgl:handle-key ((object path-animation) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots (radius theta gamma rotating) object
    (when (cond ((and (eq key :a) (eq action :press))
                 (setf rotating (not rotating))
                 t)

                ((and (eq key :w) (eq action :press))
                 (with-slots (radius theta gamma) object
                   (format t "radius: ~a~%theta: ~a~%gamma: ~a~%" radius theta gamma))
                 t)

                ((and (eq key :up) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
                 (incf theta (- (/ pi 180)))
                 t)
                ((and (eq key :down) (find :shift mod-keys) (or (eq action :press) (eq action :repeat)))
                 (incf theta (/ pi 180))
                 t)

                ((and (eq key :left)  (or (eq action :press) (eq action :repeat)))
                 (incf gamma (- (/ pi 180)))
                 t)
                ((and (eq key :right)  (or (eq action :press) (eq action :repeat)))
                 (incf gamma (/ pi 180))
                 t)

                ((and (eq key :up)  (or (eq action :press) (eq action :repeat)))
                 (setf radius (max 1.0 (* 0.95 radius)))
                 t)
                ((and (eq key :down)  (or (eq action :press) (eq action :repeat)))
                 (setf radius (min 100.0 (* 1.05 radius)))
                 t)


                (t nil))
    (set-uniforms object)))
 (call-next-method))

(defmethod update ((object path-animation))
  (with-slots (newgl:xform rotating gamma) object
    (when rotating
      (incf gamma (/ pi 360))
      (set-uniforms object)))
  (call-next-method))
