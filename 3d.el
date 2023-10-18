;;; 3d.el - a 3D graphics playground       -*- lexical-binding: t; -*-

;; Copyright (C) 2023 defanor

;; Author: defanor <defanor@uberspace.net>
;; Maintainer: defanor <defanor@uberspace.net>
;; Created: 2023-10-18
;; Keywords: 3D, graphics, amusements
;; Homepage: https://git.uberspace.net/3d.el/
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are toy routines: an external renderer, followed by
;; conversion from raster into ASCII, would be more suitable for most
;; tasks, but this is hackable and has no dependencies outside of
;; Emacs.

;; Information on the used formats and algorithms:

;; PLY format: https://paulbourke.net/dataformats/ply/

;; Computer graphics tutorials: https://scratchapixel.com/

;; Point in polygon detection:
;; https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
;; https://www.rosettacode.org/wiki/Ray-casting_algorithm
;; https://erich.realtimerendering.com/ptinpoly/

;; Linear algebra basics:
;; https://www.3blue1brown.com/topics/linear-algebra
;; https://defanor.uberspace.net/notes/online-courses-and-math-notes.xhtml

;;; Code:

(defun 3d-vector-copy (from to)
  "Copies vector elements from FROM into TO."
  (dotimes (i (min (length from) (length to)))
    (aset to i (aref from i))))

(defun 3d-dot-product (v1 v2 &optional dimensions)
  "Calculates a dot product of vectors V1 and V2.
If DIMENSIONS is set, only the first DIMENSIONS elements of each
vector are used for calculation."
  (let ((ret 0))
    (dotimes (i (min (or dimensions (length v1)) (length v1) (length v2)) ret)
      (setq ret (+ ret (* (aref v1 i) (aref v2 i)))))))

(defun 3d-cross-product (v1 v2)
  "Calculates a cross product of 3-dimensional vectors V1 and V2."
  (vector (- (* (aref v1 1) (aref v2 2)) (* (aref v1 2) (aref v2 1)))
          (- (* (aref v1 2) (aref v2 0)) (* (aref v1 0) (aref v2 2)))
          (- (* (aref v1 0) (aref v2 1)) (* (aref v1 1) (aref v2 0)))))

(defun 3d-vector-scalar-multiply (v s &optional out)
  "Multiplies vector V by scalar S.

If OUT is provided, the result is stored there, to avoid creation
of a new vector."
  (let ((ret (or out (make-vector (length v) nil))))
    (dotimes (i (length v) ret)
      (aset ret i (* (aref v i) s)))))

(defun 3d-matrix-vector-multiply (m v)
  "Multiplies matrix M by vector V."
  (let ((ret (make-vector (length (aref m 0)) nil)))
    (dotimes (row (length (aref m 0)) ret)
      (aset ret row
            (let ((val 0))
              (dotimes (col (length v) val)
                (setq val (+ val (* (aref v col)
                                    (aref (aref m col) row))))))))))

(defun 3d-matrix-multiply (m1 m2)
  "Matrix multiplication."
  (let ((ret (make-vector (length m2) nil)))
    (dotimes (col (length m2) ret)
      (aset ret col (3d-matrix-vector-multiply m1 (aref m2 col))))))

(defun 3d-vector-length (v)
  "Calculates length of an Euclidean vector V."
  (sqrt (let ((ret 0))
          (dotimes (i (length v) ret)
            (setq ret (+ ret (expt (aref v i) 2)))))))

(defun 3d-vector-normalize (v &optional out)
  "Normalizes a vector.

If OUT is provided, the result is stored there, to avoid creation
of a new vector."
  (let ((ret (or out (make-vector (length v) nil)))
        (v-len (3d-vector-length v)))
    (dotimes (i (length v) ret)
      (aset ret i (/ (aref v i) v-len)))))

(defun 3d-vector-subtract (v1 v2 &optional out)
  "Subtracts elements of V2 from elements of V1.

If OUT is provided, the result is stored there, to avoid creation
of a new vector."
  (let* ((len (min (length v1) (length v2) (if out (length out) (length v1))))
         (ret (or out (make-vector len nil))))
    (dotimes (i len ret)
      (aset ret i (- (aref v1 i) (aref v2 i))))))

(defun 3d-vector-add (v1 v2 &optional out)
  "Adds elements of V2 to elements of V1.

If OUT is provided, the result is stored there, to avoid creation
of a new vector."
  (let* ((len (min (length v1) (length v2) (if out (length out) (length v1))))
         (ret (or out (make-vector len nil))))
    (dotimes (i len ret)
      (aset ret i (+ (aref v1 i) (aref v2 i))))))

(defun 3d-polygon-plane-normal (p)
  "Calculates a normal for plane P."
  (3d-vector-normalize
     (3d-cross-product
      (3d-vector-subtract (aref p 1) (aref p 0))
      (3d-vector-subtract (aref p 2) (aref p 0)))))

(defun 3d-2d-ray-segment-intersects-p (p e1 e2)
  "A helper function for `3d-point-in-polygon'. Finds whether a ray
moving to the right from point P would intersect the edge between
points E1 and E2."
  (let ((px (car p))
        (py (cdr p))
        (a nil)
        (b nil))
    (if (< (cdr e1) (cdr e2))
        (setq a e1
              b e2)
      (setq a e2
            b e1))
    (let ((ax (car a))
          (ay (cdr a))
          (bx (car b))
          (by (cdr b)))
      (and (>= py ay)
           (<= py by)
           (< px (max ax bx))
           (or (< px (min ax bx))
               (< (/ (- by ay) (- bx ax)) (/ (- py ay) (- px ax))))))))

(defun 3d-point-in-polygon (point polygon normal)
  "A predicate checkingn whether POINT is inside POLYGON (with
NORMAL)."
  (let* ((nx (abs (aref normal 0)))
         (ny (abs (aref normal 1)))
         (nz (abs (aref normal 2)))
         (dimension-to-drop (if (> nx ny)
                                (if (> nx nz) 0 2)
                              (if (> ny nz) 1 2)))
         (as-2d (lambda (point)
                  (cons
                   (aref point (if (<= dimension-to-drop 0) 1 0))
                   (aref point (if (<= dimension-to-drop 1) 2 1)))))
         (cnt 0))
    (dotimes (i (length polygon) cnt)
      (when (3d-2d-ray-segment-intersects-p
             (funcall as-2d point)
             (funcall as-2d (aref polygon i))
             (funcall as-2d (aref polygon (mod (+ i 1) (length polygon)))))
        (setq cnt (+ cnt 1))))
    (eq (mod cnt 2) 1)))

(defun 3d-ray-plane-intersection (line-l0 line-l plane-p0 plane-n &optional out)
  "Looks for an intersection of a ray (defined by a point LINE-L0
and direction LINE-L) and a plane (defined by a point PLANE-P0
and plane normal PLANE-N).

If OUT is provided, it is used for intermediate calculations, to
avoid creation of temporary vectors."
  (when (/= (3d-dot-product line-l plane-n 3) 0)
    (let* ((D (- (3d-dot-product plane-n plane-p0 3)))
           (T (- (/ (+ (3d-dot-product plane-n line-l0 3) D)
                    (3d-dot-product plane-n line-l 3)))))
      (when (> T 0)
        (3d-vector-add line-l0 (3d-vector-scalar-multiply line-l T out) out)))))

(defun 3d-camera-rays (width height pixel-aspect-ratio field-of-view)
  "Generates camera rays, used by `3d-camera-rays-at'."
  (let ((aspect-ratio (/ width height))
        (rays (make-vector (* width height) nil)))
    (dotimes (y height rays)
      (let ((py (* (- 1 (* 2 (/ (+ y 0.5) height)))
                   (tan (/ field-of-view 2)))))
        (dotimes (x width)
          (let ((px (* (- (* 2 (/ (+ x 0.5) width)) 1)
                       aspect-ratio
                       pixel-aspect-ratio
                       (tan (/ field-of-view 2)))))
            (aset rays (+ (* y width) x)
                  (vector px py -1 1))))))))

(defun 3d-camera-rays-at (camera-to-world
                          width
                          height
                          pixel-aspect-ratio
                          field-of-view)
  "Generates camera rays, with CAMERA-TO-WORLD transform applied to the camera.

Camera resolution is WIDTH by HEIGHT, FIELD-OF-VIEW is given in
radians, and our pixels are not necessarily square, hence
PIXEL-ASPECT-RATIO."
  (let ((camera (3d-matrix-vector-multiply camera-to-world '[0 0 0 1]))
        (rays (3d-camera-rays width height pixel-aspect-ratio field-of-view)))
    (dotimes (i (length rays))
      (aset rays i
            (3d-vector-normalize
             (3d-vector-subtract
              (3d-matrix-vector-multiply camera-to-world (aref rays i))
              camera))))
    (cons camera rays)))

(defun 3d-ray-trace (ray-origin
                     ray-direction
                     polygons
                     &optional
                     point-hit-out
                     lpi-out
                     ray-sub-out)
  "Traces a ray from RAY-ORIGIN towards RAY-DIRECTION, through POLYGONS.

Returns nil if no ray is found, or a cons cell of the
intersection point and the polygon it hit.

If POINT-HIT-OUT, LPI-OUT, and RAY-SUB-OUT are provided, they are
used for storage of intermediate and final results, to avoid
creation of new vectors."
  (let ((point-hit nil)
        (polygon-hit nil)
        (lpi-store (or lpi-out (make-vector 4 nil)))
        (ray-subtract-store (or ray-sub-out (make-vector 3 nil))))
    (dotimes (i (length polygons) (when point-hit (cons point-hit polygon-hit)))
      (let* ((polygon (aref polygons i))
             (lpi (3d-ray-plane-intersection
                   ray-origin
                   ray-direction
                   (aref (car polygon) 0)
                   (cdr polygon)
                   lpi-store)))
        (when (and lpi
                   ;; Try to avoid hitting points close to the origin.
                   (< 1e-6
                      (3d-vector-length
                       (3d-vector-subtract lpi ray-origin ray-subtract-store)))
                   (3d-point-in-polygon lpi (car polygon) (cdr polygon))
                   (or (not point-hit)
                       (< (3d-vector-length
                           (3d-vector-subtract
                            point-hit ray-origin ray-subtract-store))
                          (3d-vector-length
                           (3d-vector-subtract
                            lpi ray-origin ray-subtract-store)))))
          (setq polygon-hit polygon)
          (when (not point-hit)
            (setq point-hit (or point-hit-out (make-vector 3 nil))))
          (3d-vector-copy lpi point-hit))))))

(defun 3d-ply-load ()
  "Parses data in the PLY format from current buffer, returns a
polygon vector."
  (let* ((state 'header)
         (vertex-count nil)
         (face-count nil)
         (vertex-number 0)
         (face-number 0)
         (vertices nil)
         (faces nil))
    (dolist (line (split-string (buffer-string) "\n") faces)
      (let ((words (split-string line " ")))
        (pcase words
          (`("element" "vertex" ,n)
           (setq vertex-count (string-to-number n))
           (setq vertices (make-vector vertex-count nil)))
          (`("element" "face" ,n)
           (setq face-count (string-to-number n))
           (setq faces (make-vector face-count nil)))
          (`("end_header") (setq state 'data))
          ((guard (and (eq state 'data)
                       (< vertex-number vertex-count)))
           ;; swap Y and Z: using Y as a vertical axis here, and Z
           ;; for depth.
           (aset vertices vertex-number
                 (vector (string-to-number (nth 0 words))
                         (string-to-number (nth 2 words))
                         (string-to-number (nth 1 words))))
           (setq vertex-number (+ vertex-number 1)))
          ((guard (and (eq state 'data)
                       (< face-number face-count)))
           (let* ((vertices-in-polygon (string-to-number (car words)))
                  (polygon (make-vector vertices-in-polygon nil)))
             (dotimes (i vertices-in-polygon)
               (aset polygon i
                     (aref vertices (string-to-number (nth (+ i 1) words)))))
             (aset faces face-number polygon))
           (setq face-number (+ face-number 1))))))))

(defun 3d-polygons-add-normals (polygons)
  "Adds plane normals to an array (usually a vector) of polygons."
  (dotimes (i (length polygons))
    (aset polygons i (cons (aref polygons i)
                           (3d-polygon-plane-normal (aref polygons i))))))

(defun 3d-render (polygons-with-normals
                  light-source
                  light-level
                  camera-to-world
                  width
                  height
                  pixel-aspect-ratio
                  field-of-view)
  "Renders polygons into text.

POLYGONS-WITH-NORMALS is a vector of polygons and their normals,
LIGHT-SOURCE is a point light, LIGHT-LEVEL is a string of
characters to use for different light levels, CAMERA-TO-WORLD is
a linear transform (4x4 matrix) to apply to the camera, WIDTH and
HEIGHT are given in characters, PIXEL-ASPECT-RATIO is an
estimated aspect ratio of characters, FIELD-OF_VIEW is the
camera's field of view.

The image is rendered into current buffer."
  (let* ((cr (3d-camera-rays-at
              camera-to-world
              width height pixel-aspect-ratio field-of-view))
         (camera (car cr))
         (rays (cdr cr)))
    (dotimes (i (length rays))
      (when (and (> i 0) (= (mod i width) 0))
        (insert "\n"))
      (let* ((point-hit-store (make-vector 3 nil))
             (light-point-hit-store (make-vector 3 nil))
             (lpi-store (make-vector 4 nil))
             (ray-sub-store (make-vector 3 nil))
             (rt (3d-ray-trace camera (aref rays i) polygons-with-normals
                               point-hit-store lpi-store ray-sub-store)))
        (insert
         (aref
          light-level
          (if rt
              (let* ((light-direction (3d-vector-normalize
                                       (3d-vector-subtract light-source
                                                           (car rt))))
                     (light-ray (3d-ray-trace (car rt)
                                              light-direction
                                              polygons-with-normals
                                              light-point-hit-store
                                              lpi-store
                                              ray-sub-store)))
                (if light-ray
                    1
                  (let ((face-normal (cddr rt)))
                    (+ 1 (round (* (- (length light-level) 2)
                                   (abs (3d-dot-product face-normal
                                                        light-direction))))))))
            0)))))))

;;; 3d.el ends here
