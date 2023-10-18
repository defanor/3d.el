;; Takes about 17 seconds on Xeon E3-1275 v2 with byte-compiled 3d.el.

(benchmark-run 1
  (let* ((rotate-x-angle (degrees-to-radians 80))
         (rotate-y-angle (degrees-to-radians 15))
         (rotate-z-angle (/ pi 16))
         (rotate-x (vector (vector 1 0 0 0)
                           (vector 0 (cos rotate-x-angle) (sin rotate-x-angle) 0)
                           (vector 0 (- (sin rotate-x-angle)) (cos rotate-x-angle) 0)
                           (vector 0 0 0 1)))
         (rotate-y (vector (vector (cos rotate-y-angle) 0 (- (sin rotate-y-angle)) 0)
                           (vector 0 1 0 0)
                           (vector (sin rotate-y-angle) 0 (cos rotate-y-angle) 0)
                           (vector 0 0 0 1)))
         (rotate-z (vector (vector (cos rotate-z-angle) (sin rotate-z-angle) 0 0)
                           (vector (- (sin rotate-z-angle)) (cos rotate-z-angle) 0 0)
                           (vector 0 0 1 0)
                           (vector 0 0 0 1)))
         (camera-to-world (3d-matrix-multiply
                           rotate-z
                           (3d-matrix-multiply
                            rotate-y
                            (3d-matrix-multiply
                             rotate-x
                             '[[1 0 0 0] [0 1 0 0] [0 0 1 0] [-0.3 1 9 1]]))))
         (polygons (with-current-buffer "sphere-and-text.ply" (3d-ply-load)))
         (buf (get-buffer-create "*sphere-and-text*")))
    (3d-polygons-add-normals polygons)
    (with-current-buffer buf
      (kill-region (point-min) (point-max))
      (3d-render polygons
                 '[10 20 -10]
                 " .-~^!?obO#"
                 camera-to-world
                 80
                 60
                 (/ 9.0 18)
                 (degrees-to-radians 27))
      (display-buffer buf))))
