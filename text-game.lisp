;; The nodes in the directed graph that represents the possible places that the player can be in.
(defparameter *nodes* '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden. there is a well in front of you))
			(attic (you are in the attic. there is a giant welding torch in the corner.))))

;; The edges connecting the nodes of the directed graph
(defparameter *edges* '((living-room (garden west door)
			             (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; A list of objects
(defparameter *objects* '(whiskey bucket frog chain))

;; A list of objects and where they are located
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; The starting location of the player
(defparameter *location* 'living-room)

;; Give a description of the location based on the information found in the node
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; Describe a path given the edge that holds its information
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; Describe all paths at a given location
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; List the objects that can be found at the given location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; Describe the objects in a given location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
