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
;; cadr is the same as (car (cdr '(list))), can use c****r to look at stuff in lists
;; more easily - the shortcut is only up to 4 a or d characters in place of *
;; assoc finds the value associated with a symbol in an alist, which is what *nodes* is
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; Describe a path given the edge that holds its information
;; the ` character goes into data mode, but you can flip-flop between code and 
;; data mode by using the , character. This is called quasiquoting.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; Describe all paths at a given location
;; The #' operator is equivalent to (function [something]), and makes sure that
;; functions do not get confused with variables if the names overlap. mapcar maps
;; a function onto each value of the list that is provided to it. apply is used to
;; treat items in a list as separate objects and pass them to the subsequent function.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; List the objects that can be found at the given location
;; labels allows the use of the function within the function itself
;; remove-if-not filters a list, removing items from it if the given function
;; returns false when the items are given to it.
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; Describe the objects in a given location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
