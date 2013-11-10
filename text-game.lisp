;; allows you to play a basic text-based adventure game. Use the (game-repl) command to start.

;; The nodes in the directed graph that represents the possible places that the player can be in.
(defparameter *nodes* '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden. there is a well in front of you.))
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

;; The commands that the player is allowed to use in the custom REPL
(defparameter *allowed-commands* '(look walk pickup inventory))

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

;; Describe everything that the player can see at their current location
;; This is not in the functional style! It looks directly at global variables.
;; Does different things at different times - functional style functions do
;; the same thing every time, so long as the parameters are the same.
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; Walk in a certain direction. Only possible if there is a path in that direction.
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

;; Attempt to pick up the requested object.
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;; list the items in the inventory of the player
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; a custom REPL for the game
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
	
;; custom read function to stop having to use quotes and parens
(defun game-read ()
  ;; cmd is a lisp-readable form of what the player enters,
  ;; essentially surrounding the entered string with parens.
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    ;; function which adds a quote to the start of its parameter
    ;; don't need to use the labels function as it is not being
    ;; used recursively.
    (flet ((quote-it(x)
	     (list 'quote x)))
      ;; car is the name of the function we want to execute
      ;; the cdr of cmd therefore contains all the parameters,
      ;; so we map the custom quote function to all of them.
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; custom eval function which allows only execution of certain functions
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command)))

;; tweak the given list of characters in such a way that they become sensible sentences.
(defun tweak-text (lst caps lit)
  ;; When the list isn't empty
  (when lst
    ;; Store the first char and the rest in separate variables
    (let ((item (car lst))
	  (rest (cdr lst)))
      ;; examine the first char
      ;; if it is a space, add it to the front of a recursive call to this
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ;; if the char is a sentence-ending character, make the next character caps
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ;; if it is a quotation, start or end a verbatim environment
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    ;; if the lit variable is set, take the character exactly as it is
	    (lit (cons item (tweak-text rest nil lit)))
	    ;; if caps is set, upcase the character 
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    ;; default case, add a downcased character
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;; print a list of symbols in a more readable form
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

		 
	    
	    
	     
	  