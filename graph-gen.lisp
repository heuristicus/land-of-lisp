;; generates string output for graphviz

(defparameter *nodes* '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden. there is a well in front of you.))
			(attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
			             (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defparameter *max-label-length* 30)

;; Convert a symbol into a form valid for use in graphviz identifiers
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; generate a label, ensuring that its length is less than some limit
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

;; Convert a list of edges linking nodes to the .dot file format
(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\";"))
		(cdr node)))
	edges))

;; convert a graph defined by nodes and edges into a .dot description for graphviz
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; outputs the graph information to the specified file by using a thunk to print
;; to the standard output, which is temporarily redirected to the file.
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

;; Outputs the graph specified by the nodes and edges to a given file.
;; This is done by specifying a thunk which outputs the graph to standard output,
;; which is rerouted to the file in the called function.
(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst))
	     edges)))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))