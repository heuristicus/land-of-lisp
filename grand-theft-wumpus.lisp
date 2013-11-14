(load "graph-gen")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
;; 1 in 15 chance of an edge containing a roadblock
(defparameter *cop-odds* 15)

;; generate a number which represents a random node from the graph
(defun random-node ()
  (1+ (random *node-num*)))

;; creates a list containing two edges, one from a to b and one from b to a,
;; so long as they are not equal
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;; creates a list of *edge-num* edges by pairing randomly chosen nodes
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* 
			collect (edge-pair (random-node) (random-node)))))

;; find all edges in the edge list that start from the given node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; build a list of all nodes connected to a specific node, essentially
;; finding the connected component. Visited list is the list of all connected
;; nodes
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 ;; traverse all nodes linked to this node
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; Find all the connected components in the graph
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       ;; one connected component is an island
	       (let* ((connected (get-connected (car nodes) edge-list))
		      ;; the rest of the nodes are in a different component
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 ;; keep finding islands until there are no unconnected nodes
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

;; create a list of edges which can be used to connect all disjoint
;; connected components
(defun connect-with-bridges (islands)
  ;; if there is more than one island
  (when (cdr islands)
    ;; recursively append edges to the edge pair list
    ;; caar islands = first node of first island
    ;; caadr islands = first node of first island in the remaining islands
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))
    
;; create a fully connected edge list by finding disjoint connected components and 
;; connecting them with additional edges
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))
  
;; make a list of edges to use in the city representation, some of which may contain roadblocks
(defun make-city-edges ()
  ;; numbers representing nodes
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 ;; the list of all the edges
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 ;; edges which contain roadblocks
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    ;; add the edges with roadblocks to the edge list
    (add-cops (edges-to-alist edge-list) cops)))


(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))
				
						  
			    
	       