(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))

(nconc *allowed-commands* '(help h ? combine))
(defun help ()
    (game-print '(allowed commands are--))
    (game-print '(look- gives description of current location.))
    (game-print '(walk direction- changes location if direction is valid.))
    (game-print '(pickup object- object is put into inventory if object is in current location.))
    (game-print '(inventory- lists objects currently in the inventory.))
    (game-print '(weld chain bucket attic- welds the chain to the bucket in the attic.))
    (game-print '(dunk bucket well garden- fills the bucket with water in the garden.))
    (game-print '(splash bucket wizard living-room- splashes the wizard in the living room.))
    (game-print '(combine metal-thing black-handle glue plug-and-cord- combines these items into a clothes iron.)))
(defun h ()
  (help))
(defun ? ()
  (help))

(defparameter *clothes-iron-items* '(metal-thing black-handle glue plug-and-cord))
(defparameter *iron-combined-cond* 0)
(nconc *objects* '(clothes-iron))

(defun combine (obj1 obj2 obj3 obj4)
  (cond 
    ((and (member obj1 *clothes-iron-items*)
          (member obj1 *clothes-iron-items*)
          (member obj1 *clothes-iron-items*)
          (member obj1 *clothes-iron-items*)
          (have 'metal-thing)
          (have 'black-handle)
          (have 'glue)
          (have 'plug-and-cord)
          (eq *iron-combined-cond* 0))
            (progn (setq *iron-combined-cond* 1) '(you can now iron items- if you had anything to iron which you dont.)))
    (t  
      '(you cannot combine these items yet.))))

; object, location, path


; needs location
; check if object already exists
; check if location exists
(defmacro new-object (obj loc)
    `(cond
      ((not (symbolp ,obj))
        '(object must be a symbol.))
      ((not (symbolp ,loc))
        '(location must be a symbol.))
      ((member ,obj *objects*) 
        '(object already exists.))
      ((not (assoc ,loc *nodes*)) ; searches the list for loc in each car
        '(location does not exist.))
      (t  
        (push ,obj *objects*)
        (push (list ,obj ,loc) *object-locations*))))

; possibly update a location's description
; check if location already exists
; simply needs a name and description
; (list locn (list desc)) - using this as parameter
; locn (list desc)
(defmacro new-location (loc)
  `(cond
      ((not (listp ,loc))
        '(loc must be a list.))
      ((not (symbolp (car ,loc)))
        '(car of list must be a symbol.))
      ((assoc (car ,loc) *nodes*)
        '(location already exists.))
      ((not (listp (cadr ,loc)))
        '(cdar of list must be a list.))
      ; could test if desc list are symbols
      (t
        (push ,loc *nodes*))))

(defparameter *a* '(dragon (fun land.)))
(defparameter *b* '(elevator down stairs))

; how to refrence to a certain cell
(defparameter *current-loc* *edges*) ; a copy or pointer ot what?
; path to current room from current room
; multiple paths to the same room from one room
; check car of dir if possible node to car of *nodes* until nil
; find loc and push dir (list loc2 dir2 obj)
(defmacro new-path (loc dir)
  `(cond
      ((not (symbolp ,loc))
        '(loc must be a symbol.))
      ((not (listp ,dir))
        '(dir must be a list.))
      ; doesn't check if list elements are appropriate types
      ; assume they are all symbols
      ; list size must be 3
      ((not (assoc ,loc *nodes*))
        '(location does not exist.))
      ((not (assoc (car ,dir) *nodes*))  ; checks if loc2 is a location
        '(location2 does not exist.))
      (t
        (cond
          ((not (assoc ,loc *edges*))
            (push '(dragon ,dir) *current-loc*)             ; how to not apply quote
            '(path created from loc.))
          ((loop while (not (eq ,loc (car *current-loc*)))  ; sets current-loc to appropriate loc.
            do (setf *current-loc* (cdr *edges*)))
          (nconc *current-loc* ,dir)                  ; adds path to appropriate loc.
           '(path created from loc.)))
      )
    ))















