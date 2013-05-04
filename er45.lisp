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
    (game-print '(splash bucket wizard living-room- splash the wizard in the living room.))
    (game-print '(combine metal-thing black-handle glue plug-and-cord- combines these item into a clothes iron.)))
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
            (progn (setq *iron-combined-cond* 1) '(you can now iron items- well not really)))
    (t  
      '(you cannot combine these items yet.))))
