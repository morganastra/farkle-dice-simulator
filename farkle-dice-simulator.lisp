(setf *random-state* (make-random-state t))

(defun roll-dice (n &optional (acc '()))
  (if (= n 0)
      acc
      (roll-dice (1- n) (cons (1+ (random 6)) acc))))

(defun categorize (roll)
  (cond ((straight-p roll) 'straight)
        ((six-of-kind-p roll) 'six-of-kind)
        ((three-pairs-p roll) 'three-pairs)
        ((two-triplets-p roll) 'two-triplets)
        ((five-of-kind-p roll) 'five-of-kind)
        ((four-of-kind-p roll) 'four-of-kind)
        ((three-of-kind-p roll) 'three-of-kind)
        ((some-ones-p roll) 'some-ones)
        ((some-fives-p roll) 'some-fives)))

(defun count-numbers (roll)
  (mapcar #'(lambda (x) (count x roll)) 
          (list 1 2 3 4 5 6)))

(defun match-roll-counts (n-sets tuple roll)
  (let ((roll-counts (count-numbers roll))
        (match-counts (make-list n-sets :initial-element tuple)))
    (equalp match-counts
            (loop for el in roll-counts
               when (equalp el tuple)
                 collect el))))

(defun straight-p (roll)
  (and (member 1 roll)
       (member 2 roll)
       (member 3 roll)
       (member 4 roll)
       (member 5 roll)
       (member 6 roll)))

(defun six-of-kind-p (roll)
  (match-roll-counts 1 6 roll))

(defun three-pairs-p (roll)
  (match-roll-counts 3 2 roll))

(defun two-triplets-p (roll)
  (match-roll-counts 2 3 roll))

(defun five-of-kind-p (roll)
  (match-roll-counts 1 5 roll))

(defun four-of-kind-p (roll)
  (match-roll-counts 1 4 roll))

(defun three-of-kind-p (roll)
  (match-roll-counts 1 3 roll))

(defun some-ones-p (roll)
  (member 1 roll))

(defun some-fives-p (roll)
  (member 5 roll))



(defun run-simulation (n)
  (let ((results (list)))
    (dotimes (i n)
      (let ((roll (roll-dice 6)))
        (push (cons (categorize roll) roll) results)))
    results))

