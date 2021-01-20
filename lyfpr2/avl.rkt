;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname avl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An AVL-tree is one of:
;;  - (make-node Number AVL-tree AVL-tree Nat)
;;  - "leaf"
;;
;; BST INVARIANT:
;;   every node in (node-left node) has a value
;;   that's smaller than (node-value node)
;;
;;   every node in (node-right node) has a value
;;   that's larger than (node-value node)
;;
;; AVL INVARIANT:
;;   (<= (abs (- (height (node-left node))
;;               (height (node-left node))))
;;       1)
;;
;; HEIGHT INVARIANT:
;;   the node-height selector returns
;;   the actual height of the tree

(define-struct node (value left right height))

;; height : AVL-tree -> Number
(check-expect (height #false) 0)
(check-expect (height (make-node 3 #false #false 1)) 1)

(define (height tree)
  (cond
    [(equal? tree #false) 0]
    [else (node-height tree)]))

;; build-node : Number AVL-tree AVL-tree -> AVL-tree
;; Smart constructor to ensure that the `height` field is correct.
(define (build-node value left right)
  (make-node value
             left
             right
             (+ (max (height left)
                     (height right))
                1)))

(check-expect (build-node 2 #false #false)
              (make-node 2 #false #false 1))

;; add :  Number AVL-tree -> AVL-tree
;; inserts 'value' into 'tree', returning a new AVL tree.
;; Strategy: struct. decomp.
(define (add value tree)
  (cond
    [(equal? tree #false)
     (build-node value #false #false)]
    [else
     (cond
       [(< value (node-value tree))
        (rebalance-left (node-value tree)
                        (add value (node-left tree))
                        (node-right tree)
                        (- (height (node-left tree))
                           (height (node-right tree)))
                        (height (node-left tree)))]
       [(= value (node-value tree)) tree]
       [(> value (node-value tree))
        (rebalance-right (node-value tree)
                         (node-left tree)
                         (add value (node-right tree))
                         (- (height (node-left tree))
                            (height (node-right tree)))
                         (height (node-right tree)))])]))


;; rebalance-left : Number AVL-tree AVL-tree Natural Natural -> AVL-tree
;; to combine n, left, and right into an AVL tree where
;; a number was inserted into `left`
(define (rebalance-left n left right
                        left-height-minus-right-height
                        old-left-height)
  (cond
    [(= old-left-height (height left)) (build-node n left right)]
    [(= left-height-minus-right-height 0) (build-node n left right)]
    [(= left-height-minus-right-height -1) (build-node n left right)]
    [else
     (local [(define A (node-left left))
             (define B (node-right left))]
       (cond
         [(or (= (height A) (height B))
              (= (height A) (+ (height B) 1)))
          (build-node (node-value left)
                      A
                      (build-node n B right))]
         [else
          (local [(define C (node-left B))
                  (define D (node-right B))]
            (build-node (node-value B)
                        (build-node (node-value left) A C)
                        (build-node n D right)))]))])) 

(check-expect (rebalance-left 4
                              (build-node 2 (build-node 1 #false #false)
                                          (build-node 3 #false #false))
                              (build-node 5 #false #false)
                              1
                              2)
              (make-node 4
                         (make-node 2
                                    (make-node 1 #false #false 1)
                                    (make-node 3 #false #false 1)
                                    2)
                         (make-node 5 #false #false 1)
                         3))
(check-expect (rebalance-left 2
                              (build-node 1 #false #false)
                              (build-node 3 #false #false)
                              0
                              0)
              (make-node 2 (make-node 1 #false #false 1) (make-node 3 #false #false 1) 2))
(check-expect (rebalance-left 2
                              (build-node 1 #false #false)
                              (build-node 3 #false (build-node 4 #false #false))
                              -1
                              0)
              (make-node 2 (make-node 1 #false #false 1)
                         (make-node 3 #false (make-node 4 #false #false 1) 2)
                         3)) 

(check-expect (rebalance-left 4
                              (build-node 2
                                          (build-node 1
                                                      (build-node -1 #false #false)
                                                      #false)
                                          (build-node 3 #false #false))
                              (build-node 5 #false #false)
                              2
                              2)
              (make-node
               2
               (make-node
                1
                (make-node -1 #false #false 1)
                #false
                2)
               (make-node
                4
                (make-node 3 #false #false 1)
                (make-node 5 #false #false 1)
                2)
               3))

(check-expect (rebalance-left 4
                              (build-node 1
                                          (build-node -1 #false #false)
                                          (build-node 2 #false (build-node 3 #false #false)))
                              (build-node 5 #false #false)
                              2
                              2)
              (make-node
               2
               (make-node
                1
                (make-node -1 #false #false 1)
                #false
                2)
               (make-node
                4
                (make-node 3 #false #false 1)
                (make-node 5 #false #false 1)
                2)
               3))

(check-expect (rebalance-left 3
                              (build-node 2
                                          (build-node 1 #false #false)
                                          #false)
                              #false
                              2
                              1)
              (make-node
               2
               (make-node 1 #false #false 1)
               (make-node 3 #false #false 1)
               2))



;; rebalance-right : Number AVL-tree AVL-tree Natural Natural -> AVL-tree
;; to combine n, left, and right into an AVL tree where
;; a number was inserted into `right`
(define (rebalance-right n left right
                         right-height-minus-left-height
                         old-right-height)
  (cond
    [(= old-right-height (height right)) (build-node n left right)]
    [(= right-height-minus-left-height 0) (build-node n left right)]
    [(= right-height-minus-left-height -1) (build-node n left right)]
    [else
     (local [(define A (node-left right))
             (define B (node-right right))]
       (cond
         [(or (= (height A) (height B))
              (= (+ 1 (height A)) (height B)))
          (build-node (node-value right)
                      (build-node n left A)
                      B)]
         [else
          (local [(define C (node-left A))
                  (define D (node-right A))]
            (build-node (node-value A)
                        (build-node n left C)
                        (build-node (node-value right) D B)))]))]))


(check-expect (rebalance-right 2
                               (build-node 1 #false #false)
                               (build-node 4 (build-node 3 #false #false)
                                           (build-node 5 #false #false))
                               1
                               2)
              (make-node 2 (make-node 1 #false #false 1)
                         (make-node 4 (make-node 3 #false #false 1) (make-node 5 #false #false 1) 2)
                         3))

(check-expect (rebalance-right 2
                               (build-node 1 #false #false)
                               (build-node 3 #false #false)
                               0
                               0)
              (make-node 2 (make-node 1 #false #false 1) (make-node 3 #false #false 1) 2))
(check-expect (rebalance-right 2
                               (build-node 1 (build-node 0 #false #false) #false)
                               (build-node 3 #false #false)
                               -1
                               0)
              (make-node 2 (make-node 1 (make-node 0 #false #false 1) #false 2)
                         (make-node 3 #false #false 1)
                         3))

(check-expect (rebalance-right 5
                               (build-node 4 #false #false)                                                             (build-node 7 (build-node 6 #false #false)                                                             (build-node 10 (build-node 8 #false #false) #false))
                               
                               2
                               2)
              (make-node
               7
               (make-node
                5
                (make-node 4 #false #false 1)
                (make-node 6 #false #false 1)
                2)
               (make-node
                10
                (make-node 8 #false #false 1)
                #false
                2)
               3))

(check-expect (rebalance-right 6
                               (build-node 3 #false #false)
                               (build-node 10 (build-node 8 (build-node 7 #false #false)
                                                          (build-node 9 #false #false))
                                           (build-node 11 #false
                                                       #false))
                               
                               2
                               2)
              (make-node
               8
               (make-node 6 (make-node 3 #false #false 1) (make-node 7 #false #false 1) 2)
               (make-node 10 (make-node 9 #false #false 1) (make-node 11 #false #false 1) 2)
               3))

#|
     top                     right
   /     \                 /      \
  left   right     =>    top      B 
          /  \          /   \
         A    B       left   A
        / \
       C   D

  value in A
   /     \
 top      right
/   \     / \
left C   D  B

|#


(check-expect (add 1 #false)
              (build-node 1 #false #false))
(check-expect (add 1 (build-node 1 #false #false))
              (build-node 1 #false #false))
(check-expect (add 2 (build-node 1 #false #false))
              (build-node 1 #false (build-node 2 #false #false)))
(check-expect (add 0
                   (build-node 1 #false (build-node 2 #false #false)))
              (build-node 1
                          (build-node 0 #false #false)
                          (build-node 2 #false #false)))
