(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [acc base exp]
                   (if (zero? exp)
                     acc
                     (recur (* acc base) base (dec exp))))]
                        (pow 1 base exp)))

(defn last-element [a-seq]
  (let [get-last (fn [seq-b]
                 (if (= nil (second seq-b))
                   (first seq-b)
                   (recur  (drop 1 seq-b))))]
      (get-last a-seq)))

(defn seq= [seq1 seq2]
  (let [compare-seqs (fn [seq1 seq2]
                       (cond
                         (and (empty? seq1) (empty? seq2))true
                         (or (and (empty? seq1) (not (empty? seq2) )) (and (empty? seq2) (not (empty? seq1)))) false
                         (= (first seq1) (first seq2)) (recur (drop 1 seq1) (drop 1 seq2))
                         :else false))]
    (compare-seqs seq1 seq2)))

(defn find-first-index [pred a-seq]
    (if (empty? a-seq)
      nil
      (loop [number 0]
              (if (>= number (count a-seq))
                nil
                (if (pred (nth a-seq number))
                  number
                  (recur (inc number)))))))

(defn avg [a-seq]
   (if (empty? a-seq)
      nil
      (loop [sum 0
             counter 0]
               (if (= counter (count a-seq))
                 (/ sum counter)
                 (recur (+ sum (nth a-seq counter)) (inc counter))))))

(defn parity [a-seq]
  (defn toggle [a e]
    (if (contains? a e)
      (disj a e)
      (conj a e)))
  (loop [collection-of-odds #{}
         counter 0]
           (if (= counter (count a-seq))
             collection-of-odds
             (recur (toggle collection-of-odds (nth a-seq counter)) (inc counter)))))

(defn fast-fibo [n]
     (loop [now 0
            fib-  1
            fib--  0]
              (cond
                (< n 2) n
                (= now n) (+ fib- fib--)
                (>= now 2) (recur (inc now) (+ fib- fib--) fib-)
                :else (recur (inc now) fib- fib--))))

(defn cut-at-repetition [a-seq]
  (loop [counter 1
         vector-of-first [(first a-seq)]]
          (if (= counter (count a-seq))
            (vec (reverse vector-of-first))
            (if (= (count vector-of-first) (count (set a-seq)))
              (vec (reverse vector-of-first))
(recur (inc counter) (cons (nth a-seq counter) vector-of-first))))))
