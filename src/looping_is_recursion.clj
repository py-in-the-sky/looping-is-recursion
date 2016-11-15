(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn power-loop [base exp]
  (loop [acc 1
         exp exp]
    (if (zero? exp)
      acc
      (recur (* acc base) (dec exp)))))

(defn last-element [a-seq]
  (let [b-seq (rest a-seq)]
    (if (empty? b-seq)
      (first a-seq)
      (recur b-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (and (empty? seq1) (not (empty? seq2))) false
    (and (empty? seq2) (not (empty? seq1))) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) acc
      :else (recur (inc acc) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         length 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum length)
      (recur (+ sum (first a-seq)) (inc length) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [items #{}
         a-seq a-seq]
    (if (empty? a-seq)
      items
      (recur (toggle items (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [f_a 0
         f_b 1
         n n]
    (if (== n 0)
      f_a
      (recur f_b (+ f_a f_b) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         seen #{}
         a-seq a-seq]
    (let [elem (first a-seq)]
      (cond
        (empty? a-seq) acc
        (contains? seen elem) acc
        :else (recur (conj acc elem) (conj seen elem) (rest a-seq))))))
