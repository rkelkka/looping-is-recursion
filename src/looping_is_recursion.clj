(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (= 0 (count a-seq))
                   nil
                   (if (= 1 (count a-seq))
                     (first a-seq)
                     (recur (rest a-seq)))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (if (not (= (count a-seq) (count b-seq)))
                   false
                   (if (= 0 (count a-seq))
                     true
                     (if (not (= (first a-seq) (first b-seq)))
                       false
                       (recur (rest a-seq) (rest b-seq))))))]
    (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [p? pred
         s a-seq
         idx 0]
    (if (= 0 (count s))
      nil
      (if (p? (first s))
        idx
        (recur p? (rest s) (inc idx))))))

(defn avg [a-seq]
  (loop [s a-seq
         cnt 0
         sum 0]
    (if (= 0 (count s))
      (/ sum cnt)
      (recur (rest s) (inc cnt) (+ sum (first s))))))


;procudes result in different order than the example but doesn't seem to matter for the test
(defn parity [a-seq]
  (loop [s a-seq
         res (set '())] ;must be a set
        (if (= 0 (count s))
          res
          (let [even (disj res (first s))
                odd (conj res (first s))]
            (if (some #(= (first s) %) res) ;kind of contains? for a list
              (recur (rest s) even)
              (recur (rest s) odd))))))



(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

