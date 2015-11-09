(ns looping-is-recursion)

(defn power [base exp]
  (cond
    (= 0 exp) 1
    :else (loop [result 1 ct (Math/abs exp)]
            (if (> ct 0) 
              (recur (* result base) (dec ct)) 
              (if (< exp 0) (/ 1 result) result)))))

(defn last-element [a-seq]
  (loop [seqq a-seq]
    (if (empty? (rest seqq))
      (first seqq)
      (recur (rest seqq))))) 

(defn seq= [seq1 seq2]
    (cond
      (not (= (count seq1) (count seq2))) false
      :else (loop [i-seq1 seq1
                   i-seq2 seq2]
              (cond
                (and (empty? i-seq1) (empty? i-seq1)) true
                (= (first i-seq1) (first i-seq2)) (recur (rest i-seq1) (rest i-seq2))
                :else false))))

(defn find-first-index [pred a-seq]
  (loop [i-seq a-seq
         i 0]
    (cond
      (empty? i-seq) nil
      (pred (first i-seq)) i
      :else (recur (rest i-seq) (inc i)))))

(defn avg [a-seq]
  (cond
    (empty? a-seq) 0
    :else
      (loop [i-seq a-seq
             n 0
             sum 0]
        (if (not (empty? i-seq))
          (recur (rest i-seq) (inc n) (+ (first i-seq) sum))
          (/ sum n))))) 

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)]
    (if (empty? freqs)
      #{}
      (set 
        (keys 
          (filter
            (fn [[k v]] (odd? v))
            freqs))))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
      (loop [i-1 1
             i-2 0
             i   1]
        (if (= i n)
          i-1
          (recur (+ i-1 i-2) i-1 (inc i))))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [i-seq (rest a-seq) 
           trace #{(first a-seq)}
           acc   [(first a-seq)]]
      (if (or (empty? i-seq) (contains? trace (first i-seq)))
        acc
        (let [_1st (first i-seq)]
          (recur (rest i-seq) (conj trace _1st) (conj acc _1st)))))))

