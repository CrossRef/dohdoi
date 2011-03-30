(ns dohdoi.ssh-conf
  (:use clojure.string))

(defn parse-host
  [conf-seq]
  (merge
   {:Host (second conf-seq)}
   (apply hash-map (take-while (partial not= "Host") (drop 2 conf-seq)))))

(defn write-host
  [host-map]
  (let [options (filter (partial not= "Host") (keys host-map))]
    (str "Host "
	 (:Host host-map)
	 \newline
	 (apply str (map #(str % " " (host-map %) \newline) options)))))

(defn parse-ssh-conf-seq
  [conf-seq]
  (cond
   (not (seq conf-seq))
   nil
   (= "Host" (first conf-seq)) 
   (lazy-seq
     (cons
      (parse-host conf-seq)
      (parse-ssh-conf-seq (rest conf-seq))))
   :otherwise
   (recur (rest conf-seq))))

(defn read-ssh-hosts
  []
  (parse-ssh-conf-seq
   (filter #(not= % "")
	   (split (slurp "/Users/karl/.ssh/config") #"\s"))))

(defn write-ssh-hosts
  []
  ())