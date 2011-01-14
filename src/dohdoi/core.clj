(ns dohdoi.core
  (:use clojure.java.io clojure.contrib.lazy-xml clojure.string))

(defn events-to-dois
  [lstream]
  (loop [dois [] stream lstream]
    (cond
     (empty? stream) dois
     :otherwise
     (let [type   (:type (first stream))
	   name   (:name (first stream))
	   is-doi (and (= :start-element type)
		       (= :doi name))]
       (cond
	is-doi     (recur (conj dois (:str (second stream))) (rest stream))
	:otherwise (recur dois (rest stream)))))))

(defn dois-in-file
  "Returns all the DOIs in a metadata dump file."
  [f]
  (events-to-dois (parse-seq (file f))))

(defn dois-in-dir
  "Returns all the DOIs in all metadata dump files in a dir."
  [d]
  (let [file-list (map #(file d %) (.list (file d)))]
    (mapcat dois-in-file file-list)))

(defn write-dois-from-dir
  "Writes dois from deposit dump files in a dir to a file."
  [prefix in-dir out-file]
  (let [dois (dois-in-dir in-dir)]
    (loop [first-lot (take 1000 dois)
	   rest-lot  (drop 1000 dois)]
      (let [converted-dois (map #(str prefix %) first-lot)]
	(spit out-file
	      (join \newline converted-dois)
	      :append true)
	(recur (take 1000 dois) (drop 1000 dois))))))
