(ns dohdoi.core
  (:use clojure.java.io clojure.contrib.lazy-xml clojure.string))

(defn events-to-dois
  [events]
  (cond
   (seq events)
     (let [type   (:type (first events))
	   name   (:name (first events))
	   is-doi (and (= :start-element type)
		       (= :doi name))]
       (if is-doi
	 (lazy-seq (cons (:str (second events))
			 (events-to-dois (rest events))))
	 (recur (rest events))))
   :otherwise nil))

(defn dois-in-file
  "Returns all the DOIs in a metadata dump file."
  [f]
  (events-to-dois (parse-seq (file f))))

(defn dois-in-dir
  "Returns all the DOIs in all metadata dump files in a dir."
  [d]
  (let [file-list (map #(file d %) (.list (file d)))]
    (mapcat dois-in-file file-list)))

(defn write-doi-seq
  "Writes dois from deposit dump files in a dir to a file."
  [prefix doi-seq out-file]
  (loop [first-lot (take 1000 doi-seq)
	 rest-lot  doi-seq]
    (let [converted-dois (map #(str prefix %) first-lot)]
      (spit out-file
	    (join \newline converted-dois)
	    :append true)
      (if (seq rest-lot)
	(recur (take 1000 rest-lot) (drop 1000 rest-lot))))))

; e.g. (take 5000 (dois-in-dir "/Users/karl/Data/conf"))

; or
; (write-doi-seq "http://ip-10-48-71-165.eu-west-1.compute.internal/"
; 		 (take 100000 (dois-in-dir "/Users/karl/Data/conf"))
;		 "/Users/karl/Data/100000.conf.urls.txt")