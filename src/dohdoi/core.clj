(ns dohdoi.core
  (:use clojure.java.io clojure.contrib.lazy-xml
	[clojure.string :as string])
  (:import [java.security MessageDigest]))

(defn str-from-events
  [m]
  (let [events (:rest m)
	result (:result m)
	chars-filter #(= (:type %) :characters)]
    {:result (cons
	      (apply str (map :str (take-while chars-filter events)))
	      result)
     :rest (drop-while chars-filter events)}))

(defn doi-event?
  [e]
  (and (= (:type e) :start-element) (= (:name e) :doi)))

(defn pub-event?
  [e]
  (and (= (:type e) :start-element) (= (:name e) :publication)))

(defn chars-to-str
  [e]
  (apply str (map :str (take-while #(= (:type %) :characters) e))))

(defn rest-after-chars
  [e]
  (drop-while #(= (:type %) :characters) e))

(defn events-to-pub-type
  [events]
  (if (and (seq events) (pub-event? (first events)))
    (get-in (first events) [:attrs :pubType])
    (recur (rest events))))

(defn events-to-dois
  [events]
  (when-let [s (seq events)]
    (if (doi-event? (first s))
      (lazy-seq (cons (chars-to-str (rest s))
		      (events-to-dois (rest-after-chars (rest s)))))
      (recur (rest s)))))

(defn pub-in-file
  "Return a list of maps representing publications."
  [f]
  (let [events (parse-seq (file f))]
    {:type (events-to-pub-type events)
     :dois (events-to-dois events)}))

(defn pubs-in-dir
  [d]
  (let [file-list (map #(file d %) (.list (file d)))]
    (pmap pub-in-file file-list)))

(defn dois-in-file
  "Returns all the DOIs in a metadata dump file."
  [f]
  (prn (file f))
  (events-to-dois (parse-seq (file f))))

(defn dois-in-dir
  "Returns all the DOIs in all metadata dump files in a dir."
  [d]
  (let [file-list (shuffle (map #(file d %) (.list (file d))))]
    (mapcat dois-in-file file-list)))

;; e.g. (take 5000 (dois-in-dir "/Users/karl/Data/conf"))

;; or
;; (write-doi-seq "http://ip-10-48-71-165.eu-west-1.compute.internal:9393/"
;; 		 (take 100000 (dois-in-dir "/Users/karl/Data/jor"))
;;		 "/Users/karl/Data/100000.jor.urls.txt")

(defn write-doi-seq
  "Writes dois from deposit dump files in a dir to a file."
  [prefix doi-seq out-file]
  (loop [first-lot (take 1000 doi-seq)
	 rest-lot  doi-seq]
    (let [converted-dois (map #(str prefix %) first-lot)]
      (spit out-file
	    (str (join \newline converted-dois) \newline)
	    :append true)
      (if (seq rest-lot)
	(recur (take 1000 rest-lot) (drop 1000 rest-lot))))))

;; 1. take md5, lo 8 bits, high 8 bits, to_i().to_s(34) each.
;; 2. if any in DB, add dois to db for the hash.
;;    if not in DB, add hash and dois for the hash.
;; 3. increase count for hash in db.

;; Hash
;; id hash count type (FULL, LO, HI)

;; Doi
;; id hash_id doi

(def hash-records (agent {}))

(def collisions (agent #{}))

(defn update-hash-record
  [hash-record doi]
  (let [dois (conj (:dois hash-record) doi)]
    (assoc hash-record
      :count (count dois)
      :dois dois)))

(defn create-hash-record
  [hash doi]
  {:hash hash
   :dois #{doi}
   :count 1})

(defn doi-hash
  [doi]
  (let [to-big-int #(BigInteger. 1 %)
	take-lower #(take 8 (drop 8 %))]
    (-> (MessageDigest/getInstance "MD5")
	(.digest (.getBytes doi))
	(take-lower)
	(byte-array)
        (to-big-int)
	(.toString 36))))

(defn add-doi
  [hash-records doi]
  (let [hash (doi-hash doi)
	record (hash-records hash)]
    (if record
      (let [updated-record (update-hash-record (hash-records hash) doi)]
	(if (> (:count updated-record) 1)
	  (send collisions #(conj % hash)))
	(assoc hash-records hash updated-record))
      (assoc hash-records hash (create-hash-record hash doi)))))

(defn add-dois-from-file
  [file]
  (with-open [r (reader file)]
    (doseq [doi (line-seq r)]
      (send hash-records #(add-doi % doi)))))