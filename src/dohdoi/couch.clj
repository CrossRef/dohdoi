(ns dohdoi.couch
  (:use somnium.congomongo
	clojure.java.io)
  (:require [clojure.string :as string])
  (:import [java.security MessageDigest]))

;; 1. take md5, lo 8 bits, high 8 bits, to_i().to_s(34) each.
;; 2. if any in DB, add dois to db for the hash.
;;    if not in DB, add hash and dois for the hash.
;; 3. increase count for hash in db.

;; Hash
;; id hash count type (FULL, LO, HI)

;; Doi
;; id hash_id doi

(mongo! :host "127.0.0.1" :port 27017 :db "doi-hash")

(add-index! :hashes [:hash])
(add-index! :hashes [:count])

(defn update-hash-record
  [hash-record doi]
  (let [dois (conj (set (:dois hash-record)) doi)]
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
	take-trailing #(take 8 (drop 8 %))]
    (-> (MessageDigest/getInstance "MD5")
	(.digest (.getBytes doi))
	(take-trailing)
	(byte-array)
        (to-big-int)
	(.toString 36))))
  
(defn add-doi
  [doi]
  (let [hash (doi-hash doi)
	record (fetch-one :hashes :where {:hash hash})]
    (if record
      (update! :hashes record (update-hash-record record doi))
      (insert! :hashes (create-hash-record hash doi)))))
    
(defn add-dois-from-file
  [file]
  (with-open [r (reader file)]
    (doseq [doi (line-seq r)]
      (add-doi doi))))