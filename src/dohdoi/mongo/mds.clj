(ns dohdoi.mongo.mds
  (:use somnium.congomongo
        dohdoi.mongo.conf)
  (:import [java.security MessageDigest]))

(mongo! :host mongo-host :port mongo-port :db "md")

(defrecord Doi [doi hash prefix owner-prefix citations])

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

(defn make-doi
  [doi owner-prefix]
  (Doi. doi (doi-hash doi) (first (re-split #"/" doi)) owner-prefix []))

