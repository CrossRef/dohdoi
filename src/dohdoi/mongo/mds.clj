(ns dohdoi.mongo.mds
  (:use somnium.congomongo
        dohdoi.mongo.conf
        dohdoi.unixref)
  (:require [clojure.contrib.str-utils :as su])
  (:import [java.security MessageDigest]))

(mongo! :host mongo-host :port mongo-port :db "md")

(defrecord Doi [doi hash prefix citation-id owner-prefix citations links])

(defrecord Link [type url])

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
  [doi citation-id owner-prefix & {links :links citations :citations}]
  (Doi. doi
        (doi-hash doi)
        (first (su/re-split #"/" doi))
        citation-id
        owner-prefix
        []
        []))

(defn publication->dois
  [publication]
  (map #(make-doi (:doi %)
                  (:citation-id %)
                  (:owner %))
       (get-in publication [:dois])))

(defn store-publication!
  [publication]
  (doseq [doi (publication->dois publication)]
    (insert! :dois doi)))