(ns dohdoi.unixref
  (:use clojure.java.io clojure.contrib.lazy-xml))

;; Predicates matching event types.

(defn start-element?
  [e]
  (= (:type e) :start-element))

(defn end-element?
  [e]
  (= (:type e) :end-element))

(defn characters?
  [e]
  (= (:type e) :characters))

;; Predicates matching the start of interesting element types.

(defn doi-event?
  [e]
  (and (start-element? e) (= (:name e) :doi)))

(defn record-event?
  [e]
  (and (start-element? e) (= (:name e) :doi_record)))

(defn url-event?
  [e]
  (and (start-element? e) (= (:name e) :url)))

(defn publication-event?
  [e]
  (and (start-element? e) (= (:name e) :publication)))

;; Take interesting attributes from interesting element types.

(defn get-owner-prefix
  [e]
  (when (record-event? e) (get-in e [:attrs :owner])))

(defn get-citation-id
  [e]
  (when (record-event? e) (get-in e [:attrs :citationid])))

(defn get-url-type
  [e]
  (when (url-event? e) (get-in e [:attrs :type])))

(defn get-publication-type
  [e]
  (when (publication-event? e) (get-in e [:attrs :pubType])))

(defn get-file-date
  [e]
  (when (publication-event? e) (get-in e [:attrs :filedate])))

;; Event seq processing.

(defn characters->str
  [events]
  (apply str (map :str (take-while characters? events))))

(defn collapse-characters
  "Collapses character events into the start element event that
   proceeds them."
  [events]
  (when-let [s (seq events)]
    (cond
     (start-element? (first s))
     (lazy-seq
      (cons
       (merge (first s) {:str (characters->str (rest s))})
       (collapse-characters (rest s))))
     (end-element? (first s))
     (lazy-seq (cons (first s) (collapse-characters (rest s))))
     :else
     (recur (rest s)))))

(defn partition-element
  [name events]
  (partition-by #(and (end-element? %) (= (:name %) name)) events))

;; partition by end publication
;;        map by events->publication
;;                partition by end doi_record
;;                     map by events->doi

(defn read-record-event
  [content event]
  (cond
   (record-event? event)
   (-> content
       (assoc :citation-id (get-citation-id event))
       (assoc :owner-prefix (get-owner-prefix event)))
   (doi-event? event)
   (assoc content :doi (:str event))
   :else
   content))

(defn events->doi
  [events]
  (reduce read-record-event {} events))

(defn events->dois
  [events]
  (map events->doi (partition-element :doi_record events)))

(defn events->publication
  [events]
  (if-let [publication-event (first (filter #(publication-event? %) events))]
    {:file-date (get-file-date publication-event)
     :type (get-publication-type publication-event)
     :dois (events->dois events)}))

(defn events->publications
  [events]
  (map events->publication (partition-element :publication events)))

(defn unixref->publications
  [filename]
  (-> filename
      (file)
      (parse-seq)
      (collapse-characters)
      (events->publications)))