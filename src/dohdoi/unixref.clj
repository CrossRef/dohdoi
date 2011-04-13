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

(defn useful-seq
  "Filter out uninteresting events from an event seq."
  [events]
  (let [useful? (juxt doi-event? record-event?
                      url-event? publication-event?)]
    (filter #(some true? (useful? %)) events)))

;; publication
;;     doi_record
;;         doi
;;         url
;;         url
;;     doi_record
;;         doi
;;     doi_record
;;         doi
;; publication
;;     doi_record
;;         doi
;;                   etc...

(defn characters->str
  [events]
  (apply str (map :str (take-while characters? events))))

(defn collapse-characters
  "Collapses character events into the start element event that
   proceeds them."
  [events]
  (when-let [s (seq events)]
    (if (start-element? (first s))
      (lazy-seq
       (cons
        (merge (first s) {:str (characters->str (rest s))})
        (collapse-characters (rest s))))
      (recur (rest s)))))
   
(defn read-event
  [content event]
  (cond
   (publication-event? event)
   (-> content
     (assoc-in [:type] (get-publication-type event))
     (assoc-in [:file-date] (get-file-date event)))
   (doi-event? event)
   (-> content
     (update-in [:dois] #(conj % (:str event))))
   (record-event? event)
   (-> content
     (update-in [:owners] #(conj % (get-owner-prefix event)))
     (update-in [:citation-ids] #(conj % (get-citation-id event))))
   :else
   content))

(defn events->publication
  [events]
  (let [useful (useful-seq (collapse-characters events))
        loose-publication (reduce read-event {} useful)]
    (-> loose-publication
        (dissoc :dois)
        (dissoc :owners)
        (dissoc :citation-ids)
        (assoc :dois
          (map
           #(hash-map :doi %1 :owner %2 :citation-id %3)
           (:dois loose-publication)
           (:owners loose-publication)
           (:citation-ids loose-publication))))))

(defn unixref->publication
  [filename]
  (events->publication (parse-seq (file filename))))