(ns dohdoi.unixref)

;; Predicates matching event types.

(defn start-element?
  [e]
  (= (:type e) :start-element))

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

;; Event seq processing.

(defn useful-seq
  "Filter out uninteresting events from an event seq."
  [events]
  (let [interesting? #(any-of [characters?
                               doi-event?
                               record-event?
                               url-event?
                               publication-event?]
                              %)]
    (filter interesting? events)))

(defn doi-seq
  "Convert an events seq into a seq of doi maps."
  [events]
  ())