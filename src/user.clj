(ns user
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.walk :as walk]))

(def mbs
  ;; Could stream it but only 8MB and less pleasant to dev with.
  ;; Could spec it but not for proof of concept.
  ;; Gives approximately:
  #_{:tag :MBS_XML
     :content ({:tag :Data
                :content ({:tag :ItemNum
                           :content ("3")}
                          {:tag :Description
                           :content ("string\n")})})}
  (-> "MBS-XML-20230301-v2.XML" io/resource slurp xml/parse-str))

(def item-numbers
  (do
    (assert (and (= (:tag mbs) :MBS_XML) (empty? (:attrs mbs))))
    (into (sorted-map)
      (for [{:keys [tag attrs content]} (:content mbs)
            :let [_ (assert (and (= tag :Data) (empty? attrs)))
                  {item-num :ItemNum :as item-data}
                  (into {}
                    (for [{:keys [tag attrs content] :as data-tag} content
                          :let [;_ (println data-tag)
                                _ (assert (and (empty? attrs)
                                            (<= (count content) 1)))]]
                      [tag (first content)]))]]
        [(Integer/parseInt item-num) item-data]))))

(def time-descriptions
  (->> item-numbers
    (filter (fn [[k v]] (< 23000 k 25000)))
    (map (fn [[k {:keys [Description]}]] [k Description]))))

(defn parse-desc
  "Return upper time threshold in minutes. Case-insensitive."
  [s]
  (let [[_ _ just-min h m]
        (->> s (re-find #"(?i).* to ((\d\d) minutes|(\d?\d):(\d\d) hours?)"))]
    (cond
      just-min (Integer/parseInt just-min)
      (and h m) (+ (* (Integer/parseInt h) 60) (Integer/parseInt m)))))

#_(parse-desc "31 MINUTES to 45 MINUTES (3 basic units)\n")
#_(parse-desc "17:41 HOURS TO 17:50 HOURS (103 basic units)\n")

(def time-cutoffs
  "In minutes"
  (into (sorted-map 15 23010) ; NB hardcode special case
    (for [[k v] (rest time-descriptions)]
      [(parse-desc v) k])))

(defn int24h->min [i]
  (let [h (quot i 100)
        m (rem i 100)]
    (assert (<= m 60))
    (+ (* h 60) m)))

(defn duration
  "Case duration in minutes.
  Times as integer statement of 24h clock. Can span midnight."
  ;; Separate function could detect "unsocial hours" but would need date.
  [from to]
  (let [f (int24h->min from)
        t (int24h->min to)]
    (if (< f t) (- t f) (- (+ t (* 24 60)) f))))

(defn times->item
  [from to]
  (let [d (duration from to)]
    (assert (<= d (* 24 60)))
    (->> time-cutoffs
      (drop-while (fn [[cutoff item]] (< cutoff d)))
      first
      val)))

#_(times->item 1000 1645)
