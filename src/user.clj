(ns user
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.walk :as walk]
            [clojure.test :refer [with-test is]])
  (:import (javax.swing JFrame JLabel JTextField JButton
             JComponent KeyStroke AbstractAction)
           (javax.swing.event ChangeListener)
           (java.awt GridLayout)
           (java.awt.event ActionListener KeyEvent)
           (java.awt.datatransfer StringSelection)))

(def mbs "MBS-XML-20230301-v2.XML")

(def parse (comp xml/parse-str slurp io/resource))

#_(time (and (parse mbs) nil))
;; Could stream it but only 8MB and less pleasant to dev with.
;; Could spec it but not for proof of concept.
;; Gives approximately:
#_{:tag :MBS_XML
   :content ({:tag :Data
              :content ({:tag :ItemNum
                         :content ("3")}
                        {:tag :Description
                         :content ("string\n")})})}

(defn item-numbers
  [mbs]
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
      [(Integer/parseInt item-num) item-data])))

(def time-descriptions
  (comp
    (filter (fn [[k v]] (< 23000 k 25000)))
    (map (fn [[k {:keys [Description]}]] [k Description]))))

(with-test
  (defn parse-desc
    "Return upper time threshold in minutes. Case-insensitive."
    [s]
    (let [[_ _ just-min h m]
          (re-find #"(?i).* to ((\d\d) minutes|(\d?\d):(\d\d) hours?)" s)]
      (cond
        just-min (Integer/parseInt just-min)
        (and h m) (+ (* (Integer/parseInt h) 60) (Integer/parseInt m)))))
  (is (= (parse-desc "31 MINUTES to 45 MINUTES (3 basic units)\n") 45))
  (is (= (parse-desc "17:41 HOURS TO 17:50 HOURS (103 basic units)\n") 1070)))

(defn time-cutoffs-impl
  "In minutes"
   [time-descriptions]
  (into (sorted-map 15 23010) ; NB hardcode special case
    (for [[k v] (rest time-descriptions)]
      [(parse-desc v) k])))

(def time-cutoffs
  (memoize (fn [f] (->> f parse item-numbers (eduction time-descriptions) time-cutoffs-impl))))

#_(->> "MBS-XML-20230301-v2.XML" time-cutoffs time)

(with-test
  (defn str24h->min [s]
    (let [x (Integer/parseInt s 10)
          h (quot x 100)
          m (rem x 100)]
      (assert (and (< m 60) (<= h 24)))
      (+ (* h 60) m)))
  (is (= (str24h->min "0101") 61)))

(with-test
  (defn duration
    "Case duration in minutes.
    24h times as string like \"0829\". Can span midnight."
    ;; Separate function could detect "unsocial hours" but would need date.
    [fs ts]
    (let [[f t] (map str24h->min [fs ts])]
      (if (< f t) (- t f) (- (+ t (* 24 60)) f))))
  (is (= (duration "0000" "1023") 623))
  (is (= (duration "2304" "0200") 176)))

(with-test
  (defn min->str [d]
    (let [h (quot d 60)
          m (rem d 60)]
      (format "%d:%02d" h m)))
  (is (= (min->str 1439) "23:59")))

(defn item [duration]
  {:pre [(<= duration (* 24 60))]}  
  (->> mbs time-cutoffs
    (drop-while (fn [[cutoff item]] (< cutoff duration)))
    first
    val))

(with-test
  (defn duration+item
    [fs ts]
    (let [d (duration fs ts)
          i (item d)
          s (min->str d)]
      [s i]))
  (is (= (duration+item "1100" "2330") ["12:30" 23670])))

(def d+i duration+item)

;; from jdf/comfort
(defn clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))
(defn copy!
  "Copy to system clipboard."
  [text]
  (let [selection (StringSelection. text)]
    (.setContents (clipboard) selection selection)))

(defn quick-ui []
  (let [frame (JFrame. "MBS time calc")
        times-label (JLabel. "Times")
        start-text (JTextField.)
        end-text (JTextField.)
        convert-button (JButton. "Calc + copy")
        duration-label (JLabel. "")
        item-label (JLabel. "")]
    (.addActionListener convert-button
      (reify ActionListener
        (actionPerformed [this evt]
          (try
            (let [[d i] (duration+item (.getText start-text) (.getText end-text))]
              (.setText duration-label d)
              (.setText item-label (str i))
              (copy! (str d \tab i)))
            (catch Exception e
              (.setText duration-label "invalid")
              (.setText item-label ""))))))
    (doto frame
      (.setLayout (GridLayout. 2 3 3 3))
      (.add times-label) (.add start-text) (.add end-text)
      (.add convert-button) (.add duration-label) (.add item-label)
      (.setSize 400 100) (.setVisible true))))

#_(quick-ui)
