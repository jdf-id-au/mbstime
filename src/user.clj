(ns user
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.walk :as walk]
            [clojure.test :refer [with-test is]])
  (:import (javax.swing JFrame JPanel JLabel JTextField JButton
             JComponent KeyStroke AbstractAction)
           (javax.swing.event ChangeListener)
           (javax.swing.border EmptyBorder)
           (java.awt GridLayout)
           (java.awt.event ActionListener KeyEvent FocusListener)
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

(def time-items (filter (fn [[k v]] (< 23000 k 25000))))
(def descriptions (map (fn [[k {:keys [Description]}]] [k Description])))

(with-test
  (defn parse-desc
    "Return upper time threshold in minutes. Case-insensitive."
    [s]
    (let [[_ _ just-min h m]
          (re-find #"(?i) to ((\d\d) minutes|(\d?\d):(\d\d) hours?)" s)]
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
  (memoize (fn [f] (->> f parse item-numbers
                     (eduction time-items descriptions)
                     time-cutoffs-impl))))

#_(->> mbs time-cutoffs time)

(def all-descriptions
  (memoize (fn [f] (->> f parse item-numbers (eduction descriptions)))))

(defn search-descriptions [s]
  (let [re (re-pattern (str "(?i)" s))]
    (filter (fn [[i d]] (re-find re d)) (all-descriptions mbs))))
#_ (search-descriptions "anaesthesia .* abdo")

(defn section-descriptions [x]
  (filter (fn [[i d]] (= (quot x 100) (quot i 100))) (all-descriptions mbs)))
#_ (section-descriptions 20770)

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
  (let [space 10
        frame (JFrame. "MBS time calc")
        inset (JPanel.)
        start-text (JTextField.)
        end-text (JTextField.)
        duration-label (JLabel. "")
        item-label (JLabel. "")
        invalid #(do (.setText duration-label "invalid")
                     (.setText item-label ""))
        calculate #(try
                     (let [[d i] (duration+item (.getText start-text) (.getText end-text))]
                       (.setText duration-label d)
                       (.setText item-label (str i))
                       (copy! (str d \tab i)))
                     (catch AssertionError e (invalid))
                     (catch Exception e (invalid)))
        focus-listener (reify FocusListener
                         (focusGained [this evt])
                         (focusLost [this evt]
                           (calculate)))]
    (.addFocusListener start-text focus-listener)
    (.addFocusListener end-text focus-listener)
    (doto inset
      (.setBorder (EmptyBorder. space space space space))
      (.setLayout (GridLayout. 2 2 space space))
      (.add start-text) (.add end-text)
      (.add duration-label) (.add item-label))
    (doto frame
      (.setSize 400 100) (.setLocation 960 540) (.add inset) (.setVisible true))))

#_(quick-ui)
