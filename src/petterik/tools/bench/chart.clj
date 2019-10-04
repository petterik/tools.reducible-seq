(ns petterik.tools.bench.chart
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [com.hypirion.clj-xchart :as xchart])
  (:import [java.io File]
           [javax.swing JFrame SwingUtilities JPanel JTabbedPane JScrollPane BorderFactory]
           [java.awt GridLayout BorderLayout]
           [org.knowm.xchart XChartPanel]))

(def colors
  (mapv (fn [color marker]
          {:line-color   color
           :marker-color color
           :marker-type  marker})
    (keys (dissoc xchart/colors :yellow))
    (cycle (keys xchart/markers))))

(defn bench-data->chart-data [bench-data]
  (let [chart-keys [:id :type #_:form-count]
        mark-keys (conj chart-keys :clj-version :consumable? :seq? ::file-idx)

        mark->chart-id (comp
                         (memoize
                           (fn [mark]
                             (str "["
                               (string/join "]-["
                                 (for [k mark-keys
                                       :let [v (get mark k)]
                                       :when v]
                                   (cond
                                     (= :consumable? k)
                                     "consume"

                                     (= :seq? k)
                                     "seq"

                                     (= v "PersistentVector")
                                     "Vector"

                                     (= :clj-version k)
                                     (string/replace v #"-.*" "")

                                     :else
                                     v)))
                               "]")))
                         #(select-keys % mark-keys))]
    (->> bench-data
      (sort-by :size)
      ;; Initially ids were namespaced keywords, now they're strings.
      (map #(cond-> %
              (keyword? (:id %))
              (update :id (fn [id] (format "(%s %s)" (namespace id) (name id))))))
      (map #(assoc % ::chart-id (mark->chart-id %)))
      (group-by (apply juxt chart-keys))
      (into {}
        (map (fn [[joined-values marks]]
               (let [{:keys [id]} (zipmap chart-keys joined-values)]
                 [id (reduce (fn [m [color {::keys [chart-id]
                                            :keys  [size mean lower-q upper-q]}]]
                               (-> m
                                 (update-in [chart-id :x] (fnil conj []) size)
                                 (update-in [chart-id :y] (fnil conj []) (* 1000 mean))
                                 (update-in [chart-id :error-bars] (fnil conj []) (* 1000 (- upper-q lower-q)))
                                 (assoc-in [chart-id :style] color)))
                       {}
                       (map vector colors marks))])))))))

(defn file->chart-data [idx file]
  (->> (slurp file)
    (string/split-lines)
    (map read-string)
    (map #(assoc % ::file-idx idx))
    (vec)
    (bench-data->chart-data)))


(defn- deep-merge [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn ->chart [[id chart-data]]
  (xchart/xy-chart
    chart-data
    {:width            500
     :title            (str (cond-> id (keyword? id) symbol))
     :legend           {:position :inside-sw}
     :x-axis           {:logarithmic? true}
     :y-axis           {:logarithmic?    false
                        :min 0
                        #_#_:decimal-pattern "#,###.### ms"}
     :error-bars-color :match-series}))

(defn chart-tabs [chart-colls]
  (let [frame (JFrame. "XChart")]
    (SwingUtilities/invokeLater
      #(let [tabbedPane (doto (JTabbedPane.)
                          #_(.setLayout (GridLayout. 1 1)))]
         (.add frame tabbedPane BorderLayout/CENTER)

         (doseq [[idx charts] (map-indexed vector chart-colls)
                 :let [num-cols 3
                       num-rows (inc (/ (count charts)
                                       (double num-cols)))
                       panel (doto (JPanel.)
                               (.setLayout (GridLayout. num-rows num-cols)))
                       _ (.addTab tabbedPane (str idx)
                           (doto (JScrollPane. panel)
                             (-> (.getVerticalScrollBar) (.setUnitIncrement 16))
                             (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED)
                             (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)))]
                 chart charts]
           (.add panel (XChartPanel. chart)))

         (.pack frame)
         (.setVisible frame true)))
    frame))

(defn render-chart! [^File output-dir chart]
  (xchart/spit chart
    (.getAbsolutePath
      (io/file
        output-dir
        (-> (:title (bean chart))
          (string/replace #"[/ ]" "_")
          (string/replace #"[\[\]\(\)\?]" "")
          (str  ".png"))))))

(defn -main [& [output-dir & files]]
  {:pre [(.isDirectory (io/file output-dir))
         (every? (comp #(.exists ^File %) io/file) files)]}

  (->> files
    (map-indexed file->chart-data)
    (reduce deep-merge {})
    (map ->chart)
    (run! (partial render-chart! output-dir)))
  )

(defn view-consumables-charts [& files]
  (->> files
    (mapcat (fn [file]
              (map read-string (string/split-lines (slurp file)))))
    (map #(update % :id subs 1 (dec (count (:id %)))))
    ;; TODO: Compare baseline with each other version
    ;;       of the seqs. identity, consumable, consumabe+seq.
    ;;       maybe one per tab?
    #_#_#_
    (filter (comp (partial < 1000) :size))
    (remove (every-pred :consumable? (complement :seq?)))
    (remove (every-pred
              (comp #(string/starts-with? % "1.11.0") :clj-version)
              (complement :seq?)))
    (map (fn [{:keys [size] :as m}]
           (-> m
             (update :mean / size)
             (update :lower-q / size)
             (update :upper-q / size))))
    (group-by :type)
    (map (fn [[t bench-data]]
           (->> (bench-data->chart-data bench-data)
             (sort-by key)
             (map ->chart))))
    (apply interleave)
    (vector)
    (chart-tabs)))

;; TODO: Relative speedup/slowdown to 1.10.1
;; TODO: Spit renderings out to file.
;; TODO: Create blog post draft.
;; TODO: Post in #clojure-dev
;; TODO: Change consumable! such that it doesn't recursively
;;       affect the data structure.
;; TODO: Have the consumable! object implement IConsumable
;; TODO: Test Vector & Set, instead of range and repeat?
;;       It'll test map, set and vector, chunked and not,
;; TODO: Test this with a 1e5 size