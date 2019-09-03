(ns petterik.tools.bench.chart
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [com.hypirion.clj-xchart :as xchart])
  (:import [java.io File]))


(defn file->chart-data [idx file]
  (let [colors [{:line-color   :green
                 :marker-color :green
                 :marker-type  :square}
                {:line-color   :blue
                 :marker-color :blue
                 :marker-type  :circle}
                {:line-color   :red
                 :marker-color :red
                 :marker-type  :triangle-up}
                {:line-color   :orange
                 :marker-color :orange
                 :marker-type  :triangle-down}]]
   (->> (slurp file)
     (string/split-lines)
     (map read-string)
     (vec)
     (group-by :id)
     (into {}
       (map (fn [[id marks]]
              [id (->> marks
                    (reduce (fn [m {:keys [clj-version id size mean lower-q upper-q]}]
                              (let [chart-id (format "(%s %s) [%s]-[%s]"
                                               (namespace id)
                                               (name id)
                                               (subs clj-version 0 (or (string/index-of clj-version "-") (count clj-version)))
                                               idx)]
                                (-> m
                                  (update-in [chart-id :x] (fnil conj []) size)
                                  (update-in [chart-id :y] (fnil conj []) (* 1000 mean))
                                  (update-in [chart-id :error-bars] (fnil conj []) (* 1000 (- upper-q lower-q)))
                                  (assoc-in [chart-id :style] (nth colors idx)))))
                      {}))]))))))

(defn- deep-merge [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn ->chart [[id chart-data]]
  (xchart/xy-chart
    chart-data
    {:title            (str (symbol id))
     :x-axis           {:logarithmic? true}
     :y-axis           {:logarithmic?    true
                        :decimal-pattern "####.### ms"}
     :error-bars-color :match-series}))

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