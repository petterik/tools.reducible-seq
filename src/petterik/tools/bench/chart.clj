(ns petterik.tools.bench.chart
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [com.hypirion.clj-xchart :as xchart])
  (:import [java.io File]))


(defn file->chart-data [file]
  (->> (slurp file)
    (string/split-lines)
    (map read-string)
    (vec)
    (group-by :id)
    (into {}
      (map (fn [[id marks]]
             [id
              (->> marks
                (reduce (fn [m {:keys [clj-version id size mean lower-q upper-q]}]
                          (let [chart-id (format "(%s %s) [%s]" (namespace id) (name id) (subs clj-version 0 (or (string/index-of clj-version "-") (count clj-version))))]
                            (-> m
                              (update-in [chart-id :x] (fnil conj []) size)
                              (update-in [chart-id :y] (fnil conj []) (* 1000 mean))
                              (update-in [chart-id :error-bars] (fnil conj []) (* 1000 (- upper-q lower-q)))
                              (assoc-in [chart-id :style] (if (= "1.10.1" clj-version)
                                                            {:line-color   :green
                                                             :marker-color :green
                                                             :marker-type  :square}
                                                            {:line-color   :blue
                                                             :marker-color :blue
                                                             :marker-type  :circle})))))
                  {}))])))))

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
        (str (string/replace (:title (bean chart)) "/" "_") ".png")))))

(defn -main [& [output-dir & files]]
  {:pre [(.isDirectory (io/file output-dir))]}

  (->> files
    (filter (comp #(.isFile ^File %) io/file))
    (map file->chart-data)
    (apply deep-merge)
    (map ->chart)
    (run! (partial render-chart! output-dir)))
  )