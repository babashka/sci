(ns sci.examples.forked-repl
  (:require
   [clojure.string :as str]
   [sci.core :as sci]))

(def bootstrap
  "(do
     (def ^:dynamic *scenario* :root)
     (def model (atom {:revision 0 :decisions []}))
     (defn snapshot []
       (assoc @model :scenario *scenario*))
     (defn evolve! [decision]
       (swap! model
              (fn [state]
                (-> state
                    (update :revision inc)
                    (update :decisions conj
                            {:scenario *scenario*
                             :decision decision}))))
       (snapshot)))")

(def help
  "Commands:
  :fork NAME   fork the selected world and select its child
  :use ID      select an existing world
  :tree        show the branch tree and each world's state
  :help        show this help
  :quit        exit

Any other line is evaluated in the selected SCI world. Try:
  (evolve! :hire-now)")

(defn- state-of [ctx]
  (try
    (sci/eval-string* ctx "(snapshot)")
    (catch Throwable e
      (str "<snapshot failed: " (ex-message e) ">"))))

(defn- child-ids [nodes id]
  (->> nodes
       (keep (fn [[child-id node]]
               (when (= id (:parent node)) child-id)))
       sort))

(defn- print-subtree [nodes selected id prefix last? root?]
  (let [{:keys [label ctx]} (get nodes id)
        connector (if root? "" (if last? "└─ " "├─ "))
        marker (if (= id selected) "*" " ")]
    (println (str prefix connector marker " " id " " label " "
                  (pr-str (state-of ctx))))
    (let [children (child-ids nodes id)
          child-prefix (str prefix
                            (if root?
                              ""
                              (if last? "   " "│  ")))]
      (doseq [[index child-id] (map-indexed vector children)]
        (print-subtree nodes selected child-id child-prefix
                       (= index (dec (count children))) false)))))

(defn- print-tree [nodes selected]
  (println)
  (print-subtree nodes selected 0 "" true true)
  (println))

(defn- parse-id [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException _ nil)))

(defn- fork-selected! [nodes selected next-id label]
  (let [parent-id @selected
        child-id @next-id
        parent-ctx (get-in @nodes [parent-id :ctx])
        child-ctx (sci/fork parent-ctx)
        label (if (str/blank? label)
                (str "branch-" child-id)
                label)
        scenario (keyword (str/replace label #"\s+" "-"))]
    (sci/eval-string*
     child-ctx
     (str "(alter-var-root #'*scenario* (constantly "
          (pr-str scenario) "))"))
    (swap! nodes assoc child-id {:id child-id
                                 :label label
                                 :parent parent-id
                                 :ctx child-ctx})
    (swap! next-id inc)
    (reset! selected child-id)
    (println "forked" parent-id "→" child-id label)))

(defn- use-world! [nodes selected argument]
  (if-let [id (parse-id argument)]
    (if (contains? @nodes id)
      (reset! selected id)
      (println "unknown world:" id))
    (println "usage: :use ID")))

(defn- eval-line! [nodes selected line]
  (let [ctx (get-in @nodes [@selected :ctx])]
    (try
      (prn (sci/eval-string* ctx line))
      (catch Throwable e
        (binding [*out* *err*]
          (println (ex-message e)))))))

(defn- handle-line! [nodes selected next-id line]
  (let [[command argument] (str/split (str/trim line) #"\s+" 2)]
    (case command
      ":fork" (fork-selected! nodes selected next-id argument)
      (":use" ":switch") (use-world! nodes selected argument)
      ":tree" (print-tree @nodes @selected)
      ":help" (println help)
      ":quit" false
      (eval-line! nodes selected line))
    (not= command ":quit")))

(defn -main []
  (let [root (sci/init {})
        _ (sci/eval-string* root bootstrap)
        nodes (atom {0 {:id 0 :label "root" :parent nil :ctx root}})
        selected (atom 0)
        next-id (atom 1)]
    (println "Forked SCI REPL")
    (println help)
    (print-tree @nodes @selected)
    (loop []
      (let [{:keys [id label]} (get @nodes @selected)]
        (print (format "[%d %s] user=> " id label))
        (flush)
        (when-let [line (read-line)]
          (when (or (str/blank? line)
                    (handle-line! nodes selected next-id line))
            (recur)))))))

;; Run with:
;; clojure -M:examples -m sci.examples.forked-repl
