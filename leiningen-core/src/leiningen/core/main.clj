(ns leiningen.core.main
  (:require [leiningen.core.user :as user]
            [leiningen.core.project :as project]
            [leiningen.core.classpath :as classpath]
            [leiningen.core.utils :as utils :refer [defdeprecated]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def aliases {"-h" "help", "-help" "help", "--help" "help", "-?" "help",
              "-v" "version", "-version" "version", "--version" "version",
              "überjar" "uberjar",
              "-o" ["with-profile" "offline,dev,user,default"]
              "-U" ["with-profile" "update,dev,user,default"]
              "cp" "classpath" "halp" "help"
              "with-profiles" "with-profile"
              "readme" ["help" "readme"]
              "tutorial" ["help" "tutorial"]
              "sample" ["help" "sample"]})

(defn lookup-alias [task-name project]
  (or (aliases task-name)
      (get (:aliases project) task-name)
      task-name "help"))

(defn task-args [args project]
  (if (= "help" (aliases (second args)))
    ["help" [(first args)]]
    [(lookup-alias (first args) project) (rest args)]))

(defdeprecated debug utils/debug)
(defdeprecated info  utils/info)
(defdeprecated exit  utils/exit)
(defdeprecated abort utils/abort)

(defn ^:no-project-needed task-not-found [task & _]
  (utils/abort (str task " is not a task. Use \"lein help\" to list all tasks.")))

;; TODO: got to be a cleaner way to do this, right?
(defn- drop-partial-args [pargs]
  #(for [[f & r] %
         :let [non-varargs (if (pos? (inc (.indexOf (or r []) '&)))
                             (min (count pargs) (.indexOf r '&))
                             (count pargs))]]
     (cons f (drop non-varargs r))))

(defn resolve-task
  ([task not-found]
     (let [[task & pargs] (if (coll? task) task [task])]
       (if-let [task-var (utils/resolve-symbol (symbol (str "leiningen." task) task))]
         (with-meta
           (fn [project & args] (apply task-var project (concat pargs args)))
           (update-in (meta task-var) [:arglists] (drop-partial-args pargs)))
         (not-found task))))
  ([task] (resolve-task task #'task-not-found)))

(defn ^:internal matching-arity? [task args]
  (some (fn [parameters]
          (and (if (= '& (last (butlast parameters)))
                 (>= (count args) (- (count parameters) 3))
                 (= (count parameters) (inc (count args))))
               parameters))
        (:arglists (meta task))))

(defn apply-task [task-name project args]
  (let [task (resolve-task task-name)]
    (when-not (or project (:no-project-needed (meta task)))
      (utils/abort "Couldn't find project.clj, which is needed for" task-name))
    (when-not (matching-arity? task args)
      (utils/abort "Wrong number of arguments to" task-name "task."
             "\nExpected" (rest (:arglists (meta task)))))
    (apply task project args)))

(defn leiningen-version []
  (System/getenv "LEIN_VERSION"))

(defn ^:internal version-satisfies? [v1 v2]
  (let [v1 (map #(Integer. %) (re-seq #"\d+" (first (string/split v1 #"-" 2))))
        v2 (map #(Integer. %) (re-seq #"\d+" (first (string/split v2 #"-" 2))))]
    (loop [versions (map vector v1 v2)
           [seg1 seg2] (first versions)]
      (cond (empty? versions) true
            (= seg1 seg2) (recur (rest versions) (first (rest versions)))
            (> seg1 seg2) true
            (< seg1 seg2) false))))

(def ^:private min-version-warning
  "*** Warning: This project requires Leiningen %s, but you have %s ***

Get the latest verison of Leiningen at https://github.com/technomancy/leiningen
or by executing \"lein upgrade\". ")

(defn- verify-min-version
  [{:keys [min-lein-version]}]
  (when-not (version-satisfies? (leiningen-version) min-lein-version)
    (info (format min-version-warning
                  min-lein-version (leiningen-version)))))

(defn- warn-chaining [task-name args]
  (when (and (some #(.endsWith (str %) ",") (cons task-name args))
             (not-any? #(= % "do") (cons task-name args)))
    (println "WARNING: task chaining has been moved to the \"do\" task. For example,")
    (println "\"lein javac, test\" should now be called as \"lein do javac, test\" ")
    (println "See `lein help do` for details.")))

(defn user-agent []
  (format "Leiningen/%s (Java %s; %s %s; %s)"
          (leiningen-version) (System/getProperty "java.vm.name")
          (System/getProperty "os.name") (System/getProperty "os.version")
          (System/getProperty "os.arch")))

(defn- http-settings []
  "Set Java system properties controlling HTTP request behavior."
  (System/setProperty "aether.connector.userAgent" (user-agent))
  (when-let [{:keys [host port]} (classpath/get-proxy-settings)]
    (System/setProperty "http.proxyHost" host)
    (System/setProperty "http.proxyPort" (str port))))

(defn -main
  "Run a task or comma-separated list of tasks."
  [& raw-args]
  (user/init)
  (let [project (when (.exists (io/file "project.clj"))
                  (project/init-project (project/read)))
        [task-name args] (task-args raw-args project)]
    (when (:min-lein-version project)
      (verify-min-version project))
    (http-settings)
    (when-not project
      (let [default-project (project/merge-profiles project/defaults [:default])]
        (project/load-certificates default-project)
        (project/load-plugins default-project)))
    (try (warn-chaining task-name args)
         (apply-task task-name project args)
         (catch Exception e
           (when-let [[_ code] (and (.getMessage e)
                                    (re-find #"Process exited with (\d+)"
                                             (.getMessage e)))]
             (utils/exit (Integer. code)))
           (when-not (re-find #"Suppressed exit:" (or (.getMessage e) ""))
             (.printStackTrace e))
           (utils/exit 1))))
  (utils/exit 0))
