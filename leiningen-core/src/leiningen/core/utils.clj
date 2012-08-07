(ns leiningen.core.utils
  (:require [clojure.java.io :as io]))

(defn read-file
  "Read the contents of file if it exists."
  [file]
  (when (.exists file)
    (read-string (slurp file))))

(defn ns-exists? [namespace]
  (some (fn [suffix]
          (-> (#'clojure.core/root-resource namespace)
              (subs 1)
              (str suffix)
              io/resource))
        [".clj" (str clojure.lang.RT/LOADER_SUFFIX ".class")]))

(defmacro defdeprecated [old new]
  `(let [new# ~(str (.getName (:ns (meta (resolve new)))) "/" (name new))
         warn# (delay (println "Warning:" '~old "is deprecated; use" new#))]
     (defn ~(vary-meta old assoc :doc (format "Compatibility alias for %s" new))
       [& args#]
       (force warn#)
       (apply ~(resolve new) args#))))

(def ^:dynamic *debug* (System/getenv "DEBUG"))

(defn debug [& args]
  (when *debug* (apply println args)))

(def ^:dynamic *info* true)

(defn info [& args]
  (when *info* (apply println args)))

(defn error [& args]
  (binding [*out* *err*]
    (apply println "Error:" args)))

(def ^:dynamic *exit-process?*
  "Bind to false to suppress process termination." true)

(defn exit
  "Exit the process. Rebind *exit-process?* in order to suppress actual process
  exits for tools which may want to continue operating."
  ([exit-code]
     (if *exit-process?*
       (do (shutdown-agents)
           (System/exit exit-code))
       (throw (ex-info "Suppressed exit" {:exit-code exit-code}))))
  ([] (exit 0)))

(defn abort
  "Print msg to standard err and exit with a value of 1."
  [& msg]
  (binding [*out* *err*]
    (apply println msg)
    (exit 1)))

(defn resolve-symbol
  "Resolve a fully qualified symbol by first requiring its namespace."
  [sym]
  (when-let [ns (namespace sym)]
    (when (ns-exists? ns)
      (let [ns (symbol ns)]
        (when-not (find-ns ns)
          (require ns)))
      (resolve sym))))
