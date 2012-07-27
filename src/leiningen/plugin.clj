(ns leiningen.plugin
  (:require [leiningen.core.utils :as utils]))

(defn ^:no-project-needed plugin
  "DEPRECATED. Please use the :user profile instead."
  [& args]
  (utils/abort "The plugin task has been removed.\n"
              "\nPlease see the upgrade guide for instructions on how to use"
              "the user profile to\nspecify plugins instead:"
              "https://github.com/technomancy/leiningen/wiki/Upgrading"))