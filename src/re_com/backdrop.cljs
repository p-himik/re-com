(ns re-com.backdrop
  (:require-macros [re-com.core :refer [handler-fn]]
                   [re-com.validate :refer [validate-args-macro]])
  (:require [re-com.validate :refer [number-or-string?]]))


;;--------------------------------------------------------------------------------------------------
;; Component: backdrop
;;--------------------------------------------------------------------------------------------------

(def backdrop-args-desc
  [{:name :opacity  :required false :default 0.0 :type "double | string" :validate-fn number-or-string? :description [:span "opacity of backdrop from:" [:br] "0.0 (transparent) to 1.0 (opaque)"]}
   {:name :on-click :required false              :type "-> nil"          :validate-fn fn?               :description "a function which takes no params and returns nothing. Called when the backdrop is clicked"}])

(defn backdrop
  "Renders a backdrop dive which fills the entire page and responds to clicks on it. Can also specify how tranparent it should be"
  [& {:keys [opacity on-click] :as args}]
  {:pre [(validate-args-macro backdrop-args-desc args "backdrop")]}
  [:div {:class     "rc-backdrop noselect"
         :style    {:position         "fixed"
                    :left             "0px"
                    :top              "0px"
                    :width            "100%"
                    :height           "100%"
                    :background-color "black"
                    :opacity          (if opacity opacity 0.0)}
         :on-click (handler-fn (on-click))}])
