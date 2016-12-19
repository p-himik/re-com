(ns re-com.typeahead
  (:import goog.events.KeyCodes)
  (:require-macros [re-com.core :refer [handler-fn]]
                   [cljs.core.async.macros :refer [alt! go-loop]])
  (:require [cljs.core.async :refer [chan timeout <! put!]]
            [re-com.misc     :refer [throbber input-text input-text-args-desc]]
            [re-com.util     :refer [deref-or-value px]]
            [re-com.popover  :refer [popover-tooltip]] ;; need?
            [re-com.box      :refer [h-box v-box box gap line flex-child-style align-style]] ;; need?
            [re-com.validate :refer [input-status-type? input-status-types-list regex?
                                     string-or-hiccup? css-style? html-attr? number-or-string?
                                     string-or-atom? throbber-size? throbber-sizes-list] :refer-macros [validate-args-macro]]
            [re-com.backdrop :refer [backdrop]]
            [reagent.core    :as    reagent]))

;; ------------------------------------------------------------------------------------
;;  Component: typeahead
;; ------------------------------------------------------------------------------------

(def typeahead-args-desc
  (conj input-text-args-desc
   {:name :data-source       :required true                :type "fn"               :validate-fn fn?      :description [:span [:code ":data-source"] " supplies suggestion objects. This can either accept a single string argument (the search term), or a string and a callback. For the first case, the fn should return a collection of suggestion objects (which can be anything). For the second case, the fn should return "[:code "nil" ]", and eventually result in a call to the callback with a collection of suggestion objects."]}
   {:name :on-change         :required false :default nil  :type "string -> nil"    :validate-fn fn?      :description [:span [:code ":change-on-blur?"] " controls when it is called. It is passed a suggestion object."] }
   {:name :change-on-blur?   :required false :default true :type "boolean | atom"                         :description [:span "when true, invoke " [:code ":on-change"] " when the use chooses a suggestion, otherwise invoke it on every change (navigating through suggestions with the mouse or keyboard, or if "[:code "rigid?"]" is also "[:code "false" ]", invoke it on every character typed.)"] }
   {:name :model             :required false :default nil  :type "object | atom"                          :description "The initial value of the typeahead (should match the suggestion objects returned by " [:code ":data-source"] ")."}
   {:name :debounce-delay    :required false :default 250  :type "integer"          :validate-fn integer? :description [:span "After receiving input, the typeahead will wait this many milliseconds without receiving new input before calling " [:code ":data-source"] "."]}
   {:name :render-suggestion :required false               :type "render fn"        :validate-fn fn?      :description "override the rendering of the suggestion items by passing a fn that returns hiccup forms. The fn will receive two arguments: the search term, and the suggestion object."}
   {:name :suggestion-to-string :required false            :type "suggestion -> string" :validate-fn fn?  :description "When a suggestion is chosen, the input-text value will be set to the result of calling this fn with the suggestion object."}
   {:name :rigid?            :required false :default true :type "boolean | atom"                         :description [:span "If "[:code "false"]" the user will be allowed to choose arbitrary text input rather than a suggestion from " [:code ":data-source"]". In this case, a string will be supplied in lieu of a suggestion object." ]}))


;; TODO
;; ability to focus & blur the input-text would be nice... this is also missing from input-text
;; the typeahead should blur the input-text after a selection is chosen

(declare debounce display-suggestion)
(defn- make-typeahead-state
  "Return an initial value for the typeahead state, given `args`."
  [{:keys [on-change rigid? change-on-blur? data-source suggestion-to-string debounce-delay model]}]
  (let [external-model-value (deref-or-value model)]
    (cond-> (let [c-input (chan)]
              {:input-text             ""
               :external-model         model
               :model                  (deref-or-value model)
               :waiting?               false
               :suggestions            []
               :displaying-suggestion? false
               :suggestion-to-string   (or suggestion-to-string str)
               :data-source            data-source
               :change-on-blur?        change-on-blur?
               :on-change              on-change
               :rigid?                 rigid?
               :c-input                c-input
               :c-search               (debounce c-input debounce-delay)})
      external-model-value
      (display-suggestion external-model-value))))

;; ------------------------------------------------------------------------------------
;; State predicates:  state -> value? -> boolean
;; ------------------------------------------------------------------------------------

(defn- event-updates-model?
  "Should `event` update the `typeahead` `model`?"
  [{:keys [change-on-blur? rigid?]} event]
  (let [change-on-blur? (deref-or-value change-on-blur?)
        rigid? (deref-or-value rigid?)]
    (case event
      :input-text-blurred (and change-on-blur? (not rigid?))
      :suggestion-activated (not change-on-blur?)
      :input-text-changed (not (or change-on-blur? rigid?)))))

(defn- event-displays-suggestion?
  "Should `event` cause the `input-text` value to be used to show the active suggestion?"
  [{:keys [change-on-blur?]} event]
  (let [change-on-blur? (deref-or-value change-on-blur?)]
    (case event
      :suggestion-activated (not change-on-blur?)
      false)))

;; ------------------------------------------------------------------------------------
;; State update helpers: state -> value? -> next-state
;;   all pure, _except_ that they may call `on-change`
;; ------------------------------------------------------------------------------------

(defn- update-model
  "Change the `typeahead` `model` value to `new-value`"
  [{:as state :keys [on-change]} new-value]
  (when on-change (on-change new-value))
  (assoc state :model new-value))

(defn- display-suggestion
  "Change the `input-text` `model` to the string representation of `suggestion`"
  [{:as state :keys [suggestion-to-string]} suggestion]
  (let [suggestion-string (suggestion-to-string suggestion)]
    (cond-> state
      suggestion-string (assoc :input-text suggestion-string
                               :displaying-suggestion? true))))

(defn- clear-suggestions
  [state]
  (-> state
      (dissoc :suggestions :suggestion-active-index)))

(defn- activate-suggestion-by-index
  "Make the suggestion at `index` the active suggestion"
  [{:as state :keys [suggestions]} index]
  (let [suggestion (nth suggestions index)]
    (cond-> state
      :always (assoc :suggestion-active-index index)
      (event-updates-model? state :suggestion-activated) (update-model suggestion)
      (event-displays-suggestion? state :suggestion-activated) (display-suggestion suggestion))))

(defn- choose-suggestion-by-index
  "Choose the suggestion at `index`"
  [{:as state :keys [suggestions]} index]
  (let [suggestion (nth suggestions index)]
    (-> state
        (activate-suggestion-by-index index)
        (update-model suggestion)
        (display-suggestion suggestion)
        clear-suggestions)))

(defn- choose-suggestion-active
  [{:as state :keys [suggestion-active-index]}]
  (cond-> state
    suggestion-active-index (choose-suggestion-by-index suggestion-active-index)))

(defn- wrap [index count] (mod (+ count index) count))

(defn- activate-suggestion-next
  [{:as state :keys [suggestions suggestion-active-index]}]
  (cond-> state
    suggestions
    (activate-suggestion-by-index (-> suggestion-active-index (or -1) inc (wrap (count suggestions))))))

(defn- activate-suggestion-prev
  [{:as state :keys [suggestions suggestion-active-index]}]
  (cond-> state
    suggestions
    (activate-suggestion-by-index (-> suggestion-active-index (or 0) dec (wrap (count suggestions))))))

(defn- reset-typeahead
  [state]
  (cond-> state
    :always clear-suggestions
    :always (assoc :waiting? false :input-text "" :displaying-suggestion? false)
    (event-updates-model? state :input-text-changed) (update-model nil)))

(defn- got-suggestions
  "Update state when new suggestions are available"
  [state suggestions]
  (-> state
      (assoc :suggestions suggestions
             :waiting? false
             :suggestion-active-index nil)))

(defn- input-text-will-blur
  "Update state when the `input-text` is about to lose focus."
  [{:keys [input-text displaying-suggestion?] :as state}]
  (cond-> state
    (and (not displaying-suggestion?)
         (event-updates-model? state :input-text-blurred))
    (update-model input-text)))

(defn- change-data-source
  "Update `state` given a new `data-source`. Resets the typeahead since any existing suggestions
  came from the old `data-source`."
  [state data-source]
  (-> state
      reset-typeahead
      (assoc :data-source data-source)))

(defn- external-model-changed
  "Update state when the external model value has changed."
  [state new-value]
  (-> state
      (update-model new-value)
      (display-suggestion new-value)
      clear-suggestions))

;; ------------------------------------------------------------------------------------
;; Functions with side-effects
;; ------------------------------------------------------------------------------------

(defn- search-data-source!
  "Call the `data-source` fn with `text`, and then call `got-suggestions` with the result
  (asynchronously, if `data-source` does not return a truthy value)."
  [data-source state-atom text]
  (if-let [return-value (data-source text #(swap! state-atom got-suggestions %1))]
    (swap! state-atom got-suggestions return-value)
    (swap! state-atom assoc :waiting? true)))

(defn- search-data-source-loop!
  "For every value arriving on the `c-search` channel, call `search-data-source!`."
  [state-atom c-search]
  (go-loop []
    (let [new-text (<! c-search)
          data-source (:data-source @state-atom)]
      (search-data-source! data-source state-atom new-text)
      (recur))))

(defn- input-text-on-change!
  "Update state in response to `input-text` `on-change`, and put text on the `c-input` channel"
  [state-atom new-text]
  (let [{:as state :keys [input-text c-input]} @state-atom]
    (if (= new-text input-text)
      state                                                 ;; keypresses that do not change the value still call on-change, ignore these
      (do
        (put! c-input new-text)
        (swap! state-atom
               #(cond-> %
                  :always (assoc :input-text new-text :displaying-suggestion? false)
                  (event-updates-model? state :input-text-changed) (update-model new-text)))))))

(defn- input-text-on-key-down!
  [state-atom event]
  (condp = (.-which event)
    (.-UP KeyCodes) (swap! state-atom activate-suggestion-prev)
    (.-DOWN KeyCodes) (swap! state-atom activate-suggestion-next)
    (.-ENTER KeyCodes) (swap! state-atom choose-suggestion-active)
    (.-ESC KeyCodes) (swap! state-atom reset-typeahead)
    ;; tab requires special treatment
    ;; trap it IFF there are suggestions, otherwise let the input defocus
    (.-TAB KeyCodes)
    (if (not-empty (:suggestions @state-atom))
      (do (swap! state-atom activate-suggestion-next)
          (.preventDefault event))
      (swap! state-atom input-text-will-blur))
    true))

;; ------------------------------------------------------------------------------------
;; The typeahead component
;; ------------------------------------------------------------------------------------

(defn- typeahead
  "typeahead reagent component"
  [& {:as args}]
  {:pre [(validate-args-macro typeahead-args-desc args "typeahead")]}
  (let [{:as state :keys [c-search]} (make-typeahead-state args)
        state-atom (reagent/atom state)
        input-text-model (reagent/cursor state-atom [:input-text])]
    (search-data-source-loop! state-atom c-search)
    (fn
      [& {:as   args
          :keys [data-source rigid? render-suggestion suggestion-to-string model width]}]
      {:pre [(validate-args-macro typeahead-args-desc args "typeahead")]}
      (let [{:as state :keys [suggestions waiting? suggestion-active-index external-model]} @state-atom
            last-data-source (:data-source state)
            latest-external-model (deref-or-value model)
            width (or width "250px")]
        (when (not= last-data-source data-source)
          (swap! state-atom change-data-source data-source))
        (when (not= latest-external-model (deref-or-value external-model))
          (swap! state-atom external-model-changed latest-external-model))
        [v-box
         :width width
         :style {:display :inline}
         :children
         [(into [input-text
                 :model input-text-model
                 :on-change (partial input-text-on-change! state-atom)
                 :change-on-blur? false
                 :attr (merge {:on-key-down (partial input-text-on-key-down! state-atom)}
                              (:attr args))]
                (apply concat (dissoc (select-keys args (map :name input-text-args-desc))
                                      :model :on-change :attr :change-on-blur?)))
          (if (or (not-empty suggestions) waiting?)
            [backdrop
             :on-click #(swap! state-atom clear-suggestions)])
          (if (or (not-empty suggestions) waiting?)
            [v-box
             :class "rc-typeahead-suggestions-container"
             :children [(when waiting?
                          [box :align :center :child [throbber :size :small :class "rc-typeahead-throbber"]])
                        (for [[i s] (map vector (range) suggestions)
                              :let [selected? (= suggestion-active-index i)]]
                          ^{:key i}
                          [box
                           :child (if render-suggestion
                                    (render-suggestion s)
                                    s)
                           :class (str "rc-typeahead-suggestion"
                                       (when selected? " active"))
                           :attr {:on-mouse-over #(swap! state-atom activate-suggestion-by-index i)
                                  :on-mouse-down #(do (.preventDefault %) (swap! state-atom choose-suggestion-by-index i))}])]])]]))))

(defn- debounce
  "Return a channel which will receive a value from the `in` channel only
  if no further value is received on the `in` channel in the next `ms` milliseconds."
  [in ms]
  (let [out (chan)]
    (go-loop [last-val nil]
      (let [val (if (nil? last-val) (<! in) last-val)
            timer (timeout ms)]
        (let [v (alt!
                  in ([val _] val)
                  timer (do (>! out val) nil))]
          (recur v))))
    out))
