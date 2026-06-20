(ns dapr.system
  "Integrant system definition. The only stateful components are the application
  state atom and the cljfx renderer that mounts onto it; all logic lives in the
  pure namespaces. (No HTTP server or DB, so the reitit and Integrant DB-pool
  conventions from the backend best-practices do not apply.)"
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [dapr.library.store :as store]
            [dapr.state :as state]
            [dapr.ui.events :as events]
            [dapr.ui.views :as views]
            [integrant.core :as ig]))

(defn config
  "Read the Integrant system configuration from resources/config.edn."
  []
  (-> (io/resource "config.edn")
      (slurp)
      (ig/read-string)))

(defmethod ig/init-key :dapr/store [_ _]
  {:path (store/default-path!)})

(defmethod ig/init-key :dapr/state [_ {:keys [store]}]
  (let [path (:path store)]
    (atom (-> state/initial-state
              (assoc :store-path path)
              (state/set-libraries (store/load! path))))))

(defmethod ig/halt-key! :dapr/state [_ state-atom]
  (reset! state-atom state/initial-state))

(defmethod ig/init-key :dapr/renderer [_ {:keys [state-atom]}]
  (let [handler  (events/make-handler state-atom)
        renderer (fx/create-renderer
                  :middleware (fx/wrap-map-desc (fn [s] (views/root-view s)))
                  :opts {:fx.opt/map-event-handler handler})]
    (fx/mount-renderer state-atom renderer)
    {:renderer renderer :state-atom state-atom}))

(defmethod ig/halt-key! :dapr/renderer [_ {:keys [renderer state-atom]}]
  (when (and renderer state-atom)
    (fx/unmount-renderer state-atom renderer)))
