(ns dapr.device.smb.views
  (:require [dapr.device.views :as device-views]))

(defmethod device-views/library-menu-item :smb [device-type]
  (device-views/menu-item device-type))

(defn- connect-form
  "Enter the share URL and optional credentials, then connect. A blank username
  connects as guest; credentials entered here are saved to the OS keystore."
  [{:keys [url username password workgroup loading?]}]
  {:fx/type :v-box :spacing 6
   :children
   [{:fx/type :label :text "Connect to an SMB server or share"}
    {:fx/type :text-field :prompt-text "smb://host/  (lists shares)  or  smb://host/share/" :text (or url "")
     :on-text-changed {:event/type :dapr.ui.events/browser-connect-field :field :url}}
    {:fx/type :h-box :spacing 6 :alignment :center-left
     :children [{:fx/type :text-field :h-box/hgrow :always
                 :prompt-text "Username (blank = guest)" :text (or username "")
                 :on-text-changed {:event/type :dapr.ui.events/browser-connect-field :field :username}}
                {:fx/type :text-field :h-box/hgrow :always
                 :prompt-text "Workgroup (optional)" :text (or workgroup "")
                 :on-text-changed {:event/type :dapr.ui.events/browser-connect-field :field :workgroup}}]}
    {:fx/type :password-field :prompt-text "Password" :text (or password "")
     :on-text-changed {:event/type :dapr.ui.events/browser-connect-field :field :password}}
    {:fx/type :label :wrap-text true :style "-fx-text-fill: gray;"
     :text "The password is stored in your OS keystore, not in the library file."}
    {:fx/type :button :text (if loading? "Connecting…" "Connect")
     :disable (boolean loading?)
     :on-action {:event/type :dapr.ui.events/browser-connect}}]})

(defmethod device-views/browser-content [:smb :connect] [_ browser]
  (connect-form browser))

(defmethod device-views/browser-content [:smb :browse] [_ browser]
  (device-views/folder-browser browser))

(defmethod device-views/browser-height [:smb :connect] [_]
  220)

(defmethod device-views/browser-height [:smb :browse] [_]
  330)
