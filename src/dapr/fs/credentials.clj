(ns dapr.fs.credentials
  "SMB share credentials stored in the operating system's secure keystore via
  java-keyring (Linux Secret Service/KWallet over D-Bus, macOS Keychain, Windows
  Credential Manager). This is the ONLY namespace that touches java-keyring. The
  keystore daemon is contacted only when these functions are actually called (a
  scan/save), not at load time, so building/testing needs no running keyring.

  One entry per SMB host (the keyring account), holding an EDN map
  {:workgroup :username :password}. The host is the lookup key because the
  persisted library root is deliberately credential-free (smb://host/share/…) —
  the password is never written to libraries.edn, only here, encrypted at rest by
  the OS."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (com.github.javakeyring Keyring PasswordAccessException)))

(def ^:private service
  "Keyring service name namespacing Dapr's SMB entries within the OS keystore."
  "dapr-smb")

(defn- keyring ^Keyring [] (Keyring/create))

(defn save!
  "Store `creds` ({:username :password :workgroup}) for SMB `host` in the OS
  keystore, replacing any existing entry. Blank/nil values are dropped, so saving
  {} effectively records guest access (no credentials)."
  [host creds]
  (let [creds (into {} (remove (fn [[_ v]] (str/blank? (str v))) creds))]
    (.setPassword (keyring) service host (pr-str creds))))

(defn lookup
  "Return the stored credential map for SMB `host`, or nil when none is saved
  (guest/anonymous) or the keystore is unavailable. Never throws."
  [host]
  (try
    (let [edn (.getPassword (keyring) service host)]
      (not-empty (edn/read-string edn)))
    (catch PasswordAccessException _ nil)
    (catch Exception _ nil)))

(defn forget!
  "Delete any stored credentials for SMB `host`. No-op when none exist."
  [host]
  (try (.deletePassword (keyring) service host)
       (catch Exception _ nil)))
