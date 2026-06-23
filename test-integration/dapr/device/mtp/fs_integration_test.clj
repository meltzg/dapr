(ns dapr.device.mtp.fs-integration-test
  "Integration tests for the mtp:// backend (melt-jfs + native libmtp) — the one
  thing neither the jimfs unit tests nor a container can cover, since it needs a
  real device. Part of the `clojure -M:integration` suite; runs only when an MTP
  device is attached, otherwise (CI, or no device) it skips.

  Read-only by default — discovery, storage listing, and capacity — so simply
  attaching a phone and running the suite never writes to it or scans its whole
  (potentially huge) library. A bounded copy -> catalog -> delete round-trip runs
  only when DAPR_MTP_WRITE_URL points at a small writable directory on the device
  (e.g. mtp://<vendor:product:serial>/<storage>/dapr-test/)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dapr.device.fs :as device-fs]
            [dapr.device.mtp.fs :as mtp]
            [dapr.fs.nio :as nio])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private devices
  "Attached MTP devices, or nil when discovery fails (no native libmtp) or none is
  connected — in which case these tests skip."
  (try (seq (mtp/devices!)) (catch Throwable _ nil)))

(defn- skip [why]
  (println (str "  (skipping MTP integration test — " why ")")))

(deftest device-discovery-test
  (if-not devices
    (skip "no MTP device attached")
    (testing "devices! reports each attached device with an id, name and mtp:// uri"
      (doseq [d devices]
        (is (not (str/blank? (:id d))))
        (is (not (str/blank? (:name d))))
        (is (str/starts-with? (str (:uri d)) "mtp://"))))))

(deftest storages-and-capacity-test
  (if-not devices
    (skip "no MTP device attached")
    (testing "the device's storages list (read-only) and report capacity"
      (let [storages (device-fs/dir-children! (:uri (first devices)))]
        (is (seq storages) "device should expose at least one storage")
        (is (every? :dir? storages))
        (is (<= 0 (nio/library-free! [(:uri (first storages))]))
            "library-free! should report non-negative capacity for a storage")))))

(deftest write-scan-roundtrip-test
  (let [url (System/getenv "DAPR_MTP_WRITE_URL")]
    (cond
      (not devices)    (skip "no MTP device attached")
      (str/blank? url) (skip "set DAPR_MTP_WRITE_URL (a writable device dir) to test write+scan")
      :else
      (testing "copy a file to the device, catalog! finds it, delete removes it"
        (let [rel     "dapr-integration-test.mp3"
              content "dapr-mtp-integration"
              dir     (Files/createTempDirectory "mtp-it" (make-array FileAttribute 0))]
          (spit (str (.resolve dir rel)) content)
          (try
            (nio/copy-file! (device-fs/root-path! (str (.toUri dir))) (device-fs/root-path! url) rel)
            (let [track (first (filter #(= rel (:rel %)) (nio/catalog! [url])))]
              (is (some? track) "copied track should be discovered by catalog!")
              (is (= (count (.getBytes ^String content)) (:size track))))
            (finally
              (nio/delete-file! (device-fs/root-path! url) rel)))
          (is (not (some #(= rel (:rel %)) (nio/catalog! [url])))
              "track should be gone after delete-file!"))))))
