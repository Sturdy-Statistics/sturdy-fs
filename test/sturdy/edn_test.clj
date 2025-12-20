(ns sturdy.edn-test
  (:require
    [clojure.test :refer [deftest is]]
    [babashka.fs :as fs]
    [sturdy.fs :as sfs]
    [taoensso.truss :refer [throws?]])
  (:import
    (java.nio.charset StandardCharsets)))

(defn with-temp-file
  "Create a temp file, write `s` to it (as UTF-8 unless charset provided),
   pass its path to f, and clean up."
  ([s f] (with-temp-file s f StandardCharsets/UTF_8))
  ([s f charset]
   (let [dir  (fs/create-temp-dir)
         file (fs/path dir "t.edn")]
     (try
       (fs/write-bytes file (.getBytes ^String s ^java.nio.charset.Charset charset))
       (f file)
       (finally
         (fs/delete-if-exists file)
         (fs/delete-if-exists dir))))))

(deftest slurp-edn-parses-map
  (with-temp-file "{:a 1 :b [2 3]}"
    (fn [p]
      (is (= {:a 1 :b [2 3]} (sfs/slurp-edn p))))))

(deftest slurp-edn-nonexistent-file-bubbles-io-error
  (let [p (fs/path (fs/create-temp-dir) "missing.edn")]
    (try
      (is (throws? (sfs/slurp-edn p)))
      (finally
        ;; cleanup parent dir
        (fs/delete-if-exists (fs/parent p))))))

(deftest slurp-edn-charset-handling
  (with-temp-file "{:s \"café\"}"
    (fn [p]
      (is (= {:s "café"} (sfs/slurp-edn p StandardCharsets/UTF_8)))))
  ;; ISO-8859-1 encoded file, read with ISO-8859-1:
  (let [s "{:s \"caf\u00E9\"}"] ;; 'café' with é = \u00E9
    (with-temp-file s
      (fn [p]
        (is (= {:s "café"} (sfs/slurp-edn p StandardCharsets/ISO_8859_1))))
      StandardCharsets/ISO_8859_1)))

(defn with-temp-dir [f]
  (let [dir (fs/create-temp-dir)]
    (try
      (f dir)
      (finally
        (fs/delete-tree dir)))))

(deftest spit-slurp-edn-roundtrip-defaults
  (with-temp-dir
    (fn [dir]
      (let [p   (fs/path dir "a" "b" "c.edn")
            m   {:a 1 :b [2 3] :s "café" :ratio 22/7 :set #{:x :y}}
            _   (sfs/spit-edn! p m)              ;; defaults: UTF-8, non-atomic
            out (sfs/slurp-edn p)]
        (is (= m out))
        (is (fs/exists? p))
        ;; function returns the same path value you passed (per your impl)
        (is (= p (sfs/spit-edn! p m)))))))

(deftest spit-slurp-edn-roundtrip-atomic
  (with-temp-dir
    (fn [dir]
      (let [p   (fs/path dir "atomic.edn")
            m   {:k :v, :nums [1 2 3]}
            _   (sfs/spit-edn! p m {:atomic? true})
            out (sfs/slurp-edn p)]
        (is (= m out))
        (is (fs/exists? p))))))

(deftest spit-slurp-edn-roundtrip-charset
  ;; ISO-8859-1 supports 'é', so this roundtrip is safe with that charset.
  (with-temp-dir
    (fn [dir]
      (let [p   (fs/path dir "latin1.edn")
            m   {:s "caf\u00E9"}
            cs  StandardCharsets/ISO_8859_1
            _   (sfs/spit-edn! p m {:charset cs})
            out (sfs/slurp-edn p cs)]
        (is (= m out))))))
