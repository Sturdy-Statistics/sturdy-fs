(ns sturdy.fs-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [babashka.fs :as fs]
   [sturdy.fs :as x]
   [taoensso.truss :refer [throws?]]))

(def ^:dynamic *test-tmp* nil)

(use-fixtures :each
  (fn [f]
    (let [tmp (fs/create-temp-dir {:prefix "fs-core-test."})]
      (try
        (binding [*test-tmp* tmp]
          (f))
        (finally
          (fs/delete-tree tmp))))))

(defn pth [& xs] (apply fs/path *test-tmp* xs))

(deftest ensure-parent-and-spit-string
  (let [p (pth "nested" "dir" "file.txt")]
    (x/spit-string! p "hi")
    (is (fs/exists? p))
    (is (= "hi" (x/slurp-string p)))))

(deftest with-extension-test
  (testing "replaces existing extension"
    (is (= (fs/path "foo.csv")
           (x/with-extension "foo.txt" "csv"))))

  (testing "adds extension when none exists"
    (is (= (fs/path "foo.dat")
           (x/with-extension "foo" "dat"))))

  (testing "accepts extension with leading dot"
    (is (= (fs/path "foo.json")
           (x/with-extension "foo.txt" ".json"))))

  (testing "preserves parent directory"
    (is (= (fs/path "/a/b/c.tar.zip")
           (x/with-extension "/a/b/c.tar.gz" "zip"))))

  (testing "dotfiles are treated as having no extension"
    (is (= (fs/path ".bashrc.bak")
           (x/with-extension ".bashrc" "bak")))))

(deftest append-vs-atomic-contract
  (let [p (pth "log.txt")]
    (x/spit-string! p "a\n")
    (x/spit-string! p "b\n" {:append true})
    (is (= "a\nb\n" (x/slurp-string p)))
    (testing "mutex: append + atomic?"
      (is (throws? :any (x/spit-string! p "oops" {:append true :atomic? true}))))))

(deftest write-atomic-overwrite
  (let [p (pth "conf.json")]
    (x/spit-string! (str p) "{\"a\":1}" {:atomic? true})
    (is (= "{\"a\":1}" (x/slurp-string p)))
    ;; overwrite ensures atomic replacement
    (x/spit-string! (str p) "{\"a\":2}" {:atomic? true})
    (is (= "{\"a\":2}" (x/slurp-string p)))))

(deftest read-lines-with-charset
  (let [p (pth "lines.txt")]
    (x/spit-string! p "a\nb\nc\n")
    (is (= ["a" "b" "c"] (x/read-lines p)))
    (is (= ["a" "b" "c"] (x/read-lines p "UTF-8")))))

(deftest chmod-only-on-posix
  (let [p (pth "secret")
        _ (x/spit-string! p "s")
        perms (fn [p] (-> p fs/posix-file-permissions fs/posix->str))]
    (x/chmod-600! p)
    (is (= "rw-------" (perms p)))
    (x/chmod-400! p)
    (is (= "r--------" (perms p)))))

(deftest ensure-permissions-test
  (let [p (pth "sensitive.txt")]
    ;; Create file with default permissions (usually 644 or 755)
    (x/spit-string! p "top secret")

    (testing "ensure-600"
      (testing "throws on default/incorrect permissions"
        (is (throws? :any (x/ensure-600 p))))

      (testing "returns path on success"
        (x/chmod-600! p)
        (is (= p (x/ensure-600 p))))

      (testing "throws if permissions are too restrictive (e.g. 400)"
        (x/chmod-400! p)
        (is (throws? :any (x/ensure-600 p)))))

    (testing "ensure-400"
      (testing "returns path on success"
        ;; File is currently 400 from previous step
        (is (= p (x/ensure-400 p))))

      (testing "throws on incorrect permissions"
        (x/chmod-600! p)
        (is (throws? :any (x/ensure-400 p)))))))

(deftest spit-bytes-atomic
  (let [p (pth "data.bin")
        b1 (.getBytes "hello")
        b2 (.getBytes "world")]
    ;; 1. Initial atomic write
    (x/spit-bytes! p b1 {:atomic? true})
    (is (= "hello" (x/slurp-string p)))

    ;; 2. Overwrite atomic
    (x/spit-bytes! p b2 {:atomic? true})
    (is (= "world" (x/slurp-string p)))

    ;; 3. Ensure contract holds (mutex)
    (is (throws? :any (x/spit-bytes! p b1 {:atomic? true :append true})))))

(deftest atomic-move-test
  (testing "basic atomic move (happy path)"
    (let [src (pth "src.txt")
          dst (pth "dst.txt")]
      (x/spit-string! src "important data")

      (x/atomic-move src dst)

      (is (fs/exists? dst) "Destination should exist")
      (is (not (fs/exists? src)) "Source should be gone")
      (is (= "important data" (x/slurp-string dst)) "Content should be preserved")))

  (testing "overwrites existing destination"
    (let [src (pth "new-version.txt")
          dst (pth "config.json")]
      (x/spit-string! dst "{\"version\": 1}")
      (x/spit-string! src "{\"version\": 2}")

      (x/atomic-move src dst)

      (is (= "{\"version\": 2}" (x/slurp-string dst)) "Destination should be overwritten")
      (is (not (fs/exists? src)))))

  (testing "creates missing parent directories for destination"
    (let [src (pth "deep-src.txt")
          dst (pth "a" "b" "c" "moved.txt")]
      (x/spit-string! src "deep data")

      (x/atomic-move src dst)

      (is (fs/exists? dst))
      (is (= "deep data" (x/slurp-string dst)))))

  (testing "fails when source does not exist"
    (let [src (pth "ghost.txt")
          dst (pth "wont-exist.txt")]
      ;; Expecting a java.nio.file.NoSuchFileException or similar
      (is (throws? :any (x/atomic-move src dst)))))

  (testing "move file to itself (no-op or success)"
    (let [src (pth "same.txt")]
      (x/spit-string! src "content")
      ;; Moving a file to itself should not delete the file
      (x/atomic-move src src)
      (is (= "content" (x/slurp-string src))))))
