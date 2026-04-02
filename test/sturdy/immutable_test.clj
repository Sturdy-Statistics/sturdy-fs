(ns sturdy.immutable-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [sturdy.fs.immutable :as m]
   [babashka.fs :as fs])
  (:import
   (java.util Arrays)
   (java.nio.file.attribute PosixFilePermissions)))

(set! *warn-on-reflection* true)

(deftest delete-immutable-tree-test
  (testing "Successfully unlocks and deletes a read-only directory tree"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (fs/with-temp-dir [temp-dir {:prefix "hat-fs-test-"}]
      (let [sub-dir (fs/create-dir (fs/path temp-dir "nested"))
            file-1  (fs/create-file (fs/path temp-dir "root.txt"))
            file-2  (fs/create-file (fs/path sub-dir "nested.txt"))]

        ;; Write some dummy data
        (spit (fs/file file-1) "root data")
        (spit (fs/file file-2) "nested data")

        ;; 1. Lock the tree
        (m/make-immutable! temp-dir)

        ;; Verify it's locked using PosixFilePermissions/toString
        (is (= "r-xr-xr-x" (PosixFilePermissions/toString (fs/posix-file-permissions temp-dir))))
        (is (= "r--r--r--" (PosixFilePermissions/toString (fs/posix-file-permissions file-1))))

        ;; 2. Delete the locked tree
        (m/delete-immutable-tree! temp-dir)

        ;; Verify it's completely gone
        (is (not (fs/exists? temp-dir)))
        (is (not (fs/exists? sub-dir)))
        (is (not (fs/exists? file-1)))))))

(deftest directory-hashing-determinism-test
  (testing "compute-dir-sha256 prevents boundary collisions using null-byte delimiters"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (fs/with-temp-dir [dir1 {:prefix "hash-test-A-"}]
      (fs/with-temp-dir [dir2 {:prefix "hash-test-B-"}]

        ;; Scenario A: file named "ab" with content "c"
        (spit (fs/file dir1 "ab") "c")

        ;; Scenario B: file named "a" with content "bc"
        (spit (fs/file dir2 "a") "bc")

        ;; Without delimiters in `compute-file-sha256`, both evaluate to "abc".
        (is (not
             (Arrays/equals
              (m/compute-dir-sha256 dir1)
              (m/compute-dir-sha256 dir2))))))))
