(ns sturdy.fs.immutable
  (:require
   [clojure.java.io :as io]
   [babashka.fs :as fs])
  (:import
   (java.security MessageDigest)
   (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(defn make-immutable!
  "Recursively walks a path.
   Sets files to 444 (read-only) and directories to 555 (read/execute)."
  [path]
  (let [target (fs/path path)]
    (when (fs/sym-link? target)
      (throw (ex-info "Symlinks are strictly forbidden in managed artifacts."
                      {:path (str target)})))

    (if (fs/directory? target)
      (do
        ;; Recurse through the children: walk tree "bottom-up"
        (doseq [child (fs/list-dir target)]
          (make-immutable! child))

        ;; Directories need 555
        (fs/set-posix-file-permissions target "r-xr-xr-x"))

      ;; Regular files need 444
      (fs/set-posix-file-permissions target "r--r--r--"))))

(defn- unlock-tree!
  "Recursively unlocks an immutable tree top-down."
  [root]
  (when (fs/exists? root)
    (if (fs/directory? root)
      (do
        ;; 1. Unlock the directory FIRST (700) so we can modify its contents
        (fs/set-posix-file-permissions root "rwx------")
        ;; 2. Recurse into children
        (doseq [child (fs/list-dir root)]
          (unlock-tree! child)))

      ;; Base case: unlock the file (600)
      (fs/set-posix-file-permissions root "rw-------"))))

(defn delete-immutable-tree!
  "Safely deletes a directory tree that was previously locked via make-immutable!."
  [root]
  (when (fs/exists? root)
    (unlock-tree! root)
    (fs/delete-tree root)))

(defn- compute-file-sha256
  "Computes a cryptographically bound hash of a file's relative path and its contents."
  ^bytes [root-path rel-path]
  (let [abs-path (fs/file root-path rel-path)
        md       (MessageDigest/getInstance "SHA-256")
        buffer   (byte-array 8192)]

    ;; 1. Hash the relative path with a trailing null-byte delimiter (\u0000)
    (.update md (.getBytes (str rel-path "\u0000") StandardCharsets/UTF_8))

    ;; 2. Hash the file contents
    (with-open [in (io/input-stream abs-path)]
      (loop []
        (let [bytes-read (.read in buffer)]
          (when (pos? bytes-read)
            (.update md buffer 0 bytes-read)
            (recur)))))

    (.digest md)))

(defn compute-dir-sha256
  "Deterministically hashes a directory.
   Hashes each file individually to prevent boundary collisions,
   then hashes the sorted collection of file hashes."
  ^bytes [dir-path]
  (let [root  (fs/path dir-path)
        files (->> (fs/glob root "**")
                   (filter fs/regular-file?)
                   ;; Sort strictly by relative path string for deterministic ordering
                   (map #(fs/relativize root %))
                   (sort-by str))

        ;; Compute individual file hashes
        file-hashes (map #(compute-file-sha256 root %) files)

        ;; Finally, hash the concatenated file hashes to seal the directory
        dir-md      (MessageDigest/getInstance "SHA-256")]

    (doseq [fh file-hashes]
      (.update dir-md ^bytes fh))

    (.digest dir-md)))
