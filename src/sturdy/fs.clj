(ns sturdy.fs
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [babashka.fs :as fs]
   [taoensso.truss :refer [have]])
  (:import
   (java.nio.charset Charset StandardCharsets)))

(def ^Charset utf8 StandardCharsets/UTF_8)

(defn- charset? [c]
  (cond
    (instance? Charset c) true
    (string? c)
    (try (do (Charset/forName c) true)
         (catch Exception _ false))
    :else false))

(defn ensure-parent!
  "Create parent directories for `path` if needed."
  [path]
  (when-let [dir (fs/parent (have some? path))]
    (fs/create-dirs dir)))

(defn with-extension
  "Return a path equivalent to `path` but with its filename extension replaced by `ext`."
  [path ^String ext]
  (let [p        (fs/path path)
        basename (-> p fs/file-name fs/strip-ext)
        ext      (if (string/starts-with? ext ".") ext (str "." ext))
        new-name (str basename ext)]
    (if-let [parent (fs/parent p)]
      (fs/path parent new-name)
      (fs/path new-name))))

(defn slurp-bytes
  "Read entire file as byte[]. Throws if the file does not exist."
  [p]
  (let [p (fs/path p)]
    (fs/read-all-bytes
     (have fs/exists? p))))

(defn slurp-string
  "Read entire file as String. Charset defaults to UTF-8."
  ([p] (slurp-string p utf8))
  ([p ^Charset cs]
   (String. ^bytes (slurp-bytes p) (have charset? cs))))

(defn slurp-edn
  "Read entire file as EDN. Charset defaults to UTF-8."
  ([p] (slurp-edn p utf8))
  ([p ^Charset cs]
   (let [s (slurp-string p cs)]
     (edn/read-string s))))

(defn read-lines
  "Read file as a vector of lines. Charset defaults to UTF-8."
  ([p] (read-lines p utf8))
  ([p charset]
   (vec (fs/read-all-lines
         (have fs/exists? (fs/path p))
         {:charset (have charset? charset)}))))

(defn spit-bytes!
  "Write byte[] to file. Creates parent dirs.
   Options:
     :append  - true to append (mutually exclusive with :atomic?)
     :atomic? - true to write to temp file and move (mutually exclusive with :append)
     Other opts passed to fs/write-bytes."
  ([p bytes] (spit-bytes! p bytes {}))
  ([p bytes {:keys [atomic? append]}]
   (have identity (not (and append atomic?)))
   (let [path  (fs/path p)
         bytes (have some? bytes)]
     (ensure-parent! path)
     (cond
       atomic?
       (let [dir (or (fs/parent path) (fs/cwd))
             ;; temp in same dir enables true atomic rename
             tmp (fs/create-temp-file {:dir    dir
                                       :prefix (str (fs/file-name path) ".")
                                       :suffix ".tmp"})]
         (try
           (fs/write-bytes tmp bytes {:truncate-existing true :create true})
           (fs/move tmp path {:replace-existing true :atomic-move true})
           (catch Exception e
             (fs/delete-if-exists tmp)
             (throw e))))

       append
       (fs/write-bytes path bytes {:append true :create true})

       :else
       (fs/write-bytes path bytes {:truncate-existing true :create true}))
     path)))

(defn spit-string!
  "Write String. Creates parent dirs.
   Options:
     :charset - default UTF-8
     :append  - true to append (mutually exclusive with :atomic?)
     :atomic? - true to write to temp file and move (mutually exclusive with :append)"
  ([p s] (spit-string! p s {}))
  ([p s {:keys [charset _atomic? _append] :as opts
         :or   {charset utf8}}]
   (let [bs (.getBytes ^String (have string? s) ^Charset charset)]
     ;; Pass all options (except charset) to spit-bytes!
     (spit-bytes! p bs (dissoc opts :charset)))))

(defn spit-edn!
  "Write EDN to file (prn-str). Creates parent dirs."
  ([p map_] (spit-edn! p map_ {}))
  ([p map_ {:keys [charset atomic?]
            :or   {charset utf8}}]
   (spit-string! p
                 (prn-str map_)
                 {:charset charset :atomic? atomic?})))

(defn chmod-600!
  "On POSIX file systems set rw-------."
  [path]
  (let [p (fs/path path)]
    (fs/set-posix-file-permissions p "rw-------")))

(defn chmod-400!
  "On POSIX file systems set r--------."
  [path]
  (let [p (fs/path path)]
    (fs/set-posix-file-permissions p "r--------")))

(defn- ensure-posix-perms!
  "Helper to assert POSIX permissions match an expected string format (e.g., 'rw-------')."
  [path ^String expected-set]
  (let [p            (fs/path path)
        actual-set   (-> p fs/posix-file-permissions fs/posix->str)]
    (when (not= expected-set actual-set)
      (throw (ex-info (str "Invalid file permissions for " p)
                      {:path     (str p)
                       :expected expected-set
                       :actual   actual-set})))
    p))

(defn ensure-600
  "Asserts file has rw------- permissions. Throws on mismatch. Returns path."
  [path]
  (ensure-posix-perms! path "rw-------"))

(defn ensure-400
  "Asserts file has r-------- permissions. Throws on mismatch. Returns path."
  [path]
  (ensure-posix-perms! path "r--------"))

(defn atomic-move
  "Attempts an OS-level atomic move.
   If src and dst are on different filesystems, falls back to a
   copy-delete strategy which ensures dst is updated atomically
   (though the removal of src is not atomic relative to the update of dst)."
  [src dst]
  (let [src (fs/path src)
        dst (fs/path dst)]
    (ensure-parent! dst)
    (try
      ;; try an atomic move
      (fs/move src dst {:atomic-move true :replace-existing true})
      (catch java.nio.file.AtomicMoveNotSupportedException _
        ;; fallback for Cross-Filesystem moves
        (let [dir (or (fs/parent dst) (fs/cwd))
              tmp (fs/create-temp-file {:dir dir
                                        :prefix (str (fs/file-name dst) ".")
                                        :suffix ".tmp"})]
          ;; 1. Copy content to the destination partition
          (fs/copy src tmp {:replace-existing true})
          ;; 2. Atomic swap on the destination partition
          (fs/move tmp dst {:replace-existing true :atomic-move true})
          ;; 3. Cleanup source (not strictly atomic with step 2)
          (fs/delete src))))))
