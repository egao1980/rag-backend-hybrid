(in-package #:rag-backend-hybrid/tests)

(defun %vec (&rest xs)
  (map 'vector (lambda (x) (float x 1f0)) xs))

(defun %chunk (id text &optional emb)
  (rag-protocol:make-rag-chunk :id id :document-id "d"
                               :text text :embedding emb))

(defun %q (&key text embedding (top-k 5))
  (rag-protocol:make-rag-query :text text :embedding embedding :top-k top-k))

(deftest tokenize-basic
  (ok (equal '("hello" "world") (rag-backend-hybrid:tokenize "Hello, world!")))
  (ok (null (rag-backend-hybrid:tokenize "")))
  (ok (null (rag-backend-hybrid:tokenize nil))))

(deftest bm25-ranks-lexical
  (let ((store (rag-backend-hybrid:make-bm25-store)))
    (rag-protocol:upsert store
                         (list (%chunk "a" "the cat sat on the mat")
                               (%chunk "b" "the cat")))
    (let ((hits (rag-protocol:query-store store "cat mat" :top-k 2)))
      (ok (= 2 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits)))))
      (ok (> (rag-protocol:rag-hit-score (first hits))
             (rag-protocol:rag-hit-score (second hits)))))))

(deftest bm25-needs-text
  (let ((store (rag-backend-hybrid:make-bm25-store)))
    (rag-protocol:upsert store (%chunk "a" "alpha"))
    (ok (signals (rag-protocol:query-store store (%vec 1 0) :top-k 1)
                 'rag-protocol:rag-error))))

(deftest bm25-replace-and-delete
  (let ((store (rag-backend-hybrid:make-bm25-store)))
    (rag-protocol:upsert store (%chunk "a" "old token"))
    (rag-protocol:upsert store (%chunk "a" "new token"))
    (let ((hits (rag-protocol:query-store store "new" :top-k 1)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))
    (ok (null (rag-protocol:query-store store "old" :top-k 1)))
    (ok (equal '("a") (rag-protocol:delete-ids store "a")))
    (ok (signals (rag-protocol:delete-ids store "a")
                 'rag-protocol:rag-not-found))))

(deftest rrf-merges-ranks
  (let* ((a (rag-protocol:make-rag-hit :chunk (%chunk "a" "a") :score 1f0))
         (b (rag-protocol:make-rag-hit :chunk (%chunk "b" "b") :score 1f0))
         (hits (rag-backend-hybrid:rrf-fuse (list (list a) (list b a))
                                            :k 60 :top-k 2)))
    (ok (equal "a" (rag-protocol:rag-chunk-id
                    (rag-protocol:rag-hit-chunk (first hits)))))
    (ok (= 2 (length hits)))))

(deftest hybrid-rrf-prefers-overlap
  (let* ((dense (rag-protocol:make-mock-vector-store))
         (store (rag-backend-hybrid:make-hybrid-store :vector-store dense :fetch-k 5)))
    (rag-protocol:upsert store
                         (list (%chunk "a" "red apple" (%vec 1 0))
                               (%chunk "b" "blue car" (%vec 0.95 0.05))
                               (%chunk "c" "apple apple apple" (%vec 0 1))))
    (let ((hits (rag-protocol:query-store
                 store (%q :text "apple" :embedding (%vec 1 0)) :top-k 3)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits)))))
      (ok (= 3 (length hits))))))

(deftest hybrid-vector-only
  (let* ((dense (rag-protocol:make-mock-vector-store))
         (store (rag-backend-hybrid:make-hybrid-store :vector-store dense)))
    (rag-protocol:upsert store
                         (list (%chunk "a" "alpha" (%vec 1 0))
                               (%chunk "b" "beta" (%vec 0 1))))
    (let ((hits (rag-protocol:query-store store (%vec 1 0) :top-k 1)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest hybrid-text-only
  (let* ((dense (rag-protocol:make-mock-vector-store))
         (store (rag-backend-hybrid:make-hybrid-store :vector-store dense)))
    (rag-protocol:upsert store
                         (list (%chunk "a" "alpha token" (%vec 1 0))
                               (%chunk "b" "zzz" (%vec 0 1))))
    (let ((hits (rag-protocol:query-store store "alpha" :top-k 1)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest hybrid-filter-and-delete
  (let* ((dense (rag-protocol:make-mock-vector-store))
         (store (rag-backend-hybrid:make-hybrid-store :vector-store dense)))
    (rag-protocol:upsert store
                         (list (%chunk "a" "keep apple" (%vec 1 0))
                               (%chunk "b" "drop apple" (%vec 1 0))))
    (let ((hits (rag-protocol:query-store
                 store (%q :text "apple" :embedding (%vec 1 0))
                 :top-k 5
                 :filter (lambda (ch)
                           (equal "keep apple" (rag-protocol:rag-chunk-text ch))))))
      (ok (= 1 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))
    (ok (equal '("a") (rag-protocol:delete-ids store "a")))
    (ok (signals (rag-protocol:delete-ids store "a")
                 'rag-protocol:rag-not-found))))

(deftest hybrid-needs-store
  (ok (signals (rag-backend-hybrid:make-hybrid-store)
               'rag-protocol:rag-error)))

(deftest use-hybrid-binds
  (let ((rag-protocol:*rag-store* nil)
        (dense (rag-protocol:make-mock-vector-store)))
    (rag-backend-hybrid:use-hybrid-store :vector-store dense)
    (ok (typep rag-protocol:*rag-store* 'rag-backend-hybrid:hybrid-store))
    (setf rag-protocol:*rag-store* nil)))
