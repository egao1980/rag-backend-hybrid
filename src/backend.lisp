(in-package #:rag-backend-hybrid)

;;; In-process Okapi BM25 + RRF. Lexical index is not persisted — wrap
;;; sql/pgvector for vectors; re-ingest after restart. Not tsvector.

(defun tokenize (text)
  "Lowercase alphanumeric tokens (CL characters). Empty / NIL → ()."
  (let ((s (string-downcase (or text "")))
        (out '())
        (start nil))
    (loop for i from 0 below (length s)
          for c = (char s i)
          for alnum = (alphanumericp c)
          do (cond
               ((and alnum (null start))
                (setf start i))
               ((and (not alnum) start)
                (push (subseq s start i) out)
                (setf start nil)))
          finally (when start
                    (push (subseq s start) out)))
    (nreverse out)))

(defun %unique (items)
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (dolist (x items)
      (unless (gethash x seen)
        (setf (gethash x seen) t)
        (push x out)))
    (nreverse out)))

(defun %as-list (x)
  (if (listp x) x (list x)))

(defun %query-text (query)
  (cond
    ((stringp query) query)
    ((typep query 'rag-protocol:rag-query)
     (rag-protocol:rag-query-text query))
    (t nil)))

(defun %query-embedding (query)
  (cond
    ((and (vectorp query) (not (stringp query))) query)
    ((listp query) (coerce query 'vector))
    ((typep query 'rag-protocol:rag-query)
     (rag-protocol:rag-query-embedding query))
    (t nil)))

(defun rrf-fuse (hit-lists &key (k 60) top-k)
  "Reciprocal rank fusion. Earlier lists win chunk identity on id collision."
  (let ((scores (make-hash-table :test 'equal))
        (chunks (make-hash-table :test 'equal)))
    (dolist (hits hit-lists)
      (loop for hit in hits
            for rank from 1
            for chunk = (rag-protocol:rag-hit-chunk hit)
            for id = (rag-protocol:rag-chunk-id chunk)
            do (unless (gethash id chunks)
                 (setf (gethash id chunks) chunk))
               (incf (gethash id scores 0f0)
                     (float (/ 1d0 (+ k rank)) 1f0))))
    (let ((hits (loop for id being the hash-keys of scores
                      using (hash-value score)
                      collect (rag-protocol:make-rag-hit
                               :chunk (gethash id chunks)
                               :score score))))
      (rag-protocol:rerank (rag-protocol:make-identity-reranker)
                           nil hits :top-k (or top-k (hash-table-count scores))))))

(defstruct (bm25-doc (:conc-name bdoc-))
  chunk
  tokens
  tf
  dl)

(defclass bm25-store (rag-protocol:rag-vector-store)
  ((docs :initform (make-hash-table :test 'equal) :accessor bm25-docs)
   (df :initform (make-hash-table :test 'equal) :accessor bm25-df)
   (n :initform 0 :accessor bm25-n)
   (total-dl :initform 0 :accessor bm25-total-dl)
   (k1 :initarg :k1 :accessor bm25-k1 :initform 1.2)
   (b :initarg :b :accessor bm25-b :initform 0.75)))

(defun make-bm25-store (&key (k1 1.2) (b 0.75))
  (make-instance 'bm25-store :k1 k1 :b b))

(defun use-bm25-store (&rest args &key &allow-other-keys)
  (setf rag-protocol:*rag-store* (apply #'make-bm25-store args)))

(defun %term-tf (tokens)
  (let ((tf (make-hash-table :test 'equal)))
    (dolist (tok tokens)
      (incf (gethash tok tf 0)))
    tf))

(defun %unindex (store id)
  (let ((doc (gethash id (bm25-docs store))))
    (when doc
      (decf (bm25-n store))
      (decf (bm25-total-dl store) (bdoc-dl doc))
      (maphash (lambda (term tf)
                 (declare (ignore tf))
                 (let ((c (gethash term (bm25-df store))))
                   (when c
                     (if (<= c 1)
                         (remhash term (bm25-df store))
                         (setf (gethash term (bm25-df store)) (1- c))))))
               (bdoc-tf doc))
      (remhash id (bm25-docs store))
      t)))

(defun %index (store chunk)
  (unless (rag-protocol:rag-chunk-id chunk)
    (error 'rag-protocol:rag-error :message "chunk id required for upsert"))
  (let* ((id (rag-protocol:rag-chunk-id chunk))
         (tokens (tokenize (rag-protocol:rag-chunk-text chunk)))
         (tf (%term-tf tokens))
         (dl (length tokens)))
    (%unindex store id)
    (incf (bm25-n store))
    (incf (bm25-total-dl store) dl)
    (maphash (lambda (term count)
               (declare (ignore count))
               (incf (gethash term (bm25-df store) 0)))
             tf)
    (setf (gethash id (bm25-docs store))
          (make-bm25-doc :chunk chunk :tokens tokens :tf tf :dl dl))))

(defun %idf (store term)
  (let ((n (bm25-n store))
        (df (or (gethash term (bm25-df store)) 0)))
    (if (zerop n)
        0d0
        (log (+ 1d0 (/ (+ (- n df) 0.5d0) (+ df 0.5d0)))))))

(defun %avgdl (store)
  (let ((n (bm25-n store)))
    (if (zerop n)
        1d0
        (/ (float (bm25-total-dl store) 1d0) n))))

(defun %score-doc (store doc query-terms)
  (let ((k1 (float (bm25-k1 store) 1d0))
        (b (float (bm25-b store) 1d0))
        (avgdl (%avgdl store))
        (dl (max 1 (bdoc-dl doc)))
        (tf-table (bdoc-tf doc))
        (score 0d0))
    (dolist (term query-terms)
      (let ((tf (gethash term tf-table)))
        (when (and tf (plusp tf))
          (let* ((idf (%idf store term))
                 (norm (+ tf (* k1 (+ (- 1d0 b) (* b (/ dl avgdl)))))))
            (incf score (* idf (/ (* tf (+ k1 1d0)) norm)))))))
    (float score 1f0)))

(defmethod rag-protocol:upsert ((store bm25-store) chunks)
  (dolist (ch (%as-list chunks))
    (%index store ch))
  store)

(defmethod rag-protocol:delete-ids ((store bm25-store) ids)
  (let* ((ids (%as-list ids))
         (missing '())
         (deleted '()))
    (dolist (id ids)
      (if (%unindex store id)
          (push id deleted)
          (push id missing)))
    (setf missing (nreverse missing)
          deleted (nreverse deleted))
    (when missing
      (restart-case
          (error 'rag-protocol:rag-not-found
                 :ids missing
                 :message (format nil "unknown chunk ids: ~s" missing))
        (continue ()
          :report "Skip missing ids"
          (return-from rag-protocol:delete-ids deleted))
        (use-value (value)
          :report "Return a supplied value"
          (return-from rag-protocol:delete-ids value))))
    deleted))

(defmethod rag-protocol:query-store ((store bm25-store) query &key top-k filter)
  (let* ((text (%query-text query))
         (terms (%unique (tokenize text))))
    (unless (and text (plusp (length text)))
      (error 'rag-protocol:rag-error :message "bm25 query needs text"))
    (let ((hits '()))
      (maphash (lambda (id doc)
                 (declare (ignore id))
                 (let ((chunk (bdoc-chunk doc)))
                   (when (or (null filter) (funcall filter chunk))
                     (let ((score (%score-doc store doc terms)))
                       (when (plusp score)
                         (push (rag-protocol:make-rag-hit :chunk chunk :score score)
                               hits))))))
               (bm25-docs store))
      (rag-protocol:rerank (rag-protocol:make-identity-reranker)
                           query hits :top-k (or top-k 5)))))

(defclass hybrid-store (rag-protocol:rag-vector-store)
  ((vector-store :initarg :vector-store :accessor hybrid-store-vector-store)
   (bm25 :initarg :bm25 :accessor hybrid-store-bm25)
   (rrf-k :initarg :rrf-k :accessor hybrid-store-rrf-k :initform 60)
   (fetch-k :initarg :fetch-k :accessor hybrid-store-fetch-k :initform 20)))

(defun make-hybrid-store (&key vector-store bm25 (rrf-k 60) (fetch-k 20)
                               (k1 1.2) (b 0.75))
  (unless vector-store
    (error 'rag-protocol:rag-error
           :message "hybrid-store needs :vector-store"))
  (make-instance 'hybrid-store
                 :vector-store vector-store
                 :bm25 (or bm25 (make-bm25-store :k1 k1 :b b))
                 :rrf-k rrf-k
                 :fetch-k fetch-k))

(defun use-hybrid-store (&rest args &key &allow-other-keys)
  (setf rag-protocol:*rag-store* (apply #'make-hybrid-store args)))

(defmethod rag-protocol:upsert ((store hybrid-store) chunks)
  (rag-protocol:upsert (hybrid-store-vector-store store) chunks)
  (rag-protocol:upsert (hybrid-store-bm25 store) chunks)
  store)

(defmethod rag-protocol:delete-ids ((store hybrid-store) ids)
  (handler-bind ((rag-protocol:rag-not-found
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'continue))))
    (rag-protocol:delete-ids (hybrid-store-bm25 store) ids))
  (rag-protocol:delete-ids (hybrid-store-vector-store store) ids))

(defmethod rag-protocol:query-store ((store hybrid-store) query &key top-k filter)
  (let* ((text (%query-text query))
         (vec (%query-embedding query))
         (k (or top-k 5))
         (fk (max k (hybrid-store-fetch-k store)))
         (dense nil)
         (lex nil))
    (unless (or vec (and text (plusp (length text))))
      (error 'rag-protocol:rag-error
             :message "hybrid query needs text or embedding"))
    (when vec
      (setf dense (rag-protocol:query-store (hybrid-store-vector-store store)
                                            query :top-k fk :filter filter)))
    (when (and text (plusp (length (tokenize text))))
      (setf lex (rag-protocol:query-store (hybrid-store-bm25 store)
                                          query :top-k fk :filter filter)))
    (rrf-fuse (remove nil (list dense lex))
              :k (hybrid-store-rrf-k store)
              :top-k k)))
