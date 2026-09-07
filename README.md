# rag-backend-hybrid

In-process **Okapi BM25** + protocol **`fuse`** (RRF or weighted linear) for [`rag-protocol`](https://github.com/egao1980/rag-protocol) **0.1.2+**. Wraps any `rag-vector-store` (mock / memory / sql / pgvector / tsvector). Lexical index is **not** persisted unless `:lexical-store` is a persisted backend — in-process BM25 needs a re-ingest after restart.

```lisp
(asdf:load-system "rag-backend-hybrid")

(let* ((dense (stack-rag:make-mock-vector-store))
       (store (rag-backend-hybrid:make-hybrid-store :vector-store dense)))
  (stack-rag:upsert store
                    (stack-rag:make-rag-chunk
                     :id "a" :text "red apple" :embedding #(1.0 0.0)))
  (stack-rag:query-store store
                         (stack-rag:make-rag-query
                          :text "apple" :embedding #(1.0 0.0))
                         :top-k 5))
```

`retrieve` (rag-protocol **0.1.1+**) passes a `rag-query` with both text and embedding. Bare vector → dense only. Bare string → BM25 only.

Standalone lexical: `make-bm25-store`. Defaults: k1=1.2, b=0.75, RRF k=60.

```lisp
;; Porter + English stopwords on BM25; linear fusion (dense 0.7 / lex 0.3)
(make-hybrid-store
 :vector-store dense
 :fusion :linear :weights '(0.7 0.3)
 :analyzer (stack-rag:make-simple-analyzer :stemmer :porter :stopwords :english))

;; Persist lexical side in Postgres FTS
(make-hybrid-store :vector-store dense :lexical-store tsvector-store)
```

Stem / stop / linear live on `rag-protocol` (`analyze` / `fuse`). Postgres `tsvector` is `rag-backend-tsvector`. SPLADE is `rag-backend-splade` (`encode-sparse`).

Part of [cl-stack](https://github.com/egao1980/cl-stack). Cookbook: [rag.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/rag.md).

## License

MIT — see [LICENSE](LICENSE).
