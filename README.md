# rag-backend-hybrid

In-process **Okapi BM25** + **RRF** for [`rag-protocol`](https://github.com/egao1980/rag-protocol). Wraps any `rag-vector-store` (mock / memory / sql / pgvector). Lexical index is **not** persisted — re-ingest after restart.

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

Standalone lexical: `make-bm25-store`. Tokenize = lowercase alphanumeric (CL characters). Defaults: k1=1.2, b=0.75, RRF k=60.

Not here: Postgres `tsvector`, stemmers, stopword lists, weighted linear fusion, SPLADE.

Part of [cl-stack](https://github.com/egao1980/cl-stack). Cookbook: [rag.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/rag.md).

## License

MIT — see [LICENSE](LICENSE).
