;; Simple Eraser Sharing Circle
;; Core contract for eraser checkout and type matching system

(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_BORROWED (err u409))
(define-constant ERR_UNAUTHORIZED (err u403))
(define-constant ERR_INVALID_TYPE (err u400))

(define-data-var next-eraser-id uint u1)

(define-map erasers
  { eraser-id: uint }
  {
    eraser-type: (string-ascii 20),
    borrower: (optional principal),
    borrowed-at: (optional uint),
    replacement-needed: bool
  }
)

(define-map user-borrowed-count principal uint)

(define-public (add-eraser (eraser-type (string-ascii 20)))
  (let ((eraser-id (var-get next-eraser-id)))
    (map-set erasers
      { eraser-id: eraser-id }
      {
        eraser-type: eraser-type,
        borrower: none,
        borrowed-at: none,
        replacement-needed: false
      }
    )
    (var-set next-eraser-id (+ eraser-id u1))
    (ok eraser-id)
  )
)

(define-public (checkout-eraser (eraser-id uint))
  (let ((eraser-data (unwrap! (map-get? erasers { eraser-id: eraser-id }) ERR_NOT_FOUND))
        (current-count (default-to u0 (map-get? user-borrowed-count tx-sender))))
    (asserts! (is-none (get borrower eraser-data)) ERR_ALREADY_BORROWED)
    (map-set erasers
      { eraser-id: eraser-id }
      (merge eraser-data {
        borrower: (some tx-sender),
        borrowed-at: (some stacks-block-height)
      })
    )
    (map-set user-borrowed-count tx-sender (+ current-count u1))
    (ok true)
  )
)

(define-public (return-eraser (eraser-id uint))
  (let ((eraser-data (unwrap! (map-get? erasers { eraser-id: eraser-id }) ERR_NOT_FOUND))
        (current-count (default-to u0 (map-get? user-borrowed-count tx-sender))))
    (asserts! (is-eq (get borrower eraser-data) (some tx-sender)) ERR_UNAUTHORIZED)
    (map-set erasers
      { eraser-id: eraser-id }
      (merge eraser-data {
        borrower: none,
        borrowed-at: none
      })
    )
    (map-set user-borrowed-count tx-sender (- current-count u1))
    (ok true)
  )
)

(define-public (request-replacement (eraser-id uint))
  (let ((eraser-data (unwrap! (map-get? erasers { eraser-id: eraser-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get borrower eraser-data) (some tx-sender)) ERR_UNAUTHORIZED)
    (map-set erasers
      { eraser-id: eraser-id }
      (merge eraser-data { replacement-needed: true })
    )
    (ok true)
  )
)

(define-read-only (get-eraser (eraser-id uint))
  (map-get? erasers { eraser-id: eraser-id })
)

(define-read-only (get-available-erasers-by-type (eraser-type (string-ascii 20)))
  (filter available-of-type (map get-eraser-with-id (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
)

(define-read-only (get-user-borrowed-count (user principal))
  (default-to u0 (map-get? user-borrowed-count user))
)

(define-private (get-eraser-with-id (eraser-id uint))
  (merge { eraser-id: eraser-id } (default-to { eraser-type: "", borrower: none, borrowed-at: none, replacement-needed: false } (get-eraser eraser-id)))
)

(define-private (available-of-type (eraser-data { eraser-id: uint, eraser-type: (string-ascii 20), borrower: (optional principal), borrowed-at: (optional uint), replacement-needed: bool }))
  (is-none (get borrower eraser-data))
)
