;; Governance Token Contract
;; Simple token contract for governance voting

(define-fungible-token governance-token)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))

;; Mint initial supply to contract owner
(ft-mint? governance-token u1000000 CONTRACT_OWNER)

;; Transfer tokens
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (ft-transfer? governance-token amount sender recipient)
  )
)

;; Get token balance
(define-read-only (get-balance (account principal))
  (ft-get-balance governance-token account)
)

;; Get total supply
(define-read-only (get-total-supply)
  (ft-get-supply governance-token)
)

;; Mint new tokens (only owner)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ft-mint? governance-token amount recipient)
  )
)
