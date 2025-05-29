;; Campaign Management Smart Contract
;; Handles creation, configuration, and management of crowdfunding campaigns

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CAMPAIGN-NOT-FOUND (err u101))
(define-constant ERR-CAMPAIGN-ALREADY-STARTED (err u102))
(define-constant ERR-CAMPAIGN-ALREADY-ENDED (err u103))
(define-constant ERR-INVALID-GOAL (err u104))
(define-constant ERR-INVALID-DURATION (err u105))
(define-constant ERR-INVALID-TOKEN (err u106))
(define-constant ERR-CAMPAIGN-ACTIVE (err u107))
(define-constant ERR-ALREADY-CANCELLED (err u108))

;; Campaign status constants
(define-constant STATUS-DRAFT u0)
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-SUCCESS u2)
(define-constant STATUS-FAILED u3)
(define-constant STATUS-CANCELLED u4)

;; Data variables
(define-data-var campaign-counter uint u0)

;; Data maps
(define-map campaigns
  { campaign-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    media-links: (list 5 (string-ascii 200)),
    funding-goal: uint,
    start-time: uint,
    end-time: uint,
    status: uint,
    current-funding: uint,
    accepted-tokens: (list 5 (string-ascii 10)),
    created-at: uint
  }
)

(define-map milestones
  { campaign-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-target: uint,
    completed: bool
  }
)

(define-map user-campaigns
  { owner: principal }
  { campaign-ids: (list 50 uint) }
)

;; Private functions
(define-private (is-campaign-owner (campaign-id uint) (user principal))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (is-eq (get owner campaign) user)
    false
  )
)

(define-private (get-current-time)
  stacks-block-height ;; Using block height as time proxy
)

(define-private (is-valid-token (token (string-ascii 10)))
  (or (is-eq token "STX") (is-eq token "BTC"))
)

(define-private (validate-tokens (tokens (list 5 (string-ascii 10))))
  (fold check-token-valid tokens true)
)

(define-private (check-token-valid (token (string-ascii 10)) (valid bool))
  (and valid (is-valid-token token))
)

(define-private (add-campaign-to-user (owner principal) (campaign-id uint))
  (let (
    (current-campaigns (default-to (list) (get campaign-ids (map-get? user-campaigns { owner: owner }))))
  )
    (map-set user-campaigns
      { owner: owner }
      { campaign-ids: (unwrap! (as-max-len? (append current-campaigns campaign-id) u50) false) }
    )
  )
)

;; Public functions

;; Create a new campaign
(define-public (create-campaign
    (title (string-ascii 100))
    (description (string-ascii 500))
    (media-links (list 5 (string-ascii 200)))
    (funding-goal uint)
    (duration uint)
    (accepted-tokens (list 5 (string-ascii 10)))
  )
  (let (
    (campaign-id (+ (var-get campaign-counter) u1))
    (current-time (get-current-time))
    (end-time (+ current-time duration))
  )
    ;; Validate inputs
    (asserts! (> funding-goal u0) ERR-INVALID-GOAL)
    (asserts! (> duration u0) ERR-INVALID-DURATION)
    (asserts! (validate-tokens accepted-tokens) ERR-INVALID-TOKEN)
    
    ;; Create campaign
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        media-links: media-links,
        funding-goal: funding-goal,
        start-time: current-time,
        end-time: end-time,
        status: STATUS-DRAFT,
        current-funding: u0,
        accepted-tokens: accepted-tokens,
        created-at: current-time
      }
    )
    
    ;; Add milestones
    ;; (fold add-milestone-helper milestones { campaign-id: campaign-id, index: u0 })
    
    ;; Add to user's campaign list
    (add-campaign-to-user tx-sender campaign-id)
    
    ;; Update counter
    (var-set campaign-counter campaign-id)
    
    (ok campaign-id)
  )
)

(define-private (add-milestone-helper 
    (milestone { title: (string-ascii 100), description: (string-ascii 300), funding-target: uint })
    (acc { campaign-id: uint, index: uint })
  )
  (begin
    (map-set milestones
      { campaign-id: (get campaign-id acc), milestone-id: (get index acc) }
      {
        title: (get title milestone),
        description: (get description milestone),
        funding-target: (get funding-target milestone),
        completed: false
      }
    )
    { campaign-id: (get campaign-id acc), index: (+ (get index acc) u1) }
  )
)

;; Update campaign metadata (only before campaign starts)
(define-public (update-campaign-metadata
    (campaign-id uint)
    (title (optional (string-ascii 100)))
    (description (optional (string-ascii 500)))
    (media-links (optional (list 5 (string-ascii 200))))
  )
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
  )
    ;; Check authorization
    (asserts! (is-campaign-owner campaign-id tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check campaign status
    (asserts! (is-eq (get status campaign) STATUS-DRAFT) ERR-CAMPAIGN-ALREADY-STARTED)
    
    ;; Update campaign
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign {
        title: (default-to (get title campaign) title),
        description: (default-to (get description campaign) description),
        media-links: (default-to (get media-links campaign) media-links)
      })
    )
    
    (ok true)
  )
)

;; Cancel campaign (only if not started or if owner)
(define-public (cancel-campaign (campaign-id uint))
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
  )
    ;; Check authorization
    (asserts! (is-campaign-owner campaign-id tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check if campaign can be cancelled
    (asserts! (not (is-eq (get status campaign) STATUS-CANCELLED)) ERR-ALREADY-CANCELLED)
    (asserts! (or 
      (is-eq (get status campaign) STATUS-DRAFT)
      (is-eq (get status campaign) STATUS-ACTIVE)
    ) ERR-CAMPAIGN-ALREADY-ENDED)
    
    ;; Update status to cancelled
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { status: STATUS-CANCELLED })
    )
    
    (ok true)
  )
)

;; Start campaign (move from draft to active)
(define-public (start-campaign (campaign-id uint))
  (let (
    (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR-CAMPAIGN-NOT-FOUND))
  )
    ;; Check authorization
    (asserts! (is-campaign-owner campaign-id tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check if campaign is in draft status
    (asserts! (is-eq (get status campaign) STATUS-DRAFT) ERR-CAMPAIGN-ALREADY-STARTED)
    
    ;; Update status to active
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { 
        status: STATUS-ACTIVE,
        start-time: (get-current-time)
      })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get campaign details
(define-read-only (get-campaign-details (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)


(define-private (generate-milestone-ids (campaign-id uint) (count uint))
  (map add-campaign-id-to-index (generate-sequence u0 count))
)

(define-private (add-campaign-id-to-index (index uint))
  { campaign-id: u1, milestone-id: index } ;; This would need campaign-id context
)

(define-private (generate-sequence (start uint) (end uint))
  ;; Helper to generate sequence - simplified implementation
  (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
)

(define-private (get-milestone-by-id (milestone-key { campaign-id: uint, milestone-id: uint }))
  (map-get? milestones milestone-key)
)

;; Get user's campaigns
(define-read-only (get-user-campaigns (user principal))
  (get campaign-ids (map-get? user-campaigns { owner: user }))
)

;; Get campaign status
(define-read-only (get-campaign-status (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (some (get status campaign))
    none
  )
)

;; Get total campaign count
(define-read-only (get-campaign-count)
  (var-get campaign-counter)
)

;; Check if user owns campaign
(define-read-only (is-owner (campaign-id uint) (user principal))
  (is-campaign-owner campaign-id user)
)