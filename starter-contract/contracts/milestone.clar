;; Milestone & Fund Release Smart Contract
;; Enables phased release of funds based on milestone completion

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u101))
(define-constant ERR_MILESTONE_NOT_FOUND (err u102))
(define-constant ERR_MILESTONE_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_MILESTONE_STATUS (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_VOTING_PERIOD_ENDED (err u107))
(define-constant ERR_MILESTONE_NOT_CLAIMED (err u108))

;; Data Variables
(define-data-var next-campaign-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var voting-period uint u144) ;; ~24 hours in blocks

;; Milestone Status Enum
(define-constant MILESTONE_PENDING u0)
(define-constant MILESTONE_CLAIMED u1)
(define-constant MILESTONE_VERIFIED u2)
(define-constant MILESTONE_REJECTED u3)

;; Data Maps
(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    total-funds: uint,
    released-funds: uint,
    created-at: uint
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    campaign-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    expected-date: uint,
    fund-amount: uint,
    status: uint,
    claimed-at: (optional uint),
    verified-at: (optional uint),
    votes-for: uint,
    votes-against: uint,
    voting-deadline: (optional uint)
  }
)

(define-map campaign-milestones
  { campaign-id: uint, milestone-index: uint }
  { milestone-id: uint }
)

(define-map milestone-votes
  { milestone-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

(define-map campaign-funds
  { campaign-id: uint }
  { balance: uint }
)

;; Read-only functions

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-milestone-status (milestone-id uint))
  (match (map-get? milestones { milestone-id: milestone-id })
    milestone (ok (get status milestone))
    ERR_MILESTONE_NOT_FOUND
  )
)

(define-read-only (get-campaign-milestones (campaign-id uint))
  (let ((milestone-1 (map-get? campaign-milestones { campaign-id: campaign-id, milestone-index: u1 }))
        (milestone-2 (map-get? campaign-milestones { campaign-id: campaign-id, milestone-index: u2 }))
        (milestone-3 (map-get? campaign-milestones { campaign-id: campaign-id, milestone-index: u3 })))
    {
      milestone-1: milestone-1,
      milestone-2: milestone-2,
      milestone-3: milestone-3
    }
  )
)

(define-read-only (get-campaign-funds-balance (campaign-id uint))
  (default-to u0 (get balance (map-get? campaign-funds { campaign-id: campaign-id })))
)

(define-read-only (has-voted (milestone-id uint) (voter principal))
  (is-some (map-get? milestone-votes { milestone-id: milestone-id, voter: voter }))
)

;; Private functions

(define-private (is-campaign-creator (campaign-id uint) (user principal))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (is-eq (get creator campaign) user)
    false
  )
)

(define-private (update-milestone-status (milestone-id uint) (new-status uint))
  (match (map-get? milestones { milestone-id: milestone-id })
    milestone (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone { status: new-status })
    )
    false
  )
)

;; Public functions

(define-public (create-campaign (title (string-ascii 100)))
  (let ((campaign-id (var-get next-campaign-id)))
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        title: title,
        total-funds: u0,
        released-funds: u0,
        created-at: stacks-block-height
      }
    )
    (map-set campaign-funds
      { campaign-id: campaign-id }
      { balance: u0 }
    )
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (define-milestones 
  (campaign-id uint)
  (milestone-1-title (string-ascii 100))
  (milestone-1-desc (string-ascii 500))
  (milestone-1-date uint)
  (milestone-1-amount uint)
  (milestone-2-title (string-ascii 100))
  (milestone-2-desc (string-ascii 500))
  (milestone-2-date uint)
  (milestone-2-amount uint)
  (milestone-3-title (string-ascii 100))
  (milestone-3-desc (string-ascii 500))
  (milestone-3-date uint)
  (milestone-3-amount uint))
  (let ((campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND)))
    (asserts! (is-campaign-creator campaign-id tx-sender) ERR_UNAUTHORIZED)
    
    ;; Create milestone 1
    (let ((milestone-1-id (var-get next-milestone-id)))
      (map-set milestones
        { milestone-id: milestone-1-id }
        {
          campaign-id: campaign-id,
          title: milestone-1-title,
          description: milestone-1-desc,
          expected-date: milestone-1-date,
          fund-amount: milestone-1-amount,
          status: MILESTONE_PENDING,
          claimed-at: none,
          verified-at: none,
          votes-for: u0,
          votes-against: u0,
          voting-deadline: none
        }
      )
      (map-set campaign-milestones
        { campaign-id: campaign-id, milestone-index: u1 }
        { milestone-id: milestone-1-id }
      )
      (var-set next-milestone-id (+ milestone-1-id u1))
      
      ;; Create milestone 2
      (let ((milestone-2-id (var-get next-milestone-id)))
        (map-set milestones
          { milestone-id: milestone-2-id }
          {
            campaign-id: campaign-id,
            title: milestone-2-title,
            description: milestone-2-desc,
            expected-date: milestone-2-date,
            fund-amount: milestone-2-amount,
            status: MILESTONE_PENDING,
            claimed-at: none,
            verified-at: none,
            votes-for: u0,
            votes-against: u0,
            voting-deadline: none
          }
        )
        (map-set campaign-milestones
          { campaign-id: campaign-id, milestone-index: u2 }
          { milestone-id: milestone-2-id }
        )
        (var-set next-milestone-id (+ milestone-2-id u1))
        
        ;; Create milestone 3
        (let ((milestone-3-id (var-get next-milestone-id)))
          (map-set milestones
            { milestone-id: milestone-3-id }
            {
              campaign-id: campaign-id,
              title: milestone-3-title,
              description: milestone-3-desc,
              expected-date: milestone-3-date,
              fund-amount: milestone-3-amount,
              status: MILESTONE_PENDING,
              claimed-at: none,
              verified-at: none,
              votes-for: u0,
              votes-against: u0,
              voting-deadline: none
            }
          )
          (map-set campaign-milestones
            { campaign-id: campaign-id, milestone-index: u3 }
            { milestone-id: milestone-3-id }
          )
          (var-set next-milestone-id (+ milestone-3-id u1))
          
          ;; Update campaign total funds
          (let ((total-amount (+ (+ milestone-1-amount milestone-2-amount) milestone-3-amount)))
            (map-set campaigns
              { campaign-id: campaign-id }
              (merge campaign { total-funds: total-amount })
            )
            (ok { 
              milestone-1-id: milestone-1-id,
              milestone-2-id: milestone-2-id,
              milestone-3-id: milestone-3-id
            })
          )
        )
      )
    )
  )
)

(define-public (fund-campaign (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
        (current-balance (get-campaign-funds-balance campaign-id)))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set campaign-funds
      { campaign-id: campaign-id }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-public (submit-milestone-claim (milestone-id uint))
  (let ((milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
        (campaign-id (get campaign-id milestone)))
    (asserts! (is-campaign-creator campaign-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status milestone) MILESTONE_PENDING) ERR_INVALID_MILESTONE_STATUS)
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge milestone {
        status: MILESTONE_CLAIMED,
        claimed-at: (some stacks-block-height),
        voting-deadline: (some (+ stacks-block-height (var-get voting-period)))
      })
    )
    (ok true)
  )
)

(define-public (vote-milestone (milestone-id uint) (approve bool))
  (let ((milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND)))
    (asserts! (is-eq (get status milestone) MILESTONE_CLAIMED) ERR_INVALID_MILESTONE_STATUS)
    (asserts! (not (has-voted milestone-id tx-sender)) ERR_ALREADY_VOTED)
    (asserts! (< stacks-block-height (unwrap! (get voting-deadline milestone) ERR_VOTING_PERIOD_ENDED)) ERR_VOTING_PERIOD_ENDED)
    
    ;; Record vote
    (map-set milestone-votes
      { milestone-id: milestone-id, voter: tx-sender }
      { vote: approve, voted-at: stacks-block-height }
    )
    
    ;; Update vote counts
    (if approve
      (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { votes-for: (+ (get votes-for milestone) u1) })
      )
      (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { votes-against: (+ (get votes-against milestone) u1) })
      )
    )
    (ok true)
  )
)

(define-public (verify-milestone (milestone-id uint))
  (let ((milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND)))
    (asserts! (is-eq (get status milestone) MILESTONE_CLAIMED) ERR_INVALID_MILESTONE_STATUS)
    (asserts! (>= stacks-block-height (unwrap! (get voting-deadline milestone) ERR_VOTING_PERIOD_ENDED)) ERR_VOTING_PERIOD_ENDED)
    
    ;; Check if milestone is approved (simple majority)
    (let ((votes-for (get votes-for milestone))
          (votes-against (get votes-against milestone)))
      (if (> votes-for votes-against)
        (begin
          (map-set milestones
            { milestone-id: milestone-id }
            (merge milestone {
              status: MILESTONE_VERIFIED,
              verified-at: (some stacks-block-height)
            })
          )
          (ok true)
        )
        (begin
          (map-set milestones
            { milestone-id: milestone-id }
            (merge milestone { status: MILESTONE_REJECTED })
          )
          (ok false)
        )
      )
    )
  )
)

(define-public (release-funds (milestone-id uint))
  (let ((milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
        (campaign-id (get campaign-id milestone))
        (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
        (fund-amount (get fund-amount milestone))
        (current-balance (get-campaign-funds-balance campaign-id)))
    
    (asserts! (is-eq (get status milestone) MILESTONE_VERIFIED) ERR_INVALID_MILESTONE_STATUS)
    (asserts! (>= current-balance fund-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer funds to campaign creator
    (try! (as-contract (stx-transfer? fund-amount tx-sender (get creator campaign))))
    
    ;; Update campaign funds balance
    (map-set campaign-funds
      { campaign-id: campaign-id }
      { balance: (- current-balance fund-amount) }
    )
    
    ;; Update campaign released funds
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { released-funds: (+ (get released-funds campaign) fund-amount) })
    )
    
    (ok fund-amount)
  )
)

;; Admin functions
(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)
