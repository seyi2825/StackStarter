;; Decentralized Governance Smart Contract
;; Handles project approvals, milestone verification, and platform upgrades

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_VOTING_PERIOD_ENDED (err u102))
(define-constant ERR_VOTING_PERIOD_ACTIVE (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INSUFFICIENT_VOTING_POWER (err u105))
(define-constant ERR_PROPOSAL_NOT_APPROVED (err u106))
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED (err u107))
(define-constant ERR_INVALID_VOTING_PERIOD (err u108))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var min-voting-period uint u1440) ;; 1440 blocks (~1 day)
(define-data-var max-voting-period uint u10080) ;; 10080 blocks (~1 week)
(define-data-var quorum-threshold uint u1000) ;; Minimum votes needed
(define-data-var approval-threshold uint u51) ;; 51% approval needed

;; Proposal Types
(define-constant PROPOSAL_TYPE_PROJECT u1)
(define-constant PROPOSAL_TYPE_MILESTONE u2)
(define-constant PROPOSAL_TYPE_UPGRADE u3)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: uint,
    project-data: (optional (string-ascii 200)),
    voting-start: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    total-votes: uint,
    executed: bool,
    approved: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint, block-height: uint }
)

(define-map user-voting-power
  { user: principal }
  { power: uint, staked-tokens: uint, last-update: uint }
)

(define-map delegated-power
  { delegator: principal }
  { delegate: principal, power: uint }
)

(define-map approved-projects
  { project-id: (string-ascii 50) }
  { 
    title: (string-ascii 100),
    proposer: principal,
    approval-block: uint,
    proposal-id: uint
  }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-user-voting-power (user principal))
  (default-to 
    { power: u0, staked-tokens: u0, last-update: u0 }
    (map-get? user-voting-power { user: user })
  )
)

(define-read-only (get-delegated-power (delegator principal))
  (map-get? delegated-power { delegator: delegator })
)

(define-read-only (get-approved-project (project-id (string-ascii 50)))
  (map-get? approved-projects { project-id: project-id })
)

(define-read-only (calculate-total-voting-power (user principal))
  (let (
    (base-power (get power (get-user-voting-power user)))
  )
    ;; Simplified to return base power only, as calculating delegated power 
    ;; requires iterating over all delegators which is complex in read-only functions
    base-power
  )
)

;; Private functions
(define-private (check-delegation-to-user (delegator principal) (acc uint))
  (match (get-delegated-power delegator)
    delegation (if (is-eq (get delegate delegation) tx-sender)
                  (+ acc (get power delegation))
                  acc)
    acc
  )
)

;; Public functions

;; Submit a new project proposal
(define-public (submit-project-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (project-data (string-ascii 200))
  (voting-period uint))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (voting-start stacks-block-height)
    (voting-end (+ stacks-block-height voting-period))
  )
    (asserts! (and (>= voting-period (var-get min-voting-period))
                   (<= voting-period (var-get max-voting-period)))
              ERR_INVALID_VOTING_PERIOD)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: PROPOSAL_TYPE_PROJECT,
        project-data: (some project-data),
        voting-start: voting-start,
        voting-end: voting-end,
        yes-votes: u0,
        no-votes: u0,
        total-votes: u0,
        executed: false,
        approved: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Submit a milestone verification proposal
(define-public (submit-milestone-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (project-reference (string-ascii 200))
  (voting-period uint))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (voting-start stacks-block-height)
    (voting-end (+ stacks-block-height voting-period))
  )
    (asserts! (and (>= voting-period (var-get min-voting-period))
                   (<= voting-period (var-get max-voting-period)))
              ERR_INVALID_VOTING_PERIOD)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: PROPOSAL_TYPE_MILESTONE,
        project-data: (some project-reference),
        voting-start: voting-start,
        voting-end: voting-end,
        yes-votes: u0,
        no-votes: u0,
        total-votes: u0,
        executed: false,
        approved: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Submit a platform upgrade proposal
(define-public (submit-upgrade-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (voting-period uint))
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (voting-start stacks-block-height)
    (voting-end (+ stacks-block-height voting-period))
  )
    (asserts! (and (>= voting-period (var-get min-voting-period))
                   (<= voting-period (var-get max-voting-period)))
              ERR_INVALID_VOTING_PERIOD)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: PROPOSAL_TYPE_UPGRADE,
        project-data: none,
        voting-start: voting-start,
        voting-end: voting-end,
        yes-votes: u0,
        no-votes: u0,
        total-votes: u0,
        executed: false,
        approved: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (voter-power (calculate-total-voting-power tx-sender))
    (current-block stacks-block-height)
  )
    (asserts! (> voter-power u0) ERR_INSUFFICIENT_VOTING_POWER)
    (asserts! (< current-block (get voting-end proposal)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (>= current-block (get voting-start proposal)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (is-none (get-vote proposal-id tx-sender)) ERR_ALREADY_VOTED)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote, voting-power: voter-power, block-height: current-block }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        yes-votes: (if vote 
                      (+ (get yes-votes proposal) voter-power)
                      (get yes-votes proposal)),
        no-votes: (if vote
                     (get no-votes proposal)
                     (+ (get no-votes proposal) voter-power)),
        total-votes: (+ (get total-votes proposal) voter-power)
      })
    )
    
    (ok true)
  )
)

;; Tally votes and determine if proposal is approved
(define-public (tally-votes (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (current-block stacks-block-height)
    (total-votes (get total-votes proposal))
    (yes-votes (get yes-votes proposal))
    (approval-rate (if (> total-votes u0) 
                      (/ (* yes-votes u100) total-votes) 
                      u0))
  )
    (asserts! (>= current-block (get voting-end proposal)) ERR_VOTING_PERIOD_ACTIVE)
    (asserts! (>= total-votes (var-get quorum-threshold)) ERR_INSUFFICIENT_VOTING_POWER)
    
    (let (
      (is-approved (>= approval-rate (var-get approval-threshold)))
    )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { approved: is-approved })
      )
      
      (ok is-approved)
    )
  )
)

;; Execute an approved proposal
(define-public (execute-approved-proposal (proposal-id uint) (project-id (string-ascii 50)))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR_PROPOSAL_NOT_FOUND))
  )
    (asserts! (get approved proposal) ERR_PROPOSAL_NOT_APPROVED)
    (asserts! (not (get executed proposal)) ERR_PROPOSAL_ALREADY_EXECUTED)
    (asserts! (>= stacks-block-height (get voting-end proposal)) ERR_VOTING_PERIOD_ACTIVE)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    ;; If it's a project proposal, add to approved projects
    (if (is-eq (get proposal-type proposal) PROPOSAL_TYPE_PROJECT)
      (map-set approved-projects
        { project-id: project-id }
        {
          title: (get title proposal),
          proposer: (get proposer proposal),
          approval-block: stacks-block-height,
          proposal-id: proposal-id
        }
      )
      true
    )
    
    (ok true)
  )
)

;; Stake tokens for voting power
(define-public (stake-for-voting-power (amount uint))
  (let (
    (current-power (get-user-voting-power tx-sender))
  )
    ;; In a real implementation, you'd transfer tokens to the contract here
    ;; For this example, we'll just update the voting power
    
    (map-set user-voting-power
      { user: tx-sender }
      {
        power: (+ (get power current-power) amount),
        staked-tokens: (+ (get staked-tokens current-power) amount),
        last-update: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Delegate voting power to another user
(define-public (delegate-voting-power (delegate principal) (amount uint))
  (let (
    (current-power (get-user-voting-power tx-sender))
  )
    (asserts! (>= (get power current-power) amount) ERR_INSUFFICIENT_VOTING_POWER)
    
    ;; Reduce delegator's power
    (map-set user-voting-power
      { user: tx-sender }
      (merge current-power { power: (- (get power current-power) amount) })
    )
    
    ;; Set delegation
    (map-set delegated-power
      { delegator: tx-sender }
      { delegate: delegate, power: amount }
    )
    
    (ok true)
  )
)

;; Revoke delegated voting power
(define-public (revoke-delegation)
  (match (get-delegated-power tx-sender)
    delegation (let (
      (delegated-amount (get power delegation))
      (current-power (get-user-voting-power tx-sender))
    )
      ;; Return power to delegator
      (map-set user-voting-power
        { user: tx-sender }
        (merge current-power { power: (+ (get power current-power) delegated-amount) })
      )
      
      ;; Remove delegation
      (map-delete delegated-power { delegator: tx-sender })
      
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

;; Admin functions (only contract owner)
(define-public (update-governance-parameters 
  (new-quorum uint) 
  (new-approval-threshold uint)
  (new-min-voting-period uint)
  (new-max-voting-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set quorum-threshold new-quorum)
    (var-set approval-threshold new-approval-threshold)
    (var-set min-voting-period new-min-voting-period)
    (var-set max-voting-period new-max-voting-period)
    (ok true)
  )
)
