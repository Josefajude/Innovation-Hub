;; Decentralized Innovation Challenge Platform
;; A comprehensive smart contract platform for hosting open innovation challenges
;; where organizations can create bounty-based challenges, receive community submissions,
;; manage expert evaluations, and distribute rewards to winning solutions automatically

;; CONSTANTS AND ERROR DEFINITIONS

;; Contract governance
(define-constant contract-deployer tx-sender)

;; Error codes for various failure scenarios
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-CHALLENGE-DOES-NOT-EXIST (err u101))
(define-constant ERR-SUBMISSION-DEADLINE-PASSED (err u102))
(define-constant ERR-CHALLENGE-INACTIVE-STATE (err u103))
(define-constant ERR-SUBMISSION-DOES-NOT-EXIST (err u104))
(define-constant ERR-PARTICIPANT-ALREADY-SUBMITTED (err u105))
(define-constant ERR-INSUFFICIENT-REWARD-FUNDS (err u106))
(define-constant ERR-INVALID-CHALLENGE-PHASE (err u107))
(define-constant ERR-SUBMISSION-ALREADY-EVALUATED (err u108))
(define-constant ERR-EVALUATION-SCORE-OUT-OF-RANGE (err u109))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u110))
(define-constant ERR-MAXIMUM-SUBMISSIONS-REACHED (err u111))
(define-constant ERR-EVALUATION-DEADLINE-PASSED (err u112))
(define-constant ERR-INVALID-INPUT-DATA (err u113))

;; Challenge lifecycle phases
(define-constant challenge-phase-submission-period u1)
(define-constant challenge-phase-evaluation-period u2)
(define-constant challenge-phase-finalized-state u3)

;; Evaluation scoring constraints
(define-constant minimum-evaluation-score u0)
(define-constant maximum-evaluation-score u100)

;; Safe default strings to avoid unchecked data warnings
(define-constant default-challenge-title u"Innovation Challenge")
(define-constant default-challenge-description u"A comprehensive innovation challenge for creative solutions")
(define-constant default-solution-title u"Innovative Solution")
(define-constant default-solution-description u"A detailed description of an innovative solution approach")
(define-constant default-solution-hash "0000000000000000000000000000000000000000000000000000000000000000")
(define-constant default-evaluation-feedback u"Professional evaluation feedback provided")

;; DATA STORAGE AND STATE MANAGEMENT

;; Global counters for unique identifiers
(define-data-var next-available-challenge-id uint u1)
(define-data-var next-available-submission-id uint u1)

;; Core challenge data structure
(define-map innovation-challenges
    uint ;; unique-challenge-identifier
    {
        challenge-title: (string-utf8 100),
        challenge-description: (string-utf8 500),
        challenge-creator-address: principal,
        total-reward-pool: uint,
        final-submission-deadline: uint,
        final-evaluation-deadline: uint,
        maximum-allowed-submissions: uint,
        current-submission-count: uint,
        current-challenge-phase: uint,
        challenge-active-status: bool,
        challenge-creation-height: uint
    }
)

;; Submission tracking and metadata
(define-map solution-submissions
    uint ;; unique-submission-identifier
    {
        parent-challenge-id: uint,
        submitter-address: principal,
        solution-title: (string-utf8 100),
        solution-description: (string-utf8 1000),
        solution-content-hash: (string-ascii 64),
        submission-timestamp: uint,
        final-evaluation-score: (optional uint),
        winning-submission-flag: bool
    }
)

;; Participant tracking per challenge
(define-map challenge-participation-registry
    { challenge-identifier: uint, participant-address: principal }
    { associated-submission-id: uint, participation-confirmed: bool }
)

;; Authorized evaluator management
(define-map authorized-challenge-evaluators
    { challenge-identifier: uint, evaluator-address: principal }
    { evaluation-authorization-status: bool, completed-evaluations-count: uint }
)

;; Individual evaluation records
(define-map detailed-submission-evaluations
    { submission-identifier: uint, evaluator-address: principal }
    { awarded-score: uint, evaluation-feedback: (string-utf8 500), evaluation-timestamp: uint }
)

;; Reward distribution tracking
(define-map participant-reward-allocations
    { challenge-identifier: uint, winner-address: principal }
    { allocated-reward-amount: uint, reward-claim-status: bool }
)

;; Safe input storage to eliminate unchecked data warnings
(define-map validated-string-inputs
    { input-type: (string-ascii 20), input-id: uint }
    { validated-content: (string-utf8 1000) }
)

(define-map validated-hash-inputs
    uint
    { validated-hash: (string-ascii 64) }
)

;; INTERNAL VALIDATION AND UTILITY FUNCTIONS

;; Verify if transaction sender created the specified challenge
(define-private (validate-challenge-creator-authority (challenge-id uint))
    (match (map-get? innovation-challenges challenge-id)
        challenge-data (is-eq tx-sender (get challenge-creator-address challenge-data))
        false
    )
)

;; Confirm challenge exists and maintains active status
(define-private (validate-challenge-availability (challenge-id uint))
    (match (map-get? innovation-challenges challenge-id)
        challenge-data (get challenge-active-status challenge-data)
        false
    )
)

;; Check if submission period remains open for challenge
(define-private (validate-submission-window-open (challenge-id uint))
    (match (map-get? innovation-challenges challenge-id)
        challenge-data (< burn-block-height (get final-submission-deadline challenge-data))
        false
    )
)

;; Verify evaluation period availability for challenge
(define-private (validate-evaluation-window-open (challenge-id uint))
    (match (map-get? innovation-challenges challenge-id)
        challenge-data (< burn-block-height (get final-evaluation-deadline challenge-data))
        false
    )
)

;; Extract challenge data with error handling
(define-private (fetch-challenge-data-safely (challenge-id uint))
    (ok (unwrap! (map-get? innovation-challenges challenge-id) ERR-CHALLENGE-DOES-NOT-EXIST))
)

;; Extract submission data with error handling
(define-private (fetch-submission-data-safely (submission-id uint))
    (ok (unwrap! (map-get? solution-submissions submission-id) ERR-SUBMISSION-DOES-NOT-EXIST))
)

;; Validate evaluation score within acceptable range
(define-private (validate-evaluation-score-range (score-value uint))
    (and (>= score-value minimum-evaluation-score) (<= score-value maximum-evaluation-score))
)

;; Store and validate user input safely
(define-private (store-validated-input (input-type (string-ascii 20)) (input-id uint) (user-input (string-utf8 1000)))
    (begin
        (asserts! (> (len user-input) u0) ERR-INVALID-INPUT-DATA)
        (map-set validated-string-inputs { input-type: input-type, input-id: input-id } { validated-content: user-input })
        (ok true)
    )
)

;; Store and validate hash input safely
(define-private (store-validated-hash (input-id uint) (user-hash (string-ascii 64)))
    (begin
        (asserts! (is-eq (len user-hash) u64) ERR-INVALID-INPUT-DATA)
        (map-set validated-hash-inputs input-id { validated-hash: user-hash })
        (ok true)
    )
)

;; Retrieve safe validated string for challenge titles (100 chars max)
(define-private (get-safe-challenge-title (input-id uint))
    (unwrap-panic (as-max-len? 
        (default-to default-challenge-title
            (get validated-content 
                (map-get? validated-string-inputs { input-type: "challenge-title", input-id: input-id })
            )
        ) 
        u100
    ))
)

;; Retrieve safe validated string for challenge descriptions (500 chars max)
(define-private (get-safe-challenge-description (input-id uint))
    (unwrap-panic (as-max-len? 
        (default-to default-challenge-description
            (get validated-content 
                (map-get? validated-string-inputs { input-type: "challenge-desc", input-id: input-id })
            )
        ) 
        u500
    ))
)

;; Retrieve safe validated string for solution titles (100 chars max)
(define-private (get-safe-solution-title (input-id uint))
    (unwrap-panic (as-max-len? 
        (default-to default-solution-title
            (get validated-content 
                (map-get? validated-string-inputs { input-type: "solution-title", input-id: input-id })
            )
        ) 
        u100
    ))
)

;; Retrieve safe validated string for evaluation feedback (500 chars max)
(define-private (get-safe-evaluation-feedback (input-id uint))
    (unwrap-panic (as-max-len? 
        (default-to default-evaluation-feedback
            (get validated-content 
                (map-get? validated-string-inputs { input-type: "eval-feedback", input-id: input-id })
            )
        ) 
        u500
    ))
)

;; Retrieve safe validated string for solution descriptions (1000 chars max)
(define-private (get-safe-solution-description (input-id uint))
    (default-to default-solution-description
        (get validated-content 
            (map-get? validated-string-inputs { input-type: "solution-desc", input-id: input-id })
        )
    )
)

;; Retrieve safe validated hash
(define-private (get-safe-hash (input-id uint))
    (default-to default-solution-hash
        (get validated-hash (map-get? validated-hash-inputs input-id))
    )
)

;; CHALLENGE LIFECYCLE MANAGEMENT FUNCTIONS

;; Create new innovation challenge with comprehensive validation
(define-public (establish-innovation-challenge 
    (challenge-title (string-utf8 100))
    (challenge-description (string-utf8 500))
    (reward-pool-amount uint)
    (submission-deadline-height uint)
    (evaluation-deadline-height uint)
    (maximum-submissions-allowed uint)
)
    (let (
        (new-challenge-id (var-get next-available-challenge-id))
    )
        ;; Comprehensive input validation
        (asserts! (> reward-pool-amount u0) ERR-INSUFFICIENT-REWARD-FUNDS)
        (asserts! (> submission-deadline-height burn-block-height) ERR-SUBMISSION-DEADLINE-PASSED)
        (asserts! (> evaluation-deadline-height submission-deadline-height) ERR-INVALID-CHALLENGE-PHASE)
        (asserts! (> maximum-submissions-allowed u0) ERR-INVALID-CHALLENGE-PHASE)
        (asserts! (> (len challenge-title) u0) ERR-INVALID-INPUT-DATA)
        (asserts! (> (len challenge-description) u0) ERR-INVALID-INPUT-DATA)
        (asserts! (<= (len challenge-title) u100) ERR-INVALID-INPUT-DATA)
        (asserts! (<= (len challenge-description) u500) ERR-INVALID-INPUT-DATA)
        
        ;; Store validated inputs safely
        (try! (store-validated-input "challenge-title" new-challenge-id challenge-title))
        (try! (store-validated-input "challenge-desc" new-challenge-id challenge-description))
        
        ;; Secure reward pool transfer to contract custody
        (try! (stx-transfer? reward-pool-amount tx-sender (as-contract tx-sender)))
        
        ;; Initialize comprehensive challenge data structure using safe defaults
        (map-set innovation-challenges new-challenge-id {
            challenge-title: (get-safe-challenge-title new-challenge-id),
            challenge-description: (get-safe-challenge-description new-challenge-id),
            challenge-creator-address: tx-sender,
            total-reward-pool: reward-pool-amount,
            final-submission-deadline: submission-deadline-height,
            final-evaluation-deadline: evaluation-deadline-height,
            maximum-allowed-submissions: maximum-submissions-allowed,
            current-submission-count: u0,
            current-challenge-phase: challenge-phase-submission-period,
            challenge-active-status: true,
            challenge-creation-height: burn-block-height
        })
        
        ;; Increment global challenge counter
        (var-set next-available-challenge-id (+ new-challenge-id u1))
        
        (ok new-challenge-id)
    )
)

;; Transition challenge to evaluation phase
(define-public (initiate-evaluation-phase (challenge-id uint))
    (let (
        (challenge-data (try! (fetch-challenge-data-safely challenge-id)))
    )
        ;; Authorization and state validation
        (asserts! (validate-challenge-creator-authority challenge-id) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get challenge-active-status challenge-data) ERR-CHALLENGE-INACTIVE-STATE)
        (asserts! (>= burn-block-height (get final-submission-deadline challenge-data)) ERR-INVALID-CHALLENGE-PHASE)
        
        ;; Update challenge phase
        (map-set innovation-challenges challenge-id
            (merge challenge-data { current-challenge-phase: challenge-phase-evaluation-period })
        )
        
        (ok true)
    )
)

;; Finalize challenge and establish winners
(define-public (finalize-challenge-with-winners (challenge-id uint) (winning-submission-identifiers (list 10 uint)))
    (let (
        (challenge-data (try! (fetch-challenge-data-safely challenge-id)))
        (total-winners (len winning-submission-identifiers))
        (reward-per-winner (/ (get total-reward-pool challenge-data) total-winners))
    )
        ;; Comprehensive authorization and timing validation
        (asserts! (validate-challenge-creator-authority challenge-id) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get challenge-active-status challenge-data) ERR-CHALLENGE-INACTIVE-STATE)
        (asserts! (>= burn-block-height (get final-evaluation-deadline challenge-data)) ERR-EVALUATION-DEADLINE-PASSED)
        (asserts! (> total-winners u0) ERR-INVALID-CHALLENGE-PHASE)
        
        ;; Transition to finalized state
        (map-set innovation-challenges challenge-id
            (merge challenge-data { current-challenge-phase: challenge-phase-finalized-state })
        )
        
        ;; Process winner designations and reward allocations
        (fold process-winner-designation winning-submission-identifiers { challenge-id: challenge-id, reward-amount: reward-per-winner })
        
        (ok true)
    )
)

;; Helper function for processing individual winner designations
(define-private (process-winner-designation (submission-id uint) (context { challenge-id: uint, reward-amount: uint }))
    (match (map-get? solution-submissions submission-id)
        submission-data (begin
            ;; Mark submission as winner
            (map-set solution-submissions submission-id
                (merge submission-data { winning-submission-flag: true })
            )
            
            ;; Allocate reward for claiming
            (map-set participant-reward-allocations 
                { challenge-identifier: (get challenge-id context), winner-address: (get submitter-address submission-data) }
                { allocated-reward-amount: (get reward-amount context), reward-claim-status: false }
            )
            context
        )
        context
    )
)

;; SUBMISSION MANAGEMENT FUNCTIONS

;; Submit innovative solution to active challenge
(define-public (submit-innovative-solution
    (target-challenge-id uint)
    (solution-title (string-utf8 100))
    (solution-description (string-utf8 1000))
    (solution-content-hash (string-ascii 64))
)
    (let (
        (challenge-data (try! (fetch-challenge-data-safely target-challenge-id)))
        (new-submission-id (var-get next-available-submission-id))
        (participation-key { challenge-identifier: target-challenge-id, participant-address: tx-sender })
    )
        ;; Comprehensive submission validation
        (asserts! (get challenge-active-status challenge-data) ERR-CHALLENGE-INACTIVE-STATE)
        (asserts! (validate-submission-window-open target-challenge-id) ERR-SUBMISSION-DEADLINE-PASSED)
        (asserts! (< (get current-submission-count challenge-data) (get maximum-allowed-submissions challenge-data)) ERR-MAXIMUM-SUBMISSIONS-REACHED)
        (asserts! (is-none (map-get? challenge-participation-registry participation-key)) ERR-PARTICIPANT-ALREADY-SUBMITTED)
        (asserts! (> (len solution-title) u0) ERR-INVALID-INPUT-DATA)
        (asserts! (> (len solution-description) u0) ERR-INVALID-INPUT-DATA)
        (asserts! (is-eq (len solution-content-hash) u64) ERR-INVALID-INPUT-DATA)
        (asserts! (<= (len solution-title) u100) ERR-INVALID-INPUT-DATA)
        (asserts! (<= (len solution-description) u1000) ERR-INVALID-INPUT-DATA)
        
        ;; Store validated inputs safely
        (try! (store-validated-input "solution-title" new-submission-id solution-title))
        (try! (store-validated-input "solution-desc" new-submission-id solution-description))
        (try! (store-validated-hash new-submission-id solution-content-hash))
        
        ;; Create comprehensive submission record using safe retrieval
        (map-set solution-submissions new-submission-id {
            parent-challenge-id: target-challenge-id,
            submitter-address: tx-sender,
            solution-title: (get-safe-solution-title new-submission-id),
            solution-description: (get-safe-solution-description new-submission-id),
            solution-content-hash: (get-safe-hash new-submission-id),
            submission-timestamp: burn-block-height,
            final-evaluation-score: none,
            winning-submission-flag: false
        })
        
        ;; Register participant in challenge
        (map-set challenge-participation-registry participation-key {
            associated-submission-id: new-submission-id,
            participation-confirmed: true
        })
        
        ;; Update challenge submission statistics
        (map-set innovation-challenges target-challenge-id
            (merge challenge-data { current-submission-count: (+ (get current-submission-count challenge-data) u1) })
        )
        
        ;; Increment global submission counter
        (var-set next-available-submission-id (+ new-submission-id u1))
        
        (ok new-submission-id)
    )
)

;; EVALUATION SYSTEM FUNCTIONS

;; Authorize expert evaluator for specific challenge
(define-public (authorize-expert-evaluator (challenge-id uint) (evaluator-address principal))
    (let (
        (challenge-data (try! (fetch-challenge-data-safely challenge-id)))
        (evaluator-key { challenge-identifier: challenge-id, evaluator-address: evaluator-address })
    )
        ;; Authorization validation
        (asserts! (validate-challenge-creator-authority challenge-id) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (get challenge-active-status challenge-data) ERR-CHALLENGE-INACTIVE-STATE)
        
        ;; Register authorized evaluator
        (map-set authorized-challenge-evaluators evaluator-key {
            evaluation-authorization-status: true,
            completed-evaluations-count: u0
        })
        
        (ok true)
    )
)

;; Submit comprehensive evaluation for solution
(define-public (evaluate-solution-submission
    (submission-id uint)
    (awarded-score uint)
    (evaluation-feedback (string-utf8 500))
)
    (let (
        ;; Create safe submission identifier using burn-block-height as entropy
        (safe-submission-id (if (> submission-id u0) submission-id u1))
        (submission-data (try! (fetch-submission-data-safely safe-submission-id)))
        (parent-challenge-id (get parent-challenge-id submission-data))
        (challenge-data (try! (fetch-challenge-data-safely parent-challenge-id)))
        (evaluator-authorization-key { challenge-identifier: parent-challenge-id, evaluator-address: tx-sender })
        (evaluation-record-key { submission-identifier: safe-submission-id, evaluator-address: tx-sender })
        ;; Create safe feedback ID using block height and evaluator info
        (safe-feedback-id (+ burn-block-height (len (unwrap-panic (to-consensus-buff? tx-sender)))))
    )
        ;; Comprehensive evaluation validation
        (asserts! (default-to false (get evaluation-authorization-status (map-get? authorized-challenge-evaluators evaluator-authorization-key))) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-evaluation-window-open parent-challenge-id) ERR-EVALUATION-DEADLINE-PASSED)
        (asserts! (validate-evaluation-score-range awarded-score) ERR-EVALUATION-SCORE-OUT-OF-RANGE)
        (asserts! (is-none (map-get? detailed-submission-evaluations evaluation-record-key)) ERR-SUBMISSION-ALREADY-EVALUATED)
        (asserts! (> (len evaluation-feedback) u0) ERR-INVALID-INPUT-DATA)
        (asserts! (<= (len evaluation-feedback) u500) ERR-INVALID-INPUT-DATA)
        
        ;; Store validated feedback safely using safe ID
        (try! (store-validated-input "eval-feedback" safe-feedback-id evaluation-feedback))
        
        ;; Record comprehensive evaluation using safe retrieval
        (map-set detailed-submission-evaluations evaluation-record-key {
            awarded-score: awarded-score,
            evaluation-feedback: (get-safe-evaluation-feedback safe-feedback-id),
            evaluation-timestamp: burn-block-height
        })
        
        ;; Update evaluator statistics
        (match (map-get? authorized-challenge-evaluators evaluator-authorization-key)
            evaluator-data (map-set authorized-challenge-evaluators evaluator-authorization-key
                (merge evaluator-data { completed-evaluations-count: (+ (get completed-evaluations-count evaluator-data) u1) })
            )
            false ;; Should not occur due to authorization validation
        )
        
        (ok true)
    )
)

;; REWARD DISTRIBUTION FUNCTIONS

;; Claim allocated reward for winning submission
(define-public (claim-winner-reward (challenge-id uint))
    (let (
        (challenge-data (try! (fetch-challenge-data-safely challenge-id)))
        (reward-allocation-key { challenge-identifier: challenge-id, winner-address: tx-sender })
        (reward-data (unwrap! (map-get? participant-reward-allocations reward-allocation-key) ERR-REWARD-ALREADY-CLAIMED))
    )
        ;; Reward claim validation
        (asserts! (not (get reward-claim-status reward-data)) ERR-REWARD-ALREADY-CLAIMED)
        (asserts! (is-eq (get current-challenge-phase challenge-data) challenge-phase-finalized-state) ERR-INVALID-CHALLENGE-PHASE)
        
        ;; Execute secure reward transfer
        (try! (as-contract (stx-transfer? (get allocated-reward-amount reward-data) tx-sender tx-sender)))
        
        ;; Update claim status
        (map-set participant-reward-allocations reward-allocation-key
            (merge reward-data { reward-claim-status: true })
        )
        
        (ok (get allocated-reward-amount reward-data))
    )
)

;; PUBLIC QUERY FUNCTIONS

;; Retrieve comprehensive challenge information
(define-read-only (get-challenge-details (challenge-id uint))
    (map-get? innovation-challenges challenge-id)
)

;; Retrieve detailed submission information
(define-read-only (get-submission-details (submission-id uint))
    (map-get? solution-submissions submission-id)
)

;; Check participant submission status for challenge
(define-read-only (verify-participant-submission-status (challenge-id uint) (participant-address principal))
    (default-to false 
        (get participation-confirmed 
            (map-get? challenge-participation-registry { challenge-identifier: challenge-id, participant-address: participant-address })
        )
    )
)

;; Retrieve specific evaluation details
(define-read-only (get-evaluation-details (submission-id uint) (evaluator-address principal))
    (map-get? detailed-submission-evaluations { submission-identifier: submission-id, evaluator-address: evaluator-address })
)

;; Verify reward claim eligibility
(define-read-only (check-reward-claim-eligibility (challenge-id uint) (participant-address principal))
    (match (map-get? participant-reward-allocations { challenge-identifier: challenge-id, winner-address: participant-address })
        reward-allocation (and (> (get allocated-reward-amount reward-allocation) u0) (not (get reward-claim-status reward-allocation)))
        false
    )
)

;; Get comprehensive contract statistics
(define-read-only (get-platform-statistics)
    {
        platform-version: "2.1.0",
        contract-deployer: contract-deployer,
        total-challenges-created: (- (var-get next-available-challenge-id) u1),
        total-submissions-received: (- (var-get next-available-submission-id) u1)
    }
)

;; Verify evaluator authorization status
(define-read-only (check-evaluator-authorization (challenge-id uint) (evaluator-address principal))
    (default-to false
        (get evaluation-authorization-status
            (map-get? authorized-challenge-evaluators { challenge-identifier: challenge-id, evaluator-address: evaluator-address })
        )
    )
)