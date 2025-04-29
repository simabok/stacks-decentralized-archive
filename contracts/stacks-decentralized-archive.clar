;; Decentralized Archive Contract
;;
;; A blockchain-based data management system built on Stacks
;; Enables secure storage and controlled access to  documentation  with privacy-centric user permissions and practitioner authentication


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Constants - Error Codes and Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core Error Constants
(define-constant ERR_ITEM_OWNER_ONLY (err u300))       ;; Required item owner privileges
(define-constant ERR_PRACTITIONER_INVALID (err u306))  ;; Invalid practitioner credentials
(define-constant ERR_ACCESS_REJECTED (err u308))       ;; User lacks permission to perform action
(define-constant ERR_CLASSIFICATION_INVALID (err u307));; Invalid classification parameters

;; Data Validation Errors

(define-constant ERR_DIMENSION_INVALID (err u304))     ;; Invalid dimension parameters
(define-constant ERR_ITEM_ABSENT (err u301))           ;; Requested item not in system
(define-constant ERR_ITEM_DUPLICATE (err u302))        ;; Item already exists in system
(define-constant ERR_DESCRIPTOR_INVALID (err u303))    ;; Invalid descriptor parameters
(define-constant ERR_AUTHORIZATION_FAILED (err u305))  ;; Failed authorization check

;; System Administrator Settings
(define-constant system-administrator tx-sender)       ;; System administrator privileges

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Storage Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Access Control Permissions System
(define-map viewer-authorization
  { item-identifier: uint, viewer-identity: principal } ;; Item and viewer pairing
  { authorization-status: bool }                        ;; Access permission status
)

;; Aggregated System Statistics
(define-data-var item-counter uint u0)                 ;; Tracks total number of items in system

;; Core Data Storage Structure 
(define-map healthcare-archive
  { item-identifier: uint }                            ;; Unique item identifier
  {
    subject-descriptor: (string-ascii 64),             ;; Subject's full identification
    practitioner-identifier: principal,                ;; Attending practitioner
    item-dimensions: uint,                             ;; Item file size
    creation-timestamp: uint,                          ;; Creation timestamp (block height)
    clinical-annotation: (string-ascii 128),           ;; Clinical notes
    classification-markers: (list 10 (string-ascii 32)) ;; Classification categories
  }
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Classification Marker Validation - Collection
(define-private (validate-classification-collection (markers (list 10 (string-ascii 32))))
  (and
    (> (len markers) u0)                  ;; At least one marker required
    (<= (len markers) u10)                ;; Maximum number of markers
    (is-eq (len (filter validate-classification-marker markers)) (len markers)) ;; All valid
  )
)

;; Item Existence Verification
(define-private (item-exists? (item-identifier uint))
  (is-some (map-get? healthcare-archive { item-identifier: item-identifier }))
)

;; Practitioner Ownership Verification
(define-private (is-authorized-practitioner? (item-identifier uint) (practitioner-identity principal))
  (match (map-get? healthcare-archive { item-identifier: item-identifier })
    item-details (is-eq (get practitioner-identifier item-details) practitioner-identity)
    false
  )
)

;; Item Dimension Retrieval
(define-private (retrieve-item-dimensions (item-identifier uint))
  (default-to u0
    (get item-dimensions
      (map-get? healthcare-archive { item-identifier: item-identifier })
    )
  )
)

;; Classification Marker Validation - Individual
(define-private (validate-classification-marker (marker (string-ascii 32)))
  (and 
    (> (len marker) u0)                   ;; Non-empty requirement
    (< (len marker) u33)                  ;; Maximum length requirement
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Interface Functions - Item Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update Existing Healthcare Item Details
(define-public (modify-healthcare-item 
  (item-identifier uint)                           ;; Target item identifier
  (new-subject-descriptor (string-ascii 64))       ;; Updated subject descriptor
  (new-dimensions uint)                           ;; Updated data size
  (new-annotation (string-ascii 128))             ;; Updated clinical notes
  (new-classification-markers (list 10 (string-ascii 32))) ;; Updated classifications
)
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    ;; Authorization and validation checks
    (asserts! (item-exists? item-identifier) ERR_ITEM_ABSENT)
    (asserts! (is-eq (get practitioner-identifier item-data) tx-sender) ERR_AUTHORIZATION_FAILED)

    (asserts! (> (len new-subject-descriptor) u0) ERR_DESCRIPTOR_INVALID)
    (asserts! (< (len new-subject-descriptor) u65) ERR_DESCRIPTOR_INVALID)

    (asserts! (> new-dimensions u0) ERR_DIMENSION_INVALID)
    (asserts! (< new-dimensions u1000000000) ERR_DIMENSION_INVALID)

    (asserts! (> (len new-annotation) u0) ERR_DESCRIPTOR_INVALID)
    (asserts! (< (len new-annotation) u129) ERR_DESCRIPTOR_INVALID)

    (asserts! (validate-classification-collection new-classification-markers) ERR_CLASSIFICATION_INVALID)

    ;; Update item details
    (map-set healthcare-archive
      { item-identifier: item-identifier }
      (merge item-data { 
        subject-descriptor: new-subject-descriptor, 
        item-dimensions: new-dimensions, 
        clinical-annotation: new-annotation, 
        classification-markers: new-classification-markers 
      })
    )
    (ok true)
  )
)


;; Create New Healthcare Item
(define-public (register-healthcare-item 
  (subject-descriptor (string-ascii 64))        ;; Subject's identification information
  (item-dimensions uint)                       ;; Size of healthcare data
  (clinical-annotation (string-ascii 128))     ;; Clinical notes/annotations
  (classification-markers (list 10 (string-ascii 32))) ;; Classification categories
)
  (let
    (
      (item-identifier (+ (var-get item-counter) u1))  ;; Generate unique identifier
    )
    ;; Input validation checks
    (asserts! (> (len subject-descriptor) u0) ERR_DESCRIPTOR_INVALID)          ;; Non-empty subject descriptor
    (asserts! (< (len subject-descriptor) u65) ERR_DESCRIPTOR_INVALID)         ;; Subject descriptor length limit

    (asserts! (> item-dimensions u0) ERR_DIMENSION_INVALID)                   ;; Positive dimension required
    (asserts! (< item-dimensions u1000000000) ERR_DIMENSION_INVALID)          ;; Reasonable dimension limit

    (asserts! (> (len clinical-annotation) u0) ERR_DESCRIPTOR_INVALID)        ;; Non-empty clinical annotation
    (asserts! (< (len clinical-annotation) u129) ERR_DESCRIPTOR_INVALID)      ;; Clinical annotation length limit

    (asserts! (validate-classification-collection classification-markers) ERR_CLASSIFICATION_INVALID) ;; Valid classifications

    ;; Insert new healthcare item record
    (map-insert healthcare-archive
      { item-identifier: item-identifier }
      {
        subject-descriptor: subject-descriptor,
        practitioner-identifier: tx-sender,        ;; Current sender as practitioner
        item-dimensions: item-dimensions,
        creation-timestamp: block-height,          ;; Current block height as timestamp
        clinical-annotation: clinical-annotation,
        classification-markers: classification-markers
      }
    )

    ;; Grant access to creating practitioner
    (map-insert viewer-authorization
      { item-identifier: item-identifier, viewer-identity: tx-sender }
      { authorization-status: true }
    )

    ;; Update system statistics
    (var-set item-counter item-identifier)
    (ok item-identifier)                           ;; Return new identifier
  )
)

;; Update Item's Associated Practitioner
(define-public (reassign-practitioner (item-identifier uint) (new-practitioner-identity principal))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    ;; Authorization checks
    (asserts! (item-exists? item-identifier) ERR_ITEM_ABSENT)
    (asserts! (is-eq (get practitioner-identifier item-data) tx-sender) ERR_AUTHORIZATION_FAILED)

    ;; Update practitioner assignment
    (map-set healthcare-archive
      { item-identifier: item-identifier }
      (merge item-data { practitioner-identifier: new-practitioner-identity })
    )
    (ok true)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Interface Functions - Information Retrieval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Retrieve System Statistics
(define-public (get-system-item-count)
  (ok (var-get item-counter))
)

;; Retrieve Item's Classification Categories
(define-public (retrieve-item-classifications (item-identifier uint))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    (ok (get classification-markers item-data))
  )
)

;; Retrieve Item's Associated Practitioner
(define-public (retrieve-item-practitioner (item-identifier uint))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    (ok (get practitioner-identifier item-data))
  )
)

;; Retrieve Item's Creation Timestamp
(define-public (retrieve-item-timestamp (item-identifier uint))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    (ok (get creation-timestamp item-data))
  )
)

;; Retrieve Item's Dimensions
(define-public (retrieve-item-dimensions-by-id (item-identifier uint))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    (ok (get item-dimensions item-data))
  )
)

;; Retrieve Item's Clinical Annotations
(define-public (retrieve-item-annotations (item-identifier uint))
  (let
    (
      (item-data (unwrap! (map-get? healthcare-archive { item-identifier: item-identifier }) ERR_ITEM_ABSENT))
    )
    (ok (get clinical-annotation item-data))
  )
)

;; Verify User Access Authorization
(define-public (verify-viewer-authorization (item-identifier uint) (viewer-identity principal))
  (let
    (
      (authorization-data (unwrap! (map-get? viewer-authorization { item-identifier: item-identifier, viewer-identity: viewer-identity }) ERR_ACCESS_REJECTED))
    )
    (ok (get authorization-status authorization-data))
  )
)

