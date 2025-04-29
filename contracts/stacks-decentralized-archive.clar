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
