;; VehicleTitle: Vintage Vehicle Title and Registration Platform
;; Version: 1.0.0

(define-constant ERR-NOT-OWNER (err u1))
(define-constant ERR-VEHICLE-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-TITLED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-YEAR (err u5))
(define-constant ERR-INVALID-CONDITION (err u6))
(define-constant ERR-INVALID-CATEGORY (err u7))
(define-constant ERR-INVALID-VIN (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))

(define-constant MIN-YEAR u1900)

(define-data-var next-vehicle-id uint u1)

(define-map vehicles
    uint
    {
        owner: principal,
        vehicle-vin: (string-utf8 17),
        vehicle-description: (string-utf8 150),
        vehicle-condition: (string-utf8 12),
        vehicle-category: (string-utf8 18),
        title-status: (string-utf8 15),
        manufacture-year: uint
    }
)

(define-private (validate-condition (condition (string-utf8 12)))
    (or 
        (is-eq condition u"Excellent")
        (is-eq condition u"Very-Good")
        (is-eq condition u"Good")
        (is-eq condition u"Fair")
        (is-eq condition u"Poor")
        (is-eq condition u"Restoration")
    )
)

(define-private (validate-category (category (string-utf8 18)))
    (or 
        (is-eq category u"Classic-Car")
        (is-eq category u"Vintage-Motorcycle")
        (is-eq category u"Antique-Truck")
        (is-eq category u"Sports-Car")
        (is-eq category u"Muscle-Car")
    )
)

(define-private (validate-text-length (text (string-utf8 150)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-public (register-vintage-vehicle 
    (vehicle-vin (string-utf8 17))
    (vehicle-description (string-utf8 150))
    (vehicle-condition (string-utf8 12))
    (vehicle-category (string-utf8 18))
    (manufacture-year uint))
    (let
        (
            (vehicle-id (var-get next-vehicle-id))
        )
        (asserts! (validate-text-length vehicle-vin u10 u17) ERR-INVALID-VIN)
        (asserts! (validate-text-length vehicle-description u15 u150) ERR-INVALID-DESCRIPTION)
        (asserts! (>= manufacture-year MIN-YEAR) ERR-INVALID-YEAR)
        (asserts! (validate-condition vehicle-condition) ERR-INVALID-CONDITION)
        (asserts! (validate-category vehicle-category) ERR-INVALID-CATEGORY)
        
        (map-set vehicles vehicle-id {
            owner: tx-sender,
            vehicle-vin: vehicle-vin,
            vehicle-description: vehicle-description,
            vehicle-condition: vehicle-condition,
            vehicle-category: vehicle-category,
            title-status: u"titled",
            manufacture-year: manufacture-year
        })
        (var-set next-vehicle-id (+ vehicle-id u1))
        (ok vehicle-id)
    )
)

(define-public (transfer-vehicle-title (vehicle-id uint) (new-owner principal))
    (let
        (
            (vehicle (unwrap! (map-get? vehicles vehicle-id) ERR-VEHICLE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner vehicle)) ERR-NOT-OWNER)
        (asserts! (is-eq (get title-status vehicle) u"titled") ERR-INVALID-STATUS)
        (ok (map-set vehicles vehicle-id (merge vehicle { owner: new-owner, title-status: u"transferred" })))
    )
)

(define-read-only (get-vehicle (vehicle-id uint))
    (ok (map-get? vehicles vehicle-id))
)

(define-read-only (get-vehicle-owner (vehicle-id uint))
    (ok (get owner (unwrap! (map-get? vehicles vehicle-id) ERR-VEHICLE-NOT-FOUND)))
)