(use-modules (guix build-system guile)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix profiles)
             (gnu packages)
             (gnu packages guile))

(define-public guile-swayer
  (package
    (name "guile-swayer")
    (version "0.2.0")
    (home-page "https://github.com/ebeem/guile-swayer")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ebeem/guile-swayer")
             (commit "9c962281f405453fb3770dd0546ef6951c9236dd")))
       (sha256 (base32 "09c0143q9sm75xp1qz7a7ihdqfwqg4w8nlq0mmnivhvamww775ss"))))
    (native-inputs (list guile-3.0))
    (build-system guile-build-system)
    (synopsis "Extensible Guile bindings for SwayWM")
    (description "Extensible Guile bindings for SwayWM")
    (license license:expat)))

(packages->manifest (list guile-3.0 guile-swayer))
