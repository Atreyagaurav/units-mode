;; Run all tests using (ert t)

;; conversion tests
(ert-deftest units-convert-single-test ()
  (should (= (string-to-number
	      (units-convert-single 10 "ft" "m")) 3.048))
  (should (= (string-to-number
	      (units-convert-single 1 "m" "ft")) 3.2808399))
  (should (= (string-to-number
	      (units-convert-single 1 "kg" "g")) 1e3))
  (should (= (string-to-number
	      (units-convert-single 1 "hour" "seconds")) (* 60 60)))
  (should (= (string-to-number
	      (units-convert-single 1 "day" "hour")) 24))
  (should-error (units-convert-single 10 "ft" "kg")))

(ert-deftest units-convert-simple-test ()
  (should (= (units-convert-simple 10 "ft" "m") 3.048))
  (should (= (units-convert-simple 1 "m" "ft") 3.2808399))
  (should (= (units-convert-simple 1 "kg" "g") 1e3))
  (should (= (units-convert-simple 1 "hour" "seconds") (* 60 60)))
  (should (= (units-convert-simple 1 "day" "hour") 24))
  (should-error (units-convert-simple 10 "ft" "kg")))

(ert-deftest units-convert-test ()
  (should (= (string-to-number
	      (units-convert "10ft" "m")) 3.048))
  (should (= (string-to-number
	      (units-convert "1 m" "ft")) 3.2808399))
  (should (= (string-to-number
	      (units-convert "1kg" "g")) 1e3))
  (should (= (string-to-number
	      (units-convert "1 hour" "seconds")) (* 60 60)))
  (should (= (string-to-number
	      (units-convert "1 day" "hour")) 24))
  (should (string= (units-convert "1.5 day" "day;hour") "1;12"))
  (should (string= (units-convert ".5 day" "day;hour") "0;12"))
  (should-error (units-convert "1 ft" "kg")))

(ert-deftest units-convert-formatted-test ()
  (should (string=
	   (units-convert-formatted "10ft" "m") "3.048 m"))
  (should (string=
	   (units-convert-formatted "1 m" "ft") "3.2808399 ft"))
  (should (string=
	   (units-convert-formatted "1kg" "g") "1000 g"))
  (should (string=
	   (units-convert-formatted "1 hour" "seconds") "3600 seconds"))
  (should (string=
	   (units-convert-formatted "1 day" "hours") "24 hours"))
  (should (string=
	   (units-convert-formatted "1.5 day" "day;hours")
	   "1 day + 12 hours"))
  (should (string=
	   (units-convert-formatted ".5 day" "day;hours") "12 hours"))
  (should-error (units-convert-formatted "1 ft" "kg")))

;; fails on ert test on github with units: unrecognized option
;; '--conformable'" "" "Try 'units --help' for more information.

;; (ert-deftest units-conformable-list-test ()
;;   (let ((conformable-list (units-conformable-list "1 m")))
;;     ;; there are a lot, but putting important ones
;;     (should (member "ft" conformable-list))
;;     (should (member "in" conformable-list))
;;     (should (member "mile" conformable-list))))

(ert-deftest units-ignore-test ()
  (should (= (units-ignore 1 "m") 1))
  (should (= (units-ignore 1 "") 1))
  (should (= (units-ignore 1 "ft") 1))
  (should-error (units-ignore 2)))

(ert-deftest units-atomic-weight-test ()
  (should (< (- (units-atomic-weight "H") 1.0079) 1e-2))
  (should (< (- (units-atomic-weight "He") 4.0026) 1e-2))
  (should (< (- (units-atomic-weight "Li") 6.94) 1e-2))
  (should (< (- (units-atomic-weight "Be") 9.01218) 1e-2))
  (should (< (- (units-atomic-weight "B") 10.81) 1e-2))
  (should (< (- (units-atomic-weight "C") 12.011) 1e-2))
  (should (< (- (units-atomic-weight "N") 14.007) 1e-2))
  (should (< (- (units-atomic-weight "O") 15.999) 1e-2))
  (should (< (- (units-atomic-weight "F") 18.9984) 1e-2))
  (should (< (- (units-atomic-weight "Ne") 20.1797) 1e-2))
  (should (< (- (units-atomic-weight "Na") 22.9898) 1e-2))
  (should (< (- (units-atomic-weight "Mg") 24.305) 1e-2))
  (should (< (- (units-atomic-weight "Al") 26.9815) 1e-2))
  (should (< (- (units-atomic-weight "Si") 28.085) 1e-2))
  (should (< (- (units-atomic-weight "P") 30.9738) 1e-2))
  (should (< (- (units-atomic-weight "S") 32.06) 1e-2))
  (should (< (- (units-atomic-weight "Cl") 35.45) 1e-2))
  (should (< (- (units-atomic-weight "Ar") 39.9481) 1e-2))
  (should (< (- (units-atomic-weight "K") 39.0983) 1e-2))
  (should (< (- (units-atomic-weight "Ca") 40.0784) 1e-2))
  (should (< (- (units-atomic-weight "Sc") 44.9559) 1e-2))
  (should (< (- (units-atomic-weight "Ti") 47.8671) 1e-2))
  (should (< (- (units-atomic-weight "Cr") 51.9962) 1e-2))
  (should (< (- (units-atomic-weight "Mn") 54.938) 1e-2))
  (should (< (- (units-atomic-weight "Fe") 55.8452) 1e-2))
  (should (< (- (units-atomic-weight "Co") 58.9332) 1e-2))
  (should (< (- (units-atomic-weight "Ni") 58.6934) 1e-2))
  (should (< (- (units-atomic-weight "Cu") 63.5463) 1e-2))
  (should (< (- (units-atomic-weight "Zn") 65.382) 1e-2))
  (should (< (- (units-atomic-weight "Ga") 69.7231) 1e-2))
  (should (< (- (units-atomic-weight "Se") 78.9718) 1e-2))
  (should (< (- (units-atomic-weight "Br") 79.904) 1e-2))
  (should (< (- (units-atomic-weight "Zr") 91.2242) 1e-2))
  (should (< (- (units-atomic-weight "Ag") 107.868) 1e-2))
  (should (< (- (units-atomic-weight "I") 126.904) 1e-2))
  (should (< (- (units-atomic-weight "Xe") 131.294) 1e-2))
  (should (< (- (units-atomic-weight "Cs") 132.905) 1e-2))
  (should (< (- (units-atomic-weight "La") 138.905) 1e-2))
  (should (< (- (units-atomic-weight "Eu") 151.964) 1e-2))
  ;; (should (< (- (units-atomic-weight "Tm") 168.934) 1e-2)) missing in gnu units?
  (should (< (- (units-atomic-weight "Au") 196.967) 1e-2))
  (should (< (- (units-atomic-weight "Hg") 200.592) 1e-2))
  (should (< (- (units-atomic-weight "Pb") 207.21) 1e-2))
  (should (< (- (units-atomic-weight "Bi") 208.98) 1e-2))
  (should (< (- (units-atomic-weight "Po") 209) 1e-2))
  (should (< (- (units-atomic-weight "Ac") 227.0278) 1e-2))
  (should (< (- (units-atomic-weight "Th") 232.038) 1e-2))
  (should (< (- (units-atomic-weight "Pa") 231.036) 1e-2))
  (should (< (- (units-atomic-weight "U") 238.029) 1e-2)))

(ert-deftest units-molar-weight-test ()
  (should (= (units-molar-weight "H") 1.00794))
  (should (= (units-molar-weight "He") 4.002602))
  (should (= (units-molar-weight "H2O") 18.01528))
  (should (= (units-molar-weight "CO2") 44.0098))
  (should (= (units-molar-weight "20CO2") 880.196))
  (should (= (units-molar-weight "CaCO3") 100.0872))
  (should (= (units-molar-weight "MgCO3.8H2O") 228.43644)))

(ert-deftest units-compound-composition-test ()
  (should (string= (units-compound-composition "H") "hydrogen"))
  (should (string= (units-compound-composition "He") "helium"))
  (should (string= (units-compound-composition "H2O") "hydrogen 2 + oxygen"))
  (should (string= (units-compound-composition "CO2") "carbon + oxygen 2"))
  (should (string= (units-compound-composition "20CO2") "20 (carbon + oxygen 2)"))
  (should (string= (units-compound-composition "C6H6O6") "carbon 6 + hydrogen 6 + oxygen 6"))
  (should (string= (units-compound-composition "CaCO3") "calcium + carbon + oxygen 3"))
  (should (string= (units-compound-composition "CaCO3.2H2O")
		   "calcium + carbon + oxygen 3 + 2 (hydrogen 2 + oxygen)")))

