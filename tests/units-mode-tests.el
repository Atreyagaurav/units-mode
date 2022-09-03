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

(ert-deftest units-conformable-list-test ()
  (let ((conformable-list (units-conformable-list "1 m")))
    ;; there are a lot, but putting important ones
    (should (member "ft" conformable-list))
    (should (member "in" conformable-list))
    (should (member "mile" conformable-list))))

(ert-deftest units-read-test ()
  (should (= (units-read "1m") 1))
  (should (= (units-read "1 m") 1))
  (should (= (units-read "1") 1))
  (should (= (units-read "1 ft") 1))
  (should (= (units-read "1m" "ft") 3.2808399))
  (should (= (units-read "pi" "") 3.1415927))
  (should-error (units-read "pi"))
  (should-error (units-read "meter"))
  (should-error (units-read "5 meter" "")))

