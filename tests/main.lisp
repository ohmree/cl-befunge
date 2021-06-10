(defpackage befunge/tests/main
  (:use :cl
        :befunge
        :rove))
(in-package :befunge/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :befunge)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
