# Generic Structure Reader

This system able to replace structure slot reader with generic function.

In case a function being used as argument in `structure-slot-reader' and unexpectedly returning nil instead of structure instance, it will signal an error due to unable to handle nil.

Imagine a case when there are hundred or thousand reader being used, and condition need to be added in the arguments, instead adding condition all over the files, creating one method will save the time.

Author: _Panji Kusuma <epanji@gmail.com>_

Notes:
- _You should only use this system if an error occurred because of unexpected argument in `structure-slot-reader'._
- _If all or majority readers on structure need to be replaced as generic function, maybe you should consider using class instead._
- _It is tested and only known run on SBCL._

## Usage

``` common-lisp
(in-package :cl-user)
(add-package-local-nickname "GSR" :generic-structure-reader)

(defstruct foo a b)

(gsr:define-generic-structure-reader foo-a (foo)
  (:method ((object null)) 'expected-value))
```

## Tests

```
CL-USER> (asdf:test-system "generic-structure-reader")

Running test suite GENERIC-STRUCTURE-READER-SUITE
 Running test REPLACE-STRUCTURE-READER-WITH-GENERIC-FUNCTION ....
 Running test ADDING-METHOD-FOR-STRUCTURE-READER ....
 Running test ACCESS-UNREPLACED-STRUCTURE-READER ..
 Running test ABLE-TO-CHANGE-VALUES ....
 Running test ABLE-TO-USE-WITH-SLOTS-AND-ACCESSORS ....
 Running test MISMATCH-BETWEEN-STRUCTURE-AND-READER ....
 Running test REVOKE-GENERIC-STRUCTURE-READER ..
 Did 24 checks.
    Pass: 24 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```

## License

Public Domain
