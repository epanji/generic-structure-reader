# Generic Structure Reader
### _Panji Kusuma <epanji@gmail.com>_

This package can be used to replace structure slot reader with generic function. When you expected to get structure instance from function, sometimes what you got only nil as the result. Imagine when there are hundred or thousand reader being used in project. Instead replacing condition all over the files, just create one method will save the time.

You should only use this when there is reader from structure that lead to an error. If all or majority readers on structure need to be replaced as generic function, maybe you should consider using class instead.

## Usage

``` common-lisp
(in-package :cl-user)
(use-package :generic-structure-reader)

(defstruct foo a b)

(define-generic-structure-reader foo-a (foo)
  (:method ((object null)) 'expected-value))
```

## Tests

```
CL-USER> (asdf:test-system :generic-structure-reader)

Running test suite GENERIC-STRUCTURE-READER-SUITE
 Running test REPLACE-STRUCTURE-READER-WITH-GENERIC-FUNCTION ....
 Running test ADDING-METHOD-FOR-STRUCTURE-READER ....
 Running test ACCESS-UNREPLACED-STRUCTURE-READER ..
 Running test STILL-ABLE-TO-CHANGE-VALUES ....
 Running test MISMATCH-BETWEEN-STRUCTURE-AND-READER ...
 Did 17 checks.
    Pass: 17 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```

## License

Public Domain