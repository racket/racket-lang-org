
    Title:New Contract-Related Features
    Date:2009-02-14T23:35:00.010-05:00
    Tags:

*posted by Stevie*

In SVN I've added three new major features that involve contracts. One allows for more fine-grained control of contracts, and the other two allow for the use of contracts with signatures and units.

Contract Regions
---

_Contract regions_ allow the programmer to protect a region of code with a contract boundary.  In addition to the wrapped code, the programmer also provides a name for the region which is used in blame situations and a list of exported variables which can either be protected with contracts or unprotected.  The region provides a true contract boundary, in that uses of contracted exports within the region are unprotected.  Contract regions are specified with the `with-contract` form.  The following contract region defines two mutually recursive functions:

```racket
(with-contract region1
 ([f (-> number? boolean?)]
  [g (-> number? boolean?)])
 (define (f n) (if (zero? n) #f (g (sub1 n))))
 (define (g n) (if (zero? n) #t (f (sub1 n)))))
 ```

The internal calls to `f` and `g` are uncontracted, but calls to `f`and `g` outside this region would be appropriately contracted.  First-order checks are performed at the region, so the
following region:

```racket
(with-contract region2
 ([n number?])
 (define n #t))
 ```

results in the following error:

```racket
(region region2) broke the contract number? on n; expected <number?>, given: #t
```

Notice that the blame not only gives the name of the region, but describes what type of contract boundary was involved.

For contracting a single definition, there is the `define/contract` form which has a similar syntax to define, except that it takes a
contract before the body of the definition.  

To compare the two forms, the following two definitions are equivalent:

```racket
(with-contract fact
 ([fact (-> number? number?)])
 (define (fact n)
   (if (zero? n) 1 (* n (fact (sub1 n))))))

(define/contract (fact n)
 (-> number? number?)
 (if (zero? n) 1 (* n (fact (sub1 n)))))
```

First order checks are similarly performed at the definition for
`define/contract`, so

```racket
(define/contract (fact n)
 (-> number?)
 (if (zero? n) 1 (* n (fact (sub1 n)))))
```

results in

```racket
(function fact) broke the contract (-> number?) on fact; expected a procedure that accepts no arguments without any keywords, given: #<procedure:fact>
```

Signature Contracts
---

In addition to contract regions, units are also now contract boundaries.  One way to use contracts with units is to add contracts to unit signatures via the contracted `signature` form.

```racket
(define-signature toy-factory^
 ((contracted
   [build-toys (-> integer? (listof toy?))]
   [repaint    (-> toy? symbol? toy?)]
   [toy?       (-> any/c boolean?)]
   [toy-color  (-> toy? symbol?)])))
```

Notice that contracts in a signature can use variables listed in the signature.Now if we take the following implementation of that signature:

```racket
(define-unit simple-factory@
 (import)
 (export toy-factory^)
  
 (define-struct toy (color) #:transparent)
  
 (define (build-toys n)
   (for/list ([i (in-range n)])
     (make-toy 'blue)))
  
 (define (repaint t col)
   (make-toy col)))
```

We get the appropriate contract checks on those exports:

```racket
> (define-values/invoke-unit/infer simple-factory@)
> (build-toys 3)
(#(struct:toy blue) #(struct:toy blue) #(struct:toy blue))
> (build-toys #f)
top-level broke the contract (-> integer? (listof toy?))
 on build-toys; expected >, given: #f
```

As before, uses of contracted exports inside the unit are not checked.

Since units are contract boundaries, they can be blamed appropriately.  Take the following definitions:

```racket
(define-unit factory-user@
 (import toy-factory^)
 (export)
 (let ([toys (build-toys 3)])
   (repaint 3 'blue)))

(define-compound-unit/infer factory+user@
 (import) (export)
 (link simple-factory@ factory-user@))
When we invoke the combined unit:> (define-values/invoke-unit/infer factory+user@)
(unit factory-user@) broke the contract
 (-> toy? symbol? toy?)
on repaint; expected >, given: 3
```

Unit Contracts
---

However, we may not always be able to add contracts to signatures.  For example, there are many already-existing signatures in PLT Scheme that one may want to implement, or a programmer may want to take a unit value and add contracts to it after the fact.

To do this, there is the `unit/c` contract combinator.  It takes a list of imports and exports, where each signature is paired with a list of variables and their contracts for each signature.  So if we had the uncontracted version of the toy-factory^ signature:

```racket
(define-signature toy-factory^
 (build-toys repaint toy? toy-color))
```

the following contracts would be appropriate for a unit that imports nothing and exports that signature:

```racket
(unit/c (import) (export))
(unit/c (import) (export toy-factory^))
(unit/c
 (import)
 (export (toy-factory^
          [toy-color (-> toy? symbol?)])))
(unit/c
 (import)
 (export (toy-factory^
          [build-toys (-> integer? (listof toy?))]
          [repaint    (-> toy? symbol? toy?)]
          [toy?       (-> any/c boolean?)]
          [toy-color  (-> toy? symbol?)])))
```

Unit contracts can contain a superset of the import signatures and a subset of the export signatures for a given unit value.  Also, variables that are not listed for a given signature are left alone when the contracts are being added.

Since the results of applying `unit/c` is a new unit, then adding a contract can cause link inference to fail.  For example, if we change the definition of `simple-factory@` above to

```racket
(define/contract simple-factory@
 (unit/c
  (import)
  (export (toy-factory^
           [build-toys (-> integer? (listof toy?))]
           [repaint    (-> toy? symbol? toy?)]
           [toy?       (-> any/c boolean?)]
           [toy-color  (-> toy? symbol?)])))
 (unit
   (import)
   (export toy-factory^)
  
   (define-struct toy (color) #:transparent)
  
   (define (build-toys n)
     (for/list ([i (in-range n)])
       (make-toy 'blue)))
  
   (define (repaint t col)
     (make-toy col))))
```

Then when we try to combine it with the `factory-user@` unit, we
get:

```racket
define-compound-unit/infer: not a unit definition in: simple-factory@
```

One way to solve this is to use `define-unit-binding` to set up the static information for the new contracted value.  Another possibility for unit definitions is to use `define-unit/contract`:

```racket
(define-unit/contract simple-factory@
 (import)
 (export (toy-factory^
          [build-toys (-> integer? (listof toy?))]
          [repaint    (-> toy? symbol? toy?)]
          [toy?       (-> any/c boolean?)]
          [toy-color  (-> toy? symbol?)]))

 (define-struct toy (color) #:transparent)

 (define (build-toys n)
   (for/list ([i (in-range n)])
     (make-toy 'blue)))

 (define (repaint t col)
   (make-toy col)))
```

More about these features can be found in the Reference, and a short section about signature and unit contracts has been added to the Guide.