
    Title:Typed Racket and Classes
    Date:2013-12-17T16:44:00.000-05:00
    Tags:

*posted by Asumu Takikawa*

Recently we had our inaugural [Racket Salon](https://dekvek.com/racket-salon/) meetup here in Boston, graciously organized by Dan King. At the meetup I gave a short demo about the upcoming support for classes and object-oriented programming in Typed Racket. In this blog post, I’ll go over the concepts I presented in my demo.


Background

---
As many readers already know, [Typed Racket](http://www.cs.utah.edu/plt/snapshots/current/doc/ts-guide/index.html) is a gradually-typed sister language to Racket. That means it’s a [statically-typed](http://en.wikipedia.org/wiki/Static_typing#Static_type-checking) language that accommodates the idioms of Racket. Programs written in Racket should seamlessly port to Typed Racket with the addition of type annotations here and there. You can even keep some parts of the program dynamically-typed and Typed Racket will make sure those parts won’t break the type invariants via [contracts](http://www.cs.utah.edu/plt/snapshots/current/doc/guide/contracts.html).



Of course, supporting all Racket idioms is quite a lot of work, especially since it’s a constantly evolving language. One of the big pieces missing from Typed Racket right now is support for [classes and objects](http://www.cs.utah.edu/plt/snapshots/current/doc/guide/classes.html). Since the GUI library heavily uses the class system, it’s important to support the object-oriented subset of Racket.



Supporting classes isn’t trivial though. There are a bunch of issues, but the main ones are: (1) the class system is built as a complex macro and so reconstructing the information needed to type-check is tricky, and (2) we need to make sure that interoperation between Racket and Typed Racket using classes/objects can be done safely.



I won’t go over the technical details about the implementation in this blog post, but contact me if you’re interested. In the rest of the blog post, I’ll show some examples to demonstrate what programming in Typed Racket with classes looks like. Just so you know, these examples won’t work in the current version of Typed Racket but will be supported in a future release.



Side note: If you like to live dangerously, you can track the experimental branch with support for classes [here](https://github.com/takikawa/racket/tree/tr-classes).


Fishes and types
---

To start out, let’s look at an untyped example from the Racket Guide on classes and objects. The following snippet defines a fish% class. The class has several features: an initialization argument named size, a private field named current-size, three methods get-size, grow, and eat.


```racket
(define fish%
  (class object%
    (init size)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))
```

The notation should seem mostly familiar if you’ve programmed in Java or other object-oriented languages. The % suffix is a convention for identifiers that are bound to class values. The object% value is the superclass of fish% and is the root class of all class hierarchies. The initialization argument size is used when constructing an instance of a class:



```racket
> (new fish% [size 3])
(object:fish% ...)
```

Note that in Racket, classes are just values that can be passed around like anything else. For example, you can even do silly things like define nested inheriting classes:

```racket
> (new (class (class object% (super-new) (displayln "superclass"))
         (super-new)
         (displayln "subclass")))
superclass
subclass
(object:eval:4:0 ...)
```


In practice, this feature is quite useful since it lets you define [mixins](http://www.cs.utah.edu/plt/snapshots/current/doc/guide/classes.html#%28part._.Mixins%29) easily. That’s a topic for another blog post.



Side note: In other words, Racket has first-class classes. This is a term you might see used in the programming language literature.



Adding types to the fish program is easy. First, we can introduce a type definition for the fish class.


```racket
(define-type Fish%
  (Class (init [size Real])
         [get-size (-> Real)]
         [grow (Real -> Void)]
         [draw (-> Pict)]
         [eat ((Instance Fish%) -> Void)]))
```


This type definition says that Fish% is a class type with the given initialization argument and methods types. Note that the type of the fish class Fish% is not the same as the type of its instances (Instance Fish%). This is an important distinction to make, since both the fish class value and fish object values may appear in the same program.




Side note: If you’re familiar with Typed Racket, you may be surprised that the recursive reference to Fish% in the type definition works. Future versions of Typed Racket will support implicit recursive type definitions.


With the type definition in hand, we can just annotate the class value with the type:


```racket
(: fish% : Fish%)
(define fish%
  (class object%
    (init size)
    (: current-size Real)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))
```

We do need an extra type annotation on the private field because its type isn’t included in the type definition above. This fish definition is a bit boring, so let’s spice it up a bit by making fishes drawable:


```racket
(define-type Fish%
  (Class (init [size Real])
         (get-size (-> Real))
         [grow (Real -> Void)]
         ; a type for the new method
         [draw (-> Pict)]
         [eat ((Instance Fish%) -> Void)]))
(: fish% : Fish%)
(define fish%
  (class object%
    (init size)
    (: current-size Real)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    ; new draw method
    (define/public (draw)
      (standard-fish (* current-size 10)
                     (* current-size 5)))
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))
```

The new draw method that’s been added to the fish relies on some functions from the pict library. Since that’s not currently included in Typed Racket’s standard libraries, we need to give its exports some types:


```racket
; this would go at the top of the file before fish%
(require/typed pict
               [#:opaque Pict pict?]
               [standard-fish (Real Real [#:color String] -> Pict)])

```

The `#:opaque` import form in Typed Racket lets you create a new type that corresponds to some predicate, in this case pict?. It’s useful for bringing in datatypes from dynamically-typed Racket libraries. With that new type, we can give a type for the standard-fish function.



In the end, you can interact with some fish and draw them:



```racket
> (define dory (new fish% [size 5]))
> dory
- : (Instance Fish%)
(object:fish% ...)
> (send dory draw)
- : Pict
image
> (send dory eat dory)
> (send dory draw)
- : Pict
image
```



Conclusion: 
At Racket Salon, I talked about a few other things including mixins and converting a slightly larger program that uses the GUI library. They didn’t translate well to a blog setting so I didn’t include them here. Racket Salon was a fun event, so I encourage anyone in the Boston area to attend!



Typed Racket’s support for classes will land in a future version of Racket, possibly the release after v6.0.