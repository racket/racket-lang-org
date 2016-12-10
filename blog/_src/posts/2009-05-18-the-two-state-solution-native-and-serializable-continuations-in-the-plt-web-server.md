
    Title:The Two State Solution: Native and Serializable Continuations in the PLT Web Server
    Date:2009-05-18T16:48:00.000-04:00
    Tags:

*posted by Jay McCarthy*

One of the annoyance of the [stateless Web application](http://docs.plt-scheme.org/web-server/stateless.html) language that comes with the PLT Web Server is that you can't call _third-party higher-order library procedures with arguments that try to capture serializable continuations_. (I know, you try to do that all the time.) For example:

```racket
(build-list
 3
 (lambda (i)
   (call-with-serializable-current-continuation
    (lambda (k) (serialize k)))))
```

The problem is that the stateless language performs a transformation on your program to extract the continuations into a serializable representation. If you really need to do this, we've developed a compromise called "The Two State Solution": one state on the client and the other on the server. Only the third-party parts of the continuation (in this case, the code inside `build-list`) are stored on the server; everything else is shipped to the client. You just need to annotate your code slightly to indicate where the transition is:

```racket
(serial->native
 (build-list
  3
  (lambda (i)
    (native->serial
     (call-with-serializable-current-continuation
      (lambda (k) (serialize k)))))))
```

`serial->native` signals the transition to the third-party and `native->serial` signals the transition back.

It is still a little annoying to find when you've called these _third-party higher-order library procedures with arguments that try to capture serializable continuations_, so there's a simple macro that provides a transitioning wrapper for you:

```racket
(define-native (build-list/native _ ho) build-list)
```

expands to:


```racket
(define (build-list/native fst snd)
  (serial->native
   (build-list
    fst
    (lambda args
      (native->serial
       (apply snd args))))))
```

This new feature is documented in the [online manual](http://faculty.cs.byu.edu/~jay/plt-doc/web-server/stateless.html#(part._.Serializable_.Continuations)), of course.