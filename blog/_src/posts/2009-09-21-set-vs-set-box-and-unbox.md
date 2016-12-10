
    Title:`set!` vs. `set-box!` and `unbox`
    Date:2009-09-21T07:23:00.007-04:00
    Tags:

*posted by Robby Findler*

A few weeks ago I was chatting with some PLT folks and was surprised to hear them say that they avoided `set!` because using `set-box!` and `unbox` was easier to see what was going on.


This struck me as wrong since one might pass boxes around and then you can't be sure which box you're mutating, but you cannot pass variable references around and thus which variable you're using is always lexically apparent. (Of course, when you add `lambda` into the mix that isn't really true, since you can capture a variable in a closure and pass that around.)


Their point seemed to be that you had to write something special at each use of the box, unlike with `set!` where you simply write a variable reference and it might be getting a changing quantity and it might not be. This made me realize I could do something to help, at least, and so I changed Check Syntax so that it colored `set!`'d variables in red, like this:


