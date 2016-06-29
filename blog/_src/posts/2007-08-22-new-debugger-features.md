
    Title:New Debugger Features
    Date:2007-08-22T21:54:00.001-04:00
    Tags:

*posted by Greg Cooper*



As Eli mentioned, v371 introduces support for debugging several files at a time, as well as new buttons for stepping Over and Out of expressions in the debugger.



Debugging across multiple files is easy.  Start by opening the "main" file that you want to debug and all of the files it requires (directly or indirectly) that you want to debug along with it.  Then click Debug in the main file's frame.  For example, if I wanted to see what the FrTime dataflow engine (in frp-core.ss) does when a particular program (say demo-module.ss) runs, I would open these two files and click Debug in the frame for demo-module.ss.



As each required file loads, DrScheme offers the option of debugging it.  If you choose "yes", then the file is included in the debugging session, so you can set breakpoints and step into it.  (Note that this will make the code in the file run more slowly, and single-stepping at calls to its functions will bring you into it.)   A file can only participate in one debugging session at a time, so if you're already debugging it with some other program, DrScheme will tell you so (instead of asking whether to debug it).   For best results, all of the files you debug should be modules.  Once a file is included in the debugging session, you can set breakpoints and step into it as if you were debugging it by itself.



As soon as you can debug programs that span several files, it's particularly valuable to be able to do more than set breakpoints and single-step.  This is the motivation for the new Over and Out buttons, which are also quite simple.  If the execution marker is at the start of an expression that's not in tail position, then you can step over the entire expression, which is equivalent to setting a one-shot breakpoint at the end of the expression and continuing.  (If you've set breakpoints inside the expression, or inside any functions it calls, then execution may suspend before reaching the end.)  Likewise, if execution is suspended and the current expression is evaluating within a debugging-enabled context, then you can step out to the innermost such context.  This would be difficult to simulate by hand, since you'd need to keep track of recent callers.



At any given point, either or both of the Over and Out buttons may be disabled, but over the course of a session they can eliminate a lot of tedium.



The screenshot above shows a session debugging frp-core.ss as used by demo-module.ss.  Execution is suspended on a right paren, so stepping Over is disabled, but we see the expression's value at the upper left, we've moused over b to see its value at the upper right, and it's possible to step Out.
