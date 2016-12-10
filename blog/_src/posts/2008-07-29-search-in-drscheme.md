
    Title:Search in DrScheme
    Date:2008-07-29T18:10:00.011-04:00
    Tags:

*posted by Robby Findler*


I've just overhauled search in DrScheme. Instead of popping up a dialog box and asking you for a search string, you get a new editor along the bottom of the DrScheme window where you can type a search string and DrScheme responds by circling (in purple) all of the occurrences of the search string in the file, both in the main window and in the contour window.


From there, you can use the new menu shortcuts to navigate forwards and backwards through the hits, optionally replacing occurrences of the search string with the replace strings (see the Edit menu).



With the default settings, typing a search string does not move around in the main window. Specifically (unlike Emacs, Safari, or Firefox), you don't move immediately to the first occurrence of the search string. You have to hit return or one of the search keys (in the Edit menu) for that to happen. But you can enable this in the General pane
of the Editing tab in the preferences dialog. Click on "Search using anchors". Now, when you search for a string, DrScheme will whizz you right to it and when you edit the search string, DrScheme will shift the insertion point based on the search string. In order for you to keep track of where edits will go, you'll see a funny looking little red dongle in your text. That's where the insertion point was when you
started your search, and that's where DrScheme starts all of these implicit searches from.


Try it out and let us know what you think! In particular, is the anchor-based search or the non-anchor-based search more intuitive for you?


<!-- more -->



* * *

It is better that it is intuitive or obvious to people how the anchoring works, then where it is anchored.

Sure we've all been trained by our favorite web browsers, but we're also fast learners ;).

That said, I'm quite used to the anchor being the start of the file.

— *grant rettke, 30 July 2008*

* * *

