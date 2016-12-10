
    Title:Retiring `unstable`
    Date:2015-10-30T17:34:00.000-04:00
    Tags:

*posted by Vincent St-Amour*

Some of you may be familiar with the `unstable` collection, whose purpose was to serve as a staging ground for new APIs that hadn't yet found a more permanent home. With the advent of the package system, packages can serve that same purpose, which removes the need for a dedicated `unstable` collection provided by the main distribution.

For this reason we are moving `unstable-*` packages out of the main distribution.

For backwards compatibility, the packages remain available from the package catalog. Packages that properly list their dependencies (as they should! it's an error not to!) are unaffected by this change. Packages that are missing dependencies may need to be adjusted to include the appropriate `unstable` dependencies.

The `unstable` packages contained many useful functions and APIs, and we merged many of them into established Racket libraries. Others were spun off as their own packages. The remaining APIs, which we judged too narrow or too immature, we left in `unstable` packages, where they are still available in their original form. In all cases, the original `unstable` libraries continue to export the same bindings they always did, to ensure backwards compatibility.

For completeness, here is a list of the fate of each unstable library that used to be part of the main distribution.

* `unstable/2d`
    * Moved to the `2d` package.


* `unstable/arrow`
    * Left in `unstable-lib`.


* `unstable/automata`
    * Moved to the `automata` package.


* `unstable/bytes`
    * Left in `unstable-lib`.


* `unstable/class-iop`
    * Moved to the `class-iop` package.


* `unstable/contract`
    * Moved `non-empty-string?` to `racket/string`.

    * Moved `port-number?` and `tcp-listen-port?` to `racket/tcp`, the latter renamed to `listen-port-number?`.

    * Moved `if/c`, `failure-result/c`, `predicate/c` and `rename-contract` to `racket/contract`.

    * Moved `treeof` to `plot/utils`.

    * Moved `sequence/c` to `racket/sequence`.

    * Left `path-piece?`, `maybe/c`, `truth/c` in `unstable-contract-lib`.


* `unstable/custom-write`
    * Moved `make-constructor-style-printer` to `racket/struct`.

    * Left `prop:auto-custom-write` in `unstable-lib`.


* `unstable/debug`
    * Left in `unstable-debug-lib`.


* `unstable/define`
    * Left in `unstable-lib`.


* `unstable/error`
    * Left in `unstable-lib`.


* `unstable/find`
    * Left in `unstable-lib`.


* `unstable/flonum`
    * Superseded by `math/flonum`. Left in `unstable-flonum-lib`.


* `unstable/function`
    * Merged with `racket/function`.


* `unstable/future`
    * Merged with `racket/future`.


* `unstable/gui/notify`
    * Moved to `framework/notify`, with naming changes.


* `unstable/gui/pict`
    * Moved `color/c`, `light`, `dark`, `red`, `orange`, `yellow`, `green`, `blue`, `purple`, `black`, `brown`, `gray`, `white`, `cyan`, and `magenta` to `pict/color`.

    * Moved `show`, `hide`, `pict-if`, `pict-cond`, and `pict-case` to `pict/conditional`.

    * Merged `scale-to` with `pict`'s `scale-to-fit`.

    * Merged `ellipse/border`, `circle/border`, `rectangle/border`, `rounded-rectangle/border` with `pict`'s `ellipse`, `circle`, `rectangle`, and `rounded-rectangle`, respectively.

    * Merged `pin-label-line`, `pin-arrow-label-line`, and `pin-arrows-label-line` with `pict`'s `pin-line`, `pin-arrow-line`, and `pin-arrows-line`, respectively.

    * Moved `blur`, `shadow`, and `shadow-frame` to `pict/shadow`.

    * Moved `unstable/gui/pict/align` to `ppict/align`, in the `ppict` package.

    * Left `color`, `pict-match`, `pict-combine`, `with-pict-combine`, `fill`, `strike`, `shade`, `blur-bitmap!`, `arch`, `draw-pict-centered`, `backdrop`, `cross-out`, and `make-plt-title-background` in `unstable-lib`.


* `unstable/gui/ppict`
    * Moved to the `ppict` package.


* `unstable/gui/prefs`
    * Moved to `framework/preferences`, with naming changes.


* `unstable/gui/redex`
    * Left in `unstable-redex`.


* `unstable/gui/scribble`
    * Left in `unstable-lib`.


* `unstable/gui/slideshow`
    * Moved `with-size`, `with-scale`, `big`, `small`, `with-font`, `with-style`, `bold`, `italic`, `subscript`, `superscript`, `caps`, and `blank-line` to `slideshow/text`.

    * Moved `slide/staged`, `staged`, `stage`, `stage-name`, `at`, `before`, `after`, `before/at`, `after/at` to the `staged-slide` package.

    * Left `column`, `columns`, `column-size`, `two-columns`, `mini-slide`, `tabular`, `reveal`, `revealing-slide`, and `items-slide` in `unstable-lib`.


* `unstable/gui/snip`
    * Left in `unstable-lib`.


* `unstable/hash`
    * Merged with `racket/hash`.


* `unstable/latent-contract`
    * Left in `unstable-latent-contract-lib`.


* `unstable/lazy-require`
    * `lazy-require` has been in `racket/lazy-require` for some time.

    * Left `begin-on-demand` in `unstable-lib`.


* `unstable/list`
    * Moved `check-duplicates`, `remf`, `remf*`, `group-by`, `cartesian-product`, `list-update`, and `list-set` to `racket/list`. * Moved `list-prefix?`, `take-common-prefix`, `drop-common-prefix`, and `split-common-prefix` to `racket/list`, with slight API changes to harmonize with Racket's list API.

    * Left `filter-multiple`, `extend`, `map/values`, and `map2` in `unstable-list-lib`.


* `unstable/logging`
    * Moved `with-intercepted-logging` and `with-logging-to-port` to `racket/logging`.

    * Left `start-recording` and `stop-recording` in `unstable-lib`.


* `unstable/macro-testing`
    * Moved to `syntax/macro-testing`.


* `unstable/markparam`
    * Moved to the `markparam` package.


* `unstable/open-place`
    * Moved `open-place` to `racket/place`, and renamed it `place/context`.


* `unstable/options`
    * Moved to the `option-contract` package.


* `unstable/parameter-group`
    * Moved to the `parameter-group` package.


* `unstable/pretty`
    * Merged `pretty-format/write`, `pretty-format/display`, and `pretty-format/print` with `racket/pretty`'s `pretty-format`.

    * Left `break-lines` in `unstable-pretty-lib`.


* `unstable/recontract`
    * Merged with `racket/contract` some time ago.


* `unstable/sandbox`
    * Merged with `scribble/eval`.


* `unstable/sequence`
    * Moved `in-syntax` and `in-slice`  to `racket/sequence`.

    * Left `in-pairs`, `in-sequence-forever`, and `sequence-lift` in `unstable-lib`.


* `unstable/socket`
    * Moved to the `unix-socket` package.


* `unstable/string`
    * Left in `unstable-lib`.


* `unstable/struct`
    * Moved `struct->list` to `racket/struct`.

    * Left `make` in `unstable-lib`.


* `unstable/syntax`
    * Moved `make-variable-like-transformer` to `syntax/transformer`.

    * Moved `syntax-source-directory` and `syntax-source-file-name` to `syntax/location`.

    * Left `explode-module-path-index`, `phase-of-enclosing-module`, `format-unique-id`, `syntax-length`, and `syntax-within?` in `unstable/syntax`.


* `unstable/temp-c`
    * Moved to the `temp-c` package.


* `unstable/time`
    * Left in `unstable-lib`.


* `unstable/wrapc`
    * Moved to `syntax/contract`.



<!-- more -->



* * *


Hi, it's my first day with Racket. Attempting to install the pict3d library is failing in a way that mentions not finding unstable/custom-write.

I'm guessing this probably relates to the reorganization mentioned above.

Any tips? Thanks!

— *Corporate Sheep, 27 November 2015*

* * *

