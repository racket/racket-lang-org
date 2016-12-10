
    Title:Racket Package Server Security Vulnerabilities
    Date:2015-09-22T00:10:00.000-04:00
    Tags:

*posted by Sam Tobin-Hochstadt*


Recently, we discovered several security vulnerabilities with how both the Racket package catalog server and the Racket package client work. The vulnerabilities have now all been fixed, and we do not know of any exploitation of them. However, we encourage you to take the following steps:


* Change your password on the [http://pkgs.racket-lang.org](http://pkgs.racket-lang.org) site.


* Check any packages you have uploaded to the site, to ensure that no unexpected changes have been made to them.


* Do not use the released versions of the raco pkg catalog-archive command, or the `file/untar` and `file/unzip` libraries, on untrusted inputs. If you use these tools or libraries, use a snapshot build available from [http://pre.racket-lang.org/](http://pre.racket-lang.org/). 



The errors, and how they were fixed
---

A total of 5 errors related to package handling were reported to us by Tony Garnock-Jones and Asumu Takikawa. Two were XSS vulnerabilities relating to handling user input in the package administration dialog. One was an error where unsanitized email addresses with path name components in them could allow a malicious user to impersonate someone else, whom they shared an email suffix with (such addresses are illegal on most mail servers, like Outlook and GMail, but not illegal in SMTP itself.) Two were errors in handling MANIFEST files and tar/zip archives, which allowed decompression to write to arbitrary locations on the file system. These last errors affected not only the server, which decompresses packages to analyze them, but also clients using the commands described above.



The relevant server-side code was fixed to appropriately sanitize user input. The package handling libraries now reject any attempts to navigate up the filesystem hierarchy, meaning that these attacks are no longer possible.



Unfortunately, due to the nature of these attacks, we cannot be sure that they were not exploited, but we have no evidence that they were. Therefore, we encourage anyone with an account to change their password, and to treat the password as compromised. Please also check your existing packages to make sure they are as you left them.



Furthermore, using the `file/untar` and `file/unzip` libraries, the raco pkg catalog-archive command, and the internal functions that manipulate packages is not safe on untrusted inputs in released versions of Racket. Since raco pkg install executes code, it is already unsafe to use on untrusted packages, but simply extracting malicious packages is also unsafe.



We have not released a new version of Racket, but encourage anyone who needs to perform these commands to use a snapshot build. The next version of Racket will be released on-schedule in October. If, however, you would benefit from a patched version of Racket 6.2.1, please let us know.

