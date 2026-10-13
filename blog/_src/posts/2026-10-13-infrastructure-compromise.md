    Title: Racket Infrastructure Compromise
    Date: 2026-10-13T18:00:00
    Tags: 
    Authors: Sam Tobin-Hochstadt, Matthew Flatt, Robby Findler, John Clements


On or before August 10th, a macOS system at Northwestern
University which builds and hosts Racket snapshots was
compromised. The attackers installed a crypto miner, which was detected
by Northwestern network monitoring. By August 18th, the machine was
fully deactivated, and was subsequently wiped.

After our investigation, there is no evidence that the attackers did
anything other than install a crypto miner, or that they knew the system
was connected to the Racket project, or that they used credentials on
the system to access any other Racket infrastructure. In particular,
we have confirmed that no changes were made to any Racket installers
or package catalogs.

However, the system had access to signing keys for signing Apple
binaries, and access to a machine that could sign Windows executables
with the Racket Programming Language Foundation signing
key. Therefore, we have generated new signing keys for all of these
purposes. All binaries and installers signed with the potentially
compromised keys have been replaced on download.racket-lang.org with
newly signed versions. We have informed both Apple and GlobalSign,
the issuers of these certificates, of the compromise.

GlobalSign has now revoked the Windows signing certificate. As a
result, Windows installers signed with the old certificate may no
longer be able to be installed; anyone who downloaded such an installer should
download a new one from [download.racket-lang.org](https://download.racket-lang.org). However, installed
Windows binaries are not affected.

Unfortunately, once the Apple certificate is revoked, existing
_installations_ of signed macOS binaries will no longer function. To
avoid excessive disruption to students and classes in the middle of
the term, we expect to have the Apple certificate revoked at the end
of the current semester. However, we recommend that anyone who has a
macOS installation of Racket versions from 8.12 through 9.3 install
new binaries from
[download.racket-lang.org](https://download.racket-lang.org) now,
rather than waiting for the revocation. If you installed "Minimal
Racket", then after reinstalling, you can upgrade all installed
packages that include signed libraries with `raco pkg update --all`.

To ensure that this cannot happen again, we have made several changes
to our infrastructure. The relevant machine, now wiped and
reinstalled, no longer handles many different Racket
services. Additionally, systems that sign macOS snapshot binaries are
no longer able to access Windows signing keys. Also, we now use
separate signing keys for snapshots and releases, so a problem with
snapshot systems cannot affect releases in this way.

We recognize that everyone who uses Racket trusts the project, and
trusts us, to provide them with trustworthy software. We apologize for
the failure here, and for the difficulties it imposes on all of our
users.
