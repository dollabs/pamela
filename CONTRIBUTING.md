# Contributing

Contributions are welcome!

Contributions can be bug reports, feature requests, testing and documentation
in addition to code. Please see the github guide on
[Collaborating on projects using issues and pull requests](https://help.github.com/categories/collaborating-on-projects-using-issues-and-pull-requests/) for details.

Contributions to this project are accepted on an
["inbound=outbound"](https://opensource.com/law/11/7/trouble-harmony-part-1) basis.
That means that you agree that your contributions are made under the
same license as the license for this project, the [Apache License 2.0](http://opensource.org/licenses/Apache-2.0) [LICENSE](LICENSE)

To make this understanding explicit -- and for you to assert
that you have the right to make the contribution -- commits must be
signed off indicating acceptance of the
[Developer Certificate of Origin 1.1](http://developercertificate.org/) [DCO](DCO)

A nice explanation of the DCO has been provided by
[Karl Fogel](http://www.red-bean.com/kfogel/)
in his excellent book [Producing Open Source Software](http://producingoss.com/en/contributor-agreements.html#developer-certificate-of-origin).

An explanation of the "sign-off" procedure is given by
[Linus Torvalds](https://en.wikipedia.org/wiki/Linus_Torvalds) in [Linux](https://github.com/torvalds/linux/blob/master/Documentation/SubmittingPatches#L409).

## Contribution Workflow

This is an overview of the contribution workflow:

 * Fork the repository on Github
 * Create a topic branch from where you want to base your work (usually the master branch)
 * Check the formatting rules from existing code (no trailing whitespace, mostly default indentation)
 * Ensure any new code is well-tested, and if possible, any issue fixed is covered by one or more new tests
 * Make commits to your branch using the following guidelines:
   * Start with a subject line (beginning with a capital, ending without a period, no more than 50 characters)
   * Use the imperative mood in the subject line: "Add x", "Fix y", "Support z", "Remove x"
   * Wrap the body at 72 characters
   * Use the body to explain what and why vs. how
   * Finish the commit message with the sign off: `Signed-off-by: Your Name <me@e.mail>`
 * Push your code to your fork of the repository
 * Make a Pull Request

## Emacs

Are you an Emacs user? If so you can use `magit-commit-popup`
(from [magit](https://magit.vc/)) to add these
commit options for you:
 * **-s** Add Signed-off-by line (--signoff)
 * **=S** Sign using gpg (--gpg-sign="") -- *this is extra credit*

Did you know that Emacs can delete trailing whitespace without
you *every having to think about it*? Just add this to your
Emacs configuration:

````
(add-hook 'before-save-hook 'delete-trailing-whitespace)
````
