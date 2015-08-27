## Native review process

To be able to upload native JS code to Elm's package repository, the repository name has to be [whitelisted](https://github.com/elm-lang/package.elm-lang.org/blob/master/native-whitelist.json).
The [Elm packages team](https://github.com/orgs/elm-lang/teams/packages) is responsible for reviewing the proposals and sending a pull request.
Evan is responsible for merging the pull request, and doing the final review. 

1. First, the author of the library should open a new issue with the title: `Native review for github-user/repo-name`
2. Members from the packages team will review the library and comment on the issue.
3. Once three members give their approval, the packages team will create a pull request to add the repo to the whitelist.
4. Evan will either merge the pull request, or veto whitelisting the library.

In the case that the library get's rejected, the packages team should provide feedback on the issue so the library author can make changes. There is no limit to the number of times a library can be re-reviewed, but we ask that you keep the discuss in one issue (don't create new issues for another review).

## Guidelines for native libraries

Library authors should follow these guidelines to avoid having their library rejected. Please note that even if you meet all the requirements on this list, it does not guarantee that your library will be approved. However the reviewer will give you clear feedback on what changes you need to make to get your library approved.

 * Native libraries must only be used for functions which cannot be implemented in Elm.
   * For example, in 0.16 Elm will support tail-call optimization and at that time "avoiding stack overflow" will no longer be a good enough reason to use a native library.
 * All functions must be type-safe and not cause runtime errors. This is a feature of Elm that we want to preserve.
 * All effects must be wrapped in a Signal or Task (Since 0.15, Task is preferred for most functionality).
 * All other functions must not cause side effects.
 * Native libraries should have appropriate types for all definitions. No [stringly typed](http://c2.com/cgi/wiki?StringlyTyped) functions please!


## Open issues

Members of the packages team should check the [list of open native reviews](https://github.com/elm-lang/package.elm-lang.org/labels/NativeReview) frequently and use the `NativeReview-*` tags to organize them.
