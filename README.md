## Native review process

To be able to upload native JS code to Elm's package repository, the repository name has to be [whitelisted](https://github.com/elm-lang/package.elm-lang.org/blob/master/native-whitelist.json).
The [Elm packages team](https://github.com/orgs/elm-lang/teams/packages) is responsible for reviewing the proposals and sending a pull request.
Evan is responsible for merging the pull request, and doing the final review. 

1. First, the author of the library should open a new issue with the title `Native review for github-user/repo-name`.
2. Members from the packages team will review the library and comment on the issue.
3. Once three members give their approval, the packages team will create a pull request to add the repo to the whitelist.
4. Evan will either merge the pull request, or veto whitelisting the library.

In the case that the library get's rejected, the packages team should provide feedback on the issue so the library author can make changes. There is no limit to the number of times a library can be re-reviewed, but we ask that you keep the discuss in one issue (don't create new issues for another review).

## Open issues

Members of the packages team should check the [list of open native reviews](https://github.com/elm-lang/package.elm-lang.org/labels/NativeReview) frequently and use the `NativeReview-*` tags to organize them.
