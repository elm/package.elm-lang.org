In the root of this repo, run the following commands:

```bash
tar -zxvf tmp/packages.tar.gz
elm make src/frontend/Page/PackageOverview.elm --yes --output=artifacts/Page-PackageOverview.js
elm reactor
```

At localhost:8000, navigate into the `tmp/` directory and check out the various HTML files. As you look through, notice that:

  * The "timeline" of releases looks really bad when things are too dense.
  * The styling for diffs needs some aesthetic work, especially for folks who are red-green colorblind.

To work on these things, all you need to do is make changes to `Page.PackageOverview` and its dependencies. You can recompile with the same `elm make` command as in the setup:

```bash
elm make src/frontend/Page/PackageOverview.elm --yes --output=artifacts/Page-PackageOverview.js
```

And keep using reactor to serve all the necessary assets.


## Fixing the Timeline

I think there are a few options here:

  * **Spread clumps out more.** This would mean:

      1. calculating the locations based on the date.
      2. Iteratively repositioning points based on proximity in "screen space" even if they are not overlapping in "date space"

    This has a theoretical limit though. At some point stuff just stops fitting. Furthermore, I have a feeling that the algorithm to put points on a line like this may be unstable or have weird asymptotics. It is at least non-obvious to me how it should be done.

  * **Put major/minor/patch on different lines.** So there'd be three parallel lines, each labeled with major, minor, or patch. The dots on all the lines would be the same size?

  * **Drop unimportant releases.** Maybe this means shrinking the size of the dots a lot. Maybe it means hiding them entirely. Not really sure.

  * Something else?


## Improving Diff Style

Right now things are not so good for red-green colorblind people. The diffs for changes also are confusing. So I think the ideal may be to do the background in a light green, yellow, or red (for added, changed, or removed) and the text in the normal color. Add to that some visual marker at the beginning of the line (like + - ~) as a redundant marker of what color the background is.

The challenge here is to make sure the overall design stays tasteful. Already there are a lot of colors, so it is important that these colors do not get out of control!


## Add visualization of deltas

Assuming the other parts are working, this is the next thing to add.

Above the timeline, I'd like to have a bar chart showing the number of additions, changes, and removals at each release. Instead of having an author tell you how stable they think the library is, you can look at the history and see "this author makes lots of removals frequently" or "there was a flurry of activity 6 months ago, but only patches after that". So stability is no longer an opinion.

