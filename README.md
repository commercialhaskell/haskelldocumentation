This repository is an implementation of the [intermediate Haskell documentation
proposal](https://github.com/commercialhaskell/commercialhaskell/blob/master/proposal/intermediate-haskell-documentation.md).
The goal is to have a repository where any documentation can be added,
collaboratively maintained, and linked together in various outlines to provide
a structured learning experience. Concretely: while the original proposal was
specifically to enable "intermediate Haskell documentation," this repository is
intended to support any kind of documentation.

## Licensing

All content in this repository is licensed [Attribution 4.0
International (CC BY 4.0)](http://creativecommons.org/licenses/by/4.0/). By
pushing content into this repository or sending a pull request, you are
implicitly assigning that license to the content.

## Commit access

The goal of this repository is to be incredibly open about allowing
contributors. If you would like to participate, open an issue asking for commit
access, and odds are it will be granted. Similarly, odds are pretty high that
just by sending a pull request you will be granted commit access.

The general guidelines for how to treat your access is:

* Any content for which you are the primary author, you should feel free to
  make any changes to. If the changes are significant, feel free to reach out
  to others for feedback.

* You are free to make minor modifications and corrections to *any* content in
  this repository, without consulting the original author. For more significant
  changes, it is an expected courtesy to touch base with the author. If the
  author appears to be non-responsive, check with others and, if there's general
  consensus, move ahead with the changes.

* Merging pull requests applies the same logic as above. If the changes are
  minor or on your content, you may always do it. If they're significant, check
  with the author.

## Structure

There are three main directories in this repository:

* `content` contains all of the raw content. See the `CONTENT.md` file for an
  example of what the content should look like.
* `outline` contains various outlines of the content. See the `OUTLINE.md` file
  for an example of what outlines should look like.
* `src` contains various tooling to go along with this repository.

The files in `content` and `outline` should be kept in a flat structure, i.e.
no suubdirectories. Every file must end with a `.md` file extension. The result
of this is that each content and outline file will have a unique identifier in
the form of the filename, which can be used for linking.

## Linking

We should use standard Markdown linking conventions as relative paths. For example:

* To link to a content file from another content file, the link would be `[link text](destination.md)`.
* To link to a content file from an outline file, the link would be `[link text](../outline/destination.md)`.

As a special feature, we *should* provide tooling that automatically fills in
link text when not provided by grabbing the title from the destination. This
would be triggered by having a link such as: `[insert-title](destination.md)`.

## Embedding?

This is *not* currently supporting. We may consider allowing importing of
content from another file at some point in the future.

## Authoring guidelines

For now, please see [the original
proposal](https://github.com/commercialhaskell/commercialhaskell/blob/master/proposal/intermediate-haskell-documentation.md#quality)
for some recommendations on quality. Eventually, those guidelines should be
moved into this document.

## Wikis

Essentially this is a wiki for intermediate Haskell documentation.

There is an important difference between wikis and github. Most wikis are oddly horrible for discussing content, so they actually don't scale well beyond 1 contributor per page (Wikipedia is a total disaster in this regard, but worse is better).
Pull requests are much easier to work with than most wiki systems. They make it clear that people are notified about changes and contributions are discussed (with line comments) before being committed.
