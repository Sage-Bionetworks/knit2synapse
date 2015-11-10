## knit2synapse: Knit RMarkdown files to Synapse wikis.

### Installing

Using `devtools`:

```
devtools:install_github("Sage-Bionetworks/knit2synapse")
```

### Usage

`knitfile2synapse(markdownfile, owner='syn123', parentWikiId = NULL, wikiName = NULL, overwrite = FALSE)`

* `markdownfile`: a path to a markdown file (including RMarkdown files)
* `owner`: The Entity where the Wiki page should go (this can be a `Project`, `File`, or `Folder` entity)
* `parentWikiId`: If supplied, will make a sub-Wiki underneath this Wiki ID. Note that this is not a Synapse ID (does not start with `syn`, it is an integer)


### Contributing
To contribute, [fork](http://help.github.com/fork-a-repo/) the [main repo](https://github.com/Sage-Bionetworks/knit2synapse), branch off a [feature branch](https://www.google.com/search?q=git+feature+branches) from `master`, make your changes and [commit](http://git-scm.com/docs/git-commit) them, [push](http://git-scm.com/docs/git-push) to your fork and submit a [pull request](http://help.github.com/send-pull-requests/) for `Sage-Bionetworks/knit2synapse:develop`.
