To start tracking an existing project, use 
```
git init
```

To clone a repository, use
```
git clone [url] [directory]
```

To find the current status of the project, use
```
git status
```

To add files to the staging area, use
```
git add [filenames]
```

To untrack files, add them to a file named `.gitignore`. Note that you may use
glob patterns to specify filenames.

To find changes made on files, use
```
git diff [filename]
```

To commit changes, use
```
git commit
```

To skip staging area and commit every file being tracked, use
```
git commit -a
```

To stop tracking files, use
```
git rm [filenames]
```

To rename a file, use
``
git mv [old-filename] [new-filename]
``

To view commit history, use
```
git log
```

For displaying the log in a specific format, use
```
git log --pretty=format:"[format specifiers]"
```
where `format specifiers` are the following:

|	Option	|					Description	|
|	-----	|	--------------------------	|
|	%H		|					Commit hash	|
|	%h		|		Abbreviated commit hash	|
|	%T		|					Tree hash	|
|	%t		|		Abbreviated tree hash	|
|	%P		|					Parent hash	|
|	%p		|		Abbreviated parent hash	|
|	%an		|					Author name	|
|	%ae		|				Author email	|
|	%ad		|				Author date		|
|	%ar		|		Author date, relative	|
|	%cn		|				Committer name	|
|	%ce		|				Committer email	|
|	%cd		|				Committer date	|
|	%cr		|	Committer date, relative	|
|	%s		|						Subject	|

Some other common options for `git log` are
|	Option			|	Description	|
|	-----	|	--------------------------	|
|	-p				|	Show the patch introduced with each commit	|
|	--stat			|	Show statistics for files modified in each commit	|
|	--shortstat		|	Display only changed/insertions/deletions line from the `--stat`command	|
|	--name-only		|	Show list of files modified after commit info	|
|	--name-status	|	Show list of files with added/modified/deleted info	|
|	--abbrev-commit	|	Show only first few characters of SHA-1 checksum	|
|	--relative-date	|	Display date in relative format	|
|	--graph			|	Display ASCII graph showing branch and merge history	|
|	--pretty		|	Show commits in an alternate format	|

Some options to limit output of `git log`
|	Option	|	Description	|
|	-----	|	--------------------------	|
|	-(n)	|	Show only the last n commits	|
|	--since, --after	|	Limit the commits to those made after the specified date.	|
|	--until, --before	|	Limit the commits to those made before the specified date.	|
|	--author	|	Only show commits in which the author entry matches specified string.	|
|	--committer	|	Only show commits in which the committer entry matches specified string.	|
|	--grep	| Only show commits with a commit message containing the string.  |
|	-S	|	Only show commits adding or removing code matching the string	|

To add current staging area into previous commit, use
```
git commit --amend
```

To remove a file from the staging area, use
```
git reset HEAD [filenames]
```

To revert a file to the previous commit, use
```
git checkout -- [filenames]
```

To list all remotes, use
```
git remote
```

Use the `-v` option for a verbose listing

To add a new remote, use
```
git remote add [shortname] [url]
```

To fetch information from a remote, use
```
git fetch [remote-name]
```

To push changes upstream, use
```
git push [remote-name] [branch-name]
```

To get more information about a particular remote, use
```
git remote show [remote-name]
```

To rename a remote, use
```
git remote rename [old-name] [new-name]
```

To remove a remote, use
```
git remote rm [remote-name]
```

Tags are used to specify points in history as being important.

To list available tags, use
```
git tag
```

To search for a particular tag, use
```
git tag -l "glob"
```

Git uses two main types of tags: lightweight and annotated.

A lightweight tag is just a pointer to an specific commit. Annotated tages are
stored as full objects in the Git database. They are checksummed; contain the
trigger name, email, and date; have a tagging message; and can be signed and
verified with GPG.

To create an annotated tag for the latest commit, use `-a` while running the 
`tag` command:
```
git tag -a [tag-name] -m "[tag-message]"
```

To view tag data along with the commit that was tagged, use
```
git show [tag-name]
```

To create a lightweight tag, don't supply the `-a`, `-s` or `-m` options:
```
git tag [tag-name]
```

To tag a previous commit, use
```
git tag -a [tag-name] [checksum]
```

Tags must be explicitly pushed to a shared server
```
git push [remote] [tag-name]
```

To push all tags at once, use
```
git push origin --tags
```

To put a version of the project with the working directory that looks like a
specific tag, use
```
git checkout -b [branch-name] [tag-name]
```

To setup aliases, use
```
git config --global alias.[alias-name] '[command]'
git config --global alias.unstage 'reset HEAD --'
```
