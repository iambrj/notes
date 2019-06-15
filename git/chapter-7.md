## Revision selection
## Single revisions
* SHA-1
* Short SHA-1: atleast 4 characters, unambigious
__reflog__: a log of where your HEAD and branch references have been recently

## Ancestory references
- Use `^` to specify parent of a commit, ex: `d921970^2` means second parent of
  `d921270`
## Commit Ranges

### Double dot
* Used to resolve a range of commits that are reachable from one commit but
  aren't rechable from another
Ex: `master..experiment` = all commits reachable by experiment that aren't
					   	   rechable by master
### Multiple points
Use `--not` and `^` just like `..`
The following are equivalent
```
git log refA..refB
git log ^refA refB
git log refB --not refA
```

### Triple dot
Use `...` to specify commits rechable by either, but not both
```
git log master...experiment
```

### Interactive staging
To start the interactive mode use `git add -i`
__Staging patches__: Use the `p` option to add only certain parts of a file
while in interactive mode. The same script can be directly started using `git
add -p` or `git add --patch`.

## Stashing
_Stashing_ takes the dirty state of your working directory and saves it on a
stack of unfinished changes that you can reapply at any time

To push a new stash onto your stack, run `git stash` or `git stash save`

To see which changes you've stores use `git stash list`

To apply a stash, use `git stash apply <name>`

Stashes can be applied across branches

Use `git stash drop <name>` to remove a stash from the stack

To apply both changes from a stash and staged changes, use `git stash apply
--index`

Use `git stash add --keep-index` to not stash anything that has already been
staged.

Use `git stash -u` or `git stash --include-untracked` to also stash any
untracked files created

Use `git stash --patch` to interactively select which changes to stash

Use `git stash branch <name>` to create a new branch, checkout the commit when
the changes were stashed, reapply changes in the new branch and the drop the
stash if the application was successful

To remove all changes from the working directory, use `git clean`

## Searching
Use `git grep` to search for a particular string through the files in the
working directory (across trees)

A few options for `git grep` are
|		Option		|			Description			|
|		------		|			-----------			|
|		`-n`		| 		Print line numbers		|
|	`--count`		| 	Show file - count	pairs	|
|	`-p`			|	Show function in which matched |
|	`--and`			| Multiple matches in the same line |

### Git Log Searching
To find __when__ a term existed/was introduced, use `git log -S[term]`

Similarly, use `git log -G[regexp]` to search using regular expressions

To find the history of a function or line of code in the codebase, use `git log
-L :[functionName]:[fileName]`. [functionName] can also be a regex

## Rewriting history
### Changing the Last Commit
To change the last commit message, use
```
git commit --amend
```
Amending changes the SHA-1 of the commit

To add changes to the previous commit, stage new changes and then amend

### Changing multiple commit messages
The rebase tool can be used to rebase a series of commits onto the HEAD they
were originally based on instead of moving them to another one.

Use `-i` to run rebase interactively

The parent of the last commit to be modified must be passed as a parameter in
interactive rebasing, ex:`git rebase -i HEAD~3` (fourth commit backwards)

### Filter branch
Filter branch is used to rewrite huge swaths of history

To remove a file from entire history, run
```
git filter-branch --tree-filter 'rm -f passwords.txt' HEAD
```
The `--tree-filter` option runs the specified command after each checkout of the
project and recommits the results

#### Making a subdirectory the new root
Use
```
git filter-branch --subdirectory-filter <subdir name> HEAD
```
## Reset
__The HEAD__: The HEAD is the pointer to the current branch reference which is
in turn a pointer to the last commit made on that branch. It is the snapshot of
the last commit.

__The INDEX__: The index is the proposed next commit, a.k.a "Staging Area"

__The Working Directory__: The Working Directory unpacks the efficiently
compressed files from the `.git` directory into actual files and acts as a
sandbox where you can try out changes before committing.

### The Role of Reset
1. __Step 1: Move HEAD__ - The first thing `reset` will do is move what HEAD
  points to. (stops here if `--soft` specified)
2. __Step 2: Updating the INDEX__ - The next thing `reset` will do is update the
   Index with the contents of whatever snapshot HEAD points to. (stops here if
`--hard` not specified)
3. __Step 3: Updating the Working Directory__ - The third thing that `reset`
   will do is to make the Working Directory look like the Index. It will
   continue to this stage only if you use the `--hard` option.

### Checkout vs reset
`git --reset hard [branch]` updates all three trees. However, `git --checkout
[branch]` tries to do a trivial merge in the Working Directory, so all of the
files that haven't changed in will be updated. Another difference is that
`reset` move the branch that HEAD points to, `checkout` will move HEAD itself to
point to another branch

# Advanced Merging
## Aborting a merge
To back out of a merge, use `git merge --abort`. It reverts back to the state
before the merge was ran.
## Ignoring whitespace
To ignore whitespace __completely__ use the `-Xignore-all-space` option. To
treat sequenecs of one or more whitespace characters as equivalent, use
`-Xignore-space-change`
## Undoing merges
To make a new commit which undoes all the changes from an existing one, use `git
revert [commit]`
