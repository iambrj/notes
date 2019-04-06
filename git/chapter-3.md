Git stores data as a series of snapshots.

Each commit has a pointer to its previou(s) commits.

A branch is simply a lightweight movable pointer to one of these commits. Every
commit moves this pointer forward.

To create a new branch, use
```
git branch [branch-name]
```
This creates a pointer to the same commit you're currently on.

Git keeps a special pointer called HEAD which points to the current branch.

To switch branches, use
```
git checkout [branch-name]
```

To create a branch and switch to it at the same time, use
```
git checkout -b [branch-name]
```

Git does not allow changing branches with uncommitted changes.

If one branch can be reached by following the history of another branch, the
first branch can be merged using a "fast-forward" merge
```
git merge [forward-branch]
```

To delete a branch, use
```
git branch -d [branch-name]
```

Merge conflicts occur when same part of a file has different contents in
different branches. All merge conflicts must be resolved manually. Staging a
file indicates that merge conflicts in that file have been resolved.

Use
```
git mergetool
```
to fire up a visual merge tool and walks through the conflicts

To list all current branches, use
```
git branch
```

To get remote references, including branches, tags and so on, use
```
git remote show
```

Remote references take the form `[remote]/[branch]`

To synchronize work, run
```
git fetch origin
```
This command looks up which server "origin" is, fetches any new data from it
and updates local database.

To push local work onto a remote, use
```
git push [remote] [branch]
```

To create a tracking/upstream branch corresponding to a remote branch, use
```
git checkout -b [local-branch] [remote-name]/[remote-branch-name]
```
The shorthand for this is
```
git checkout --track [remote]/[branch]
```
An even shorterhand is
```
git checkout [branch]
```
To explicity set a local branch to a remote, use
```
git branch -u(--set-upstream-to) [remote]/[branch]
```

If a tracking branch has been set up, it can be referenced using @{upstream} or
@{u} shorthand.

To list all tracking branches, use
```
git branch -vv
```
this only reports things since last fetch, to get all up to date numbers, use
```
git fetch -- all; git branch -vv
```
Note that `git fetch` does not actually modify the working directory - it will
fetch down all the changes on the server (metadata). Use `git pull` to fetch all
the data from current upstreaming branch. It usually does a `git fetch` followed
by a `git merge`. It is always better to explicitly `fetch` and `merge` rather 
than rely on `pull`.

To delete a remote branch, use
```
git push [remote] --delete [branch]
```

Using rebasing, all changes committed on one branch can be replayed onto another
```
git checkout [changed-branch]
git rebase [rabase-onto-branch]
```

To avoid `checkout` on each rebase, use
```
git rebase [basebranch] [topicbranch]
```
This rebases all changes in [topicbranch] onto [basebranch]
