# Workflows
* Centralized Workflow: One central hub, everyone synchronizes their work to it
* Integration-Manager Workflow: "fork, change and merge" - GitHub
* Dictator and Lieutenants: Heirarchial - Linux kernel

# Small private team
Each developer clones repo, makes changes, merges with master for any other
changes occurred simultaneously and then pushes local changes
```
git clone https://github.com/iambrj/notes
vim notes/git/chapter-6.md
git commit -am "updated notes/git/chapter-6.md"
git fetch origin
git merge origin/master
git push origin master
```

# Private managed team
Each developer creates and works on branches and maintainers merge branches onto
master
```
git clone https://github.com/iambrj
git checkout -b chapter-6
vim notes/git/chapter-6.md
git commit -am "added chapter-6"
git push -u origin chapter-6:chapter-six # push local's chapter-6 to server's chapter-six

```

# Forked public project
Clone, makes changes on topic branch, fork, push changes to fork and pull
request.

Use `git request-pull` to create a summary of changes.

# Public project over Email
Use `git format-patch` to generate mbox-formatted files to email developer
mailing lists requesting merges. Subject is the first line of the commit message
