# Basics
For every commit, Git takes a picture of all files at that moment and stores a
reference to that snapshot. Git thinks (not really) about its data more like a
stream of snapshots.

Everything in Git is check-summed before it is stored and is then referred to by
that checksum. Git uses SHA-1 for checksumming.

Git has three main states that a file can reside in: committed, modified, and
staged. Committed means that the data is safely stored in the local database.
Modified means that the file has been changed but not committed to the local
database yet. Staged means that a modified file has been marked in its current
version to go into the next commit snapshot.

The Git directory is where metadata and object database for the project are
stored. The working directory is a single checkout of one version of the
project. The staging area is a file that stores information about what will go
into the next commit.
