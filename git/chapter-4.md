A remote repository is a *bare repository* - a Git repository with no working
directory. It only has the contents of the project's `.git` directory and
nothing else.

Major protocols used are
* Local
* HTTP
* SSH
* Git

## Local protocol
In the *Local protocol*, the remote repository is in another directory on disk.

To clone a local repository, use
```
git clone /path/to/project.git
```
or
```
git clone file:///path/to/project.git
```
In the latter, git uses processes that it normally uses to transfer data over a
network.

To add a local repository, use
```
git remote add [short-name] /path/to/project.git
```
### Pros
* They're simple and use existing file permissions and network access.
* Fast
### Cons
* Shared access is more difficult to set up and reach from multiple locations
  than basic network access.
* No protection against accidental damage.
* Slower over a network compared to SSH etc.

## HTTP
These are of two kinds
* Smart HTTP (Git version > 1.6.6)
* Dumb HTTP (Git version < 1.6.6)

The smart HTTP protocol operates like SSH or Git protocol but runs over standard
HTTP/S ports and can use various HTTP authentication mechanisms.

If the server does not respond with a Git HTTP smart service, the Git client
will try to fallback to the simpler dumb HTTP protocol. It expects the bare Git
repository to be served like normal files from the web server.

To allow read access to a repository ovef HTTP, do something like
```
cd /var/www/htdocs
git clone --bare /path/to/git_project gitproject.git
cd gitproject.git
mv hooks/post-update.sample hooks/post-update
chmod a+x hooks/post-update
```

### Pros
* Having single URL for all types of access
* Authentication via username and password
* Fast and efficient
* Can serve repositories read-only, encryption, SSL, firewalls

### Cons
* Tricky to set up

## SSH protocol
To clone a Git repository over SSH, use
```
git clone ssh://user@server/project.git
```
Alternatively, use scp-like syntax
```
git clone user@server:project.git
```
If user is not specified, currently logged in user is taken

### Pros
* SSH relatively easy to set up
* SSH daemons are commonplace
* SSH access is secure - encryption + authentication
* Efficient
### Cons
* Cannot serve anonymous access

## Git protocol
This is a special daemon that comes packaged with Git; it listens to a dedicated
port (9418) that provides a service similar to SSH, but with no authentication.

### Pros
* Fastest network transfer protocol available
### Cons
* No authentication

To initially set up any Git server, an existing repository must be exported into
a new bare repository - a repository without the working directory. To do this,
```
git clone --bare [project-name] [project-name].git
```
Next, copy the bare repo onto the server.

#TODO: Rest of chapter-4
