# Atom packages

Sadly, there's no package manifest like Sublime has, so you have to manually deal with packages. Fortunately, there is a sync service of sorts: <https://atom.io> lets you log in and star packages. You can interact with the list of starred packages using `apm` (the Atom Package Manager) and your API token from <https://atom.io/account>.

 -  Use `apm stars` to get a list of starred packages
 -  Use `apm stars --install` to install all starred packages
 -  Use `apm star --installed` to star all currently installed packages
