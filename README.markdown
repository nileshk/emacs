Emacs Configuration
===================

Description
-----------

This is my Emacs configuration files (currently only a subset of
them).  This configuration is fairly personalized, so I wouldn't
recommend it as a drop-in set of configuration files.  But some of the
modes and functions might be useful, and the actual configuration may
be useful as an example.
    
Organization
------------

* `startup.el` - The main configuration file which I load from my .emacs
* `/nileshk` - This folder contains modes, functions, etc that I have
  mostly written myself or 3rd party packages which I have modified
* `/snippets` - YASnippet template files
* `/support` - Misc scripts and other files that support Emacs config
* `/vendor` - This folder contains unmodified 3rd party packages

Setup
-----

Create an ~/.emacs file and add the following to it (with the correct paths):

    (load "/Users/username/Emacs/startup-packages.el")
    (load "/Users/username/Emacs/startup.el")

* Emacs configuration will not work the first time and will result in
errors because packages need to be installed via package-install.
* Execute `M-x update-packages` to install list of specified packages
using package-install.
* Restart Emacs and it should load properly.

* `init` - bash script for initializing dependencies (fetching files
  needed)
* `update` - bash script for updating dependencies (fetching files
  needed)
