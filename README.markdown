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

* `init` - bash script for initializing dependencies (fetching files
  needed)
* `update` - bash script for updating dependencies (fetching files
  needed)
