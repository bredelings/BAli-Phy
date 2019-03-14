% Contributing changes with Git

This document describes how to "fork" the BAli-Phy repository and submit a "pull request"
in order to contribute changes.

If instead you just want to get the source code, then you should ignore this tutorial
and just "clone" the repository.

## Fork and download the source code 

1. Click the "Fork" button on [https://github.com/bredelings/BAli-Phy/](https://github.com/bredelings/BAli-Phy) to create a version of the code under your own account.  (You will need a github account for this.)

1. Download your version of the code:
   ``` sh
   git clone git@github.com:your-username/BAli-Phy.git
   ```
   The name `origin` in your local repo will then refer to your modified version of BAli-Phy.

1. Create a new remote called `upstream` to refer to the upstream version:
   ``` sh
   cd BAli-Phy/
   git remote add upstream https://github.com/bredelings/BAli-Phy.git
   git remote -v
   git fetch upstream
   ```
1. Make the `master` branch track upstream changes.
   ``` sh
   git checkout master
   git branch --set-upstream-to upstream/master
   ```
   This is optional, but recommended.  The plan is that you will use the `master` branch to
   track the upstream source, and the make other branches to hold your changes.

## Make changes on a branch

1. Create a new branch to contain your changes.  In this example the branch is called `my-new-feature-branch`,
   but you should pick a name that describes the changes you are making.
   ``` sh
   git fetch upstream
   git branch my-new-feature-branch upstream/master
   git checkout my-new-feature-branch
   ```

1. Edit files and commit changes.

1. Publish your changes to to github.
   ``` sh
   git checkout my-new-feature-branch
   git push origin my-new-feature-branch
   ```

## Contribute changes to the BAli-Phy project
1. Send a [pull request](https://help.github.com/articles/about-pull-requests/) through github:
   1. Go to [https://github.com/bredelings/BAli-Phy](https://github.com/bredelings/BAli-Phy) and click `New pull request`
   1. Click `compare across forks`.
   1. Select `your-username/BAli-Phy` for the _head repository_.
   1. Select `compare:my-new-feature-branch`
   1. Write a title and a short message.
   1. Click `Create pull request`.

Tests will run automatically on the proposed changes, and we will review the changes.  When everything is ready, changes will be merged to the master branch.


