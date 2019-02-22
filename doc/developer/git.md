% Contributing changes with Git

We are excited to see what you will contribute!  This document describes how to download the bali-phy sources and contribute changes.

## Get the source code

1. Click the "Fork" button on [https://github.com/bredelings/BAli-Phy/](https://github.com/bredelings/BAli-Phy) to create a version of the code under your own account.

1. Download your version of the code:
   ``` sh
   git clone git@github.com:your-username/BAli-Phy.git
   ```
   The name `origin` in your local repo will then refer to your modified version of BAli-Phy.

1. Create a new remote called `upstream` to refer to the upstream version:
   ``` sh
   cd BAli-Phy/
   git remote add upstream https://github.com:bredelings/BAli-Phy.git
   git remote -v
   ```

## Contribute a change

1. Create a new branch to contain your changes.
   ``` sh
   git fetch upstream
   git branch my-new-feature-branch upstream/master
   git checkout my-new-feature-branch
   ```

1. Make changes on this branch and push them to github.
   ``` sh
   git checkout my-new-feature-branch
   git push origin my-new-feature-branch
   ```

1. Send a [pull request](https://help.github.com/articles/about-pull-requests/) through github:
   1. Go to [https://github.com/bredelings/BAli-Phy] and click `New pull request`
   1. Click on `compare across forks`
   1. The `head repository` should be `your-username/BAli-Phy`
   1. Select `compare:my-new-feature-branch`
   1. Add a short message and then click `Create pull request`.

Tests will run automatically on the proposed changes, and we will review the changes.  When everything is ready, changes will be merged to the master branch.


