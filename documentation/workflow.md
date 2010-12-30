# Workflow

## Features

Features are isolated pieces of functionality (or code in general). They are best represented in Git as a seperate branch. While developing a feature we are allowed to merge the master into the feature branch, but we are NOT allowed to merge our feature branch into the master branch unless we are done with our feature.

Let's create our first feature branch:

    $ git branch
    * master
    # start working on a new feature
    $ git feature open add_projects_to_the_cms
    $ git branch
      master
    * features/add_projects_to_the_cms

Now you can work on your feature in isolation. Wen you are done you will want to merge your feature with the master.

    # merge a feature with the master branch
    $ git feature merge add_projects_to_the_cms
    $ git branch
    * master

Sometimes it can take a longer time before you are able to finish a feature. In this case you will want to push a feature branch to a remote.

    # push a feature
    $ git push origin features/add_projects_to_the_cms
    # as you can see this is just regular git.

And then we micht want to open this feature on a different machine.

    $ git branch -a
    * master
      origin/master
      origin/features/add_projects_to_the_cms
    # open an existing feature branch
    $ git feature open add_projects_to_the_cms
    $ git branch
      master
    * features/add_projects_to_the_cms

## Hotfixes (or patches)

Hotfixes are small changes to the code (usualy a single commit) that need to be applied to all the branches that come (git-wise) after a given commit.

Let's make a hotfix

    $ git hotfix fix_that_critical_bug_in_the_login_system 33092b5
    $ git branch
      master
    * hotfix/fix_that_critical_bug_in_the_login_system
    # work on your hotfix
    $ git hotfix apply

