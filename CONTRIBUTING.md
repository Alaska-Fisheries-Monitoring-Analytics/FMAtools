# CONTRIBUTE

This document describes how to contribute to this project.

-   Great to have you here.
-   You can help make this project better!
-   Thank you for your efforts.

## Code of Conduct

This project and everyone participating in it is governed by the [AFSC
Code of
Conduct](https://sites.google.com/noaa.gov/myafsc/home/about-afsc). By
participating, you are expected to uphold this code.

## Team members

**Lead**: Jason E. Jannot, NOAA Fisheries AFSC FMA Division, Seattle,
WA.  
**Contributors**:

-   Jennifer Cahalan, NOAA Fisheries AFSC FMA Division, Seattle, WA.

-   Craig Faunce, NOAA Fisheries AFSC FMA Division, Seattle, WA.

-   Andy Kingham, NOAA Fisheries AFSC FMA Division, Seattle, WA.

-   Geoff Mayhew, NOAA Fisheries AFSC FMA Division, Seattle, WA.

## Getting Started

-   Make sure you have a GitHub account.
-   Clone the repository from GitHub to your local machine.
-   Questions? email <jason.jannot@noaa.gov>

## Git Workflow for Collaborating

A Git workflow is a recommendation for how to use Git to accomplish work
in a consistent and productive manner. The goals is that the workflow
enhances the effectiveness of the team and does not limit productivity.
A good workflow proactively reduces the number of merge conflicts and
merges that need to be reverted. The choice of workflow by a team should
be a joint decision. Jason’s recommendation is to use the
[GitFlow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow)
workflow because it accomplishes two important, but somewhat competing,
tactics to reduce merge conflicts when collaborating with git:

1.  **Branch life should be minimized** The risk of merge conflicts
    increase in proportion to the time the branch has been separate from
    the main branch. Short-lived branches promote cleaner merges.

2.  **Branches should be tested before merging** Testing a branch before
    merging into the main branch reduces problems. However, accidents
    happen, thus a good workflow allows for easy reverts that don’t
    cause issues for other contributors.

*Having said all that, I welcome all discussions on how to best develop
our Git workflow!* - Jason.

For those interested a comparison of Git workflows can be found
[here](https://www.atlassian.com/git/tutorials/comparing-workflows).

## Making Changes

The following uses the Gitflow method as the workflow.

-   Pull the most recent code.
-   Switch to the “Develop” branch.
-   Create a new branch with a name of the form: `jason/newstuff`.
-   Make your changes.
-   Commit your changes. See the [Git Commit
    Messages](#git-commit-messages) styleguide below.
-   Push your changes to the remote Github repository.
-   Go to Github and create a ‘pull request’. See the [Pull
    Requests](#pull-requests) section below.
-   Assign a reviewer.

## Styleguides

### Git Commit Messages

As a general rule, you should commit when you finish something that
allows your code to work - usually ends up being a couple times an hour.

-   Use the present tense (“Add feature” not “Added feature”)
-   Use the imperative mood (“Move cursor to…” not “Moves cursor to…”)
-   Limit the first line to 72 characters or less
-   Reference issues and pull requests liberally after the first line

### Pull Requests

For general guidelines, please see [Github’s Pull
Request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)
page.

Other things to consider: \* In the message, please include the
following headers: \* Description of the Issue or New Feature \*
Description of What Has Been Done \* Usage \* Examples and/or how others
might test the change \* Assign a Reviewer - this will most likely be
the Merge Master. In the case of the Merge Master, this will be another
appropriate contributor.

### Coding conventions

Start reading our code and you’ll get the hang of it. We optimize for
readability:

-   Write functions. There’s a good chance that your script can be
    simplified into a function. “Everything that happens is a function
    call.” - John Chambers

-   Always put spaces after list items and method parameters (
    1, 2, 3
    , not
    1, 2, 3
    ) and around operators (x + y = 1, not x+y=1).

-   Eliminate unnecessary white space.

-   Use a styler and IDE to keep your code clean.
    [`stylr`](https://styler.r-lib.org/) is a good R package for keeping
    your code tidy and easy to use.

-   `tidyverse` methods, especially those using pipes, `%>%`, increase
    readability and make reviewing code much more pleasant.

-   When in doubt, consult the [`tidyverse` style
    guide](https://style.tidyverse.org/)

This is collaborative software. Consider the people who will read your
code, and make it look nice for them. It’s sort of like driving a car:
Perhaps you love doing donuts when you’re alone, but with passengers the
goal is to make the ride as smooth as possible.

## Reviewing Pull Requests

-   Open the pull request
-   Review the code changes
-   Reviewer - provide comments and feedback in GitHub
-   Originator - respond to comments, perhaps add comments
-   Approve changes (upper right corner) and add approval comment
-   **Merge Master/Code owner merges all pull requests! Please do not
    merge your own pull request.** If the Merge Master is pushing code,
    then the reviewer should be responsible for merging the pull
    request.
-   MergeMaster will delete the branch once the code has been merged.
-   **DONT FORGET TO PULL the new code** to your local instance to get
    latest code.

<!-- # Documentation -->
<!-- This section includes any help you need with the documentation and where it can be found. Code needs explanation, and sometimes those who know the code well have trouble explaining it to someone just getting into it.  -->
<!-- * Help us with documentation here -->

-   If you have further questions, contact: Jason Jannot
    <jason.jannot@noaa.gov>
