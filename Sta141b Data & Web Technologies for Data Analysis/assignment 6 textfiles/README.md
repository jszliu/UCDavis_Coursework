# Assignment 6


Instructions

1. Accept the assignment via the link: [https://classroom.github.com/a/hU83-8B5]
1. A repo containing your username `xxxx-assignment-n-USERNAME` will be created.<br>
    Due to an ongoing technical reason, the assignment will be created at https://github.com/ucdavis-sta-assignments, not the class repo https://github.com/ucdavis-sta141b-sq-2020
1. Create a new RStudio project with the repo containing your username  (do not clone the public repo `assignment-n`)
1. Edit the corresponding R Markdown files
1. **Important** Knit the R Markdown files as html documents. Submission without the html files will score 0 points.
1. Commit the changes, including the html file
1. Push the changes to the `master` branch of the repo.
1. Double check if the changes (and the html files) are pushed to your remote repo. The file may not be viewable on GitHub directly, don't worry about it.


Visit https://happygitwithr.com/ for tutorials.


Points: 12.5% of the course grade.
Due Date: 6/10/2020 11:59 pm



### How to fetch assignment revisions

First of all, the easiest way is to copy and paste the revised content to your assignment.

But however, if you want to learn a bit more about git commands. It is what you need

```sh
# commit all changes first
git remote add upstream https://github.com/ucdavis-sta141b-sq-2020/assignment-x.git
git fetch upstream
git merge upstream/master --allow-unrelated-histories
# then resolve any potential conflicts and commit the resolved conflict
```
