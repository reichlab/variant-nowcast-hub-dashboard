# SARS-CoV-2 Variant Nowcast Hub Dashboard Repository

## How to generate and add a new report

Each report will need a page as in `pages/report.md` that will serve as the shell
page for the report with an include that points to a file in `pages/includes`, 
such as `pages/includes/report.html`. To make a new report that is its own page,
you need to follow these (currently manual) steps:

1. Update the reference date in the `src/report.Rmd` file and render the file as an html fragment.
2. Copy the rendered html fragment in `src/report.html` to `pages/includes/YYYY-MM-DD-report.html`. 
[Note: we need to establish a standard date naming scheme.]
3. Copy the `pages/report.md` to `pages/YYYY-MM-DD-report.md` and point the included
file in that document to the html fragment from step 2.
4. Test out the build locally. 
5. If the site looks good, submit a PR to the repo and this should trigger a rebuild of the site. 

## Building the site locally

You might want to build the site locally to test out whether the changes you made 
have worked. You will need to have Docker installed to do this.

1. Run the following command in a terminal to install the site builder tool:
```
docker pull ghcr.io/hubverse-org/hub-dash-site-builder:main
```
2. Navigate to the root directory of this repo in a terminal window.
3. Run the following command in a terminal window:
```
docker run --platform=linux/amd64 --rm -ti -v "$(pwd)":"/site" ghcr.io/hubverse-org/hub-dash-site-builder:main bash render.sh
```
4. Open the file `pages/_site/index.html` in a web browser.

## How to trigger a rebuild of the site on GitHub pages

Any new commit will cause the website to rebuild. If you don't have a commit to make
you can open a new issue and then 
[add a new comment on an issue](https://github.com/reichlab/flusight-dashboard/issues/6#issuecomment-2504916376) 
with the words 
```
/hub build site
```

Further instructions about how the hubdashboard app works can be found at 
[the app website](https://github.com/apps/hubdashboard).

Build progress can be monitored at [the hub-dashboard-control-room Actions page](https://github.com/hubverse-org/hub-dashboard-control-room/actions).
