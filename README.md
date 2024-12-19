# SARS-CoV-2 Variant Nowcast Hub Dashboard Repository

## How to generate and add a new report

Each report will need a markdown page as in `pages/2024-12-04-report.md` that will serve as the shell
page for the report with an include that points to a file in `pages/includes`, 
such as `pages/includes/2024-12-04-report.html`. To make a new report that is its own page,
you need to follow these (currently manual) steps:

1. Update the reference date in the `src/report.Rmd` file and render the file as an html fragment.
2. Copy the rendered html fragment in `src/report.html` to `pages/includes/YYYY-MM-DD-report.html`, 
where `YYYY-MM-DD` is the reference date.
3. Copy an existing markdown page such as `pages/2024-12-04-report.md` to 
`pages/YYYY-MM-DD-report.md` and point the included
file in that document to the html fragment from step 2.
4. Edit the `site-config.yml` to add a new `text` and `href` field at the top of the `menu:` header,
following the format of the other entries that are there, for the `YYYY-MM-DD` that you have built.
5. Test out the build locally. (See instructions below)
6. If the site looks good, submit a PR to the repo and this should trigger a rebuild of the site. 

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
