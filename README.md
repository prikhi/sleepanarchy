# SleepAnarchy.com

[![SleepAnarchy.com CI Build Status](https://github.com/prikhi/sleepanarchy/actions/workflows/main.yml/badge.svg?branch=master)](https://github.com/prikhi/sleepanarchy/actions/workflows/main.yml)


This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

It is currently in active development & not live on the website - the current
website is dead due to the deprecation of Python 2. To view the old Python /
Django / Mezzanien site, see the `0.1.0-mezzanine-last` tag.


## Develop

Create database:

```sh
createuser -DRS sleepanarchy-blog
createdb sleepanarchy-blog -O sleepanarchy-blog
````

Install `sql-migrate` & migrate the database:

```sh
export PATH="${HOME}/.local/bin:${PATH}"
GOBIN="${HOME}/.local/bin" go install -v github.com/rubenv/sql-migrate/...@v1.1.2
cd sleepanarchy-api && sql-migrate up
```

Run the API server:

```sh
cd sleepanarchy-api
stack run sleepanarchy-api
```

In another terminal, start the client & API proxy server:

```sh
cd sleepanarchy-website
nvm use
npm i
npm run watch
```

The website should now be browseable at https://localhost:8080/. If you want to
hit the API, you can make requests to https://localhost:8080/api/.

The API serves it's own documentation as markdown. You can build & view it like
so:

```sh
echo '<link href="https://gitcdn.link/cdn/develephant/mkdocs-codehilite-themes/master/css/monokai.css" rel="stylesheet" />' > docs.html
curl localhost:9001/docs | markdown_py -x codehilite -x extra >> docs.html
python -m http.server 8000 &
firefox http://localhost:8000/docs.html
```

The API server has handler tests you can run:

```sh
cd sleepanarchy-api
createdb sleepanarchy-blog-test -O sleepanarchy-blog
stack test --fast
```


## TODO

Currently in the middle of migrating from Python/Django/Mezzanine. Once this
list has been trimmed down a bit, we'll actually migrate & deploy the site.

### SERVER

* General
    * Haskell `manage.hs` script for running server & client watchers in one
      terminal(see SESE website repo).
    * Migration script from old django DB to new DB
* Blog Posts
    * Paginated list route?
    * Prev/Next links on details page
    * RSS / ATOM feeds?
    * Re-evaluate description auto-generation(render as markdown & then take
      first paragraph? Never auto-gen?)
* Link Dump
    * Sidebar data? Has "recent links", "top viewed links", "top categories",
      "tag cloud". But do we actually care? Maybe later?
    * Admin: Create category
    * Admin: Add Link
    * Admin: Delete Link
* Pastes
    * Decide to keep or ditch this? We link to a few pastes in our posts...
* Media
    * Optimize PNG, GIF, & JPEGs on upload?
    * Delete files


### CLIENT

* General
    * 404 page component - use to set title/description/prerender-code
    * Loading spinner(fade-in?)
    * Scroll to element in hash on initial load(only required on BlogPostView)
    * Scroll to top of page or to hash on page/url change.
    * Set prerender.io meta element after page data load.
    * Helpers for `pageDataReceived` on init when component has no `apiData`
      field or no action for `initialize`.
    * Throw my github/gitlab/linkedin in footer as icons
* Blog Posts
    * published date formatting(X days ago)
    * Add feeds to sidebar? Or footer? Or both?
    * 404 page
* Admin site
    * Add icons to sidebar items
    * Collapseable sidebar
    * Standardize styles
    * Forms
        * validation
        * take aribtrary optional properties? for custom `type` or
          `autocomplete`
        * use `name` property
        * convert args into config record
    * Preview markdown fields (tabbed view? live side-by-side update?)
    * (soft?) delete posts, w/ confirm
    * list/add/edit/delete blog categories
    * list/add/edit/delete links & categories
    * media: delete files
    * media: folder icon, icons for files based on mimetype


### DEPLOY

* Dockerize server + prerender + nginx into containers
    * ~~CI builds images on release/tagged commits, pushes to dockerhub~~
    * docker compose instruments services
    * ~~nginx only thing exposed to outside world~~
    * ~~nginx proxies API server & shares media directory with it~~
    * ~~nginx serves frontend prod builds~~
    * nginx uses certbot for SSL certs
    * nginx uses prerender for server side rendering
    * ~~api server has postgres~~
    * ~~use api server container to run db migrations, mgmt commands~~
    * commands to remotely update prod via SSH
    * keep server JWT key secret
    * support DB password auth - in backend code & compose file
    * health commands for docker


## LICENSE

GPL 3.0+
