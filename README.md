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
    * Cache sidebar data - bust/regen on post create/edit
    * Prev/Next links on details page
    * Serverside rendering of markdown -> html w/ syntax highlighting?
        * Doing syntax highlighting for _all_ languages add 1MB to the client
          size(300KB -> 1.4MB). We've pared down the languages to a subset to
          keep the size reasonable for now _but_ we could just use pandoc to
          render the markdown w/ pygments & only include the style css.
    * RSS / ATOM feeds?
    * Re-evaluate description auto-generation(render as markdown & then take
      first paragraph? Never auto-gen?)
* Link Dump
    * DB Model
    * Fetch Route
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
    * Page Title, SEO, structured meta-data
        * On index.html for initial load
        * Update on page changes(send effect after api data loads?)
    * switch api data from `Maybe (Either ...)` to `RemoteData`
    * standardize rendering of notasked/loading/error/success api data
    * store & render previous page on page change, delay rendering of loading
      state by some ms to prevent quick flash
        * BaseSite should create & subscribe to an emitter on init. When we get
          a route change message from pushstate, move currentPage to
          previousPage state field, set new currentPage, and fork thread that
          delays by ~50ms & then sends the "still waiting" message via the
          emitter. "Still waiting" should clear the previousPage from the
          state. When rendering, if previousPage exists, render that instead of
          the current page.
        * can we have a wrapper we insert before the `mkEval` call on Page
          components that looks at `apiData` in the page's state & sends a
          message to the BaseSite parent when the field goes from Loading to
          Success or Error? This message should clear the "previous page" so
          that the current page is rendered.
* Blog Posts
    * Markdown rendering for post descriptions in list views
    * published date formatting(X days ago)
    * Add feeds to sidebar? Or footer? Or both?
    * 404 page
* Link Dump
    * root category page
    * sub-category pages?
    * `/l/` shortlinks (simply proxy a server route that returns redirect?)
* Admin site
    * Collapseable sidebar/nav
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

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
