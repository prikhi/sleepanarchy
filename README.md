# SleepAnarchy.com

[![SleepAnarchy.com CI Build Status](https://github.com/prikhi/sleepanarchy/actions/workflows/main.yml/badge.svg?branch=master)](https://github.com/prikhi/sleepanarchy/actions/workflows/main.yml)


This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* General
    * DB migration files & mgmt(e.g., soda)
        * Create index on blog_post.category_id
    * Sitemap Generation
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
    * Admin: Get/Update post routes
        * Re-evaluate description auto-generation(take first paragraph & render
          markdown?)
    * Admin: Publish post route?
* Link Dump
    * DB Model
    * Fetch Route
    * Admin: Create category
    * Admin: Add Link
    * Admin: Delete Link
* Pastes
    * Decide to keep or ditch this? We link to a few pastes in our posts...
* Media
    * DB Model(folders + files)? Or just introspect filesystem?
    * Passthrough public endpoint to filesystem(Development env only)
    * Admin: Get folders / files
    * Admin: Create folder
    * Admin: Upload file


### CLIENT

* General
    * Page Title, SEO, structured meta-data
        * On index.html for initial load
        * Update on page changes(send effect after api data loads?)
    * robots.txt
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
    * published date formatting(X days ago)
    * Add feeds to sidebar? Or footer? Or both?
    * 404 page
* Link Dump page + redirect
* Admin site
    * login page
    * add/edit/delete posts
    * add/edit/delete links & categories
    * browse media directory, create folders, upload & delete files


## DEPLOY

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
