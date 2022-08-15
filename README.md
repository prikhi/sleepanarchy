# Sleepanarchy.com

This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* General
    * DB migration files & mgmt(e.g., soda)
    * Sitemap
* Blog Posts
    * Paginated list route
    * Cache sidebar data - bust/regen on post create/edit
    * Archive post list(posts within some @YYYY-MM@)
    * Categories & Tags(maybe just tags?) post lists
    * Prev/Next links on details page
    * Admin: Get/Update post routes
        * Re-evaluate description auto-generation(take first paragraph & render
          markdown?)
    * Admin: Publish post route?
* Link Dump
    * DB Model
    * Fetch Route
    * Admin: Create category
    * Admin: Add Link
* Pastes
    * Decide to keep or ditch this
* Media
    * DB Model(folders + files)
    * Passthrough public endpoint to filesystem(Development env only)
    * Admin: Get folders / files
    * Admin: Create folder
    * Admin: Upload file


### CLIENT

* General
    * Favicon
    * SEO, structured meta-data
        * On index.html
        * Update on page changes
    * robots.txt
    * unify json & http errors instead of coercing to string
    * switch api data from `Maybe (Either ...)` to `RemoteData`
    * standardize rendering of notasked/loading/error/success api data
    * store & render previous page on page change, delay rendering of loading
      state by some ms to prevent quick flash
* Styling - check post styling after adding markdown rendering
* Blog Posts
    * render post bodies as markdown
    * published date formatting(X days ago)
    * Sidebar(recent + archive + feeds?)
* Link Dump page + redirect
* Admin site
    * add/edit/delete posts
    * add/edit/delete links & categories
    * browse media directory, create folders, upload & delete files


## DEPLOY

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
