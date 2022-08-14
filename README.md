# Sleepanarchy.com

This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* Blog Posts
    * Paginated list route
    * Categories & Tags(maybe just tags?)
    * Admin: Get/Update post routes
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
* Sitemap


### CLIENT

* General
    * Favicon
    * SEO, structured meta-data
        * On index.html
        * Update on page changes
    * robots.txt
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
