# Sleepanarchy.com

This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* Blog Posts
    * Paginated list route
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

* Navigation - href+onclick link helper
* Styling - infra in place, need to actually write styles now
* Blog Post published date formatting
* Blog Sidebar(recent + archive + feeds?)
* Link Dump page + redirect
* Favicon
* SEO, structured meta-data
    * On index.html
    * Update on page changes
* robots.txt
* Admin site
    * add/edit/delete posts
    * add/edit/delete links & categories
    * browse media directory, create folders, upload & delete files


## DEPLOY

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
