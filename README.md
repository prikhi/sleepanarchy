# Sleepanarchy.com

This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* General
    * Request Logging
    * Env type for SQL & HTTP logging, Secure cookie setting
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
    * Passthrough public endpoint to filesystem
    * Admin: Get folders / files
    * Admin: Create folder
    * Admin: Upload file
* Sitemap


### CLIENT

* Navigation
* Structured API requests & decoding
* Styling
* Blog post details
* Blog Sidebar(recent + archive + feeds?)
* Link Dump page + redirect
* Footer
* Admin site(add/edit/delete posts, add/edit/delete links&categories)


## DEPLOY

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
