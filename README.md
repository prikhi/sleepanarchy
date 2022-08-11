# Sleepanarchy.com

This repo contains the Haskell backend API server & Purescript client used to
serve [sleepanarchy.com](https://sleepanarchy.com).

## TODO

Currently migrting from Python/Django/Mezzanine.

### SERVER

* General
    * Request Logging
    * Env type for SQL & HTTP logging
    * Authentication (HttpOnly + SameSite + Secure Cookie)
* Blog Posts
    * Paginated list route
    * Admin: Create post route
    * Admin: Publish post route
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

* Everything


## DEPLOY

* Dockerize server + prerender + nginx into containers?


## LICENSE

GPL 3.0+
