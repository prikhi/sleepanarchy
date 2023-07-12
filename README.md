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

## Deploy

### Initial Deployment

We provisioned a $6/mo droplet on DigitalOcean for the webapp. The hostname is
set to `blog.sleepanarchy.com`(for the `PTR` record DO will create) and we
pointed the domain to it's IP. We created a user, installed docker &
docker-compose, enabled the docker service, & locked down the droplet.

Locally, we have a `Host blog ...` entry in our `.ssh/config` that lets us run
`ssh blog` to connect to the droplet. The `remote-` commands in the `Makefile`
use this so we don't have to hardcode our username, ssh key, hostname, etc.

We need an environment file for docker to pass our secrets to the remote host's
containers. In the `sleepanarchy-api/` folder run `stack run
sleepanarchy-api-management generate-jwk` and paste the resulting JSON into a
`env.production.sh` file under the `API_JWK=<key-string-here>` variable name.

We need to do something a little different during the initial deployment of the
webapp. Our custom nginx docker container contains a throwaway self-signed cert
to bootstrap the certificate generation from `certbot`.

We perform this bootstrapping with `make remote-bootstrap-certs`. This will
create our networks, volumes, & containers on the remote host. It uses the
`deploy/docker-compose.bootstrap.yml` override file to do the initial database
migration & the `certbot` certificate generation.

You can monitor the progress of certificate generation by running `make
remote-logs`. After the `sleepanarchy-certbot-1` container exits successfully,
you should reload the nginx server with `make remote-reload-nginx` to make it
pick up the new certificates.

Now you should be able to visit https://sleepanarchy.com without seeing the
self-signed certificate warning.

Next we'll need to migrate our old django database into our new one. The
makefile will again make this very straightforward:

```sh
# Add a leading space to prevent adding the shell history
 CMD="create-user <my-user> <my-pass>" make remote-api-mgmt
cp ~/blog-dump.psql .
make remote-migrate-old-db
```

You should now see all the old posts & links on the new site & be able to sign
in to the admin.


## TODO

Currently in the middle of migrating from Python/Django/Mezzanine. Once this
list has been trimmed down a bit, we'll actually migrate & deploy the site.

### SERVER

* General
    * Haskell `manage.hs` script for running server & client watchers in one
      terminal(see SESE website repo).
    * Migration script from old django DB to new DB
    * Clean shutdowns using signals
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
    * Helpers for `pageDataReceived` on init when component has no `apiData`
      field or no action for `initialize`.
    * Throw my github/gitlab/linkedin in footer as icons
* Blog Posts
    * published date formatting(X days ago)
    * Add feeds to sidebar? Or footer? Or both?
    * 404 page(or redirect to `NotFound` route when api req returns 404)
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

* Support DB password auth - in backend code & compose file
* Allow cycling API server w/o restarting nginx. This occurs because of the
  `depends_on` from nginx to api. We need this right now because nginx fails to
  start if it can't reach the api or prerender hosts. Maybe a way around this
  is using `set` for the hostnames in nginx, but my first attempt caused this
  to throw "not initialized" errors.
    * I fixed the `set` directives so they work properly, but need to test
      removing `depends_on` and see if nginx stays up with no `api` service
* Remove `tmp` subdomain from certbot bootstrap. Was added since we got
  rate-limited for a week on 7/11 by making too many valid certs while testing.
* Tag docker images with `latest` only for builds on `master` branch.
* Tag docker images with tag name for release/tag builds.
* Implement SSL certificate renewals.
    * Move certbot to nginx container & add cronjob?
    * Make custom certbot container & entrypoint script that continuously
      renews & sleeps for 24hrs. Add nginx cronjob to reload server daily?
    * Make cronjobs on host machine that uses `docker-compose run`?
* Use `--start-interval=1s` & expand `--start-period` flags for `HEALTHCHECK`
  commands once host is on docker v25+.


## LICENSE

GPL 3.0+
