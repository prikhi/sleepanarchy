# Local management tasks
local-up:
	docker compose up -d

local-migrate:
	docker compose exec api sql-migrate up -env production

local-api-mgmt:
	docker compose exec api sleepanarchy-api-management ${CMD}

docker-build:
	docker compose build

docker-tag: docker-images
	docker tag lysergia/sleepanarchy-nginx:latest lysergia/sleepanarchy-nginx:$(shell git rev-parse HEAD)
	docker push lysergia/sleepanarchy-nginx:$(shell git rev-parse HEAD)
	docker tag lysergia/sleepanarchy-api:latest lysergia/sleepanarchy-api:$(shell git rev-parse HEAD)
	docker push lysergia/sleepanarchy-api:$(shell git rev-parse HEAD)

# Remote deployment tasks - relies on `blog` entry in `.ssh/config`.

# Pull newest images on remote server
remote-pull:
	docker -H ssh://blog compose pull

# Start/update remote containers
remote-deploy:
	docker -H ssh://blog compose pull
	docker -H ssh://blog compose --env-file env.production.sh up -d

# Stop the entire remote cluster
remote-stop:
	docker -H ssh://blog compose stop

# Watch the logs in the remote cluster
remote-logs:
	docker -H ssh://blog compose logs -f

# Cleanup disk space by removing unused docker bits
remote-clean:
	docker -H ssh://blog system prune -f

remote-api-mgmt:
	docker -H ssh://blog compose --env-file env.production.sh exec api sleepanarchy-api-management ${CMD}

remote-psql:
	docker -H ssh://blog compose --env-file env.production.sh exec db psql -U sleepanarchy-blog sleepanarchy-blog

# Migrate the remote database
remote-migrate:
	docker -H ssh://blog compose --env-file env.production.sh run api sql-migrate up -env production

# Bootstrap the certbot setup
remote-bootstrap-certs:
	docker -H ssh://blog compose --env-file env.production.sh -f docker-compose.yml -f deploy/docker-compose.bootstrap.yml up -d

# Migrate the old database to the new one
remote-migrate-old-db:
	# copy data & script
	docker -H ssh://blog compose cp blog-dump.psql db:/
	docker -H ssh://blog compose cp deploy/migrate-old-db.sh db:/
	# run script
	docker -H ssh://blog compose exec db su - postgres /migrate-old-db.sh
	# cleanup
	docker -H ssh://blog compose exec db rm /blog-dump.psql
	docker -H ssh://blog compose exec db rm /migrate-old-db.sh
	# regenerate html & flush cache via restart
	docker -H ssh://blog compose --env-file env.production.sh exec api sleepanarchy-api-management regenerate-html
	docker -H ssh://blog compose --env-file env.production.sh restart api

# Reload the nginx configuration
remote-reload-nginx:
	docker -H ssh://blog compose --env-file env.production.sh exec nginx nginx -s reload
