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

# TODO: use docker -H ssh://... compose for remote mgmt
