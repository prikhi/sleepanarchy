# syntax=docker/dockerfile:1

# Build sql-migrate
FROM golang:1-buster AS sql-migrate-builder

RUN GOBIN=$HOME/.local/bin go install github.com/rubenv/sql-migrate/...@latest
RUN mkdir -p /dist
RUN mv $HOME/.local/bin/sql-migrate /dist/sql-migrate


# Build project dependencies
FROM haskell:9.4.5-buster AS dependency-builder

WORKDIR /src

RUN apt-get update && apt-get install -yqq libpq-dev

COPY ./sleepanarchy-api/stack.yaml ./sleepanarchy-api/stack.yaml.lock \
     ./sleepanarchy-api/package.yaml /src/
RUN stack build --system-ghc --no-install-ghc --dependencies-only


# Build project
FROM haskell:9.4.5-buster AS project-builder

WORKDIR /src

COPY --from=dependency-builder /root/.stack /root/.stack
COPY --from=dependency-builder /src/.stack-work/ /src/.stack-work/

RUN apt-get update && apt-get install -yqq libpq-dev

COPY ./sleepanarchy-api /src
RUN stack install --system-ghc --no-install-ghc

RUN mkdir -p /dist
RUN mv $HOME/.local/bin/* /dist/


# Final minimal image
FROM debian:buster-slim

COPY --from=sql-migrate-builder /dist/sql-migrate /bin/
COPY --from=project-builder /dist/* /bin/

RUN apt-get update && apt-get install -yqq libpq-dev curl

HEALTHCHECK --interval=30s --timeout=5s --start-period=1s --retries=2 CMD [ "curl", "http://localhost:9001/blog/posts" ]

WORKDIR /app
COPY ./sleepanarchy-api/dbconfig.yml .
COPY ./sleepanarchy-api/migrations/ ./migrations/

CMD ["sh", "-c", "sleep 5 && /bin/sleepanarchy-api"]
