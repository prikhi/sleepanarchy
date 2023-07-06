# syntax=docker/dockerfile:1

# Build client files
FROM node:20 AS builder

WORKDIR /src

COPY ./sleepanarchy-website/package*.json /src/
RUN npm ci


COPY ./sleepanarchy-website/ /src/
RUN npm run build

RUN mv dist /


# Final minimal image
FROM nginx:1-alpine

RUN apk --no-cache add curl

RUN rm /etc/nginx/conf.d/default.conf

COPY /deploy/nginx/nginx.conf /etc/nginx/conf.d/sleepanarchy.conf
COPY --from=builder /dist/* /www/

HEALTHCHECK --interval=30s --timeout=5s --start-period=1s --retries=2 CMD [ "curl", "http://localhost:80/" ]
