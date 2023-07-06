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

RUN rm /etc/nginx/conf.d/default.conf

COPY /deploy/nginx/nginx.conf /etc/nginx/conf.d/sleepanarchy.conf
COPY --from=builder /dist/* /www/
