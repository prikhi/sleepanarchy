# syntax=docker/dockerfile:1

# Build client files
FROM node:20 AS builder

WORKDIR /src

COPY ./sleepanarchy-website/package*.json /src/
RUN npm ci


COPY ./sleepanarchy-website/ /src/
RUN npm run build

RUN mv dist /

RUN mkdir /ssl

RUN openssl req -x509 -newkey rsa:4096 -keyout /ssl/privkey.pem -out /ssl/fullchain.pem -sha256 -days 3650 -nodes -subj "/C=XX/ST=StateName/L=CityName/O=CompanyName/OU=CompanySectionName/CN=localhost"


# Final minimal image
FROM nginx:1-alpine

RUN apk --no-cache add curl

RUN rm /etc/nginx/conf.d/default.conf

# Self-signed cert, directory will be overriden by certbot if enabled
RUN mkdir -p /etc/nginx/ssl/live/sleepanarchy.com/
COPY --from=builder /ssl/* /etc/nginx/ssl/live/sleepanarchy.com/

COPY /deploy/nginx/nginx.conf /etc/nginx/conf.d/sleepanarchy.conf
COPY --from=builder /dist/* /www/

HEALTHCHECK --interval=30s --timeout=5s --start-period=1s --retries=2 CMD [ "curl", "https://localhost/", "--insecure" ]
