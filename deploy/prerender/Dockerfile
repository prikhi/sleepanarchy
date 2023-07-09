# syntax=docker/dockerfile:1

FROM node:20

RUN apt-get update && apt-get install -yqq curl chromium

RUN useradd -ms /bin/bash prerender
USER prerender

WORKDIR /home/prerender/

COPY ./deploy/prerender/package*.json .
RUN npm ci

COPY ./deploy/prerender/server.js .

CMD ["/home/prerender/server.js"]
HEALTHCHECK --interval=60s --timeout=5s --start-period=1s --retries=2 CMD [ "curl", "http://localhost:3000/render?url=https://www.google.com/" ]
