# syntax=docker/dockerfile:1

FROM node:20

RUN apt-get update && apt-get install -yqq dumb-init curl chromium

RUN useradd -ms /bin/bash prerender
USER prerender

WORKDIR /home/prerender/

COPY ./deploy/prerender/package*.json .
RUN npm ci

COPY ./deploy/prerender/server.js .

CMD ["dumb-init", "node", "/home/prerender/server.js"]
HEALTHCHECK --interval=60s --timeout=10s --start-period=5s --retries=2 CMD [ "curl", "http://localhost:3000/render?url=https://www.google.com/" ]
