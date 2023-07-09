#!/usr/bin/env node

var prerender = require("prerender");

var server = prerender({
    chromeLocation: "/usr/bin/chromium",
    chromeFlags: [
        "--headless",
        "--disable-gpu",
        "--remote-debugging-port=9222",
        "--hide-scrollbars",
        "--no-sandbox",
    ],
    logRequests: false,
});

server.use(prerender.sendPrerenderHeader());
server.use(prerender.removeScriptTags());
server.use(prerender.httpHeaders());
server.use(prerender.blockResources());
server.use(require("prerender-memory-cache"));

server.start();
