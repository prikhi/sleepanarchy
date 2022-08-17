import hljs from "highlight.js/lib/core";

import apache from "highlight.js/lib/languages/apache";
import bash from "highlight.js/lib/languages/bash";
import c from "highlight.js/lib/languages/c";
import css from "highlight.js/lib/languages/css";
import diff from "highlight.js/lib/languages/diff";
import dockerfile from "highlight.js/lib/languages/dockerfile";
import elm from "highlight.js/lib/languages/elm";
import haskell from "highlight.js/lib/languages/haskell";
import html from "highlight.js/lib/languages/xml";
import javascript from "highlight.js/lib/languages/javascript";
import json from "highlight.js/lib/languages/json";
import less from "highlight.js/lib/languages/less";
import nginx from "highlight.js/lib/languages/nginx";
import nix from "highlight.js/lib/languages/nix";
import php from "highlight.js/lib/languages/php";
import python from "highlight.js/lib/languages/python";
import sql from "highlight.js/lib/languages/sql";
import vim from "highlight.js/lib/languages/vim";
import yaml from "highlight.js/lib/languages/yaml";

const langs = [
    [apache, "apache", "apacheconf"],
    [bash, "bash", "zsh"],
    [c, "c"],
    [css, "css"],
    [diff, "diff"],
    [dockerfile, "dockerfile", "docker"],
    [elm, "elm"],
    [haskell, "haskell"],
    [html, "html"],
    [javascript, "javascript"],
    [json, "json"],
    [less, "less"],
    [nginx, "nginx"],
    [nix, "nix"],
    [php, "php"],
    [python, "python"],
    [sql, "sql", "psql"],
    [vim, "vim", "nvim"],
    [yaml, "yaml", "yml"],
];
langs.forEach(([lang, ...names]) =>
    names.forEach((name) => hljs.registerLanguage(name, lang))
);

export const highlight = (str) => (language) => {
    try {
        return hljs.highlight(str, { language }).value;
    } catch (_) {
        return str;
    }
};
