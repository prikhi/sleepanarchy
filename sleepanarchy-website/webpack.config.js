const path = require("path");
const { spawn, spawnSync } = require("node:child_process");

const CspHtmlWebpackPlugin = require("csp-html-webpack-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ImageMinimizerWebpackPlugin = require("image-minimizer-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const RobotsTxtPlugin = require("robotstxt-webpack-plugin");
const { SubresourceIntegrityPlugin } = require("webpack-subresource-integrity");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");

class SpagoWatchWebpackPlugin {
    watchPid = null;
    PLUGIN_NAME = "Spago";

    apply(compiler) {
        const logger = compiler.getInfrastructureLogger(this.PLUGIN_NAME);
        compiler.hooks.environment.tap(this.PLUGIN_NAME, () => {
            logger.info("Running Initial Build");
            const args =
                compiler.options.mode === "production"
                    ? ["bundle-app", "-y"]
                    : ["bundle-app"];
            const buildPid = spawnSync("spago", args);
            this.logPidOutput(logger, buildPid.stdout);
            this.logPidOutput(logger, buildPid.stderr);
        });
        compiler.hooks.watchRun.tapAsync(this.PLUGIN_NAME, (_, callback) => {
            // TODO: watch `spago.dhall` - if changes, kill & re-run watcher
            if (this.watchPid === null) {
                logger.info("Starting Spago Build Server");
                this.watchPid = spawn("spago", ["bundle-app", "-w"]);
                this.watchPid.stdout.on("data", (data) => {
                    this.logPidOutput(logger, data);
                });
                this.watchPid.stderr.on("data", (data) => {
                    this.logPidOutput(logger, data);
                });
            }
            callback();
        });
        compiler.hooks.shutdown.tapAsync(this.PLUGIN_NAME, (callback) => {
            if (this.watchPid !== null) {
                logger.info("Killing Spago Build Server");
                this.watchPid.kill("SIGINT");
                this.watchPid = null;
            }
            callback();
        });
    }

    logPidOutput(logger, data) {
        const str = data.toString();
        const lines = str.split(/\r?\n/);
        lines.forEach((line) => logger.info(line));
    }
}

module.exports = (env, _) => {
    const isProduction = !env.WEBPACK_SERVE;
    const metaDescription =
        "Guides, blog posts, & a link collection on Haskell, Programming, System Administration, & Life.";
    return {
        mode: "development",
        entry: {
            index: "./index.js",
            styles: ["./src/skylighting-molokai.css", "./src/styles.sass"],
        },
        output: {
            filename: "[name].[contenthash].js",
            path: path.resolve(__dirname, "dist"),
            publicPath: "/",
            crossOriginLoading: "anonymous",
            clean: true,
        },
        module: {
            rules: [
                {
                    test: /\.s[ac]ss$/i,
                    use: [
                        isProduction
                            ? MiniCssExtractPlugin.loader
                            : "style-loader",
                        "css-loader",
                        "sass-loader",
                    ],
                },
                {
                    test: /\.css$/i,
                    use: [
                        isProduction
                            ? MiniCssExtractPlugin.loader
                            : "style-loader",
                        "css-loader",
                    ],
                },
            ],
        },
        plugins: [
            new SpagoWatchWebpackPlugin(),
            new HtmlWebpackPlugin({
                title: "Sleep Anarchy",
                meta: {
                    "google-site-verification":
                        "mw_vy86UEcJ1tGnIBEG_hs77Zy3mo6VYsRYe_FB7Igg",
                    description: metaDescription,
                    "og:description": {
                        property: "og:description",
                        content: metaDescription,
                    },
                    "og:site_name": {
                        property: "og:site_name",
                        content: "Sleep Anarchy",
                    },
                },
            }),
            new SubresourceIntegrityPlugin({
                hashFuncNames: ["sha512"],
            }),
            new CspHtmlWebpackPlugin(
                {
                    "connect-src": "'self'",
                    "font-src": "'self'",
                    "frame-src": "'self'",
                    "img-src": "'self'",
                    "object-src": "'none'",
                    "script-src": "'self'",
                    "style-src": ["'self'", "'unsafe-inline'"],
                    "upgrade-insecure-requests": [],
                },
                {
                    enabled: isProduction,
                    nonceEnabled: {
                        "style-src": false,
                    },
                }
            ),
            new MiniCssExtractPlugin({
                filename: "[name].[contenthash].css",
                chunkFilename: "[id].[contenthash].css",
            }),
            new RobotsTxtPlugin({
                policy: [
                    {
                        userAgent: "*",
                        crawlDelay: 2,
                        allow: "/",
                        disallow: "/admin",
                    },
                ],
                sitemap: "https://sleepanarchy.com/sitemap.xml",
                host: "https://sleepanarchy.com",
            }),
            new FaviconsWebpackPlugin({
                logo: "src/favicon.svg",
                prefix: "",
                inject: true,
                favicons: {
                    appName: "Sleep Anarchy",
                    background: "#1b1d1e",
                    theme_color: "#1b1d1e",
                    icons: {
                        favicons: true,
                        android: true,
                        appleIcon: true,
                    },
                },
            }),
            new ImageMinimizerWebpackPlugin({
                minimizer: {
                    implementation: ImageMinimizerWebpackPlugin.imageminMinify,
                    options: {
                        plugins: [["optipng", { optimizationLevel: 7 }]],
                    },
                },
            }),
        ],
        devServer: {
            static: "./dist",
            server: "spdy",
            historyApiFallback: true,
            hot: true,
            proxy: {
                "/api": {
                    target: "http://127.0.0.1:9001",
                    pathRewrite: { "^/api": "" },
                },
                "/media": {
                    target: "http://127.0.0.1:9001/",
                },
                "/sitemap.xml": {
                    target: "http://127.0.0.1:9001/sitemap.xml",
                    pathRewrite: { ".*": "" },
                },
                "/l/": {
                    target: "http://127.0.0.1:9001",
                    pathRewrite: { "^/l/": "links/redirect/" },
                },
            },
        },
        optimization: {
            runtimeChunk: "single",
            usedExports: true,
            splitChunks: {
                chunks: "all",
            },
            minimizer: ["...", new CssMinimizerPlugin()],
        },
        stats: {
            colors: true,
            chunks: false,
        },
    };
};
