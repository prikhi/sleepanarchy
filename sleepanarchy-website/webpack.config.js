const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const { spawn, spawnSync } = require("node:child_process");

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

module.exports = {
    mode: "development",
    entry: {
        index: "./index.js",
    },
    output: {
        filename: "[name].[contenthash].js",
        path: path.resolve(__dirname, "dist"),
        publicPath: "/",
        clean: true,
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: "Sleep Anarchy",
            meta: {
                "google-site-verification":
                    "mw_vy86UEcJ1tGnIBEG_hs77Zy3mo6VYsRYe_FB7Igg",
            },
        }),
        new SpagoWatchWebpackPlugin(),
    ],
    devServer: {
        static: "./dist",
        server: "spdy",
        historyApiFallback: true,
        proxy: {
            "/api": {
                target: "http://127.0.0.1:9001",
                pathRewrite: { "^/api": "" },
            },
        },
    },
    optimization: {
        runtimeChunk: "single",
    },
    stats: {
        colors: true,
        chunks: false,
    },
};
