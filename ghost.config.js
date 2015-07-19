// NOTE: the GHOST_CONTENT environment variable is defined in the Dockerfile
var path = require('path'),
    config;
config = {
    production: {
        url: 'http://nullyork.younix.us/ghost',
        database: {
            client: 'sqlite3',
            connection: {
                filename: path.join(process.env.GHOST_CONTENT, '/data/ghost.db')
            },
            debug: false
        },
        server: {
            host: '0.0.0.0',
            port: '2368'
        },
        paths: {
            contentPath: path.join(process.env.GHOST_CONTENT, '/')
        },
        privacy: {
            useGoogleFonts: false,
            useGravatar: false
        }
    },
    development: {
        url: 'http://localhost:2368/',
        database: {
            client: 'sqlite3',
            connection: {
                filename: path.join(process.env.GHOST_CONTENT, '/data/ghost-dev.db')
            },
            debug: true
        },
        server: {
            host: '127.0.0.1',
            port: '2368'
        },
        paths: {
            contentPath: path.join(process.env.GHOST_CONTENT, '/')
        },
        privacy: {
            useGoogleFonts: false,
            useGravatar: false
        }
    },
};
module.exports = config;
