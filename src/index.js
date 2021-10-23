'use strict';
require('./index.html');
require('./styles/style.scss');

const { Elm } = require('./Main.elm');

const cardSet = require('./json/set1.json')

Elm.Main.init({flags: cardSet});
