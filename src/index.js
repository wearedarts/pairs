'use strict'
require('./index.html')
require('./styles/style.scss')

const { Elm } = require('./Main.elm')

const urlParams = new URLSearchParams(window.location.search)
const setName = urlParams.get('setName') ? urlParams.get('setName') : 'set1'
const cardSet = require('./json/' + setName + '.json')

Elm.Main.init({flags: cardSet})
