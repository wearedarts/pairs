'use strict'
require('./index.html')
require('./styles/style.scss')

const { Elm } = require('./Main.elm')

const urlParams = new URLSearchParams(window.location.search)
const setName = urlParams.get('set') ? urlParams.get('set') : 'colour-shade'
const cardSet = require('./json/' + setName + '.json')

Elm.Main.init({flags: {cardJson: cardSet, filename: setName}})
