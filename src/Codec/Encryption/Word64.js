
exports.showHex = function(a) {
  return ('00000000'+((a>>>0).toString(16))).slice(-8);
}

exports.readHex = function (a) {
  return parseInt(a, 16);
}
