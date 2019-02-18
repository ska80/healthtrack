"use strict";

var RN = require('react-native');

exports.storeData_ = function(key) {
  return function(val) {
    return function (onError, onSuccess) {
      RN.AsyncStorage
        .setItem(key, val)
        .then(onSuccess)
        .catch(onError);

      return function (cancelError, cancelerError, cancelerSuccess) {
        // nothing to do, just succeed
        cancelerSuccess();
      };
    };
  };
};

exports.retrieveData_ = function (key) {
  return function (onError, onSuccess) { // and callbacks
    RN.AsyncStorage
      .getItem(key)
      .then(function(val) {
        return onSuccess(val || "");
      })
      .catch(onError);

    return function (cancelError, cancelerError, cancelerSuccess) {
      // nothing to do, just succeed
      cancelerSuccess();
    };
  };
};
