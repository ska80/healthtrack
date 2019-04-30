"use strict";

var AsyncStorage = require('@react-native-community/async-storage').default;

exports.storeData_ = function(key) {
  return function(val) {
    return function (onError, onSuccess) {
      AsyncStorage
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
    AsyncStorage
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
