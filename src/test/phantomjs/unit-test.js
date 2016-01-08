/* Copyright © 2016 Dynamic Object Language Labs Inc.

   This software is licensed under the terms of the
   Apache License, Version 2.0 which can be found in
   the file LICENSE at the root of this distribution. */

var page = require('webpage').create();
var url = phantom.args[0];
var testsrun = false;

function exit(code) {
  setTimeout(function(){ phantom.exit(code); }, 0);
  phantom.onError = function(){};
}

page.onConsoleMessage = function (message) {
  console.log(message);
};

function runtests() {
  console.log("Loaded____: " + url);

  var result = page.evaluate(function() {
    return testing.runner.runtests(true);
  });

  testsrun = true;

  if (result != 0) {
    // console.log("*** Test failed! ***");
    exit(1);
  }
  else {
    // console.log("Test succeeded.");
    exit(0);
  }
}

page.onLoadFinished = function (status) {
  if (testsrun) {
    ; // console.log("PhantomJS called onLoadFinished twice")
  } else {
    if (status != "success") {
      console.log('Failed (' + status + ') to load ' + url);
      exit(1);
    } else {
      // console.log('Succeeded (' + status + ') to load ' + url);
      runtests();
    }
  }
};

function openCallback(status) {
  if (status != "success") {
    console.log('Failed (' + status + ') to open ' + url);
    phantom.exit(1);
  }
  // console.log("opened: " + url);
};

console.log("Loading...: " + url);

page.open(url, openCallback);
