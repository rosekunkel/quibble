'use strict';

var quizServices = angular.module('quizServices', ['ngResource']);

quizServices.factory('Clues', ['$resource',
  function($resource){
    return $resource('questions/random', {}, {
      query: {method:'GET'}
    });
  }]);
