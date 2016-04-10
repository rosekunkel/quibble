'use strict';

/* Controllers */

var quizControllers = angular.module('quizControllers', []);

quizControllers.controller('ClueListCtrl', ['$scope', 'Clues', function($scope, Clues) {
 
    $scope.entry = Clues.query(function() { 
    //console.log($scope.entry);
    $scope.content = $scope.entry.content[0];
    });
    $scope.index = 1;

    $scope.addClue = function() {
	if($scope.index < $scope.entry.content.length) {
	    $scope.content = $scope.content + " " +  $scope.entry.content[$scope.index];
	    $scope.index = $scope.index + 1;
	}
    }

    $scope.nextQuestionClick = function() {
	$scope.index = 1;
	 $scope.entry = Clues.query(function() { 
	     //console.log($scope.entry);
	     $scope.content = $scope.entry.content[0];
	 });

    }

}]);
