'use strict'

const $ = require('jquery');
const React = require('react');
const d3 = require('d3');

const ExpenseGraph = React.createClass({
    render: function() {

        d3.select('#thechart')
        /* var expenseNodes = this.props.expenses.map(function(expense) {
           console.log("Looping over expenses: "+expense);
           return (
           );
           }); */
      return (
          <div id="thechart">
          </div>
      )
  }
});

module.exports = ExpenseGraph;
