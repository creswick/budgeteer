'use strict'

const $ = require('jquery');
const React = require('react');
const Expense = require('./Expense.jsx');

const ExpenseList = React.createClass({
  getInitialState: function() {
    return {data: []};
  },

  componentDidMount: function() {
    $.ajax({
      url: '/expenses', //this.props.url,
      dataType: 'json',
      cache: false,
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(this.props.url, status, err.toString());
      }.bind(this)
    });
  },

  render: function() {
    var expenseNodes = this.state.data.map(function(expense) {
      return (
          <Expense expense={expense} />
      );
    });
    return (
      <div>
        <h2>Budgeteer: List of Expenses</h2>
          <ul>
             {expenseNodes}
          </ul>
      </div>
    )
  }
});

module.exports = ExpenseList;
