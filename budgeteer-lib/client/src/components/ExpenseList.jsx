'use strict'

const $ = require('jquery');
const React = require('react');
const Expense = require('./Expense.jsx');

const ExpenseList = React.createClass({
  render: function() {
      var expenseNodes = this.props.expenses.map(function(expense) {
          console.log("Looping over expenses: "+expense);
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
