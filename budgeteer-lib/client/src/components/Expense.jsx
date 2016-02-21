'use strict'

const React = require('react');

const Expense = React.createClass ({
  render() {
    return (
      <div>
        <h2>{this.props.expense.name}</h2>
        <ul>
            <li>Replace Date: {this.props.expense.replace_date}</li>
            <li>Replace Cost: {this.props.expense.replace_cost}</li>
            <li>Lifetime: {this.props.expense.lifetime.count} {this.props.expense.lifetime.unit}</li>
        </ul>
      </div>
    )
  }
});

module.exports = Expense;
