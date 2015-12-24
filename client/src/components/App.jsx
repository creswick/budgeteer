'use strict';

const React = require('react');
const { Router, Route, Link } = require('react-router');
const ExpenseList = require('./ExpenseList.jsx');

const sampleData =
[ { "name": "Replace the roof"
  , "replace_date": "Dec 24 11:24:36 PST 2018"
  , "replace_cost": 10000
  , "lifetime": { "count" : 20
                , "unit": "Year" }}

, { "name": "Replace matress"
  , "replace_date": "Aug 24 2011"
  , "replace_cost": 1500
  , "lifetime": { "count" : 10
                , "unit": "Year" }}

, { "name": "GitHub hosting"
  , "replace_date": "Dec 12 2015"
  , "replace_cost": 8
  , "lifetime": { "count" : 1
                , "unit": "Year" }}

, { "name": "Rental mortgage"
  , "replace_date": "Dec 12 2015"
  , "replace_cost": 2400
  , "lifetime": { "count" : 1
                , "unit": "Month" }}

];


const App = React.createClass({
    getInitialState: function() {
        return {data: []};
    },

    componentDidMount: function() {
        this.setState({data: sampleData});
        /* $.ajax({
           url: '/expenses', //this.props.url,
           dataType: 'json',
           cache: false,
           success: function(data) {
           this.setState({data: data});
           }.bind(this),
           error: function(xhr, status, err) {
           console.error(this.props.url, status, err.toString());
           }.bind(this)
           }); */
    },

    render: function() {
        return (
            <div>
                <h1>App</h1>
                <ul>
                    {/* need to pass in this.state.data */}
                    <li><Link to="/expenses">Expenses</Link></li>
                    <li><Link to="/docs">API Docs</Link></li>
                </ul>
                <ExpenseList expenses={this.state.data}/>
                {this.props.children}
            </div>
        )}
});

module.exports = App;
