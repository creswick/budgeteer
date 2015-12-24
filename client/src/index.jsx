'use strict'

const $ = require('jquery');

// bootstrap does not export anything:
require("bootstrap-webpack");

const React = require('react');
const { render } = require('react-dom');
const { Router, Route, Link } = require('react-router');

const App = require('./components/App.jsx');
const ExpenseList = require('./components/ExpenseList.jsx');
const Docs = require('./components/Docs.jsx');

render((
  <Router>
    <Route path="/" component={App}>
      <Route path="expenses" component={ExpenseList} />
      <Route path="docs" component={Docs} />
    </Route>
  </Router>
), document.body);
