export default {
  testEnvironment: 'node',
  transform: {},
  collectCoverageFrom: [
    'js-src/**/*.js',
    '!js-src/index.js'
  ],
  testMatch: [
    '**/test/**/*.test.js'
  ]
};