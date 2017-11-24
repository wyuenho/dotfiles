module.exports = {
  aliases: {
    $: 'jquery',
    _: 'lodash',
    styles: './{filename}.css',
    environments: [
      'es2017',
      'node',
      'browser',
      'jasmine',
      'jest',
      'worker',
      'mocha',
      'qunit',
      'serviceworker',
      'webextensions',
      'devtools'
    ],
    importDevDependencies: true
  }
};
