global.SRC_FOLDER = 'src';
global.BUILD_FOLDER = 'build';
global.RELEASE_FOLDER = 'release';
global.TMP_FOLDER = 'tmp';

global.requireFromProject = function (moduleName) {
  return require(process.cwd() + '/' + moduleName)
}


global.config = {
  paths: {
    src: {
      index: SRC_FOLDER + '/index.html',
      assets: [SRC_FOLDER + '/assets/**/*', '!' + SRC_FOLDER + '/assets/images/**/*'],
      images: SRC_FOLDER + '/assets/images/**/*',
      scripts: SRC_FOLDER + '/modules/**/*.coffee',
      styles: SRC_FOLDER + '/styles/app.less',
      vendorStyles: SRC_FOLDER + '/styles/vendor.less',
      stylesGlob: SRC_FOLDER + '/styles/**/*.less',
      templates: SRC_FOLDER + '/modules/**/*.html',
      templatesHTML: SRC_FOLDER + '/modules/**/*.html',
      templatesCompiled: TMP_FOLDER + '/templates',
      livereload: [BUILD_FOLDER + '/**/*', '!' + BUILD_FOLDER + '/assets/**/*'],
      modules: './' + SRC_FOLDER + '/modules/index.coffee',
      node_modules: './' + "node_modules",
      packageJson: './package.json'
    },
    test: {
      scripts: './' + 'test'
    },
    dest: {
      test: TMP_FOLDER + '/test',
      build: {
        styles: BUILD_FOLDER,
        scripts: BUILD_FOLDER,
        images: BUILD_FOLDER + '/assets/images',
        assets: BUILD_FOLDER + '/assets',
        vendorAssets: BUILD_FOLDER + '/assets/vendor',
        index: BUILD_FOLDER,
        server: BUILD_FOLDER
      },
      release: {
        styles: RELEASE_FOLDER,
        scripts: RELEASE_FOLDER,
        images: RELEASE_FOLDER + '/assets/images',
        assets: RELEASE_FOLDER + '/assets',
        vendorAssets: RELEASE_FOLDER + '/assets/vendor',
        index: RELEASE_FOLDER,
        server: RELEASE_FOLDER
      }
    }
  },
  filenames: {
    build: {
      styles: 'bundle.css',
      vendorStyles: 'vendor.css',
      scripts: 'bundle.js',
      vendorScripts: 'vendor.js'
    },
    release: {
      styles: 'bundle.min.css',
      vendorStyles: 'vendor.min.css',
      scripts: 'bundle.min.js',
      vendorScripts: 'vendor.min.js'
    },
    templates: {
      compiled: 'templates.js',
      angular: {
        moduleName: 'app.templates',
        prefix: '',
        stripPrefix: 'app/'
      }
    }
  },
  packageJson: requireFromProject('package.json'),
  ports: {
    staticServer: 8080,
    livereloadServer: 35729
  }
};

var fs = require('fs');
if (fs.existsSync('gulpConfig.json')) {
  var _ = require('lodash');
  var extendify = require('extendify');
  _.extend = extendify({
    inPlace: true,
    arrays: "replace",
    booleans: "replace",
    numbers: "replace",
    strings: "replace"
  });

  var predefConfig = global.config;
  var overrideConfig = requireFromProject('gulpConfig.json');
  console.log("merging build configuration with: " + JSON.stringify(overrideConfig));
  var merged = _.extend(predefConfig, overrideConfig);
  global.config = merged;
}

