'use strict';

// Browser and PDF rendering share delimiters, TeX packages, and literal-code exclusions.
// SVG paths need no downloadable math fonts or dynamically loaded TeX extensions.
const guideMathJax = {
    tex: {
        inlineMath: [['\\(', '\\)']],
        displayMath: [['\\[', '\\]']],
        packages: ['base', 'ams'],
        processEnvironments: false,
        processEscapes: false,
    },
    options: {
        skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code', 'annotation', 'annotation-xml'],
        ignoreHtmlClass: 'mathjax_ignore|userinput',
    },
    svg: {fontCache: 'none'},
};

if (typeof module === 'object' && module.exports)
    module.exports = guideMathJax;
else
    window.MathJax = guideMathJax;
