'use strict';

const fs = require('node:fs');
const {mathjax} = require('mathjax-full/js/mathjax.js');
const {MathML} = require('mathjax-full/js/input/mathml.js');
const {SVG} = require('mathjax-full/js/output/svg.js');
const {liteAdaptor} = require('mathjax-full/js/adaptors/liteAdaptor.js');
const {RegisterHTMLHandler} = require('mathjax-full/js/handlers/html.js');

const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);

// Replace MathML with self-contained SVG while preserving the surrounding HTML and links.
// Ordinary SVG elements let WeasyPrint position inline math without MathJax's browser CSS.
function renderMath(html)
{
    const document = mathjax.document(html, {
        InputJax: new MathML(),
        OutputJax: new SVG({fontCache: 'none'}),
        compileError: (_document, _math, error) => { throw error; },
        typesetError: (_document, _math, error) => { throw error; },
    });
    document.render();
    for (const math of document.math) {
        // Some malformed MathML compiles to an error node instead of throwing an exception.
        math.root.walkTree(node => {
            if (node.kind === 'merror')
                throw new Error(`Cannot render MathML: ${math.math}`);
        });
        const container = math.typesetRoot;
        const svg = adaptor.firstChild(container);
        if (adaptor.kind(svg) !== 'svg')
            throw new Error('MathJax did not produce an SVG equation');

        // Preserve SVG dimensions and its negative depth offset: together they align the
        // mathematical baseline with surrounding text. Display math keeps a centered block.
        const replacement = math.display ?
            adaptor.node('div', {style: 'text-align: center; margin: 1em 0;'}, [svg]) : svg;
        adaptor.replace(replacement, container);
    }
    // The generated MathJax stylesheet targets containers we have replaced with ordinary SVG.
    for (const style of adaptor.tags(adaptor.head(document.document), 'style')) {
        if (adaptor.getAttribute(style, 'id') === 'MJX-SVG-styles')
            adaptor.remove(style);
    }
    return adaptor.doctype(document.document) + '\n' + adaptor.outerHTML(adaptor.root(document.document));
}

// Write only after successful conversion, so equation errors cannot produce a partial output.
function main()
{
    const [input, output] = process.argv.slice(2);
    if (!input || !output || process.argv.length !== 4)
        throw new Error('Usage: node render-math-svg.cjs INPUT.html OUTPUT.html');
    fs.writeFileSync(output, renderMath(fs.readFileSync(input, 'utf8')));
}

if (require.main === module) {
    try {
        main();
    } catch (error) {
        console.error(`render-math-svg: ${error.message}`);
        process.exitCode = 1;
    }
}

module.exports = {renderMath};
