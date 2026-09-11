'use strict';

const assert = require('node:assert/strict');
const {renderMath} = require('./render-math-svg.cjs');

// Whole-guide builds cannot reliably detect lost anchors, flattened baselines, or error glyphs.
// Keep this small contract check while the PDF pipeline substitutes SVG for MathML.
const input = `<!DOCTYPE html><html><head><link rel="stylesheet" href="user-guide.css"></head>
<body><h1 id="section">A &amp; B</h1><a href="#section">Jump</a><p>Before
<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>L</mi><mn>2</mn></msup></math>
after.</p><math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
<mfrac><mi>a</mi><mi>b</mi></mfrac></math></body></html>`;
const result = renderMath(input);
assert.equal((result.match(/<svg\b/g) || []).length, 2);
assert.doesNotMatch(result, /<math\b|<mjx-container\b|<use\b|MJX-SVG-styles/);
assert.match(result, /<h1 id="section">A &amp; B<\/h1>/);
assert.match(result, /href="#section"/);
assert.match(result, /href="user-guide.css"/);
assert.match(result, /Before\s*<svg/);
assert.match(result, /<\/svg>\s*after\./);
assert.match(result, /vertical-align: -[\d.]+ex/);
assert.match(result, /<div style="text-align: center; margin: 1em 0;"><svg/);
assert.match(result, /<path /);
assert.throws(() => renderMath('<html><body><math><msup><mi>x</mi></msup></math></body></html>'),
              /Cannot render MathML/);
