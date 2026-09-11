'use strict';

const assert = require('node:assert/strict');
const {renderMath} = require('./render-math-svg.cjs');

// Full-guide builds cannot detect accidental math in code or lost anchors and baselines.
// Keep this contract check while browser and PDF rendering share TeX delimiter rules.
const input = String.raw`<!DOCTYPE html><html><head><link rel="stylesheet" href="user-guide.css"></head>
<body><h1 id="section">A &amp; B</h1><a href="#section">Jump</a><p>Before
\(L^2\) after.</p>\[\frac{a}{b}\]<dl><dt>C1.P\(n\).fastas</dt></dl>
<p>$HOME costs $5. $x$ is literal. \begin{equation}x\end{equation}</p>
<pre>\(code\)</pre><code>\(inline\)</code><span class="userinput">\(input\)</span></body></html>`;
const result = renderMath(input);
assert.equal((result.match(/<svg\b/g) || []).length, 3);
assert.doesNotMatch(result, /<math\b|<mjx-container\b|<use\b|MJX-SVG-styles/);
assert.match(result, /<h1 id="section">A &amp; B<\/h1>/);
assert.match(result, /href="#section"/);
assert.match(result, /href="user-guide.css"/);
assert.match(result, /Before\s*<svg/);
assert.match(result, /<\/svg>\s*after\./);
assert.match(result, /C1\.P<svg.*<\/svg>\.fastas/);
assert.ok(result.includes(String.raw`$HOME costs $5. $x$ is literal. \begin{equation}x\end{equation}`));
assert.ok(result.includes(String.raw`<pre>\(code\)</pre><code>\(inline\)</code>`));
assert.ok(result.includes(String.raw`<span class="userinput">\(input\)</span>`));
assert.match(result, /vertical-align: -[\d.]+ex/);
assert.match(result, /<div style="text-align: center; margin: 1em 0;"><svg/);
assert.match(result, /<path /);
assert.throws(() => renderMath(String.raw`<html><body>\(\frac{a}\)</body></html>`), /Cannot render TeX/);
