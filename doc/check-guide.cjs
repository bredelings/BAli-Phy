'use strict';

const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {liteAdaptor} = require('mathjax-full/js/adaptors/liteAdaptor.js');

// Check rendering assets without fetching ordinary external links. A successful PDF build
// alone does not establish that its fonts, stylesheets, or images were found.
function checkResource(filename, reference)
{
    const url = new URL(reference, pathToFileURL(filename));
    if (url.protocol === 'data:')
        return;
    if (url.protocol !== 'file:')
        throw new Error(`Remote rendering asset in ${filename}: ${reference}`);
    if (!fs.existsSync(url) || !fs.statSync(url).isFile())
        throw new Error(`Missing asset in ${filename}: ${reference}`);
}

// Reuse MathJax's HTML parser to check links and assets, avoiding a separate parser dependency.
// Keep these checks while rendering can succeed despite duplicate IDs or unresolved links.
function main()
{
    if (process.argv.length !== 3)
        throw new Error('Usage: node check-guide.cjs BUILD_DIR');
    const output = path.resolve(process.argv[2]);
    const adaptor = liteAdaptor();
    for (const name of ['README.html', 'README.print.html']) {
        const filename = path.join(output, name);
        const document = adaptor.parse(fs.readFileSync(filename, 'utf8'));
        const ids = new Set(), fragments = [];
        const nodes = [adaptor.root(document)];
        while (nodes.length) {
            const node = nodes.pop();
            if (node.kind.startsWith('#'))
                continue;
            nodes.push(...adaptor.childNodes(node));
            const id = adaptor.getAttribute(node, 'id');
            if (id) {
                if (ids.has(id))
                    throw new Error(`Duplicate ID in ${filename}: ${id}`);
                ids.add(id);
            }
            const href = adaptor.getAttribute(node, 'href') || '';
            if (href.startsWith('#'))
                fragments.push(decodeURIComponent(href.slice(1)));
            const src = adaptor.getAttribute(node, 'src');
            if (src)
                checkResource(filename, src);
            const rel = (adaptor.getAttribute(node, 'rel') || '').split(/\s+/);
            if (node.kind === 'link' && href && rel.some(value => ['stylesheet', 'icon', 'preload'].includes(value)))
                checkResource(filename, href);
        }
        for (const fragment of fragments)
            if (!ids.has(fragment))
                throw new Error(`Unresolved link in ${filename}: #${fragment}`);
    }
    for (const name of fs.readdirSync(output, {recursive: true})) {
        if (!name.endsWith('.css'))
            continue;
        const filename = path.join(output, name);
        for (const match of fs.readFileSync(filename, 'utf8').matchAll(/url\(([^)]+)\)/g))
            checkResource(filename, match[1].trim().replace(/^['"]|['"]$/g, ''));
    }
    console.log('Guide links and local assets verified.');
}

main();
