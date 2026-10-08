#!/usr/bin/env python3
"""Generate a function reference as Pandoc Markdown, independently of the website framework."""

import argparse
import html
import itertools
import json
import posixpath
import re
import shlex
import sys
from pathlib import Path
from urllib.parse import quote

SOURCE_URL = 'https://github.com/bredelings/BAli-Phy/blob/master/bindings/'
STATE_DEFAULTS = {
    'alphabet': 'The alphabet in the current context',
    'topology': 'The topology in the current context',
    'tree': 'The tree in the current context',
    'branch_categories': 'The branch categories in the current context',
    'branch_category_vectors': 'The branch category vectors in the current context',
}
# Match quoted strings first, so an @ inside a string is never treated as a reference.
DEFAULT_TOKEN = re.compile(r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|@([A-Za-z_][A-Za-z_0-9]*)')


def code(text):
    return '<code>' + html.escape(str(text)) + '</code>'


def link(label, target):
    return '<a href="' + html.escape(target, quote=True) + '">' + html.escape(label) + '</a>'


# Binding prose uses Markdown code spans and intentional line breaks (including equations).
# Escape angle brackets outside code spans; Pandoc already escapes the contents of code spans.
def prose(text):
    pieces, end = [], 0
    for match in re.finditer(r'(`+)(.*?)\1', text, re.DOTALL):
        pieces.extend([html.escape(text[end:match.start()]), match[0]])
        end = match.end()
    pieces.append(html.escape(text[end:]))
    # Multiplication between operands is literal; preserve emphasis such as *neutral*.
    # Odd-numbered pieces are code spans, whose contents Pandoc already treats literally.
    for i in range(0, len(pieces), 2):
        pieces[i] = re.sub(r'(?<=[\w)\]])\*(?=[\w(])', '&#42;', pieces[i])
    blocks = []
    # Consecutive indented source lines form explanatory blocks, not code listings.
    # Keep their explicit line breaks while letting the surrounding prose wrap normally.
    for indented, lines in itertools.groupby(''.join(pieces).splitlines(),
                                             key=lambda line: bool(line.strip()) and line[:1].isspace()):
        block = '  \n'.join(line.lstrip() if indented else line for line in lines)
        if indented:
            block = '<div class="prose-indent">\n\n' + block + '\n\n</div>'
        blocks.append(block)
    return '\n\n'.join(blocks)


def relative_url(current, target):
    return quote(posixpath.relpath(str(target), str(current)), safe='/') + '/'


# Retain source ordering while following the CLI's canonical-name/alias precedence.
def load_bindings(root):
    entries = {}
    for path in sorted(root.rglob('*.json')):
        slug = path.relative_to(root).with_suffix('')
        entry = json.loads(path.read_text())
        if entry['name'] in (b['name'] for b in entries.values()):
            raise ValueError(f'Duplicate binding name: {entry["name"]}')
        names = [arg['name'] for arg in entry['args']]
        if len(names) != len(set(names)):
            raise ValueError(f'Duplicate argument name in {path}')
        entries[slug] = entry
    lookup = {}
    for field in ('name', 'synonyms', 'deprecated-synonyms'):
        for slug, entry in entries.items():
            names = [entry['name']] if field == 'name' else entry.get(field, [])
            for name in names:
                lookup.setdefault(name, slug)
    return entries, lookup


# Render argument references as links without interpreting the expression itself.
# The caller collects changed expressions into a single disclosure with their original syntax.
def default_text(value, names, warnings):
    state = re.fullmatch(r'get_state\((\w+)\)', value)
    if state and state[1] in STATE_DEFAULTS:
        rendered = html.escape(STATE_DEFAULTS[state[1]])
    else:
        parts, end = [], 0
        for match in DEFAULT_TOKEN.finditer(value):
            parts.append(html.escape(value[end:match.start()]))
            name = match[1]
            if name in names:
                parts.append(f'<a class="argument-reference" href="#arg-{quote(name)}" '
                             f'title="Value of argument {html.escape(name)}">{html.escape(name)}</a>')
            else:
                if name:
                    warnings.append(f'Unknown argument reference @{name}')
                parts.append(html.escape(match[0]))
            end = match.end()
        parts.append(html.escape(value[end:]))
        rendered = '<code>' + ''.join(parts) + '</code>'
    return rendered


# Citations in the bindings are either prose or a small structured article record.
def citation_text(citation):
    if isinstance(citation, str):
        return html.escape(citation)
    authors = '; '.join(a['name'] for a in citation['author'])
    journal = citation['journal']
    issue = f'({journal["number"]})' if journal.get('number') else ''
    text = (f'{authors} ({citation["year"]}). {citation["title"]}. '
            f'{journal["name"]} {journal["volume"]}{issue}: {journal["pages"]}.')
    links = [link('Article', x['url']) for x in citation.get('link', [])]
    bases = {'doi': 'https://doi.org/', 'pmid': 'https://pubmed.ncbi.nlm.nih.gov/',
             'pmcid': 'https://pmc.ncbi.nlm.nih.gov/articles/'}
    for item in citation.get('identifier', []):
        if item['type'] in bases:
            links.append(link(item['type'].upper() + ': ' + item['id'],
                              bases[item['type']] + quote(item['id'], safe='/')))
    return html.escape(text) + (' ' + ' · '.join(links) if links else '')


# Breadcrumbs use directory URLs so the Markdown is independent of PHP or static HTML output.
def navigation(slug):
    crumbs = [link('Function reference', relative_url(slug, Path('.')))]
    for parent in reversed(slug.parents):
        if parent != Path('.'):
            crumbs.append(link(parent.name.capitalize() if len(parent.parts) == 1 else parent.name,
                               relative_url(slug, parent)))
    return '<nav class="reference-breadcrumbs" aria-label="Breadcrumb">' + ' / '.join(crumbs) + '</nav>'


# Group each typed argument with its punctuation so CSS can wrap at real argument boundaries.
# Ordinary text spaces preserve a readable signature when copied or read without the stylesheet.
def signature_html(entry):
    arguments = [html.escape(arg['name']) + ': <span class="signature-type">'
                 + html.escape(arg['type']) + '</span>' for arg in entry['args']]
    name = html.escape(entry['name'])
    if 'fixity' in entry and len(arguments) == 2:
        prefix = ''
        units = ['(' + arguments[0] + ')', name + ' (' + arguments[1] + ')']
    else:
        prefix = '<span class="signature-unit">' + name + ('(' if arguments else '()') + '</span><wbr>'
        if not arguments:
            prefix += ' '
        units = [argument + (',' if i < len(arguments) - 1 else ')')
                 for i, argument in enumerate(arguments)]
    units.append('→ <span class="signature-type">' + html.escape(entry['result_type']) + '</span>')
    signature = prefix + ' '.join('<span class="signature-unit">' + unit + '</span>' for unit in units)
    return '<div class="reference-signature"><code>' + signature + '</code></div>'


# Render only the public documentation fields; implementation expressions are deliberately omitted.
def binding_page(slug, entry, lookup, warnings):
    args = entry['args']
    names = [arg['name'] for arg in args]
    parts = ['::: {.reference-entry}', navigation(slug), '<header class="reference-header">',
             '# ' + code(entry['name'])]
    if entry.get('title'):
        parts.append('<p class="reference-subtitle">' + html.escape(entry['title']) + '</p>')
    parts.append(signature_html(entry))
    metadata = []
    if entry.get('constraints'):
        metadata.append('**Type constraints:** ' + ', '.join(map(code, entry['constraints'])))
    if 'fixity' in entry:
        fixity = entry['fixity']
        metadata.append(f'**Operator:** precedence {fixity["precedence"]}; '
                     f'associativity: {html.escape(fixity["associativity"])}.')
    if entry.get('synonyms'):
        metadata.append('**Aliases:** ' + ', '.join(map(code, entry['synonyms'])))
    if metadata:
        parts += ['<div class="reference-metadata">', *metadata, '</div>']
    parts += ['</header>', '## Arguments']
    if not args:
        parts.append('This entry has no arguments.')
    notes = []
    if any(match[1] in names for arg in args
           for match in DEFAULT_TOKEN.finditer(arg.get('default_value', ''))):
        notes.append('Underlined names in default expressions refer to other arguments.')
    if any(arg.get('default_value', '').startswith('~') for arg in args):
        notes.append('A default beginning with `~` specifies a prior distribution.')
    if notes:
        parts += ['<div class="argument-note">', ' '.join(notes), '</div>']
    originals = []
    if args:
        parts.append('<dl class="arguments">')
    for arg in args:
        label = html.escape(arg['name'])
        if arg.get('description'):
            label += '<span class="argument-colon">:</span>'
        parts += ['<div class="argument">', f'<dt id="arg-{quote(arg["name"])}"><code>{label}</code></dt>',
                  '<dd class="argument-description">']
        if arg.get('description'):
            parts.append(prose(arg['description']))
        parts.append('</dd>')
        default = arg.get('default_value')
        if default is not None:
            rendered = default_text(default, names, warnings)
            parts += ['<dd class="argument-default">',
                      '<strong class="default-label">Default:</strong> ' + rendered, '</dd>']
            if rendered != code(default):
                originals.append(f'<dt>{code(arg["name"])}</dt><dd><pre>{code(default)}</pre></dd>')
        parts.append('</div>')
    if args:
        parts.append('</dl>')
    if originals:
        parts += ['<details class="original-defaults"><summary>Original default expressions</summary>',
                  '<dl>' + '\n'.join(originals) + '</dl>', '</details>']
    if entry.get('description'):
        parts += ['## Description', '<div class="reference-description">',
                  prose(entry['description']), '</div>']
    if entry.get('examples'):
        parts.append('## Examples')
        parts.extend('<pre>' + code(example) + '</pre>' for example in entry['examples'])
    if entry.get('citation'):
        parts += ['## Citation', citation_text(entry['citation'])]
    related = []
    for name in dict.fromkeys(entry.get('see', [])):
        if name in lookup:
            related.append(link(name, relative_url(slug, lookup[name])))
        else:
            warnings.append(f'Unknown related entry: {name}')
            related.append(code(name))
    if related:
        parts += ['## See also', ', '.join(related)]
    parts.append('<footer class="reference-footer">')
    parts += ['Terminal help: ' + code('bali-phy help ' + shlex.quote(entry['name'])) + ' · '
              + link('Binding source', SOURCE_URL + quote(str(slug) + '.json', safe='/')),
              '</footer>', ':::']
    return '\n\n'.join(parts)


# Use ordinary YAML metadata and Markdown, with HTML only where links or layout need attributes.
def write_page(output, slug, metadata, body):
    path = output / slug / 'index.md'
    path.parent.mkdir(parents=True, exist_ok=True)
    header = '\n'.join(key + ': ' + json.dumps(value, ensure_ascii=False) for key, value in metadata.items())
    path.write_text('---\n' + header + '\n---\n\n' + body + '\n')


# Every index works without JavaScript; the website progressively adds a name/alias/title filter.
def index_page(slug, title, selected, entries, groups):
    parts = ([] if slug == Path('.') else [navigation(slug)]) + ['# ' + html.escape(title)]
    if slug == Path('.'):
        parts += ['Models, probability distributions, and functions available in model expressions.',
                  'Browse by category below or use the ' + link('alphabetical index', 'alphabetical/') + '.',
                  'Entries describe the current master branch. For help matching your installed version, '
                  'use `bali-phy help NAME`.']
    children = [group for group in groups if group.parent == slug and group != slug]
    if children:
        parts.append('\n'.join('- ' + link(group.name.capitalize() if len(group.parts) == 1 else group.name,
                                           relative_url(slug, group)) for group in sorted(children)))
    parts.append('<div class="reference-filter" hidden>\n'
                 '<label for="reference-query">Filter by name, alias, or title</label>\n'
                 '<input id="reference-query" type="search" aria-controls="reference-entries">\n'
                 '<p id="reference-count" role="status"></p>\n</div>')
    parts.append('<div id="reference-entries">')
    ordered = sorted(selected, key=lambda p: entries[p]['name'].casefold())
    if slug == Path('.'):
        categories = {'models': 0, 'distributions': 1, 'functions': 2}
        ordered.sort(key=lambda p: categories.get(p.parts[0], 3))
    previous_group = None
    for target in ordered:
        group = target.parts[0] if slug == Path('.') else ''
        if group != previous_group:
            if previous_group is not None:
                parts.append('</ul></section>')
            parts.append('<section class="reference-group">')
            if group:
                parts.append('<h2>' + link(group.capitalize(), relative_url(slug, Path(group))) + '</h2>')
            parts.append('<ul class="reference-entries">')
            previous_group = group
        entry = entries[target]
        names = [entry['name']] + entry.get('synonyms', [])
        deprecated = entry.get('deprecated-synonyms', [])
        visible_search = ' '.join(names + [entry.get('title', '')])
        search = visible_search + ' ' + ' '.join(deprecated)
        attributes = {'search': search, 'visible-search': visible_search,
                      'names': json.dumps(names), 'deprecated': json.dumps(deprecated)}
        item = '<li ' + ' '.join(f'data-{key}="{html.escape(value, quote=True)}"'
                                for key, value in attributes.items()) + '>'
        item += link(entry['name'], relative_url(slug, target))
        if entry.get('title'):
            item += ' — ' + html.escape(entry['title'])
        if entry.get('synonyms'):
            item += '<span class="reference-aliases">Aliases: ' + ', '.join(map(code, entry['synonyms'])) + '</span>'
        if deprecated:
            item += '<span class="deprecated-match" hidden></span>'
        parts.append(item + '</li>')
    if previous_group is not None:
        parts.append('</ul></section>')
    parts.append('</div>')
    return '\n\n'.join(parts)


# Validate the catalogue before writing it, then report documentation gaps without inventing prose.
def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path, help='directory for generated Markdown')
    parser.add_argument('--bindings', type=Path, default=Path(__file__).resolve().parents[1] / 'bindings')
    options = parser.parse_args()
    entries, lookup = load_bindings(options.bindings)
    groups = {parent for slug in entries for parent in slug.parents}
    indexes = groups | {Path('alphabetical')}
    if indexes & entries.keys():
        raise ValueError('A binding URL collides with an index URL')
    if not entries:
        raise ValueError('No bindings found')
    missing_main = missing_args = 0
    for slug, entry in entries.items():
        warnings = []
        body = binding_page(slug, entry, lookup, warnings)
        for warning in warnings:
            print(f'{slug}: {warning}', file=sys.stderr)
        write_page(options.output, slug, {'title': entry['name'], 'name': entry['name'],
                   'summary': entry.get('title', ''), 'category': str(slug.parent),
                   'aliases': entry.get('synonyms', []),
                   'deprecated-aliases': entry.get('deprecated-synonyms', []),
                   'source': 'bindings/' + str(slug) + '.json'}, body)
        missing_main += not bool(entry.get('description'))
        missing_args += sum(not bool(arg.get('description')) for arg in entry['args'])
    for slug in sorted(indexes):
        title = 'Function reference' if slug == Path('.') else (slug.name.capitalize() if len(slug.parts) == 1
                                                           else slug.name)
        if slug == Path('alphabetical'):
            title = 'Alphabetical index'
        selected = [p for p in entries if slug in p.parents or slug == Path('alphabetical')]
        write_page(options.output, slug, {'title': title}, index_page(slug, title, selected, entries, groups))
    print(f'Generated {len(entries)} entries and {len(indexes)} indexes. '
          f'Documentation gaps: {missing_main} main descriptions, {missing_args} argument descriptions.',
          file=sys.stderr)


if __name__ == '__main__':
    main()
